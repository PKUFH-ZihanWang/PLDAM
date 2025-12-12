import pandas as pd
import os
import re

# =============================
# Config
# =============================

BASE_DIR = "/mnt/external_drive/盆脂数据结果"
OUTPUT_DIR = "/mnt/external_drive/数据清理结果"
DIAGNOSIS_FILE = "/mnt/external_drive/主医院/2023诊断表.xlsx"

COLUMNS_TO_REMOVE = [
    'diagnostics_Versions_PyRadiomics', 'diagnostics_Versions_Numpy', 'diagnostics_Versions_PyWavelet',
    'diagnostics_Versions_SimpleITK', 'diagnostics_Versions_Python',
    'diagnostics_Configuration_Settings', 'diagnostics_Configuration_EnabledImageTypes',
    'diagnostics_Image-original_Hash', 'diagnostics_Image-original_Dimensionality',
    'diagnostics_Image-original_Spacing', 'diagnostics_Image-original_Size',
    'diagnostics_Mask-original_Spacing', 'diagnostics_Mask-original_Size',
    'diagnostics_Mask-original_BoundingBox', 'diagnostics_Mask-original_CenterOfMassIndex',
    'diagnostics_Mask-original_CenterOfMass', 'ImagePath', 'MaskPath', 'hospital', 'type',
    'Pelvic', 'pelvic'
]


# =============================
# Functions
# =============================

def extract_content_in_parentheses(file_path: str):
    """Extract the text inside parentheses, used to get patient ID."""
    if not isinstance(file_path, str) or not file_path:
        return None
    match = re.search(r'\((.*?)\)', file_path)
    return match.group(1) if match else None


def check_for_condition(text: str):
    """Return 1 if contains '盆腔脂肪增多症', else 0."""
    return 1 if isinstance(text, str) and "盆腔脂肪增多症" in text else 0


def process_excel_file(input_file: str, output_dir: str):
    """Convert Excel file to CSV for easier merging."""
    print(f"→ Preprocessing: {input_file}")
    df = pd.read_excel(input_file)
    base_name = os.path.basename(input_file).rsplit('.', 1)[0]
    output_file = os.path.join(output_dir, f"{base_name}_handle.csv")
    os.makedirs(output_dir, exist_ok=True)
    df.to_csv(output_file, index=False)
    print(f"✓ Saved: {output_file}")
    return output_file


def remove_empty_rows_from_multiple_files(directory, columns_to_remove, merged_output_file, column_to_check='病人编号'):
    """Clean and merge multiple CSV files in a directory."""
    print(f"\n🧹 Cleaning directory: {directory}")

    file_names = [os.path.join(directory, f) for f in os.listdir(directory) if f.endswith('.csv')]
    dfs = []
    empty_row_indices = set()

    # --- Load and preprocess each file ---
    for file in file_names:
        df = pd.read_csv(file)
        base_name = os.path.basename(file)
        suffix = base_name.split('handle')[0]
        df.drop(columns=columns_to_remove, inplace=True, errors='ignore')
        exclude_columns = ['ID']
        df.columns = [f"{col}_{suffix}" if col not in exclude_columns else col for col in df.columns]
        dfs.append(df)
        empty_indices = df[df.isnull().any(axis=1)].index
        empty_row_indices.update(empty_indices)

    # --- Drop empty rows & merge ---
    clean_dfs = []
    for i, df in enumerate(dfs):
        cleaned_df = df.drop(index=empty_row_indices)
        base_name = os.path.basename(file_names[i]).rsplit('.', 1)[0]
        output_file = os.path.join(directory, f"{base_name}_clean.csv")
        cleaned_df.to_csv(output_file, index=False)
        clean_dfs.append(cleaned_df)

    merged_df = pd.concat(clean_dfs, axis=1)
    merged_df.to_csv(merged_output_file, index=False)
    print(f"Merged CSV saved: {merged_output_file}")
    return merged_df


def process_diagnosis_file(input_file, output_file, path_column):
    """Add gender, age, check date, and diagnosis result columns."""
    print(f"Processing diagnosis file: {input_file}")
    df = pd.read_excel(input_file)
    df['Gender'] = df['性别'].map({'男': 1, '女': 0})
    df['Age'] = df['年龄'].astype(str).str.replace('岁', '').astype(float)
    df['CheckTime'] = pd.to_datetime(df['检查时间'], errors='coerce').dt.strftime('%Y-%m-%d')
    df['diagnosis_result'] = df[path_column].apply(check_for_condition)
    df.to_excel(output_file, index=False)
    print(f"✓ Saved diagnosis data: {output_file}")
    return output_file


def merge_excel_files(feature_file, diagnosis_file, id_column, output_file):
    """Merge feature table with diagnosis table by patient ID."""
    print(f"Merging features with diagnosis: {feature_file}")
    df1 = pd.read_csv(feature_file)
    df2 = pd.read_excel(diagnosis_file)
    unique_df2 = df2.drop_duplicates(subset=[id_column], keep='first')
    merged_df = pd.merge(df1, unique_df2, on=id_column, how='left', sort=False)
    merged_df.to_excel(output_file, index=False)
    print(f"✓ Final merged file: {output_file}")


# =============================
# Main Execution
# =============================

if __name__ == '__main__':
    hospitals = ["北大第一医院", "健宫医院", "应急总医院", "密云医院", "复兴医院"]
    modalities = ["CT", "MRI"]

    for hospital in hospitals:
        for modality in modalities:
            input_dir = os.path.join(BASE_DIR, hospital, modality)
            if not os.path.exists(input_dir):
                continue

            handle_dir = os.path.join(input_dir, 'handle')
            os.makedirs(handle_dir, exist_ok=True)

            merged_output = os.path.join(handle_dir, 'merged_data.csv')

            # Step 1: Convert Excel files to CSV
            excel_files = [os.path.join(input_dir, f) for f in os.listdir(input_dir) if f.endswith('.xlsx')]
            for excel_file in excel_files:
                process_excel_file(excel_file, handle_dir)

            # Step 2: Remove empty rows & merge
            merged_df = remove_empty_rows_from_multiple_files(handle_dir, COLUMNS_TO_REMOVE, merged_output)

            # Step 3: Merge with diagnosis info
            diagnosis_clean = process_diagnosis_file(DIAGNOSIS_FILE, os.path.join(OUTPUT_DIR, "diagnosis_clean.xlsx"), "diagnosis_result")
            final_output = os.path.join(OUTPUT_DIR, f"{hospital}_{modality}_final.xlsx")
            merge_excel_files(merged_output, diagnosis_clean, "ID", final_output)

    print("\n✅ All hospitals and modalities processed successfully!")
