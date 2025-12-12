import os
import re
import pandas as pd

# ---------------------------
# Utility functions
# ---------------------------

def extract_content_in_parentheses(text):
    """
    Extract patient ID or content inside parentheses from a file path string.
    Example: "/path/CHANG GUI HUA (P11681455)/Image.nrrd" → "P11681455"
    """
    match = re.search(r'\((.*?)\)', str(text))
    return match.group(1) if match else None


def check_for_condition(text, keyword="盆腔脂肪增多症"):
    """
    Check if a given medical report contains a specific keyword.
    Returns 1 if found, else 0.
    """
    return 1 if pd.notna(text) and keyword in str(text) else 0


def process_excel_file(input_file, output_file, path_column, extract_method):
    """
    Process one Excel file:
    - Extract patient ID from a path column.
    - Add a binary column indicating condition presence.
    """
    df = pd.read_excel(input_file)
    df["Patient_ID"] = df[path_column].apply(extract_method)
    df["Condition_Flag"] = df[path_column].apply(check_for_condition)
    os.makedirs(os.path.dirname(output_file), exist_ok=True)
    df.to_excel(output_file, index=False)
    print(f"[Processed] {input_file} → {output_file}")
    return df


def merge_excel_files(dfs, id_column, output_file):
    """
    Merge multiple hospital datasets based on patient ID.
    Only keep unique IDs across hospitals.
    """
    merged_df = pd.concat(dfs, axis=0, ignore_index=True)
    merged_df.drop_duplicates(subset=[id_column], keep="first", inplace=True)
    os.makedirs(os.path.dirname(output_file), exist_ok=True)
    merged_df.to_excel(output_file, index=False)
    print(f"[Merged] All hospitals → {output_file}")
    return merged_df


# ---------------------------
# Multi-hospital data processing
# ---------------------------

def process_all_hospitals(root_dir, path_column, output_root):
    """
    Process all hospitals under root_dir.
    Expected structure:
    /mnt/external_drive/hospitals/
        ├── 主医院第一硬盘CT/
        │   └── CT/handle/*.xlsx
        ├── 主医院第二硬盘MRI/
        │   └── MRI/handle/*.xlsx
        └── 协作医院X/
            └── CT/handle/*.xlsx
    """
    all_dfs = []

    for hospital in os.listdir(root_dir):
        hospital_path = os.path.join(root_dir, hospital)
        if not os.path.isdir(hospital_path):
            continue

        for modality in ["CT", "MRI"]:
            modality_path = os.path.join(hospital_path, modality, "handle")
            if not os.path.exists(modality_path):
                continue

            for file in os.listdir(modality_path):
                if file.endswith(".xlsx"):
                    input_file = os.path.join(modality_path, file)
                    output_file = os.path.join(output_root, hospital, modality, "processed", file)
                    df = process_excel_file(input_file, output_file, path_column, extract_content_in_parentheses)
                    df["Hospital"] = hospital
                    df["Modality"] = modality
                    all_dfs.append(df)

    # Merge all hospitals’ data
    merged_output = os.path.join(output_root, "merged_all_hospitals.xlsx")
    merge_excel_files(all_dfs, id_column="Patient_ID", output_file=merged_output)


# ---------------------------
# Main execution
# ---------------------------

if __name__ == "__main__":
    ROOT_DIR = "/mnt/external_drive/hospitals"       # Linux external drive path
    OUTPUT_ROOT = "/mnt/external_drive/results"      # Output root directory
    PATH_COLUMN = "影像学诊断"                         # Column name to extract ID from

    process_all_hospitals(ROOT_DIR, PATH_COLUMN, OUTPUT_ROOT)
    print("All hospital Excel files processed and merged successfully!")
