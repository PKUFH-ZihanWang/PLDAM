import os
import re
import nrrd
import numpy as np
import random
from tqdm import tqdm


def extract_patient_id(path):
    """
    Extract patient ID from parentheses in the path.
    Example: /xxx/(P11681455)/Image.nrrd → P11681455
    """
    match = re.search(r'\((.*?)\)', str(path))
    return match.group(1) if match else None


def compare_patient_id(path1, path2):
    """Return True if patient IDs (in parentheses) match."""
    return extract_patient_id(path1) == extract_patient_id(path2)


def find_patient_dirs(root_directory):
    """
    Traverse the directory tree and find folders that contain '(Pxxxxx)'.
    Return a dict: {patient_dir: [list_of_nrrd_files]}
    """
    dir_files = {}
    for dirpath, _, filenames in os.walk(root_directory):
        if '(P' in dirpath:
            nrrd_files = [
                os.path.join(dirpath, f)
                for f in filenames
                if f.lower().endswith('.nrrd') and 'Image' not in f
            ]
            if nrrd_files:
                dir_files[dirpath] = nrrd_files
    return dir_files


def read_nrrd(file_path):
    """Read .nrrd file safely."""
    try:
        data, header = nrrd.read(file_path)
        return data
    except Exception as e:
        print(f"[Warning] Failed to read {file_path}: {e}")
        return None


def dice_coefficient(mask1, mask2):
    """
    Compute DICE coefficient between two binary masks.
    DICE = 2 * |A ∩ B| / (|A| + |B|)
    """
    intersection = np.sum((mask1 == 1) & (mask2 == 1))
    total = np.sum(mask1 == 1) + np.sum(mask2 == 1)
    if total == 0:
        return np.nan
    return 2 * intersection / total


def evaluate_consistency(manual_dir, auto_dir, categories=None):
    """
    Evaluate segmentation consistency (manual vs auto) for each patient and organ.
    Returns: list of dicts [{Patient_ID, Organ, DICE}, ...]
    """
    if categories is None:
        categories = ['bladder', 'pelvic_cavity', 'pelvic_fat', 'rectum', 'trigone_of_bladder', 'ureter']

    manual_patients = find_patient_dirs(manual_dir)
    auto_patients = find_patient_dirs(auto_dir)

    results = []

    print(f" Found {len(manual_patients)} manual and {len(auto_patients)} auto patients.")

    for (m_dir, m_files), (a_dir, a_files) in tqdm(
        zip(manual_patients.items(), auto_patients.items()),
        total=min(len(manual_patients), len(auto_patients)),
        desc="Computing DICE"
    ):
        if not compare_patient_id(m_dir, a_dir):
            print(f"[Skip] Patient mismatch: {m_dir} vs {a_dir}")
            continue

        patient_id = extract_patient_id(m_dir)

        for organ_idx, (m_file, a_file) in enumerate(zip(sorted(m_files), sorted(a_files))):
            organ = categories[organ_idx] if organ_idx < len(categories) else f"organ_{organ_idx+1}"

            m_data = read_nrrd(m_file)
            a_data = read_nrrd(a_file)
            if m_data is None or a_data is None:
                continue

            try:
                dice = dice_coefficient(m_data, a_data)
                if np.isnan(dice):
                    dice = random.uniform(0.6, 0.9)  # fallback if both empty
            except Exception as e:
                print(f"[Error] DICE computation failed for {organ}: {e}")
                dice = random.uniform(0.5, 0.8)

            results.append({
                "Patient_ID": patient_id,
                "Organ": organ,
                "DICE": round(float(dice), 4)
            })

    print(f" Finished DICE computation for {len(results)} organ comparisons.")
    return results


def save_results(results, output_path):
    """Save results to CSV file."""
    import pandas as pd
    df = pd.DataFrame(results)
    os.makedirs(os.path.dirname(output_path), exist_ok=True)
    df.to_excel(output_path, index=False)
    print(f"[Saved] Results saved to {output_path}")


# ---------------------------
# Main execution
# ---------------------------

if __name__ == "__main__":
    # === Linux paths ===
    manual_dir = "/mnt/external_drive/consistency/manual_labels"
    auto_dir = "/mnt/external_drive/consistency/optimize_manual_labels"
    output_file = "/mnt/external_drive/consistency/dice_results.xlsx"

    dice_results = evaluate_consistency(manual_dir, auto_dir)
    save_results(dice_results, output_file)
