"""One-off script to combine trans csv files into one large file for
ingest into BigQuery"""

import pandas as pd
import glob

path = '/home/jacob/DataScience/DataEngineering/citybus_analytics/data'
ext = '/*.csv'


def find_files(path, ext):
    """Returns a list of files with specified extension"""
    needed_files = glob.glob(path + ext)
    return needed_files


def combine_files(files_list):
    """Combines all files from find_files function into single dataframe"""
    combined_data = pd.concat([pd.read_csv(f) for f in files_list], ignore_index=True)
    combined_data = combined_data[combined_data.Product.notnull()]
    return combined_data


all_files = find_files(path, ext)


all_trans = combine_files(all_files)

print(len(all_trans))
