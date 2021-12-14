import pandas as pd
import dask.dataframe as dd
import numpy as np

import os
import os.path
from os import listdir
from os.path import isfile, join

from functools import reduce

import datetime
from calendar import monthrange

from scipy.stats import norm

import settings as s

def create_year_month_matrices(start_year=2010, end_year=2017):
    """
    """
    if start_year == 2010:
        for y in range(start_year, end_year):
            for m in range(1, 13):
                pit_raw_path = join(s.PIT_PATH, f"raw/{y}_{m}_{y + 1}_{m}.csv")
                pit_percent_path = join(s.PIT_PATH, f"percent/{y}_{m}_{y + 1}_{m}.csv")
                if not isfile(pit_raw_path) or \
                    not isfile(pit_percent_path):
                    print(f"{y} - {m} -> {y + 1} - {m}")
                    df1 = pd.read_csv(join(s.EXPORT_PATH, f"annual/{y}_{m}.csv"))
                    df2 = pd.read_csv(join(s.EXPORT_PATH, f"annual/{y + 1}_{m}.csv"))

                    df1 = df1[df1["classe"] != 11]
                    df_merged = df1.merge(df2, left_on = 'ID_BCR_TRS', right_on = 'ID_BCR_TRS')

                    df_cross_raw = pd.crosstab(df_merged['classe_x'], df_merged['classe_y'])
                    df_cross_percent = pd.crosstab(df_merged['classe_x'], df_merged['classe_y'], normalize="index")

                    print("Exporting...")
                    df_cross_raw.to_csv(pit_raw_path)
                    df_cross_percent.to_csv(pit_percent_path)
                    print("Done!\n")
    if start_year == 2017:
        for y in range(start_year, end_year):
            for m in range(1, 13):
                pit_raw_path = join(s.PIT_PATH, f"raw/{y}_{m}_{y + 1}_{m}.csv")
                pit_percent_path = join(s.PIT_PATH, f"percent/{y}_{m}_{y + 1}_{m}.csv")
                if not isfile(pit_raw_path) or \
                    not isfile(pit_percent_path):
                    print(f"{y} - {m} -> {y + 1} - {m}")
                    df1 = pd.read_csv(join(s.EXPORT_PATH, f"annual/{y}_{m}.csv"))
                    df2 = pd.read_csv(join(s.EXPORT_PATH, f"annual/{y + 1}_{m}.csv"))

                    df1 = df1[df1["classe"] != 11]
                    df_merged = df1.merge(df2, left_on = 'ID_BCR_TRS', right_on = 'ID_BCR_TRS')

                    df_cross_raw = pd.crosstab(df_merged['classe_x'], df_merged['classe_y'])
                    df_cross_percent = pd.crosstab(df_merged['classe_x'], df_merged['classe_y'], normalize="index")

                    print("Exporting...")
                    df_cross_raw.to_csv(pit_raw_path)
                    df_cross_percent.to_csv(pit_percent_path)
                    print("Done!\n")
            
def create_ttc_matrix(start_year=2010, end_year=2017):
    """
    """
    ttc_path = join(s.EXPORT_PATH, f"ttc_{start_year}_{1}_{end_year}_{12}.csv")
    print("Reading...")
    dfs = list()
    for y in range(start_year, end_year):
        for m in range(1, 13):
            #pit_raw_path = join(s.PIT_PATH, f"raw/{y}_{m}_{y + 1}_{m}.csv")
            pit_percent_path = join(s.PIT_PATH, f"percent/{y}_{m}_{y + 1}_{m}.csv")
            if isfile(pit_percent_path):
                t_df = pd.read_csv(pit_percent_path, index_col=0)
                dfs.append(t_df)
    print("Done!")

    print("Concatenating...")
    df = reduce(lambda x, y: x.add(y, fill_value=0), dfs) / len(dfs)
    print("Done!")
    print("Exporting...")
    df.to_csv(ttc_path)
    print("Done!\n")

if __name__ == "__main__":
    create_year_month_matrices(start_year=2010, end_year=2017)
    create_year_month_matrices(start_year=2017, end_year=2021)
    create_ttc_matrix()



