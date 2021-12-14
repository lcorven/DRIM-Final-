import pandas as pd
import dask.dataframe as dd
import numpy as np

import os.path
from os import listdir
from os.path import isfile, join

import datetime
from calendar import monthrange

import settings as s

def create_year_month_csv(base_name="base_1.csv"):
    """
    """
    path_base = join(s.DATA_PATH, base_name)
    for y in range(2017, 2021):
        for m in range(1, 13):
            export_path = join(s.EXPORT_PATH, f"annual/{y}_{m}.csv")
            if not isfile(export_path):
                # Load
                print("Loading...")
                ddf = dd.read_csv(
                    path_base, 
                    parse_dates=['dtf_per_trt'], 
                    dtype={
                        'ID_BCR_TRS': str,
                        'classe': int
                    }
                )
                print("Done!")
                # Extract
                month_range = monthrange(y, m)
                l = datetime.datetime(y, m, 1)
                r = datetime.datetime(y, m , month_range[1])
                print(f"{l.strftime('%Y-%m-%d')} - {r.strftime('%Y-%m-%d')}")
                ddf = ddf.set_index('dtf_per_trt').loc[(l.strftime('%Y-%m-%d')):(r.strftime('%Y-%m-%d'))].compute()
                # Export
                ddf.to_csv(export_path)

def create_last_class_per_id(base_name="base_1.csv"):
    path_id_last_class = join(s.EXPORT_PATH, f"id_last_class.csv")
    path_base = join(s.DATA_PATH, base_name)
    if not isfile(path_id_last_class):
        # Load
        print("Loading...")
        ddf = dd.read_csv(
            path_base, 
            parse_dates=['dtf_per_trt'], 
            dtype={
                'ID_BCR_TRS': str,
                'classe': int
            }
        )
        print("Done!")
        # Extract
        print("Grouping and computing...")
        df = ddf.set_index('dtf_per_trt').compute()
        df = df.groupby('ID_BCR_TRS').tail(1)
        print("Done!")
        # Export
        print("Exporting...")
        df.to_csv(f"./export/id_last_class.csv")
        print("Done!")

def create_year_month_count(base_name="base_1.csv"):
    path_year_mounth_count = join(s.EXPORT_PATH, f"year_month_count.csv")
    path_base = join(s.DATA_PATH, base_name)
    if not isfile(f"./export/year_month_count.csv"):
        # Load
        print("Loading...")
        ddf = dd.read_csv(
            path_base, 
            parse_dates=['dtf_per_trt'], 
            dtype={
                'ID_BCR_TRS': str,
                'classe': int
            }
        )
        print("Done!")
        # Extract
        print("Computing...")
        ddf["year"] = ddf['dtf_per_trt'].dt.year
        ddf["month"] = ddf['dtf_per_trt'].dt.month
        ddf["count"] = ddf["classe"]
        ddf = ddf[["year", "month", "classe", "count"]].groupby(["year", "month", "classe"])["count"].count()
        #ddf.columns = ["year", "month", "value", "count"] 
        df = ddf.compute()
        print("Done!")
        # Export
        print("Exporting...")
        df.to_csv(path_year_mounth_count)
        print("Done!")

if __name__ == "__main__":
    create_year_month_csv()
    #create_last_class_per_id()
    #create_year_month_count()