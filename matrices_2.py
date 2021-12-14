import pandas as pd
import numpy as np

import seaborn as sns
import matplotlib.pyplot as plt

import os.path
from os import listdir
from os.path import isfile, join

import settings as s

def create_default_rate_serie(start_year=2010, end_year=2017):
    dates = list()
    prob = list()
    path_dr_pit_serie = join(s.EXPORT_PATH, f"dr_pit_serie_{start_year}_{end_year}.csv")
    for y in range(start_year, end_year):
            for m in range(1, 13):
                path_pit_raw = join(s.PIT_PATH, f"raw/{y}_{m}_{y + 1}_{m}.csv")
                path_pit_percent = join(s.PIT_PATH, f"percent/{y}_{m}_{y + 1}_{m}.csv")
                if isfile(path_pit_raw) or \
                    isfile(path_pit_percent):
                    df_raw = pd.read_csv(
                        path_pit_raw, 
                        index_col=0
                    )

                    a = df_raw.iloc[:, 10].to_numpy().sum()
                    b = df_raw.to_numpy().sum()
                    p = a / b 
                    #print(p)
                    dates.append(f"{y}-{m:02d}")
                    prob.append(p)

    print("Exporting...")
    pd.DataFrame(
        {
            'date' : dates,
            'default_rate' : prob
        }
    ).to_csv(path_dr_pit_serie, index=False)
    print("Done!\n")

def create_migration_proba_serie(start_year=2010, end_year=2017):
    proba_serie_dict = dict()
    path_migration_proba_serie = join(s.EXPORT_PATH, f"migration_proba_serie_{start_year}_{end_year}.csv")
    for i in range(1, 11):
        for j in range(1, 12):
            #print(f"From {i} to {j}")
            proba_serie_dict[f"f_{i}_t_{j}"] = []

    for y in range(2010, 2017):
        for m in range(1, 13):
            path_pit_percent = join(s.PIT_PATH, f"percent/{y}_{m}_{y + 1}_{m}.csv")
            df = pd.read_csv(
                path_pit_percent, 
                index_col=0
            )
            for i in range(1, 11):
                for j in range(1, 12):
                    k = f"f_{i}_t_{j}"
                    p = df.iloc[i - 1, j - 1]
                    proba_serie_dict[k].append(p)

    df = pd.DataFrame(proba_serie_dict)
    df.to_csv(path_migration_proba_serie, index=False)

def create_cumulative_ttc_matrix(start_year=2010, end_year=2017):
    ttc_path = join(s.EXPORT_PATH, f"ttc_{start_year}_{1}_{end_year}_{12}.csv")
    cumulative_ttc_path = join(s.EXPORT_PATH, f"cumulative_ttc_{start_year}_{1}_{end_year}_{12}.csv")
    df_ttc = pd.read_csv(ttc_path, index_col=0)
    
    df = df_ttc.iloc[:, ::-1].cumsum(axis=1).iloc[:, ::-1]
    df.loc[11] = [1] + [0 for i in range(10)]
    print(df)

    print("Exporting...")
    df.to_csv(cumulative_ttc_path)
    print("Done!\n")

def plot_pds(start_year=2010, end_year=2017):
    path_dr_pit_serie = join(s.EXPORT_PATH, f"dr_pit_serie_{start_year}_{end_year}.csv")
    df1 = pd.read_csv(
        path_dr_pit_serie,
        header=0,
        names=["date", "pd"]
    )
    df1['id'] = [1 for i in range(df1.shape[0])]
    df1["time"] = [i + 1 for i in range(df1.shape[0])]
    print(df1)

    if start_year == 2010:
        df2 = pd.read_csv(
            "./data/historical_default_rate.csv",
            header=0,
            names=["date", "pd"],
            index_col="date"
        ).loc["31/01/2010":"31/12/2016"]#[1:84]
        df2['id'] = [2 for i in range(df2.shape[0])]
        df2["time"] = [i + 1 for i in range(df2.shape[0])]
        print(df2)
    if start_year == 2017:
        df2 = pd.read_csv(
            "./data/historical_default_rate.csv",
            header=0,
            names=["date", "pd"],
            index_col="date"
        ).loc["31/01/2017":"31/12/2019"]#[85:121]
        df2['id'] = [2 for i in range(df2.shape[0])]
        df2["time"] = [i + 1 for i in range(df2.shape[0])]

    df = pd.concat([df1, df2], axis = 0, ignore_index=True)#.dropna()
    print(df)

    sns.lineplot(
        x="time",
        y="pd",
        hue="id",
        data=df
    )
    plt.show()

if __name__ == "__main__":
    #create_default_rate_serie(start_year=2017, end_year=2021)
    #create_migration_proba_serie(start_year=2017, end_year=2021)
    #create_cumulative_ttc_matrix()
    plot_pds(start_year=2017, end_year=2021)