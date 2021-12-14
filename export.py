import pandas as pd
import numpy as np

import os

def create_default_rate_serie(handle_missings="out_nr"):
    dates = list()
    prob = list()
    for y in range(2010, 2017):
            for m in range(1, 13):
                if os.path.isfile(f"./export/{handle_missings}/matrices_percent/{y}_{m}_{y + 1}_{m}.csv") or \
                    os.path.isfile(f"./export/{handle_missings}/matrices_raw/{y}_{m}_{y + 1}_{m}.csv"):
                    df_raw = pd.read_csv(
                        f"./export/{handle_missings}/matrices_raw/{y}_{m}_{y + 1}_{m}.csv", 
                        index_col="classe_x"
                    )
                    if handle_missings == "out" or handle_missings == "infer":
                        a = df_raw.iloc[:, 10].to_numpy().sum()
                        b = df_raw.to_numpy().sum()
                    if handle_missings == "out_nr":
                        a = df_raw.iloc[:, 10].to_numpy().sum()
                        b = df_raw.to_numpy().sum() - df_raw.iloc[:, 11].to_numpy().sum()
                    p = a / b 
                    print(p)
                    dates.append(f"{y}-{m:02d}")
                    prob.append(p)

    print("Exporting...")
    pd.DataFrame(
        {
            'date' : dates,
            'default_rate' : prob
        }
    ).to_csv(f"./export/{handle_missings}/default_rates_{2010}_{2017}.csv", index=False)
    print("Done!\n")

def create_default_rate_serie_cumulative(handle_missings="out_nr"):
    dates = list()
    prob = list()

    matrices_percent_path = f"./export/monthly/matrices_percent/"
    only_files = [f for f in listdir(matrices_percent_path) if isfile(join(matrices_percent_path, f))]
    for y in range(2010, 2017):
            for m in range(1, 13):
                if os.path.isfile(f"./export/{handle_missings}/matrices_percent/{y}_{m}_{y + 1}_{m}.csv") or \
                    os.path.isfile(f"./export/{handle_missings}/matrices_raw/{y}_{m}_{y + 1}_{m}.csv"):
                    df_raw = pd.read_csv(
                        f"./export/{handle_missings}/matrices_raw/{y}_{m}_{y + 1}_{m}.csv", 
                        index_col="classe_x"
                    )
                    if handle_missings == "out" or handle_missings == "infer":
                        a = df_raw.iloc[:, 10].to_numpy().sum()
                        b = df_raw.to_numpy().sum()
                    if handle_missings == "out_nr":
                        a = df_raw.iloc[:, 10].to_numpy().sum()
                        b = df_raw.to_numpy().sum() - df_raw.iloc[:, 11].to_numpy().sum()
                    p = a / b 
                    print(p)
                    dates.append(f"{y}-{m:02d}")
                    prob.append(p)

    print("Exporting...")
    pd.DataFrame(
        {
            'date' : dates,
            'default_rate' : prob
        }
    ).to_csv(f"./export/{handle_missings}/default_rates_{2010}_{2017}.csv", index=False)
    print("Done!\n")


def create_migration_proba_serie():
    proba_serie_dict = dict()
    for i in range(1, 11):
        for j in range(1, 12):
            #print(f"From {i} to {j}")
            proba_serie_dict[f"f_{i}_t_{j}"] = []

    for y in range(2010, 2017):
        for m in range(1, 13):
            df = pd.read_csv(
                f"./export/out/matrices_percent/{y}_{m}_{y + 1}_{m}.csv", 
                index_col='classe_x'
            )
            for i in range(1, 11):
                for j in range(1, 12):
                    k = f"f_{i}_t_{j}"
                    p = df.iloc[i - 1, j - 1]
                    proba_serie_dict[k].append(p)

    df = pd.DataFrame(proba_serie_dict)
    df.to_csv(f"./export/migration_proba_serie.csv", index=False)

def create_cumulative_ttc_matrix():
    df_ttc = pd.read_csv(f"./export/ttc_2010_1_2017_12.csv", index_col='classe_x')

    df = pd.DataFrame(
        data=1,
        index=range(1, 12),
        columns=range(1, 12)
    )

    for i in range(1, 11):
        for j in range(1, 12):
            df.iloc[i - 1, j - 1] = df_ttc.iloc[i - 1, range(j - 1, (12 - 1))].sum()

    print("Exporting...")
    df.to_csv(f"./export/cumulative_ttc_2010_1_2017_12.csv")
    print("Done!\n")

def plot_pds():
    df1 = pd.read_csv(
        "./export/infer/default_rates_2010_2017.csv",
        header=0,
        names=["date", "pd"]
    )
    df1['id'] = [1 for i in range(df1.shape[0])]
    df1["time"] = [i + 1 for i in range(df1.shape[0])]

    df2 = pd.read_csv(
        "./historical_default_rate.csv",
        header=0,
        names=["date", "pd"]
    ).loc[1:84]
    df2['id'] = [2 for i in range(df2.shape[0])]
    df2["time"] = [i + 1 for i in range(df2.shape[0])]

    print(df1.shape)
    print(df2.shape)

    df = pd.concat([df1, df2], axis = 0, ignore_index=True).dropna()
    print(df.head())

    import seaborn as sns
    import matplotlib.pyplot as plt

    sns.lineplot(
        x="time",
        y="pd",
        hue="id",
        data=df
    )
    plt.show()

if __name__ == "__main__":
    pass