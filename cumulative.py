import pandas as pd
import numpy as np

from os import listdir
from os.path import isfile, join

def create_month_month_matrices():
    """
    """
    matrices_percent_path = f"./export/annual/"

    only_files = [f for f in listdir(matrices_percent_path) if isfile(join(matrices_percent_path, f))]
    print(only_files)

    return

    for y in range(2010, 2018):
            for m in range(1, 13):
                if m == 12 and y < 2017:
                    if not os.path.isfile(f"./export/monthly/matrices_raw/{y}_{m}_{y + 1}_{1}.csv") or \
                    not os.path.isfile(f"./export/monthly/matrices_percent/{y}_{m}_{y + 1}_{1}.csv"):
                        print(f"{y} - {m} -> {y + 1} - {1}")
                        df1 = pd.read_csv(f"./export/annual/{y}_{m}.csv")
                        df2 = pd.read_csv(f"./export/annual/{y + 1}_{1}.csv")

                        df1 = df1[df1["classe"] != 11]
                        
                        df_merged = df1.merge(df2, left_on = 'ID_BCR_TRS', right_on = 'ID_BCR_TRS')

                        df_cross_raw = pd.crosstab(df_merged['classe_x'], df_merged['classe_y'])
                        df_cross_percent = pd.crosstab(df_merged['classe_x'], df_merged['classe_y'], normalize="index")

                        print("Exporting...")
                        df_cross_raw.to_csv(f"./export/monthly/matrices_raw/{y}_{m}_{y + 1}_{1}.csv")
                        df_cross_percent.to_csv(f"./export/monthly/matrices_percent/{y}_{m}_{y + 1}_{1}.csv")
                        print("Done!\n")
                elif m == 12 and y > 2016:
                    continue
                else:
                    if not os.path.isfile(f"./export/monthly/matrices_raw/{y}_{m}_{y}_{m + 1}.csv") or \
                    not os.path.isfile(f"./export/monthly/matrices_percent/{y}_{m}_{y}_{m + 1}.csv"):
                        print(f"{y} - {m} -> {y} - {m + 1}")
                        df1 = pd.read_csv(f"./export/annual/{y}_{m}.csv")
                        df2 = pd.read_csv(f"./export/annual/{y}_{m + 1}.csv")

                        df1 = df1[df1["classe"] != 11]
                        
                        df_merged = df1.merge(df2, left_on = 'ID_BCR_TRS', right_on = 'ID_BCR_TRS')

                        df_cross_raw = pd.crosstab(df_merged['classe_x'], df_merged['classe_y'])
                        df_cross_percent = pd.crosstab(df_merged['classe_x'], df_merged['classe_y'], normalize="index")

                        print("Exporting...")
                        df_cross_raw.to_csv(f"./export/monthly/matrices_raw/{y}_{m}_{y}_{m + 1}.csv")
                        df_cross_percent.to_csv(f"./export/monthly/matrices_percent/{y}_{m}_{y}_{m + 1}.csv")
                        print("Done!\n")

if __name__ == "__main__":
    create_month_month_matrices()