import pandas as pd
import numpy as np
from sklearn.metrics import mean_squared_error, mean_absolute_percentage_error

from os import listdir
from os.path import isfile, join

import settings as s

if __name__ == "__main__":
    path_historical_default_rate = join(s.EXPORT_PATH, f"dr_pit_serie_2010_2017.csv")
    dr = pd.read_csv(path_historical_default_rate, index_col="date")

    path_cumulative_ttc = join(s.EXPORT_PATH, f"cumulative_ttc_2010_1_2017_12.csv")
    ttc = pd.read_csv(
        path_cumulative_ttc, 
        index_col=0
    )

    pit_percent_path = join(s.PIT_PATH, f"percent/")
    #pit_list = [f for f in listdir(pit_percent_path) if isfile(join(pit_percent_path, f))][-36:]
    pit_list = []
    for y in range(2017, 2020):
        for m in range(1, 13):
            pit_list.append(join(pit_percent_path, f"{y}_{m}_{y + 1}_{m}.csv"))

    pit_proj_path = join(s.EXPORT_PATH, f"pit_proj/")
    pit_proj_list = [f"{i}.csv" for i in range(1, 37)]

    pit_proj_gam_path = join(s.EXPORT_PATH, f"pit_proj_gam/")
    pit_proj_gam_list = [f"{i}.csv" for i in range(1, 37)]

    for idx, (i, j) in enumerate(zip(pit_proj_list, pit_list)):
        predicted = pd.read_csv(
            join(pit_proj_path, i),
            index_col=0
        ).to_numpy().flatten()

        actual = pd.read_csv(
            join(j),
            index_col=0
        ).to_numpy().flatten()
        
        rmse = np.sqrt(mean_squared_error(
            predicted,
            actual
        ))
        mape = mean_absolute_percentage_error(
            predicted,
            actual
        )

        with open(f'./exports/metrics/pit_proj.txt', 'a') as file:
            file.write(f"H = {idx + 1}\n")
            file.write(f"rmse = {rmse}\n")
            file.write(f"mape = {mape}\n")
            file.write(f"\n")

    for idx, (i, j) in enumerate(zip(pit_proj_gam_list, pit_list)):
        predicted = pd.read_csv(
            join(pit_proj_gam_path, i),
            index_col=0
        ).to_numpy().flatten()

        actual = pd.read_csv(
            join(j),
            index_col=0
        ).to_numpy().flatten()
        
        rmse = np.sqrt(mean_squared_error(
            predicted,
            actual
        ))
        mape = mean_absolute_percentage_error(
            predicted,
            actual
        )

        with open(f'./exports/metrics/pit_proj_gam.txt', 'a') as file:
            file.write(f"H = {idx + 1}\n")
            file.write(f"rmse = {rmse}\n")
            file.write(f"mape = {mape}\n")
            file.write(f"\n")