import pandas as pd
import numpy as np
from scipy.stats import norm
from sklearn.metrics import mean_squared_error
from sklearn.metrics import mean_absolute_percentage_error

import os.path
from os import listdir
from os.path import isfile, join

import settings as s

def compute_rho(default_rates):
    df = default_rates
    pd.to_datetime(df.index, utc=True)

    #default_rates = df.loc['31/01/2010':'31/12/2016']['default_rate'] # bonne série ? bon intervalle ?
    default_rates = df['default_rate']
    delta = norm.ppf(default_rates)
    var_delta = np.var(delta)
    rho = var_delta / (1 + var_delta)

    print(f"Rho : {rho}")
    return rho

def compute_z(default_rates, rho, default_rate_proj):
    df = default_rates
    pd.to_datetime(df.index, utc=True)

    #default_rates = df.loc['31/01/2010':'31/12/2016']['default_rate']
    default_rates = df['default_rate']
    ppf_dr_mean = norm.ppf(default_rates.mean())
    ppf_dr_proj = norm.ppf(default_rate_proj) # à changer avec dr_proj modèle
    z = (ppf_dr_mean - (np.sqrt(1 - rho) * ppf_dr_proj)) / (np.sqrt(rho))
    print(f"Z : {z}")
    return z

def compute_pit_knowing_z(rho, z, ttc, marginal=True):
    ttc = ttc.loc[0:10]
    ppf_ttc = norm.ppf(ttc)

    ratio = (ppf_ttc - (np.sqrt(rho) * z)) / np.sqrt(1 - rho)
    p = norm.cdf(ratio)
    if marginal:
        for r in range(p.shape[0]):
            for l in range(p.shape[1] - 1):
                p[r, l] = p[r, l] - p[r, l + 1]
        print(f"P(M|Z) = {p}\n")
        return p
    print(f"P(M|Z) = {p}\n")
    return p

if __name__ == "__main__":
    #path_historical_default_rate = join(s.DATA_PATH, f"historical_default_rate.csv")
    path_historical_default_rate = join(s.EXPORT_PATH, f"dr_pit_serie_2010_2017.csv")
    dr = pd.read_csv(path_historical_default_rate, index_col="date")

    path_cumulative_ttc = join(s.EXPORT_PATH, f"cumulative_ttc_2010_1_2017_12.csv")
    ttc = pd.read_csv(
        path_cumulative_ttc, 
        index_col=0
    )

    for i in range(1, 37):
        path_default_rate_proj = join(s.DATA_PATH, f"predictions_gam_pit.csv")
        dr_proj = pd.read_csv(
            path_default_rate_proj, 
            index_col=0
        ).loc[i]["pred"]

        rho = compute_rho(default_rates=dr)
        z = compute_z(default_rates=dr, rho=rho, default_rate_proj=dr_proj)
        pm_z = compute_pit_knowing_z(rho=rho, z=z, ttc=ttc, marginal=True)

        with open(f'./exports/logs/pit_proj_{i}.txt', 'w') as file:
            file.write(f'rho = {rho}\n')
            file.write(f'z = {z}\n')

        df_pm_z = pd.DataFrame(
            data=pm_z,
            columns=range(pm_z.shape[1])
        )
        df_pm_z.to_csv(f"./exports/pit_proj/{i}.csv", index=True)
