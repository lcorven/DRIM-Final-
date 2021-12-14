import pandas as pd
import numpy as np
from scipy.stats import norm

def compute_rho(default_rates):
    df = default_rates
    pd.to_datetime(df.index, utc=True)

    default_rates = df.loc['31/01/2010':'31/12/2016']['default_rate'] # bonne série ? bon intervalle ?
    #default_rates = df.loc['31/01/2010':'31/12/2019']['default_rate']
    delta = norm.ppf(default_rates)
    var_delta = np.var(delta)
    rho = var_delta / (1 + var_delta)

    print(f"Rho : {rho}")
    return rho

def compute_z(default_rates, rho):
    df = default_rates
    pd.to_datetime(df.index, utc=True)

    default_rates = df.loc['31/01/2010':'31/12/2016']['default_rate']
    ppf_dr_mean = norm.ppf(default_rates.mean())
    dr_proj = default_rates.loc['31/12/2016'] # à changer avec dr_proj modèle
    z = (ppf_dr_mean - (np.sqrt(1 - rho) * dr_proj)) / (np.sqrt(rho))
    print(f"Z : {z}")
    return z

def compute_pit_knowing_z(rho, z, ttc, marginal=True):
    ppf_ttc = norm.ppf(ttc)

    ratio = (ppf_ttc - (np.sqrt(rho) * z)) / np.sqrt(1 - rho)
    p = norm.cdf(ratio)
    if marginal:
        for r in range(p.shape[0]):
            for l in range(p.shape[1] - 1):
                p[r, l] = p[r, l] - p[r, l + 1]
        print(f"P(M|Z) = {p}")
        return p
    print(f"P(M|Z) = {p}")
    return p

if __name__ == "__main__":
    default_rates = pd.read_csv("./historical_default_rate.csv", index_col="date")

    ttc = pd.read_csv(
        f"./export/cumulative_ttc_2010_1_2017_12.csv", 
        index_col=0
    )

    rho = compute_rho(default_rates=default_rates)
    z = compute_z(default_rates=default_rates, rho=rho)
    pm_z = compute_pit_knowing_z(rho=rho, z=z, ttc=ttc, marginal=True)

    df_pm_z = pd.DataFrame(
        data=pm_z,
        columns=range(pm_z.shape[1])
    )
    df_pm_z.to_csv("./export/test_pit_proj.csv", index=True)
    print(df_pm_z)