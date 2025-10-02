# Libraries
import pandas as pd

def remove_outliers(df: pd.DataFrame) -> pd.DataFrame:
    """
    Processes a pandas DataFrame by removing NA's and outlier observations 
    which DO NOT meet the creteria wind_max > 25 and rain_total > 50.
    Where wind_max < 25 & rain_total < 50 perhaps these effects are not because of 
    a tropical cyclone.

    Parameters:
    df (pd.DataFrame): Input DataFrame containing the raw data

    Returns:
    pd.DataFrame: A new DataFrame with data without outliers.
    """
    # copying the df to avoid modifying original DataFrame
    df_copy = df.copy()

    # removing NAs
    df_copy = (
        df_copy
        .dropna(subset = ["damage_perc"]) # lambda is anonymous function with param d
        .pipe(lambda d: d[~((d["wind_max"] < 25) & (d["rain_total"] < 50))])
    )

    return df_copy