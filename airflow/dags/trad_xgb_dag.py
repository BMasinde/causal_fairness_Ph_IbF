from airflow import DAG
from airflow import PythonOperator
from airflow.scripts.data_cleaning import remove_outliers 
from datetime import datetime

# DAG workflow orchestration for the trad_xgb model
with DAG(
    dag_id = "trad_xgb_pipeline",
    start_date = datetime(2025,10,1),
    schedule_interval = None, # No need for frequent execution 
    catchup= False
) as dag:

    clean_task = PythonOperator(
        task_id = "remove NAs and cases where wind_max < 25 & rainfall_total < 50",
        python_callable = remove_outliers
    )