library(targets)
library(tarchetypes)
library(data.table)
library(dplyr)

list(
    tar_target(
        load_data,
        fread(
            "..\\..\\..\\ownCloud\\2023_unicef_covid\\raw_data\\DH_12Countries_110222.csv"
        )
    )
)