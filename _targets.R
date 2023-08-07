library(targets)
library(tarchetypes)
library(data.table)
library(dplyr)

list(
    tar_target(
        raw_data_file,
        "..\\..\\..\\ownCloud\\2023_unicef_covid\\raw_data\\DH_12Countries_110222.csv",
        format = "file"
    ),

    tar_target(
        raw_data,
        fread(
            raw_data_file
        ) %>% 
            data.frame()
    ),

    tar_target(
        clean_data,
        clean_data(
            raw_data
        )
    )
)