library(targets)
library(tarchetypes)
library(data.table)
library(dplyr)
library(ggplot2)
library(magrittr)
library(quarto)
library(reshape2)
library(svglite)

source("R\\functions.R")

list(
    tar_target(
        raw_data_file,
        "..\\..\\..\\ownCloud - Tom Metherell@cloud.mrc-cbu.cam.ac.uk\\2023_unicef_covid\\raw_data\\DH_12Countries_110222.csv",
        format = "file"
    ),

    tar_target(
        data_raw,
        fread(
            raw_data_file
        ) %>% 
            data.frame()
    ),

    tar_target(
        data_clean,
        clean_data(
            data_raw
        )
    ),

    tar_target(
        overall_NA,
        data.frame(var = names(data_clean), missingness = colSums(is.na(data_clean)) / nrow(data_clean))
    ),

    tar_target(
        overall_NA_plot,
        plot_overall_NA(
            overall_NA
        )
    ),

    tar_target(
        NA_by_country,
        country_missingness(
            data_clean
        )
    ),
    
    tar_target(
        country_NA_plots,
        plot_country_NA(
            NA_by_country
        )
    ),

    tar_target(
        NA_by_language,
        language_missingness(
            data_clean
        )
    ),
    
    tar_target(
        language_NA_plots,
        plot_language_NA(
            NA_by_language
        )
    ),

    tar_quarto(
        NA_report,
        "NA_report.Qmd"
    )
)