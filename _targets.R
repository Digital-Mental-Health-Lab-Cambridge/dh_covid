library(targets)
library(tarchetypes)
library(data.table)
library(dplyr)
library(ggplot2)
library(lavaan)
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
    ),

    tar_target(
        swbs_country_cfa_model,
        conf_fact_analysis(
            data_clean,
            "lf =~ H4a + H4b + H4c + H4d + H4e + H4f",
            group = "COUNTRY"
        )
    ),

    tar_target(
        swbs_country_metric_model,
        conf_fact_analysis(
            data_clean,
            "lf =~ H4a + H4b + H4c + H4d + H4e + H4f",
            group = "COUNTRY",
            TRUE
        )
    ),

    tar_target(
        anx_country_cfa_model,
        conf_fact_analysis(
            data_clean,
            "lf =~ H4_NEW_a + H4_NEW_b + H4_NEW_c + H4_NEW_d + H4_NEW_e + H4_NEW_f + H4_NEW_g",
            group = "COUNTRY"
        )
    ),

    tar_target(
        anx_country_metric_model,
        conf_fact_analysis(
            data_clean,
            "lf =~ H4_NEW_a + H4_NEW_b + H4_NEW_c + H4_NEW_d + H4_NEW_e + H4_NEW_f + H4_NEW_g",
            group = "COUNTRY",
            TRUE
        )
    ),

    tar_target(
        cesd_country_cfa_model,
        conf_fact_analysis(
            data_clean,
            "lf =~ H5a + H5b + H5f + H5g",
            group = "COUNTRY"
        )
    ),

    tar_target(
        cesd_country_metric_model,
        conf_fact_analysis(
            data_clean,
            "lf =~ H5a + H5b + H5f + H5g",
            group = "COUNTRY",
            TRUE
        )
    ),

    tar_quarto(
        cfa_report,
        "cfa_report.Qmd"
    ),

    tar_target(
        data_MZmeanimpute,
        MZ_mean_impute(
            data_clean
        )
    ),

    tar_target(
        swbs_cfa_model,
        conf_fact_analysis(
            data_clean,
            "lf =~ H4a + H4b + H4c + H4d + H4e + H4f"
        )
    ),

    tar_target(
        data_swbs_calc,
        cfa_calc(
            data_MZmeanimpute,
            swbs_cfa_model,
            "CW_SWBS",
            c("H4a", "H4b", "H4c", "H4d", "H4e", "H4f")
        )
    )#,

    #tar_target(
    #    data_mi,
    #    multiple_imputation(
    #        data_swbs_calc
    #    )
    #)
)