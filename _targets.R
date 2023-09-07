library(targets)
library(tarchetypes)
library(broom.mixed)
library(data.table)
library(dplyr)
library(ggplot2)
library(lavaan)
library(lme4)
library(magrittr)
library(mice)
library(miceadds)
library(parallel)
library(quarto)
library(reshape2)
library(scales)
library(svglite)

source("R\\functions.R")

set.seed(2578)

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
        swbs_cfa,
        conf_fact_analysis(
            data_clean,
            "lf =~ H4a + H4b + H4c + H4d + H4e + H4f"
        )
    ),

    tar_target(
        anx_cfa,
        conf_fact_analysis(
            data_clean,
            "lf =~ H4_NEW_a + H4_NEW_b + H4_NEW_c + H4_NEW_d + H4_NEW_e + H4_NEW_f + H4_NEW_g"
        )
    ),

    tar_target(
        cesd_cfa,
        conf_fact_analysis(
            data_clean,
            "lf =~ H5a + H5b + H5f + H5g"
        )
    ),

    tar_target(
        paykel_cfa,
        conf_fact_analysis(
            data_clean,
            "lf =~ H7a + H7b + H7c + H7d + H7e"
        )
    ),

    tar_target(
        cg_swbs_cfa,
        conf_fact_analysis(
            data_clean,
            "lf =~ L39a + L39b + L39c + L39d + L39e + L39f"
        )
    ),
    
    tar_target(
        cg_anx_cfa,
        conf_fact_analysis(
            data_clean,
            "lf =~ L51a + L51b + L51c + L51d + L51e + L51f + L51g"
        )
    ),

    tar_target(
        cg_cesd_cfa,
        conf_fact_analysis(
            data_clean,
            "lf =~ L40a + L40b + L40f + L40g"
        )
    ),

    tar_target(
        cg_paykel_cfa,
        conf_fact_analysis(
            data_clean,
            "lf =~ L42a + L42b + L42c + L42d + L42e"
        )
    ),

    tar_target(
        data_vars_calc,
        data_clean %>%
            cfa_calc(swbs_cfa, "CW_SWBS", c("H4a", "H4b", "H4c", "H4d", "H4e", "H4f")) %>%
            cfa_calc(anx_cfa, "ANX", c("H4_NEW_a", "H4_NEW_b", "H4_NEW_c", "H4_NEW_d", "H4_NEW_e", "H4_NEW_f", "H4_NEW_g")) %>%
            cfa_calc(cesd_cfa, "CES_D", c("H5a", "H5b", "H5f", "H5g")) %>%
            cfa_calc(paykel_cfa, "PAYKEL", c("H7a", "H7b", "H7c", "H7d", "H7e")) %>%
            cfa_calc(cg_swbs_cfa, "CG_CW_SWBS", c("L39a", "L39b", "L39c", "L39d", "L39e", "L39f")) %>%
            cfa_calc(cg_anx_cfa, "CG_ANX", c("L51a", "L51b", "L51c", "L51d", "L51e", "L51f", "L51g")) %>%
            cfa_calc(cg_cesd_cfa, "CG_CES_D", c("L40a", "L40b", "L40f", "L40g")) %>%
            cfa_calc(cg_paykel_cfa, "CG_PAYKEL", c("L42a", "L42b", "L42c", "L42d", "L42e"))
    ),
    
    tar_target(
        list_imputed_data,
        multiple_imputation(
            data_vars_calc
        )
    ),

    tar_quarto(
        mice_report,
        "mice_report.Qmd"
    ),

    tar_target(
        data_imputed,
        imputations_delist(
            list_imputed_data
        )
    ),

    tar_target(
        connection_ls_model,
        linear_mixed_model(
            data_imputed,
            "H1",
            "CO2*CO3 + ECS_SELECTED_CH_GENDER + ECS_SELECTED_CH_AGE + A1 + L1 + L4 + INCOME + L9 + L11 + L12 + L14"
        )
    ),

    tar_target(
        connection_swbs_model,
        linear_mixed_model(
            data_imputed,
            "CW_SWBS",
            "CO2*CO3 + ECS_SELECTED_CH_GENDER + ECS_SELECTED_CH_AGE + A1 + L1 + L4 + INCOME + L9 + L11 + L12 + L14"
        )
    ),

    tar_target(
        connection_anx_model,
        linear_mixed_model(
            data_imputed,
            "ANX",
            "CO2*CO3 + ECS_SELECTED_CH_GENDER + ECS_SELECTED_CH_AGE + A1 + L1 + L4 + INCOME + L9 + L11 + L12 + L14"
        )
    ),

    tar_target(
        connection_cesd_model,
        linear_mixed_model(
            data_imputed,
            "CES_D",
            "CO2*CO3 + ECS_SELECTED_CH_GENDER + ECS_SELECTED_CH_AGE + A1 + L1 + L4 + INCOME + L9 + L11 + L12 + L14"
        )
    ),

    tar_target(
        connection_sh_model,
        logistic_mixed_model(
            data_imputed,
            "H6",
            "CO2*CO3 + ECS_SELECTED_CH_GENDER + ECS_SELECTED_CH_AGE + A1 + L1 + L4 + INCOME + L9 + L11 + L12 + L14"
        )
    ),

    tar_target(
        connection_paykel_model,
        linear_mixed_model(
            data_imputed,
            "PAYKEL",
            "CO2*CO3 + ECS_SELECTED_CH_GENDER + ECS_SELECTED_CH_AGE + A1 + L1 + L4 + INCOME + L9 + L11 + L12 + L14"
        )
    )
)