# Loading dependencies
library(targets)
library(tarchetypes)
library(data.table)
library(dplyr)
library(ggplot2)
library(lavaan)
library(magrittr)
library(MASS)
library(mice)
library(mitools)
library(purrr)
library(quarto)
library(reshape2)
library(robsurvey)
library(scales)
library(survey)
library(svglite)

# Loading additional functions
source("R/functions.R")

# Setting random seed
set.seed(2578)

list(
    # Defining raw data file
    tar_target(
        raw_data_file,
        "raw_data/DH_12Countries_110222.csv",
        format = "file"
    ),

    # Reading raw data file and storing as a data frame
    tar_target(
        data_raw,
        fread(
            raw_data_file
        ) %>% 
            data.frame()
    ),

    # Cleaning data
    tar_target(
        data_clean,
        clean_data(
            data_raw
        )
    ),

    # Summarising overall missingness
    tar_target(
        overall_NA,
        data.frame(
            var = names(data_clean), 
            missingness = colSums(is.na(data_clean)) / nrow(data_clean)
        )
    ),

    # Plotting overall missingness
    tar_target(
        overall_NA_plot,
        plot_overall_NA(
            overall_NA
        )
    ),

    # Summarising missingness by country
    tar_target(
        NA_by_country,
        country_missingness(
            data_clean
        )
    ),
    
    # Plotting missingness by country
    tar_target(
        country_NA_plots,
        plot_country_NA(
            NA_by_country
        )
    ),

    # Summarising missingness by language
    tar_target(
        NA_by_language,
        language_missingness(
            data_clean
        )
    ),
    
    # Plotting missingness by language
    tar_target(
        language_NA_plots,
        plot_language_NA(
            NA_by_language
        )
    ),

    # Producing report of missingness summary plots
    tar_quarto(
        NA_report,
        "NA_report.Qmd" 
    ),

    # Fitting CFA models for wellbeing variables (grouped by country) to test metric invariance
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
            "loadings"
        )
    ),

    tar_target(
        swbs_country_scalar_model,
        conf_fact_analysis(
            data_clean,
            "lf =~ H4a + H4b + H4c + H4d + H4e + H4f",
            group = "COUNTRY",
            c("loadings", "intercepts")
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
            "loadings"
        )
    ),

    tar_target(
        anx_country_scalar_model,
        conf_fact_analysis(
            data_clean,
            "lf =~ H4_NEW_a + H4_NEW_b + H4_NEW_c + H4_NEW_d + H4_NEW_e + H4_NEW_f + H4_NEW_g",
            group = "COUNTRY",
            c("loadings", "intercepts")
        )
    ),

    tar_target(
        anx_abridged_country_cfa_model,
        conf_fact_analysis(
            data_clean,
            "lf =~ H4_NEW_a + H4_NEW_c + H4_NEW_f + H4_NEW_g",
            group = "COUNTRY"
        )
    ),

    tar_target(
        anx_abridged_country_metric_model,
        conf_fact_analysis(
            data_clean,
            "lf =~ H4_NEW_a + H4_NEW_c + H4_NEW_f + H4_NEW_g",
            group = "COUNTRY",
            "loadings"
        )
    ),

    tar_target(
        anx_abridged_country_scalar_model,
        conf_fact_analysis(
            data_clean,
            "lf =~ H4_NEW_a + H4_NEW_c + H4_NEW_f + H4_NEW_g",
            group = "COUNTRY",
            c("loadings", "intercepts")
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
            "loadings"
        )
    ),

    tar_target(
        cesd_country_scalar_model,
        conf_fact_analysis(
            data_clean,
            "lf =~ H5a + H5b + H5f + H5g",
            group = "COUNTRY",
            c("loadings", "intercepts")
        )
    ),

    tar_target(
        cesd_abridged_country_cfa_model,
        conf_fact_analysis(
            data_clean,
            "lf =~ H5b + H5f + H5g",
            group = "COUNTRY"
        )
    ),

    tar_target(
        cesd_abridged_country_metric_model,
        conf_fact_analysis(
            data_clean,
            "lf =~ H5b + H5f + H5g",
            group = "COUNTRY",
            "loadings"
        )
    ),

    tar_target(
        cesd_abridged_country_scalar_model,
        conf_fact_analysis(
            data_clean,
            "lf =~ H5b + H5f + H5g",
            group = "COUNTRY",
            c("loadings", "intercepts")
        )
    ),

    tar_target(
        paykel_country_cfa_model,
        conf_fact_analysis(
            data_clean,
            "lf =~ H7a + H7b + H7c + H7d + H7e",
            group = "COUNTRY"
        )
    ),

    tar_target(
        paykel_country_metric_model,
        conf_fact_analysis(
            data_clean,
            "lf =~ H7a + H7b + H7c + H7d + H7e",
            group = "COUNTRY",
            "loadings"
        )
    ),

    tar_target(
        paykel_country_scalar_model,
        conf_fact_analysis(
            data_clean,
            "lf =~ H7a + H7b + H7c + H7d + H7e",
            group = "COUNTRY",
            c("loadings", "intercepts")
        )
    ),

    tar_target(
        paykel_abridged_country_cfa_model,
        conf_fact_analysis(
            data_clean,
            "lf =~ H7a + H7c + H7d + H7e",
            group = "COUNTRY"
        )
    ),

    tar_target(
        paykel_abridged_country_metric_model,
        conf_fact_analysis(
            data_clean,
            "lf =~ H7a + H7c + H7d + H7e",
            group = "COUNTRY",
            "loadings"
        )
    ),

    tar_target(
        paykel_abridged_country_scalar_model,
        conf_fact_analysis(
            data_clean,
            "lf =~ H7a + H7c + H7d + H7e",
            group = "COUNTRY",
            c("loadings", "intercepts")
        )
    ),

    # Producing metric invariance report
    tar_quarto(
        cfa_report,
        "cfa_report.Qmd"
    ),

    # Fitting final CFA models
    tar_target(
        swbs_cfa,
        conf_fact_analysis(
            data_clean,
            "lf =~ H4a + H4b + H4c + H4d + H4e + H4f"
        )
    ),

    tar_target(
        anx_abridged_cfa,
        conf_fact_analysis(
            data_clean,
            "lf =~ H4_NEW_a + H4_NEW_c + H4_NEW_f + H4_NEW_g"
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
        cesd_abridged_cfa,
        # The "1 minus" is here to reverse the CES-D scoring, to make the interpretation more intuitive
        1 - conf_fact_analysis(
            data_clean,
            "lf =~ H5b + H5f + H5g"
        )
    ),

    tar_target(
        cesd_cfa,
        1 - conf_fact_analysis(
            data_clean,
            "lf =~ H5a + H5b + H5f + H5g"
        )
    ),

    tar_target(
        paykel_abridged_cfa,
        conf_fact_analysis(
            data_clean,
            "lf =~ H7a + H7c + H7d + H7e"
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
        cg_anx_abridged_cfa,
        conf_fact_analysis(
            data_clean,
            "lf =~ L51a + L51c + L51f + L51g"
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
        cg_cesd_abridged_cfa,
        conf_fact_analysis(
            data_clean,
            "lf =~ L40b + L40f + L40g"
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
        cg_paykel_abridged_cfa,
        conf_fact_analysis(
            data_clean,
            "lf =~ L42a + L42c + L42d + L42e"
        )
    ),

    tar_target(
        cg_paykel_cfa,
        conf_fact_analysis(
            data_clean,
            "lf =~ L42a + L42b + L42c + L42d + L42e"
        )
    ),

    # Adding CFA variables to data frames and removing constituent variables
    tar_target(
        data_vars_calc,
        data_clean %>%
            cfa_calc(swbs_cfa, "CW_SWBS", c("H4a", "H4b", "H4c", "H4d", "H4e", "H4f")) %>%
            cfa_calc(anx_abridged_cfa, "ANX", c("H4_NEW_a", "H4_NEW_b", "H4_NEW_c", "H4_NEW_d", "H4_NEW_e", "H4_NEW_f", "H4_NEW_g")) %>%
            cfa_calc(cesd_abridged_cfa, "CES_D", c("H5a", "H5b", "H5f", "H5g")) %>%
            cfa_calc(paykel_abridged_cfa, "PAYKEL", c("H7a", "H7b", "H7c", "H7d", "H7e")) %>%
            cfa_calc(cg_swbs_cfa, "CG_CW_SWBS", c("L39a", "L39b", "L39c", "L39d", "L39e", "L39f")) %>%
            cfa_calc(cg_anx_abridged_cfa, "CG_ANX", c("L51a", "L51b", "L51c", "L51d", "L51e", "L51f", "L51g")) %>%
            cfa_calc(cg_cesd_abridged_cfa, "CG_CES_D", c("L40a", "L40b", "L40f", "L40g")) %>%
            cfa_calc(cg_paykel_abridged_cfa, "CG_PAYKEL", c("L42a", "L42b", "L42c", "L42d", "L42e"))
    ),

    # For sensitivity checks: version with all items included in CFAs
    tar_target(
        data_full_wb_vars,
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
    
    # Performing multiple imputation
    tar_target(
        imputed_data,
        multiple_imputation(
            data_vars_calc
        )
    ),

    tar_target(
        imputed_data_full_wb_vars,
        multiple_imputation(
            data_full_wb_vars
        )
    ),

    # Producing multiple imputation report
    tar_quarto(
        mice_report,
        "mice_report.Qmd"
    ),

    # Producing report of descriptive plots
    tar_quarto(
        descriptives_report,
        "descriptives_report.Qmd"
    ),

    tar_target(
        imputed_data_standardised,
        standardise_data(
            imputed_data
        )
    ),
    
    tar_target(
        imputed_data_full_wb_vars_standardised,
        standardise_data(
            imputed_data_full_wb_vars
        )
    ),

    tar_target(
        imputed_data_relevelled,
        covid_relevel(
            imputed_data_standardised
        )
    ),
    
    tar_target(
        imputed_data_full_wb_vars_relevelled,
        covid_relevel(
            imputed_data_full_wb_vars_standardised
        )
    ),

    # Fitting regression models
    tar_target(
        connection_ls_models,
        robust_linear_models(
            imputed_data_standardised,
            "H1",
            "dv_covid_status + ECS_SELECTED_CH_AGE + L1 + L4 + INCOME + L9 + L11 + L12 + L14 + L50"
        )
    ),

    tar_target(
        connection_ls_models_summary,
        summarise_results(
            connection_ls_models,
            c("dv_covid_statusLockdown, disconnected", "dv_covid_statusLockdown, connected")
        )
    ),

    tar_target(
        pooled_connection_ls_models,
        pooled_robust_linear_models(
            imputed_data_standardised,
            "H1",
            "dv_covid_status + ECS_SELECTED_CH_AGE + L1 + L4 + INCOME + L9 + L11 + L12 + L14 + L50 + COUNTRY"
        )
    ),

    tar_target(
        pooled_connection_ls_models_summary,
        summarise_pooled_results(
            pooled_connection_ls_models,
            c("dv_covid_statusLockdown, disconnected", "dv_covid_statusLockdown, connected")
        )
    ),

    tar_target(
        connection_swbs_models,
        robust_linear_models(
            imputed_data_standardised,
            "CW_SWBS",
            "dv_covid_status + ECS_SELECTED_CH_AGE + L1 + L4 + INCOME + L9 + L11 + L12 + L14 + CG_CW_SWBS"
        )
    ),

    tar_target(
        connection_swbs_models_summary,
        summarise_results(
            connection_swbs_models,
            c("dv_covid_statusLockdown, disconnected", "dv_covid_statusLockdown, connected")
        )
    ),

    tar_target(
        pooled_connection_swbs_models,
        pooled_robust_linear_models(
            imputed_data_standardised,
            "CW_SWBS",
            "dv_covid_status + ECS_SELECTED_CH_AGE + L1 + L4 + INCOME + L9 + L11 + L12 + L14 + CG_CW_SWBS + COUNTRY"
        )
    ),

    tar_target(
        pooled_connection_swbs_models_summary,
        summarise_pooled_results(
            pooled_connection_swbs_models,
            c("dv_covid_statusLockdown, disconnected", "dv_covid_statusLockdown, connected")
        )
    ),

    tar_target(
        connection_anx_models,
        robust_linear_models(
            imputed_data_standardised,
            "ANX",
            "dv_covid_status + ECS_SELECTED_CH_AGE + L1 + L4 + INCOME + L9 + L11 + L12 + L14 + CG_ANX"
        )
    ),

    tar_target(
        connection_anx_models_summary,
        summarise_results(
            connection_anx_models,
            c("dv_covid_statusLockdown, disconnected", "dv_covid_statusLockdown, connected")
        )
    ),

    tar_target(
        pooled_connection_anx_models,
        pooled_robust_linear_models(
            imputed_data_standardised,
            "ANX",
            "dv_covid_status + ECS_SELECTED_CH_AGE + L1 + L4 + INCOME + L9 + L11 + L12 + L14 + CG_ANX + COUNTRY"
        )
    ),

    tar_target(
        pooled_connection_anx_models_summary,
        summarise_pooled_results(
            pooled_connection_anx_models,
            c("dv_covid_statusLockdown, disconnected", "dv_covid_statusLockdown, connected")
        )
    ),

    tar_target(
        connection_cesd_models,
        robust_linear_models(
            imputed_data_standardised,
            "CES_D",
            "dv_covid_status + ECS_SELECTED_CH_AGE + L1 + L4 + INCOME + L9 + L11 + L12 + L14 + CG_CES_D"
        )
    ),

    tar_target(
        connection_cesd_models_summary,
        summarise_results(
            connection_cesd_models,
            c("dv_covid_statusLockdown, disconnected", "dv_covid_statusLockdown, connected")
        )
    ),

    tar_target(
        pooled_connection_cesd_models,
        pooled_robust_linear_models(
            imputed_data_standardised,
            "CES_D",
            "dv_covid_status + ECS_SELECTED_CH_AGE + L1 + L4 + INCOME + L9 + L11 + L12 + L14 + CG_CES_D + COUNTRY"
        )
    ),

    tar_target(
        pooled_connection_cesd_models_summary,
        summarise_pooled_results(
            pooled_connection_cesd_models,
            c("dv_covid_statusLockdown, disconnected", "dv_covid_statusLockdown, connected")
        )
    ),

    tar_target(
        connection_sh_models,
        logistic_models(
            imputed_data_standardised,
            "H6",
            "dv_covid_status + ECS_SELECTED_CH_AGE + L1 + L4 + INCOME + L9 + L11 + L12 + L14 + L41"
        )
    ),

    tar_target(
        connection_sh_models_summary,
        summarise_results(
            connection_sh_models,
            c("dv_covid_statusLockdown, disconnected", "dv_covid_statusLockdown, connected")
        )
    ),

    tar_target(
        pooled_connection_sh_models,
        pooled_logistic_models(
            imputed_data_standardised,
            "H6",
            "dv_covid_status + ECS_SELECTED_CH_AGE + L1 + L4 + INCOME + L9 + L11 + L12 + L14 + L41 + COUNTRY"
        )
    ),

    tar_target(
        pooled_connection_sh_models_summary,
        summarise_pooled_results(
            pooled_connection_sh_models,
            c("dv_covid_statusLockdown, disconnected", "dv_covid_statusLockdown, connected")
        )
    ),

    tar_target(
        connection_paykel_models,
        robust_linear_models(
            imputed_data_standardised,
            "PAYKEL",
            "dv_covid_status + ECS_SELECTED_CH_AGE + L1 + L4 + INCOME + L9 + L11 + L12 + L14 + CG_PAYKEL"
        )
    ),

    tar_target(
        connection_paykel_models_summary,
        summarise_results(
            connection_paykel_models,
            c("dv_covid_statusLockdown, disconnected", "dv_covid_statusLockdown, connected")
        )
    ),

    tar_target(
        pooled_connection_paykel_models,
        pooled_robust_linear_models(
            imputed_data_standardised,
            "PAYKEL",
            "dv_covid_status + ECS_SELECTED_CH_AGE + L1 + L4 + INCOME + L9 + L11 + L12 + L14 + CG_PAYKEL + COUNTRY"
        )
    ),

    tar_target(
        pooled_connection_paykel_models_summary,
        summarise_pooled_results(
            pooled_connection_paykel_models,
            c("dv_covid_statusLockdown, disconnected", "dv_covid_statusLockdown, connected")
        )
    ),

    tar_target(
        connection_ls_models_relevelled,
        robust_linear_models(
            imputed_data_relevelled,
            "H1",
            "dv_covid_status + ECS_SELECTED_CH_AGE + L1 + L4 + INCOME + L9 + L11 + L12 + L14 + L50"
        )
    ),

    tar_target(
        connection_ls_models_relevelled_summary,
        summarise_results(
            connection_ls_models_relevelled,
            c("dv_covid_statusNo lockdown", "dv_covid_statusLockdown, connected")
        )
    ),

    tar_target(
        pooled_connection_ls_models_relevelled,
        pooled_robust_linear_models(
            imputed_data_relevelled,
            "H1",
            "dv_covid_status + ECS_SELECTED_CH_AGE + L1 + L4 + INCOME + L9 + L11 + L12 + L14 + L50 + COUNTRY"
        )
    ),

    tar_target(
        pooled_connection_ls_models_relevelled_summary,
        summarise_pooled_results(
            pooled_connection_ls_models_relevelled,
            c("dv_covid_statusNo lockdown", "dv_covid_statusLockdown, connected")
        )
    ),

    tar_target(
        connection_swbs_models_relevelled,
        robust_linear_models(
            imputed_data_relevelled,
            "CW_SWBS",
            "dv_covid_status + ECS_SELECTED_CH_AGE + L1 + L4 + INCOME + L9 + L11 + L12 + L14 + CG_CW_SWBS"
        )
    ),

    tar_target(
        connection_swbs_models_relevelled_summary,
        summarise_results(
            connection_swbs_models_relevelled,
            c("dv_covid_statusNo lockdown", "dv_covid_statusLockdown, connected")
        )
    ),

    tar_target(
        pooled_connection_swbs_models_relevelled,
        pooled_robust_linear_models(
            imputed_data_relevelled,
            "CW_SWBS",
            "dv_covid_status + ECS_SELECTED_CH_AGE + L1 + L4 + INCOME + L9 + L11 + L12 + L14 + CG_CW_SWBS + COUNTRY"
        )
    ),

    tar_target(
        pooled_connection_swbs_models_relevelled_summary,
        summarise_pooled_results(
            pooled_connection_swbs_models_relevelled,
            c("dv_covid_statusNo lockdown", "dv_covid_statusLockdown, connected")
        )
    ),

    tar_target(
        connection_anx_models_relevelled,
        robust_linear_models(
            imputed_data_relevelled,
            "ANX",
            "dv_covid_status + ECS_SELECTED_CH_AGE + L1 + L4 + INCOME + L9 + L11 + L12 + L14 + CG_ANX"
        )
    ),

    tar_target(
        connection_anx_models_relevelled_summary,
        summarise_results(
            connection_anx_models_relevelled,
            c("dv_covid_statusNo lockdown", "dv_covid_statusLockdown, connected")
        )
    ),

    tar_target(
        pooled_connection_anx_models_relevelled,
        pooled_robust_linear_models(
            imputed_data_relevelled,
            "ANX",
            "dv_covid_status + ECS_SELECTED_CH_AGE + L1 + L4 + INCOME + L9 + L11 + L12 + L14 + CG_ANX + COUNTRY"
        )
    ),

    tar_target(
        pooled_connection_anx_models_relevelled_summary,
        summarise_pooled_results(
            pooled_connection_anx_models_relevelled,
            c("dv_covid_statusNo lockdown", "dv_covid_statusLockdown, connected")
        )
    ),

    tar_target(
        connection_cesd_models_relevelled,
        robust_linear_models(
            imputed_data_relevelled,
            "CES_D",
            "dv_covid_status + ECS_SELECTED_CH_AGE + L1 + L4 + INCOME + L9 + L11 + L12 + L14 + CG_CES_D"
        )
    ),

    tar_target(
        connection_cesd_models_relevelled_summary,
        summarise_results(
            connection_cesd_models_relevelled,
            c("dv_covid_statusNo lockdown", "dv_covid_statusLockdown, connected")
        )
    ),

    tar_target(
        pooled_connection_cesd_models_relevelled,
        pooled_robust_linear_models(
            imputed_data_relevelled,
            "CES_D",
            "dv_covid_status + ECS_SELECTED_CH_AGE + L1 + L4 + INCOME + L9 + L11 + L12 + L14 + CG_CES_D + COUNTRY"
        )
    ),

    tar_target(
        pooled_connection_cesd_models_relevelled_summary,
        summarise_pooled_results(
            pooled_connection_cesd_models_relevelled,
            c("dv_covid_statusNo lockdown", "dv_covid_statusLockdown, connected")
        )
    ),

    tar_target(
        connection_sh_models_relevelled,
        logistic_models(
            imputed_data_relevelled,
            "H6",
            "dv_covid_status + ECS_SELECTED_CH_AGE + L1 + L4 + INCOME + L9 + L11 + L12 + L14 + L41"
        )
    ),

    tar_target(
        connection_sh_models_relevelled_summary,
        summarise_results(
            connection_sh_models_relevelled,
            c("dv_covid_statusNo lockdown", "dv_covid_statusLockdown, connected")
        )
    ),

    tar_target(
        pooled_connection_sh_models_relevelled,
        pooled_logistic_models(
            imputed_data_relevelled,
            "H6",
            "dv_covid_status + ECS_SELECTED_CH_AGE + L1 + L4 + INCOME + L9 + L11 + L12 + L14 + L41 + COUNTRY"
        )
    ),

    tar_target(
        pooled_connection_sh_models_relevelled_summary,
        summarise_pooled_results(
            pooled_connection_sh_models_relevelled,
            c("dv_covid_statusNo lockdown", "dv_covid_statusLockdown, connected")
        )
    ),

    tar_target(
        connection_paykel_models_relevelled,
        robust_linear_models(
            imputed_data_relevelled,
            "PAYKEL",
            "dv_covid_status + ECS_SELECTED_CH_AGE + L1 + L4 + INCOME + L9 + L11 + L12 + L14 + CG_PAYKEL"
        )
    ),

    tar_target(
        connection_paykel_models_relevelled_summary,
        summarise_results(
            connection_paykel_models_relevelled,
            c("dv_covid_statusNo lockdown", "dv_covid_statusLockdown, connected")
        )
    ),

    tar_target(
        pooled_connection_paykel_models_relevelled,
        pooled_robust_linear_models(
            imputed_data_relevelled,
            "PAYKEL",
            "dv_covid_status + ECS_SELECTED_CH_AGE + L1 + L4 + INCOME + L9 + L11 + L12 + L14 + CG_PAYKEL + COUNTRY"
        )
    ),

    tar_target(
        pooled_connection_paykel_models_relevelled_summary,
        summarise_pooled_results(
            pooled_connection_paykel_models_relevelled,
            c("dv_covid_statusNo lockdown", "dv_covid_statusLockdown, connected")
        )
    ),

    tar_target(
        internet_ls_models,
        robust_linear_models(
            imputed_data_standardised,
            "H1", 
            "B1*CO2 + ECS_SELECTED_CH_AGE + L1 + L4 + INCOME + L9 + L11 + L12 + L14 + L18 + L36a + L36b + L36c + L36f + L50"
        )
    ),

    tar_target(
        internet_ls_models_summary,
        summarise_results(
            internet_ls_models,
            c("B1", "CO21", "B1:CO21")
        )
    ),

    tar_target(
        pooled_internet_ls_models,
        pooled_robust_linear_models(
            imputed_data_standardised,
            "H1",
            "B1*CO2 + ECS_SELECTED_CH_AGE + L1 + L4 + INCOME + L9 + L11 + L12 + L14 + L18 + L36a + L36b + L36c + L36f + L50 + COUNTRY"
        )
    ),

    tar_target(
        pooled_internet_ls_models_summary,
        summarise_pooled_results(
            pooled_internet_ls_models,
            c("B1", "CO21", "B1:CO21")
        )
    ),

    tar_target(
        internet_swbs_models,
        robust_linear_models(
            imputed_data_standardised,
            "CW_SWBS",
            "B1*CO2 + ECS_SELECTED_CH_AGE + L1 + L4 + INCOME + L9 + L11 + L12 + L14 + L18 + L36a + L36b + L36c + L36f + CG_CW_SWBS"
        )
    ),

    tar_target(
        internet_swbs_models_summary,
        summarise_results(
            internet_swbs_models,
            c("B1", "CO21", "B1:CO21")
        )
    ),

    tar_target(
        pooled_internet_swbs_models,
        pooled_robust_linear_models(
            imputed_data_standardised,
            "CW_SWBS",
            "B1*CO2 + ECS_SELECTED_CH_AGE + L1 + L4 + INCOME + L9 + L11 + L12 + L14 + L18 + L36a + L36b + L36c + L36f + CG_CW_SWBS + COUNTRY"
        )
    ),

    tar_target(
        pooled_internet_swbs_models_summary,
        summarise_pooled_results(
            pooled_internet_swbs_models,
            c("B1", "CO21", "B1:CO21")
        )
    ),

    tar_target(
        internet_anx_models,
        robust_linear_models(
            imputed_data_standardised,
            "ANX",
            "B1*CO2 + ECS_SELECTED_CH_AGE + L1 + L4 + INCOME + L9 + L11 + L12 + L14 + L18 + L36a + L36b + L36c + L36f + CG_ANX"
        )
    ),

    tar_target(
        internet_anx_models_summary,
        summarise_results(
            internet_anx_models,
            c("B1", "CO21", "B1:CO21")
        )
    ),

    tar_target(
        pooled_internet_anx_models,
        pooled_robust_linear_models(
            imputed_data_standardised,
            "ANX",
            "B1*CO2 + ECS_SELECTED_CH_AGE + L1 + L4 + INCOME + L9 + L11 + L12 + L14 + L18 + L36a + L36b + L36c + L36f + CG_ANX + COUNTRY"
        )
    ),

    tar_target(
        pooled_internet_anx_models_summary,
        summarise_pooled_results(
            pooled_internet_anx_models,
            c("B1", "CO21", "B1:CO21")
        )
    ),

    tar_target(
        internet_cesd_models,
        robust_linear_models(
            imputed_data_standardised,
            "CES_D",
            "B1*CO2 + ECS_SELECTED_CH_AGE + L1 + L4 + INCOME + L9 + L11 + L12 + L14 + L18 + L36a + L36b + L36c + L36f + CG_CES_D"
        )
    ),

    tar_target(
        internet_cesd_models_summary,
        summarise_results(
            internet_cesd_models,
            c("B1", "CO21", "B1:CO21")
        )
    ),

    tar_target(
        pooled_internet_cesd_models,
        pooled_robust_linear_models(
            imputed_data_standardised,
            "CES_D",
            "B1*CO2 + ECS_SELECTED_CH_AGE + L1 + L4 + INCOME + L9 + L11 + L12 + L14 + L18 + L36a + L36b + L36c + L36f + CG_CES_D + COUNTRY"
        )
    ),

    tar_target(
        pooled_internet_cesd_models_summary,
        summarise_pooled_results(
            pooled_internet_cesd_models,
            c("B1", "CO21", "B1:CO21")
        )
    ),

    tar_target(
        internet_sh_models,
        logistic_models(
            imputed_data_standardised,
            "H6",
            "B1*CO2 + ECS_SELECTED_CH_AGE + L1 + L4 + INCOME + L9 + L11 + L12 + L14 + L18 + L36a + L36b + L36c + L36f + L41"
        )
    ),

    tar_target(
        internet_sh_models_summary,
        summarise_results(
            internet_sh_models,
            c("B1", "CO21", "B1:CO21")
        )
    ),

    tar_target(
        pooled_internet_sh_models,
        pooled_logistic_models(
            imputed_data_standardised,
            "H6",
            "B1*CO2 + ECS_SELECTED_CH_AGE + L1 + L4 + INCOME + L9 + L11 + L12 + L14 + L18 + L36a + L36b + L36c + L36f + L41 + COUNTRY"
        )
    ),

    tar_target(
        pooled_internet_sh_models_summary,
        summarise_pooled_results(
            pooled_internet_sh_models,
            c("B1", "CO21", "B1:CO21")
        )
    ),

    tar_target(
        internet_paykel_models,
        robust_linear_models(
            imputed_data_standardised,
            "PAYKEL",
            "B1*CO2 + ECS_SELECTED_CH_AGE + L1 + L4 + INCOME + L9 + L11 + L12 + L14 + L18 + L36a + L36b + L36c + L36f + CG_PAYKEL"
        )
    ),

    tar_target(
        internet_paykel_models_summary,
        summarise_results(
            internet_paykel_models,
            c("B1", "CO21", "B1:CO21")
        )
    ),

    tar_target(
        pooled_internet_paykel_models,
        pooled_robust_linear_models(
            imputed_data_standardised,
            "PAYKEL",
            "B1*CO2 + ECS_SELECTED_CH_AGE + L1 + L4 + INCOME + L9 + L11 + L12 + L14 + L18 + L36a + L36b + L36c + L36f + CG_PAYKEL + COUNTRY"
        )
    ),

    tar_target(
        pooled_internet_paykel_models_summary,
        summarise_pooled_results(
            pooled_internet_paykel_models,
            c("B1", "CO21", "B1:CO21")
        )
    ),

    tar_target(
        connection_ls_models_weighted,
        weighted_robust_linear_models(
            imputed_data_standardised,
            "H1",
            "dv_covid_status + ECS_SELECTED_CH_AGE + L1 + L4 + INCOME + L9 + L11 + L12 + L14 + L50"
        )
    ),

    tar_target(
        connection_ls_models_weighted_summary,
        summarise_results(
            connection_ls_models_weighted,
            c("dv_covid_statusLockdown, disconnected", "dv_covid_statusLockdown, connected")
        )
    ),

    tar_target(
        pooled_connection_ls_models_weighted,
        pooled_weighted_robust_linear_models(
            imputed_data_standardised,
            "H1",
            "dv_covid_status + ECS_SELECTED_CH_AGE + L1 + L4 + INCOME + L9 + L11 + L12 + L14 + L50 + COUNTRY"
        )
    ),

    tar_target(
        pooled_connection_ls_models_weighted_summary,
        summarise_pooled_results(
            pooled_connection_ls_models_weighted,
            c("dv_covid_statusLockdown, disconnected", "dv_covid_statusLockdown, connected")
        )
    ),

    tar_target(
        connection_swbs_models_weighted,
        weighted_robust_linear_models(
            imputed_data_standardised,
            "CW_SWBS",
            "dv_covid_status + ECS_SELECTED_CH_AGE + L1 + L4 + INCOME + L9 + L11 + L12 + L14 + CG_CW_SWBS"
        )
    ),

    tar_target(
        connection_swbs_models_weighted_summary,
        summarise_results(
            connection_swbs_models_weighted,
            c("dv_covid_statusLockdown, disconnected", "dv_covid_statusLockdown, connected")
        )
    ),

    tar_target(
        pooled_connection_swbs_models_weighted,
        pooled_weighted_robust_linear_models(
            imputed_data_standardised,
            "CW_SWBS",
            "dv_covid_status + ECS_SELECTED_CH_AGE + L1 + L4 + INCOME + L9 + L11 + L12 + L14 + CG_CW_SWBS + COUNTRY"
        )
    ),

    tar_target(
        pooled_connection_swbs_models_weighted_summary,
        summarise_pooled_results(
            pooled_connection_swbs_models_weighted,
            c("dv_covid_statusLockdown, disconnected", "dv_covid_statusLockdown, connected")
        )
    ),

    tar_target(
        connection_anx_models_weighted,
        weighted_robust_linear_models(
            imputed_data_standardised,
            "ANX",
            "dv_covid_status + ECS_SELECTED_CH_AGE + L1 + L4 + INCOME + L9 + L11 + L12 + L14 + CG_ANX"
        )
    ),

    tar_target(
        connection_anx_models_weighted_summary,
        summarise_results(
            connection_anx_models_weighted,
            c("dv_covid_statusLockdown, disconnected", "dv_covid_statusLockdown, connected")
        )
    ),

    tar_target(
        pooled_connection_anx_models_weighted,
        pooled_weighted_robust_linear_models(
            imputed_data_standardised,
            "ANX",
            "dv_covid_status + ECS_SELECTED_CH_AGE + L1 + L4 + INCOME + L9 + L11 + L12 + L14 + CG_ANX + COUNTRY"
        )
    ),

    tar_target(
        pooled_connection_anx_models_weighted_summary,
        summarise_pooled_results(
            pooled_connection_anx_models_weighted,
            c("dv_covid_statusLockdown, disconnected", "dv_covid_statusLockdown, connected")
        )
    ),

    tar_target(
        connection_cesd_models_weighted,
        weighted_robust_linear_models(
            imputed_data_standardised,
            "CES_D",
            "dv_covid_status + ECS_SELECTED_CH_AGE + L1 + L4 + INCOME + L9 + L11 + L12 + L14 + CG_CES_D"
        )
    ),

    tar_target(
        connection_cesd_models_weighted_summary,
        summarise_results(
            connection_cesd_models_weighted,
            c("dv_covid_statusLockdown, disconnected", "dv_covid_statusLockdown, connected")
        )
    ),

    tar_target(
        pooled_connection_cesd_models_weighted,
        pooled_weighted_robust_linear_models(
            imputed_data_standardised,
            "CES_D",
            "dv_covid_status + ECS_SELECTED_CH_AGE + L1 + L4 + INCOME + L9 + L11 + L12 + L14 + CG_CES_D + COUNTRY"
        )
    ),

    tar_target(
        pooled_connection_cesd_models_weighted_summary,
        summarise_pooled_results(
            pooled_connection_cesd_models_weighted,
            c("dv_covid_statusLockdown, disconnected", "dv_covid_statusLockdown, connected")
        )
    ),

    tar_target(
        connection_sh_models_weighted,
        weighted_logistic_models(
            imputed_data_standardised,
            "H6",
            "dv_covid_status + ECS_SELECTED_CH_AGE + L1 + L4 + INCOME + L9 + L11 + L12 + L14 + L41"
        )
    ),

    tar_target(
        connection_sh_models_weighted_summary,
        summarise_results(
            connection_sh_models_weighted,
            c("dv_covid_statusLockdown, disconnected", "dv_covid_statusLockdown, connected")
        )
    ),

    tar_target(
        pooled_connection_sh_models_weighted,
        pooled_weighted_logistic_models(
            imputed_data_standardised,
            "H6",
            "dv_covid_status + ECS_SELECTED_CH_AGE + L1 + L4 + INCOME + L9 + L11 + L12 + L14 + L41 + COUNTRY"
        )
    ),

    tar_target(
        pooled_connection_sh_models_weighted_summary,
        summarise_pooled_results(
            pooled_connection_sh_models_weighted,
            c("dv_covid_statusLockdown, disconnected", "dv_covid_statusLockdown, connected")
        )
    ),

    tar_target(
        connection_paykel_models_weighted,
        weighted_robust_linear_models(
            imputed_data_standardised,
            "PAYKEL",
            "dv_covid_status + ECS_SELECTED_CH_AGE + L1 + L4 + INCOME + L9 + L11 + L12 + L14 + CG_PAYKEL"
        )
    ),

    tar_target(
        connection_paykel_models_weighted_summary,
        summarise_results(
            connection_paykel_models_weighted,
            c("dv_covid_statusLockdown, disconnected", "dv_covid_statusLockdown, connected")
        )
    ),

    tar_target(
        pooled_connection_paykel_models_weighted,
        pooled_weighted_robust_linear_models(
            imputed_data_standardised,
            "PAYKEL",
            "dv_covid_status + ECS_SELECTED_CH_AGE + L1 + L4 + INCOME + L9 + L11 + L12 + L14 + CG_PAYKEL + COUNTRY"
        )
    ),

    tar_target(
        pooled_connection_paykel_models_weighted_summary,
        summarise_pooled_results(
            pooled_connection_paykel_models_weighted,
            c("dv_covid_statusLockdown, disconnected", "dv_covid_statusLockdown, connected")
        )
    ),

    tar_target(
        connection_ls_models_relevelled_weighted,
        weighted_robust_linear_models(
            imputed_data_relevelled,
            "H1",
            "dv_covid_status + ECS_SELECTED_CH_AGE + L1 + L4 + INCOME + L9 + L11 + L12 + L14 + L50"
        )
    ),

    tar_target(
        connection_ls_models_relevelled_weighted_summary,
        summarise_results(
            connection_ls_models_relevelled_weighted,
            c("dv_covid_statusNo lockdown", "dv_covid_statusLockdown, connected")
        )
    ),

    tar_target(
        pooled_connection_ls_models_relevelled_weighted,
        pooled_weighted_robust_linear_models(
            imputed_data_relevelled,
            "H1",
            "dv_covid_status + ECS_SELECTED_CH_AGE + L1 + L4 + INCOME + L9 + L11 + L12 + L14 + L50 + COUNTRY"
        )
    ),

    tar_target(
        pooled_connection_ls_models_relevelled_weighted_summary,
        summarise_pooled_results(
            pooled_connection_ls_models_relevelled_weighted,
            c("dv_covid_statusNo lockdown", "dv_covid_statusLockdown, connected")
        )
    ),

    tar_target(
        connection_swbs_models_relevelled_weighted,
        weighted_robust_linear_models(
            imputed_data_relevelled,
            "CW_SWBS",
            "dv_covid_status + ECS_SELECTED_CH_AGE + L1 + L4 + INCOME + L9 + L11 + L12 + L14 + CG_CW_SWBS"
        )
    ),

    tar_target(
        connection_swbs_models_relevelled_weighted_summary,
        summarise_results(
            connection_swbs_models_relevelled_weighted,
            c("dv_covid_statusNo lockdown", "dv_covid_statusLockdown, connected")
        )
    ),

    tar_target(
        pooled_connection_swbs_models_relevelled_weighted,
        pooled_weighted_robust_linear_models(
            imputed_data_relevelled,
            "CW_SWBS",
            "dv_covid_status + ECS_SELECTED_CH_AGE + L1 + L4 + INCOME + L9 + L11 + L12 + L14 + CG_CW_SWBS + COUNTRY"
        )
    ),

    tar_target(
        pooled_connection_swbs_models_relevelled_weighted_summary,
        summarise_pooled_results(
            pooled_connection_swbs_models_relevelled_weighted,
            c("dv_covid_statusNo lockdown", "dv_covid_statusLockdown, connected")
        )
    ),

    tar_target(
        connection_anx_models_relevelled_weighted,
        weighted_robust_linear_models(
            imputed_data_relevelled,
            "ANX",
            "dv_covid_status + ECS_SELECTED_CH_AGE + L1 + L4 + INCOME + L9 + L11 + L12 + L14 + CG_ANX"
        )
    ),

    tar_target(
        connection_anx_models_relevelled_weighted_summary,
        summarise_results(
            connection_anx_models_relevelled_weighted,
            c("dv_covid_statusNo lockdown", "dv_covid_statusLockdown, connected")
        )
    ),

    tar_target(
        pooled_connection_anx_models_relevelled_weighted,
        pooled_weighted_robust_linear_models(
            imputed_data_relevelled,
            "ANX",
            "dv_covid_status + ECS_SELECTED_CH_AGE + L1 + L4 + INCOME + L9 + L11 + L12 + L14 + CG_ANX + COUNTRY"
        )
    ),

    tar_target(
        pooled_connection_anx_models_relevelled_weighted_summary,
        summarise_pooled_results(
            pooled_connection_anx_models_relevelled_weighted,
            c("dv_covid_statusNo lockdown", "dv_covid_statusLockdown, connected")
        )
    ),

    tar_target(
        connection_cesd_models_relevelled_weighted,
        weighted_robust_linear_models(
            imputed_data_relevelled,
            "CES_D",
            "dv_covid_status + ECS_SELECTED_CH_AGE + L1 + L4 + INCOME + L9 + L11 + L12 + L14 + CG_CES_D"
        )
    ),

    tar_target(
        connection_cesd_models_relevelled_weighted_summary,
        summarise_results(
            connection_cesd_models_relevelled_weighted,
            c("dv_covid_statusNo lockdown", "dv_covid_statusLockdown, connected")
        )
    ),

    tar_target(
        pooled_connection_cesd_models_relevelled_weighted,
        pooled_weighted_robust_linear_models(
            imputed_data_relevelled,
            "CES_D",
            "dv_covid_status + ECS_SELECTED_CH_AGE + L1 + L4 + INCOME + L9 + L11 + L12 + L14 + CG_CES_D + COUNTRY"
        )
    ),

    tar_target(
        pooled_connection_cesd_models_relevelled_weighted_summary,
        summarise_pooled_results(
            pooled_connection_cesd_models_relevelled_weighted,
            c("dv_covid_statusNo lockdown", "dv_covid_statusLockdown, connected")
        )
    ),

    tar_target(
        connection_sh_models_relevelled_weighted,
        weighted_logistic_models(
            imputed_data_relevelled,
            "H6",
            "dv_covid_status + ECS_SELECTED_CH_AGE + L1 + L4 + INCOME + L9 + L11 + L12 + L14 + L41"
        )
    ),

    tar_target(
        connection_sh_models_relevelled_weighted_summary,
        summarise_results(
            connection_sh_models_relevelled_weighted,
            c("dv_covid_statusNo lockdown", "dv_covid_statusLockdown, connected")
        )
    ),

    tar_target(
        pooled_connection_sh_models_relevelled_weighted,
        pooled_weighted_logistic_models(
            imputed_data_relevelled,
            "H6",
            "dv_covid_status + ECS_SELECTED_CH_AGE + L1 + L4 + INCOME + L9 + L11 + L12 + L14 + L41 + COUNTRY"
        )
    ),

    tar_target(
        pooled_connection_sh_models_relevelled_weighted_summary,
        summarise_pooled_results(
            pooled_connection_sh_models_relevelled_weighted,
            c("dv_covid_statusNo lockdown", "dv_covid_statusLockdown, connected")
        )
    ),

    tar_target(
        connection_paykel_models_relevelled_weighted,
        weighted_robust_linear_models(
            imputed_data_relevelled,
            "PAYKEL",
            "dv_covid_status + ECS_SELECTED_CH_AGE + L1 + L4 + INCOME + L9 + L11 + L12 + L14 + CG_PAYKEL"
        )
    ),

    tar_target(
        connection_paykel_models_relevelled_weighted_summary,
        summarise_results(
            connection_paykel_models_relevelled_weighted,
            c("dv_covid_statusNo lockdown", "dv_covid_statusLockdown, connected")
        )
    ),

    tar_target(
        pooled_connection_paykel_models_relevelled_weighted,
        pooled_weighted_robust_linear_models(
            imputed_data_relevelled,
            "PAYKEL",
            "dv_covid_status + ECS_SELECTED_CH_AGE + L1 + L4 + INCOME + L9 + L11 + L12 + L14 + CG_PAYKEL + COUNTRY"
        )
    ),

    tar_target(
        pooled_connection_paykel_models_relevelled_weighted_summary,
        summarise_pooled_results(
            pooled_connection_paykel_models_relevelled_weighted,
            c("dv_covid_statusNo lockdown", "dv_covid_statusLockdown, connected")
        )
    ),

    tar_target(
        internet_ls_models_weighted,
        weighted_robust_linear_models(
            imputed_data_standardised,
            "H1", 
            "B1*CO2 + ECS_SELECTED_CH_AGE + L1 + L4 + INCOME + L9 + L11 + L12 + L14 + L18 + L36a + L36b + L36c + L36f + L50"
        )
    ),

    tar_target(
        internet_ls_models_weighted_summary,
        summarise_results(
            internet_ls_models_weighted,
            c("B1", "CO21", "B1:CO21")
        )
    ),

    tar_target(
        pooled_internet_ls_models_weighted,
        pooled_weighted_robust_linear_models(
            imputed_data_standardised,
            "H1",
            "B1*CO2 + ECS_SELECTED_CH_AGE + L1 + L4 + INCOME + L9 + L11 + L12 + L14 + L18 + L36a + L36b + L36c + L36f + L50 + COUNTRY"
        )
    ),

    tar_target(
        pooled_internet_ls_models_weighted_summary,
        summarise_pooled_results(
            pooled_internet_ls_models_weighted,
            c("B1", "CO21", "B1:CO21")
        )
    ),

    tar_target(
        internet_swbs_models_weighted,
        weighted_robust_linear_models(
            imputed_data_standardised,
            "CW_SWBS",
            "B1*CO2 + ECS_SELECTED_CH_AGE + L1 + L4 + INCOME + L9 + L11 + L12 + L14 + L18 + L36a + L36b + L36c + L36f + CG_CW_SWBS"
        )
    ),

    tar_target(
        internet_swbs_models_weighted_summary,
        summarise_results(
            internet_swbs_models_weighted,
            c("B1", "CO21", "B1:CO21")
        )
    ),

    tar_target(
        pooled_internet_swbs_models_weighted,
        pooled_weighted_robust_linear_models(
            imputed_data_standardised,
            "CW_SWBS",
            "B1*CO2 + ECS_SELECTED_CH_AGE + L1 + L4 + INCOME + L9 + L11 + L12 + L14 + L18 + L36a + L36b + L36c + L36f + CG_CW_SWBS + COUNTRY"
        )
    ),

    tar_target(
        pooled_internet_swbs_models_weighted_summary,
        summarise_pooled_results(
            pooled_internet_swbs_models_weighted,
            c("B1", "CO21", "B1:CO21")
        )
    ),

    tar_target(
        internet_anx_models_weighted,
        weighted_robust_linear_models(
            imputed_data_standardised,
            "ANX",
            "B1*CO2 + ECS_SELECTED_CH_AGE + L1 + L4 + INCOME + L9 + L11 + L12 + L14 + L18 + L36a + L36b + L36c + L36f + CG_ANX"
        )
    ),

    tar_target(
        internet_anx_models_weighted_summary,
        summarise_results(
            internet_anx_models_weighted,
            c("B1", "CO21", "B1:CO21")
        )
    ),

    tar_target(
        pooled_internet_anx_models_weighted,
        pooled_weighted_robust_linear_models(
            imputed_data_standardised,
            "ANX",
            "B1*CO2 + ECS_SELECTED_CH_AGE + L1 + L4 + INCOME + L9 + L11 + L12 + L14 + L18 + L36a + L36b + L36c + L36f + CG_ANX + COUNTRY"
        )
    ),

    tar_target(
        pooled_internet_anx_models_weighted_summary,
        summarise_pooled_results(
            pooled_internet_anx_models_weighted,
            c("B1", "CO21", "B1:CO21")
        )
    ),

    tar_target(
        internet_cesd_models_weighted,
        weighted_robust_linear_models(
            imputed_data_standardised,
            "CES_D",
            "B1*CO2 + ECS_SELECTED_CH_AGE + L1 + L4 + INCOME + L9 + L11 + L12 + L14 + L18 + L36a + L36b + L36c + L36f + CG_CES_D"
        )
    ),

    tar_target(
        internet_cesd_models_weighted_summary,
        summarise_results(
            internet_cesd_models_weighted,
            c("B1", "CO21", "B1:CO21")
        )
    ),

    tar_target(
        pooled_internet_cesd_models_weighted,
        pooled_weighted_robust_linear_models(
            imputed_data_standardised,
            "CES_D",
            "B1*CO2 + ECS_SELECTED_CH_AGE + L1 + L4 + INCOME + L9 + L11 + L12 + L14 + L18 + L36a + L36b + L36c + L36f + CG_CES_D + COUNTRY"
        )
    ),

    tar_target(
        pooled_internet_cesd_models_weighted_summary,
        summarise_pooled_results(
            pooled_internet_cesd_models_weighted,
            c("B1", "CO21", "B1:CO21")
        )
    ),

    tar_target(
        internet_sh_models_weighted,
        weighted_logistic_models(
            imputed_data_standardised,
            "H6",
            "B1*CO2 + ECS_SELECTED_CH_AGE + L1 + L4 + INCOME + L9 + L11 + L12 + L14 + L18 + L36a + L36b + L36c + L36f + L41"
        )
    ),

    tar_target(
        internet_sh_models_weighted_summary,
        summarise_results(
            internet_sh_models_weighted,
            c("B1", "CO21", "B1:CO21")
        )
    ),

    tar_target(
        pooled_internet_sh_models_weighted,
        pooled_weighted_logistic_models(
            imputed_data_standardised,
            "H6",
            "B1*CO2 + ECS_SELECTED_CH_AGE + L1 + L4 + INCOME + L9 + L11 + L12 + L14 + L18 + L36a + L36b + L36c + L36f + L41 + COUNTRY"
        )
    ),

    tar_target(
        pooled_internet_sh_models_weighted_summary,
        summarise_pooled_results(
            pooled_internet_sh_models_weighted,
            c("B1", "CO21", "B1:CO21")
        )
    ),

    tar_target(
        internet_paykel_models_weighted,
        weighted_robust_linear_models(
            imputed_data_standardised,
            "PAYKEL",
            "B1*CO2 + ECS_SELECTED_CH_AGE + L1 + L4 + INCOME + L9 + L11 + L12 + L14 + L18 + L36a + L36b + L36c + L36f + CG_PAYKEL"
        )
    ),

    tar_target(
        internet_paykel_models_weighted_summary,
        summarise_results(
            internet_paykel_models_weighted,
            c("B1", "CO21", "B1:CO21")
        )
    ),

    tar_target(
        pooled_internet_paykel_models_weighted,
        pooled_weighted_robust_linear_models(
            imputed_data_standardised,
            "PAYKEL",
            "B1*CO2 + ECS_SELECTED_CH_AGE + L1 + L4 + INCOME + L9 + L11 + L12 + L14 + L18 + L36a + L36b + L36c + L36f + CG_PAYKEL + COUNTRY"
        )
    ),

    tar_target(
        pooled_internet_paykel_models_weighted_summary,
        summarise_pooled_results(
            pooled_internet_paykel_models_weighted,
            c("B1", "CO21", "B1:CO21")
        )
    ),

    # Producing report of model results
    tar_quarto(
        models_report,
        "models_report.Qmd"
    ),

    # Fitting sensitivity check regression models
    tar_target(
        connection_ls_models_fullwb,
        robust_linear_models(
            imputed_data_full_wb_vars_standardised,
            "H1",
            "dv_covid_status + ECS_SELECTED_CH_AGE + L1 + L4 + INCOME + L9 + L11 + L12 + L14 + L50"
        )
    ),

    tar_target(
        connection_ls_models_fullwb_summary,
        summarise_results(
            connection_ls_models_fullwb,
            c("dv_covid_statusLockdown, disconnected", "dv_covid_statusLockdown, connected")
        )
    ),

    tar_target(
        pooled_connection_ls_models_fullwb,
        pooled_robust_linear_models(
            imputed_data_full_wb_vars_standardised,
            "H1",
            "dv_covid_status + ECS_SELECTED_CH_AGE + L1 + L4 + INCOME + L9 + L11 + L12 + L14 + L50 + COUNTRY"
        )
    ),

    tar_target(
        pooled_connection_ls_models_fullwb_summary,
        summarise_pooled_results(
            pooled_connection_ls_models_fullwb,
            c("dv_covid_statusLockdown, disconnected", "dv_covid_statusLockdown, connected")
        )
    ),

    tar_target(
        connection_swbs_models_fullwb,
        robust_linear_models(
            imputed_data_full_wb_vars_standardised,
            "CW_SWBS",
            "dv_covid_status + ECS_SELECTED_CH_AGE + L1 + L4 + INCOME + L9 + L11 + L12 + L14 + CG_CW_SWBS"
        )
    ),

    tar_target(
        connection_swbs_models_fullwb_summary,
        summarise_results(
            connection_swbs_models_fullwb,
            c("dv_covid_statusLockdown, disconnected", "dv_covid_statusLockdown, connected")
        )
    ),

    tar_target(
        pooled_connection_swbs_models_fullwb,
        pooled_robust_linear_models(
            imputed_data_full_wb_vars_standardised,
            "CW_SWBS",
            "dv_covid_status + ECS_SELECTED_CH_AGE + L1 + L4 + INCOME + L9 + L11 + L12 + L14 + CG_CW_SWBS + COUNTRY"
        )
    ),

    tar_target(
        pooled_connection_swbs_models_fullwb_summary,
        summarise_pooled_results(
            pooled_connection_swbs_models_fullwb,
            c("dv_covid_statusLockdown, disconnected", "dv_covid_statusLockdown, connected")
        )
    ),

    tar_target(
        connection_anx_models_fullwb,
        robust_linear_models(
            imputed_data_full_wb_vars_standardised,
            "ANX",
            "dv_covid_status + ECS_SELECTED_CH_AGE + L1 + L4 + INCOME + L9 + L11 + L12 + L14 + CG_ANX"
        )
    ),

    tar_target(
        connection_anx_models_fullwb_summary,
        summarise_results(
            connection_anx_models_fullwb,
            c("dv_covid_statusLockdown, disconnected", "dv_covid_statusLockdown, connected")
        )
    ),

    tar_target(
        pooled_connection_anx_models_fullwb,
        pooled_robust_linear_models(
            imputed_data_full_wb_vars_standardised,
            "ANX",
            "dv_covid_status + ECS_SELECTED_CH_AGE + L1 + L4 + INCOME + L9 + L11 + L12 + L14 + CG_ANX + COUNTRY"
        )
    ),

    tar_target(
        pooled_connection_anx_models_fullwb_summary,
        summarise_pooled_results(
            pooled_connection_anx_models_fullwb,
            c("dv_covid_statusLockdown, disconnected", "dv_covid_statusLockdown, connected")
        )
    ),

    tar_target(
        connection_cesd_models_fullwb,
        robust_linear_models(
            imputed_data_full_wb_vars_standardised,
            "CES_D",
            "dv_covid_status + ECS_SELECTED_CH_AGE + L1 + L4 + INCOME + L9 + L11 + L12 + L14 + CG_CES_D"
        )
    ),

    tar_target(
        connection_cesd_models_fullwb_summary,
        summarise_results(
            connection_cesd_models_fullwb,
            c("dv_covid_statusLockdown, disconnected", "dv_covid_statusLockdown, connected")
        )
    ),

    tar_target(
        pooled_connection_cesd_models_fullwb,
        pooled_robust_linear_models(
            imputed_data_full_wb_vars_standardised,
            "CES_D",
            "dv_covid_status + ECS_SELECTED_CH_AGE + L1 + L4 + INCOME + L9 + L11 + L12 + L14 + CG_CES_D + COUNTRY"
        )
    ),

    tar_target(
        pooled_connection_cesd_models_fullwb_summary,
        summarise_pooled_results(
            pooled_connection_cesd_models_fullwb,
            c("dv_covid_statusLockdown, disconnected", "dv_covid_statusLockdown, connected")
        )
    ),

    tar_target(
        connection_sh_models_fullwb,
        logistic_models(
            imputed_data_full_wb_vars_standardised,
            "H6",
            "dv_covid_status + ECS_SELECTED_CH_AGE + L1 + L4 + INCOME + L9 + L11 + L12 + L14 + L41"
        )
    ),

    tar_target(
        connection_sh_models_fullwb_summary,
        summarise_results(
            connection_sh_models_fullwb,
            c("dv_covid_statusLockdown, disconnected", "dv_covid_statusLockdown, connected")
        )
    ),

    tar_target(
        pooled_connection_sh_models_fullwb,
        pooled_logistic_models(
            imputed_data_full_wb_vars_standardised,
            "H6",
            "dv_covid_status + ECS_SELECTED_CH_AGE + L1 + L4 + INCOME + L9 + L11 + L12 + L14 + L41 + COUNTRY"
        )
    ),

    tar_target(
        pooled_connection_sh_models_fullwb_summary,
        summarise_pooled_results(
            pooled_connection_sh_models_fullwb,
            c("dv_covid_statusLockdown, disconnected", "dv_covid_statusLockdown, connected")
        )
    ),

    tar_target(
        connection_paykel_models_fullwb,
        robust_linear_models(
            imputed_data_full_wb_vars_standardised,
            "PAYKEL",
            "dv_covid_status + ECS_SELECTED_CH_AGE + L1 + L4 + INCOME + L9 + L11 + L12 + L14 + CG_PAYKEL"
        )
    ),

    tar_target(
        connection_paykel_models_fullwb_summary,
        summarise_results(
            connection_paykel_models_fullwb,
            c("dv_covid_statusLockdown, disconnected", "dv_covid_statusLockdown, connected")
        )
    ),

    tar_target(
        pooled_connection_paykel_models_fullwb,
        pooled_robust_linear_models(
            imputed_data_full_wb_vars_standardised,
            "PAYKEL",
            "dv_covid_status + ECS_SELECTED_CH_AGE + L1 + L4 + INCOME + L9 + L11 + L12 + L14 + CG_PAYKEL + COUNTRY"
        )
    ),

    tar_target(
        pooled_connection_paykel_models_fullwb_summary,
        summarise_pooled_results(
            pooled_connection_paykel_models_fullwb,
            c("dv_covid_statusLockdown, disconnected", "dv_covid_statusLockdown, connected")
        )
    ),

    tar_target(
        connection_ls_models_fullwb_relevelled,
        robust_linear_models(
            imputed_data_full_wb_vars_relevelled,
            "H1",
            "dv_covid_status + ECS_SELECTED_CH_AGE + L1 + L4 + INCOME + L9 + L11 + L12 + L14 + L50"
        )
    ),

    tar_target(
        connection_ls_models_fullwb_relevelled_summary,
        summarise_results(
            connection_ls_models_fullwb_relevelled,
            c("dv_covid_statusNo lockdown", "dv_covid_statusLockdown, connected")
        )
    ),

    tar_target(
        pooled_connection_ls_models_fullwb_relevelled,
        pooled_robust_linear_models(
            imputed_data_full_wb_vars_relevelled,
            "H1",
            "dv_covid_status + ECS_SELECTED_CH_AGE + L1 + L4 + INCOME + L9 + L11 + L12 + L14 + L50 + COUNTRY"
        )
    ),

    tar_target(
        pooled_connection_ls_models_fullwb_relevelled_summary,
        summarise_pooled_results(
            pooled_connection_ls_models_fullwb_relevelled,
            c("dv_covid_statusNo lockdown", "dv_covid_statusLockdown, connected")
        )
    ),

    tar_target(
        connection_swbs_models_fullwb_relevelled,
        robust_linear_models(
            imputed_data_full_wb_vars_relevelled,
            "CW_SWBS",
            "dv_covid_status + ECS_SELECTED_CH_AGE + L1 + L4 + INCOME + L9 + L11 + L12 + L14 + CG_CW_SWBS"
        )
    ),

    tar_target(
        connection_swbs_models_fullwb_relevelled_summary,
        summarise_results(
            connection_swbs_models_fullwb_relevelled,
            c("dv_covid_statusNo lockdown", "dv_covid_statusLockdown, connected")
        )
    ),

    tar_target(
        pooled_connection_swbs_models_fullwb_relevelled,
        pooled_robust_linear_models(
            imputed_data_full_wb_vars_relevelled,
            "CW_SWBS",
            "dv_covid_status + ECS_SELECTED_CH_AGE + L1 + L4 + INCOME + L9 + L11 + L12 + L14 + CG_CW_SWBS + COUNTRY"
        )
    ),

    tar_target(
        pooled_connection_swbs_models_fullwb_relevelled_summary,
        summarise_pooled_results(
            pooled_connection_swbs_models_fullwb_relevelled,
            c("dv_covid_statusNo lockdown", "dv_covid_statusLockdown, connected")
        )
    ),

    tar_target(
        connection_anx_models_fullwb_relevelled,
        robust_linear_models(
            imputed_data_full_wb_vars_relevelled,
            "ANX",
            "dv_covid_status + ECS_SELECTED_CH_AGE + L1 + L4 + INCOME + L9 + L11 + L12 + L14 + CG_ANX"
        )
    ),

    tar_target(
        connection_anx_models_fullwb_relevelled_summary,
        summarise_results(
            connection_anx_models_fullwb_relevelled,
            c("dv_covid_statusNo lockdown", "dv_covid_statusLockdown, connected")
        )
    ),

    tar_target(
        pooled_connection_anx_models_fullwb_relevelled,
        pooled_robust_linear_models(
            imputed_data_full_wb_vars_relevelled,
            "ANX",
            "dv_covid_status + ECS_SELECTED_CH_AGE + L1 + L4 + INCOME + L9 + L11 + L12 + L14 + CG_ANX + COUNTRY"
        )
    ),

    tar_target(
        pooled_connection_anx_models_fullwb_relevelled_summary,
        summarise_pooled_results(
            pooled_connection_anx_models_fullwb_relevelled,
            c("dv_covid_statusNo lockdown", "dv_covid_statusLockdown, connected")
        )
    ),

    tar_target(
        connection_cesd_models_fullwb_relevelled,
        robust_linear_models(
            imputed_data_full_wb_vars_relevelled,
            "CES_D",
            "dv_covid_status + ECS_SELECTED_CH_AGE + L1 + L4 + INCOME + L9 + L11 + L12 + L14 + CG_CES_D"
        )
    ),

    tar_target(
        connection_cesd_models_fullwb_relevelled_summary,
        summarise_results(
            connection_cesd_models_fullwb_relevelled,
            c("dv_covid_statusNo lockdown", "dv_covid_statusLockdown, connected")
        )
    ),

    tar_target(
        pooled_connection_cesd_models_fullwb_relevelled,
        pooled_robust_linear_models(
            imputed_data_full_wb_vars_relevelled,
            "CES_D",
            "dv_covid_status + ECS_SELECTED_CH_AGE + L1 + L4 + INCOME + L9 + L11 + L12 + L14 + CG_CES_D + COUNTRY"
        )
    ),

    tar_target(
        pooled_connection_cesd_models_fullwb_relevelled_summary,
        summarise_pooled_results(
            pooled_connection_cesd_models_fullwb_relevelled,
            c("dv_covid_statusNo lockdown", "dv_covid_statusLockdown, connected")
        )
    ),

    tar_target(
        connection_sh_models_fullwb_relevelled,
        logistic_models(
            imputed_data_full_wb_vars_relevelled,
            "H6",
            "dv_covid_status + ECS_SELECTED_CH_AGE + L1 + L4 + INCOME + L9 + L11 + L12 + L14 + L41"
        )
    ),

    tar_target(
        connection_sh_models_fullwb_relevelled_summary,
        summarise_results(
            connection_sh_models_fullwb_relevelled,
            c("dv_covid_statusNo lockdown", "dv_covid_statusLockdown, connected")
        )
    ),

    tar_target(
        pooled_connection_sh_models_fullwb_relevelled,
        pooled_logistic_models(
            imputed_data_full_wb_vars_relevelled,
            "H6",
            "dv_covid_status + ECS_SELECTED_CH_AGE + L1 + L4 + INCOME + L9 + L11 + L12 + L14 + L41 + COUNTRY"
        )
    ),

    tar_target(
        pooled_connection_sh_models_fullwb_relevelled_summary,
        summarise_pooled_results(
            pooled_connection_sh_models_fullwb_relevelled,
            c("dv_covid_statusNo lockdown", "dv_covid_statusLockdown, connected")
        )
    ),

    tar_target(
        connection_paykel_models_fullwb_relevelled,
        robust_linear_models(
            imputed_data_full_wb_vars_relevelled,
            "PAYKEL",
            "dv_covid_status + ECS_SELECTED_CH_AGE + L1 + L4 + INCOME + L9 + L11 + L12 + L14 + CG_PAYKEL"
        )
    ),

    tar_target(
        connection_paykel_models_fullwb_relevelled_summary,
        summarise_results(
            connection_paykel_models_fullwb_relevelled,
            c("dv_covid_statusNo lockdown", "dv_covid_statusLockdown, connected")
        )
    ),

    tar_target(
        pooled_connection_paykel_models_fullwb_relevelled,
        pooled_robust_linear_models(
            imputed_data_full_wb_vars_relevelled,
            "PAYKEL",
            "dv_covid_status + ECS_SELECTED_CH_AGE + L1 + L4 + INCOME + L9 + L11 + L12 + L14 + CG_PAYKEL + COUNTRY"
        )
    ),

    tar_target(
        pooled_connection_paykel_models_fullwb_relevelled_summary,
        summarise_pooled_results(
            pooled_connection_paykel_models_fullwb_relevelled,
            c("dv_covid_statusNo lockdown", "dv_covid_statusLockdown, connected")
        )
    ),

    tar_target(
        internet_ls_models_fullwb,
        robust_linear_models(
            imputed_data_full_wb_vars_standardised,
            "H1", 
            "B1*CO2 + ECS_SELECTED_CH_AGE + L1 + L4 + INCOME + L9 + L11 + L12 + L14 + L18 + L36a + L36b + L36c + L36f + L50"
        )
    ),

    tar_target(
        internet_ls_models_fullwb_summary,
        summarise_results(
            internet_ls_models_fullwb,
            c("B1", "CO21", "B1:CO21")
        )
    ),

    tar_target(
        pooled_internet_ls_models_fullwb,
        pooled_robust_linear_models(
            imputed_data_full_wb_vars_standardised,
            "H1",
            "B1*CO2 + ECS_SELECTED_CH_AGE + L1 + L4 + INCOME + L9 + L11 + L12 + L14 + L18 + L36a + L36b + L36c + L36f + L50 + COUNTRY"
        )
    ),

    tar_target(
        pooled_internet_ls_models_fullwb_summary,
        summarise_pooled_results(
            pooled_internet_ls_models_fullwb,
            c("B1", "CO21", "B1:CO21")
        )
    ),

    tar_target(
        internet_swbs_models_fullwb,
        robust_linear_models(
            imputed_data_full_wb_vars_standardised,
            "CW_SWBS",
            "B1*CO2 + ECS_SELECTED_CH_AGE + L1 + L4 + INCOME + L9 + L11 + L12 + L14 + L18 + L36a + L36b + L36c + L36f + CG_CW_SWBS"
        )
    ),

    tar_target(
        internet_swbs_models_fullwb_summary,
        summarise_results(
            internet_swbs_models_fullwb,
            c("B1", "CO21", "B1:CO21")
        )
    ),

    tar_target(
        pooled_internet_swbs_models_fullwb,
        pooled_robust_linear_models(
            imputed_data_full_wb_vars_standardised,
            "CW_SWBS",
            "B1*CO2 + ECS_SELECTED_CH_AGE + L1 + L4 + INCOME + L9 + L11 + L12 + L14 + L18 + L36a + L36b + L36c + L36f + CG_CW_SWBS + COUNTRY"
        )
    ),

    tar_target(
        pooled_internet_swbs_models_fullwb_summary,
        summarise_pooled_results(
            pooled_internet_swbs_models_fullwb,
            c("B1", "CO21", "B1:CO21")
        )
    ),

    tar_target(
        internet_anx_models_fullwb,
        robust_linear_models(
            imputed_data_full_wb_vars_standardised,
            "ANX",
            "B1*CO2 + ECS_SELECTED_CH_AGE + L1 + L4 + INCOME + L9 + L11 + L12 + L14 + L18 + L36a + L36b + L36c + L36f + CG_ANX"
        )
    ),

    tar_target(
        internet_anx_models_fullwb_summary,
        summarise_results(
            internet_anx_models_fullwb,
            c("B1", "CO21", "B1:CO21")
        )
    ),

    tar_target(
        pooled_internet_anx_models_fullwb,
        pooled_robust_linear_models(
            imputed_data_full_wb_vars_standardised,
            "ANX",
            "B1*CO2 + ECS_SELECTED_CH_AGE + L1 + L4 + INCOME + L9 + L11 + L12 + L14 + L18 + L36a + L36b + L36c + L36f + CG_ANX + COUNTRY"
        )
    ),

    tar_target(
        pooled_internet_anx_models_fullwb_summary,
        summarise_pooled_results(
            pooled_internet_anx_models_fullwb,
            c("B1", "CO21", "B1:CO21")
        )
    ),

    tar_target(
        internet_cesd_models_fullwb,
        robust_linear_models(
            imputed_data_full_wb_vars_standardised,
            "CES_D",
            "B1*CO2 + ECS_SELECTED_CH_AGE + L1 + L4 + INCOME + L9 + L11 + L12 + L14 + L18 + L36a + L36b + L36c + L36f + CG_CES_D"
        )
    ),

    tar_target(
        internet_cesd_models_fullwb_summary,
        summarise_results(
            internet_cesd_models_fullwb,
            c("B1", "CO21", "B1:CO21")
        )
    ),

    tar_target(
        pooled_internet_cesd_models_fullwb,
        pooled_robust_linear_models(
            imputed_data_full_wb_vars_standardised,
            "CES_D",
            "B1*CO2 + ECS_SELECTED_CH_AGE + L1 + L4 + INCOME + L9 + L11 + L12 + L14 + L18 + L36a + L36b + L36c + L36f + CG_CES_D + COUNTRY"
        )
    ),

    tar_target(
        pooled_internet_cesd_models_fullwb_summary,
        summarise_pooled_results(
            pooled_internet_cesd_models_fullwb,
            c("B1", "CO21", "B1:CO21")
        )
    ),

    tar_target(
        internet_sh_models_fullwb,
        logistic_models(
            imputed_data_full_wb_vars_standardised,
            "H6",
            "B1*CO2 + ECS_SELECTED_CH_AGE + L1 + L4 + INCOME + L9 + L11 + L12 + L14 + L18 + L36a + L36b + L36c + L36f + L41"
        )
    ),

    tar_target(
        internet_sh_models_fullwb_summary,
        summarise_results(
            internet_sh_models_fullwb,
            c("B1", "CO21", "B1:CO21")
        )
    ),

    tar_target(
        pooled_internet_sh_models_fullwb,
        pooled_logistic_models(
            imputed_data_full_wb_vars_standardised,
            "H6",
            "B1*CO2 + ECS_SELECTED_CH_AGE + L1 + L4 + INCOME + L9 + L11 + L12 + L14 + L18 + L36a + L36b + L36c + L36f + L41 + COUNTRY"
        )
    ),

    tar_target(
        pooled_internet_sh_models_fullwb_summary,
        summarise_pooled_results(
            pooled_internet_sh_models_fullwb,
            c("B1", "CO21", "B1:CO21")
        )
    ),

    tar_target(
        internet_paykel_models_fullwb,
        robust_linear_models(
            imputed_data_full_wb_vars_standardised,
            "PAYKEL",
            "B1*CO2 + ECS_SELECTED_CH_AGE + L1 + L4 + INCOME + L9 + L11 + L12 + L14 + L18 + L36a + L36b + L36c + L36f + CG_PAYKEL"
        )
    ),

    tar_target(
        internet_paykel_models_fullwb_summary,
        summarise_results(
            internet_paykel_models_fullwb,
            c("B1", "CO21", "B1:CO21")
        )
    ),

    tar_target(
        pooled_internet_paykel_models_fullwb,
        pooled_robust_linear_models(
            imputed_data_full_wb_vars_standardised,
            "PAYKEL",
            "B1*CO2 + ECS_SELECTED_CH_AGE + L1 + L4 + INCOME + L9 + L11 + L12 + L14 + L18 + L36a + L36b + L36c + L36f + CG_PAYKEL + COUNTRY"
        )
    ),

    tar_target(
        pooled_internet_paykel_models_fullwb_summary,
        summarise_pooled_results(
            pooled_internet_paykel_models_fullwb,
            c("B1", "CO21", "B1:CO21")
        )
    ),

    tar_target(
        connection_ls_models_fullwb_weighted,
        weighted_robust_linear_models(
            imputed_data_full_wb_vars_standardised,
            "H1",
            "dv_covid_status + ECS_SELECTED_CH_AGE + L1 + L4 + INCOME + L9 + L11 + L12 + L14 + L50"
        )
    ),

    tar_target(
        connection_ls_models_fullwb_weighted_summary,
        summarise_results(
            connection_ls_models_fullwb_weighted,
            c("dv_covid_statusLockdown, disconnected", "dv_covid_statusLockdown, connected")
        )
    ),

    tar_target(
        pooled_connection_ls_models_fullwb_weighted,
        pooled_weighted_robust_linear_models(
            imputed_data_full_wb_vars_standardised,
            "H1",
            "dv_covid_status + ECS_SELECTED_CH_AGE + L1 + L4 + INCOME + L9 + L11 + L12 + L14 + L50 + COUNTRY"
        )
    ),

    tar_target(
        pooled_connection_ls_models_fullwb_weighted_summary,
        summarise_pooled_results(
            pooled_connection_ls_models_fullwb_weighted,
            c("dv_covid_statusLockdown, disconnected", "dv_covid_statusLockdown, connected")
        )
    ),

    tar_target(
        connection_swbs_models_fullwb_weighted,
        weighted_robust_linear_models(
            imputed_data_full_wb_vars_standardised,
            "CW_SWBS",
            "dv_covid_status + ECS_SELECTED_CH_AGE + L1 + L4 + INCOME + L9 + L11 + L12 + L14 + CG_CW_SWBS"
        )
    ),

    tar_target(
        connection_swbs_models_fullwb_weighted_summary,
        summarise_results(
            connection_swbs_models_fullwb_weighted,
            c("dv_covid_statusLockdown, disconnected", "dv_covid_statusLockdown, connected")
        )
    ),

    tar_target(
        pooled_connection_swbs_models_fullwb_weighted,
        pooled_weighted_robust_linear_models(
            imputed_data_full_wb_vars_standardised,
            "CW_SWBS",
            "dv_covid_status + ECS_SELECTED_CH_AGE + L1 + L4 + INCOME + L9 + L11 + L12 + L14 + CG_CW_SWBS + COUNTRY"
        )
    ),

    tar_target(
        pooled_connection_swbs_models_fullwb_weighted_summary,
        summarise_pooled_results(
            pooled_connection_swbs_models_fullwb_weighted,
            c("dv_covid_statusLockdown, disconnected", "dv_covid_statusLockdown, connected")
        )
    ),

    tar_target(
        connection_anx_models_fullwb_weighted,
        weighted_robust_linear_models(
            imputed_data_full_wb_vars_standardised,
            "ANX",
            "dv_covid_status + ECS_SELECTED_CH_AGE + L1 + L4 + INCOME + L9 + L11 + L12 + L14 + CG_ANX"
        )
    ),

    tar_target(
        connection_anx_models_fullwb_weighted_summary,
        summarise_results(
            connection_anx_models_fullwb_weighted,
            c("dv_covid_statusLockdown, disconnected", "dv_covid_statusLockdown, connected")
        )
    ),

    tar_target(
        pooled_connection_anx_models_fullwb_weighted,
        pooled_weighted_robust_linear_models(
            imputed_data_full_wb_vars_standardised,
            "ANX",
            "dv_covid_status + ECS_SELECTED_CH_AGE + L1 + L4 + INCOME + L9 + L11 + L12 + L14 + CG_ANX + COUNTRY"
        )
    ),

    tar_target(
        pooled_connection_anx_models_fullwb_weighted_summary,
        summarise_pooled_results(
            pooled_connection_anx_models_fullwb_weighted,
            c("dv_covid_statusLockdown, disconnected", "dv_covid_statusLockdown, connected")
        )
    ),

    tar_target(
        connection_cesd_models_fullwb_weighted,
        weighted_robust_linear_models(
            imputed_data_full_wb_vars_standardised,
            "CES_D",
            "dv_covid_status + ECS_SELECTED_CH_AGE + L1 + L4 + INCOME + L9 + L11 + L12 + L14 + CG_CES_D"
        )
    ),

    tar_target(
        connection_cesd_models_fullwb_weighted_summary,
        summarise_results(
            connection_cesd_models_fullwb_weighted,
            c("dv_covid_statusLockdown, disconnected", "dv_covid_statusLockdown, connected")
        )
    ),

    tar_target(
        pooled_connection_cesd_models_fullwb_weighted,
        pooled_weighted_robust_linear_models(
            imputed_data_full_wb_vars_standardised,
            "CES_D",
            "dv_covid_status + ECS_SELECTED_CH_AGE + L1 + L4 + INCOME + L9 + L11 + L12 + L14 + CG_CES_D + COUNTRY"
        )
    ),

    tar_target(
        pooled_connection_cesd_models_fullwb_weighted_summary,
        summarise_pooled_results(
            pooled_connection_cesd_models_fullwb_weighted,
            c("dv_covid_statusLockdown, disconnected", "dv_covid_statusLockdown, connected")
        )
    ),

    tar_target(
        connection_sh_models_fullwb_weighted,
        weighted_logistic_models(
            imputed_data_full_wb_vars_standardised,
            "H6",
            "dv_covid_status + ECS_SELECTED_CH_AGE + L1 + L4 + INCOME + L9 + L11 + L12 + L14 + L41"
        )
    ),

    tar_target(
        connection_sh_models_fullwb_weighted_summary,
        summarise_results(
            connection_sh_models_fullwb_weighted,
            c("dv_covid_statusLockdown, disconnected", "dv_covid_statusLockdown, connected")
        )
    ),

    tar_target(
        pooled_connection_sh_models_fullwb_weighted,
        pooled_weighted_logistic_models(
            imputed_data_full_wb_vars_standardised,
            "H6",
            "dv_covid_status + ECS_SELECTED_CH_AGE + L1 + L4 + INCOME + L9 + L11 + L12 + L14 + L41 + COUNTRY"
        )
    ),

    tar_target(
        pooled_connection_sh_models_fullwb_weighted_summary,
        summarise_pooled_results(
            pooled_connection_sh_models_fullwb_weighted,
            c("dv_covid_statusLockdown, disconnected", "dv_covid_statusLockdown, connected")
        )
    ),

    tar_target(
        connection_paykel_models_fullwb_weighted,
        weighted_robust_linear_models(
            imputed_data_full_wb_vars_standardised,
            "PAYKEL",
            "dv_covid_status + ECS_SELECTED_CH_AGE + L1 + L4 + INCOME + L9 + L11 + L12 + L14 + CG_PAYKEL"
        )
    ),

    tar_target(
        connection_paykel_models_fullwb_weighted_summary,
        summarise_results(
            connection_paykel_models_fullwb_weighted,
            c("dv_covid_statusLockdown, disconnected", "dv_covid_statusLockdown, connected")
        )
    ),

    tar_target(
        pooled_connection_paykel_models_fullwb_weighted,
        pooled_weighted_robust_linear_models(
            imputed_data_full_wb_vars_standardised,
            "PAYKEL",
            "dv_covid_status + ECS_SELECTED_CH_AGE + L1 + L4 + INCOME + L9 + L11 + L12 + L14 + CG_PAYKEL + COUNTRY"
        )
    ),

    tar_target(
        pooled_connection_paykel_models_fullwb_weighted_summary,
        summarise_pooled_results(
            pooled_connection_paykel_models_fullwb_weighted,
            c("dv_covid_statusLockdown, disconnected", "dv_covid_statusLockdown, connected")
        )
    ),

    tar_target(
        connection_ls_models_fullwb_relevelled_weighted,
        weighted_robust_linear_models(
            imputed_data_full_wb_vars_relevelled,
            "H1",
            "dv_covid_status + ECS_SELECTED_CH_AGE + L1 + L4 + INCOME + L9 + L11 + L12 + L14 + L50"
        )
    ),

    tar_target(
        connection_ls_models_fullwb_relevelled_weighted_summary,
        summarise_results(
            connection_ls_models_fullwb_relevelled_weighted,
            c("dv_covid_statusNo lockdown", "dv_covid_statusLockdown, connected")
        )
    ),

    tar_target(
        pooled_connection_ls_models_fullwb_relevelled_weighted,
        pooled_weighted_robust_linear_models(
            imputed_data_full_wb_vars_relevelled,
            "H1",
            "dv_covid_status + ECS_SELECTED_CH_AGE + L1 + L4 + INCOME + L9 + L11 + L12 + L14 + L50 + COUNTRY"
        )
    ),

    tar_target(
        pooled_connection_ls_models_fullwb_relevelled_weighted_summary,
        summarise_pooled_results(
            pooled_connection_ls_models_fullwb_relevelled_weighted,
            c("dv_covid_statusNo lockdown", "dv_covid_statusLockdown, connected")
        )
    ),

    tar_target(
        connection_swbs_models_fullwb_relevelled_weighted,
        weighted_robust_linear_models(
            imputed_data_full_wb_vars_relevelled,
            "CW_SWBS",
            "dv_covid_status + ECS_SELECTED_CH_AGE + L1 + L4 + INCOME + L9 + L11 + L12 + L14 + CG_CW_SWBS"
        )
    ),

    tar_target(
        connection_swbs_models_fullwb_relevelled_weighted_summary,
        summarise_results(
            connection_swbs_models_fullwb_relevelled_weighted,
            c("dv_covid_statusNo lockdown", "dv_covid_statusLockdown, connected")
        )
    ),

    tar_target(
        pooled_connection_swbs_models_fullwb_relevelled_weighted,
        pooled_weighted_robust_linear_models(
            imputed_data_full_wb_vars_relevelled,
            "CW_SWBS",
            "dv_covid_status + ECS_SELECTED_CH_AGE + L1 + L4 + INCOME + L9 + L11 + L12 + L14 + CG_CW_SWBS + COUNTRY"
        )
    ),

    tar_target(
        pooled_connection_swbs_models_fullwb_relevelled_weighted_summary,
        summarise_pooled_results(
            pooled_connection_swbs_models_fullwb_relevelled_weighted,
            c("dv_covid_statusNo lockdown", "dv_covid_statusLockdown, connected")
        )
    ),

    tar_target(
        connection_anx_models_fullwb_relevelled_weighted,
        weighted_robust_linear_models(
            imputed_data_full_wb_vars_relevelled,
            "ANX",
            "dv_covid_status + ECS_SELECTED_CH_AGE + L1 + L4 + INCOME + L9 + L11 + L12 + L14 + CG_ANX"
        )
    ),

    tar_target(
        connection_anx_models_fullwb_relevelled_weighted_summary,
        summarise_results(
            connection_anx_models_fullwb_relevelled_weighted,
            c("dv_covid_statusNo lockdown", "dv_covid_statusLockdown, connected")
        )
    ),

    tar_target(
        pooled_connection_anx_models_fullwb_relevelled_weighted,
        pooled_weighted_robust_linear_models(
            imputed_data_full_wb_vars_relevelled,
            "ANX",
            "dv_covid_status + ECS_SELECTED_CH_AGE + L1 + L4 + INCOME + L9 + L11 + L12 + L14 + CG_ANX + COUNTRY"
        )
    ),

    tar_target(
        pooled_connection_anx_models_fullwb_relevelled_weighted_summary,
        summarise_pooled_results(
            pooled_connection_anx_models_fullwb_relevelled_weighted,
            c("dv_covid_statusNo lockdown", "dv_covid_statusLockdown, connected")
        )
    ),

    tar_target(
        connection_cesd_models_fullwb_relevelled_weighted,
        weighted_robust_linear_models(
            imputed_data_full_wb_vars_relevelled,
            "CES_D",
            "dv_covid_status + ECS_SELECTED_CH_AGE + L1 + L4 + INCOME + L9 + L11 + L12 + L14 + CG_CES_D"
        )
    ),

    tar_target(
        connection_cesd_models_fullwb_relevelled_weighted_summary,
        summarise_results(
            connection_cesd_models_fullwb_relevelled_weighted,
            c("dv_covid_statusNo lockdown", "dv_covid_statusLockdown, connected")
        )
    ),

    tar_target(
        pooled_connection_cesd_models_fullwb_relevelled_weighted,
        pooled_weighted_robust_linear_models(
            imputed_data_full_wb_vars_relevelled,
            "CES_D",
            "dv_covid_status + ECS_SELECTED_CH_AGE + L1 + L4 + INCOME + L9 + L11 + L12 + L14 + CG_CES_D + COUNTRY"
        )
    ),

    tar_target(
        pooled_connection_cesd_models_fullwb_relevelled_weighted_summary,
        summarise_pooled_results(
            pooled_connection_cesd_models_fullwb_relevelled_weighted,
            c("dv_covid_statusNo lockdown", "dv_covid_statusLockdown, connected")
        )
    ),

    tar_target(
        connection_sh_models_fullwb_relevelled_weighted,
        weighted_logistic_models(
            imputed_data_full_wb_vars_relevelled,
            "H6",
            "dv_covid_status + ECS_SELECTED_CH_AGE + L1 + L4 + INCOME + L9 + L11 + L12 + L14 + L41"
        )
    ),

    tar_target(
        connection_sh_models_fullwb_relevelled_weighted_summary,
        summarise_results(
            connection_sh_models_fullwb_relevelled_weighted,
            c("dv_covid_statusNo lockdown", "dv_covid_statusLockdown, connected")
        )
    ),

    tar_target(
        pooled_connection_sh_models_fullwb_relevelled_weighted,
        pooled_weighted_logistic_models(
            imputed_data_full_wb_vars_relevelled,
            "H6",
            "dv_covid_status + ECS_SELECTED_CH_AGE + L1 + L4 + INCOME + L9 + L11 + L12 + L14 + L41 + COUNTRY"
        )
    ),

    tar_target(
        pooled_connection_sh_models_fullwb_relevelled_weighted_summary,
        summarise_pooled_results(
            pooled_connection_sh_models_fullwb_relevelled_weighted,
            c("dv_covid_statusNo lockdown", "dv_covid_statusLockdown, connected")
        )
    ),

    tar_target(
        connection_paykel_models_fullwb_relevelled_weighted,
        weighted_robust_linear_models(
            imputed_data_full_wb_vars_relevelled,
            "PAYKEL",
            "dv_covid_status + ECS_SELECTED_CH_AGE + L1 + L4 + INCOME + L9 + L11 + L12 + L14 + CG_PAYKEL"
        )
    ),

    tar_target(
        connection_paykel_models_fullwb_relevelled_weighted_summary,
        summarise_results(
            connection_paykel_models_fullwb_relevelled_weighted,
            c("dv_covid_statusNo lockdown", "dv_covid_statusLockdown, connected")
        )
    ),

    tar_target(
        pooled_connection_paykel_models_fullwb_relevelled_weighted,
        pooled_weighted_robust_linear_models(
            imputed_data_full_wb_vars_relevelled,
            "PAYKEL",
            "dv_covid_status + ECS_SELECTED_CH_AGE + L1 + L4 + INCOME + L9 + L11 + L12 + L14 + CG_PAYKEL + COUNTRY"
        )
    ),

    tar_target(
        pooled_connection_paykel_models_fullwb_relevelled_weighted_summary,
        summarise_pooled_results(
            pooled_connection_paykel_models_fullwb_relevelled_weighted,
            c("dv_covid_statusNo lockdown", "dv_covid_statusLockdown, connected")
        )
    ),

    tar_target(
        internet_ls_models_fullwb_weighted,
        weighted_robust_linear_models(
            imputed_data_full_wb_vars_standardised,
            "H1", 
            "B1*CO2 + ECS_SELECTED_CH_AGE + L1 + L4 + INCOME + L9 + L11 + L12 + L14 + L18 + L36a + L36b + L36c + L36f + L50"
        )
    ),

    tar_target(
        internet_ls_models_fullwb_weighted_summary,
        summarise_results(
            internet_ls_models_fullwb_weighted,
            c("B1", "CO21", "B1:CO21")
        )
    ),

    tar_target(
        pooled_internet_ls_models_fullwb_weighted,
        pooled_weighted_robust_linear_models(
            imputed_data_full_wb_vars_standardised,
            "H1",
            "B1*CO2 + ECS_SELECTED_CH_AGE + L1 + L4 + INCOME + L9 + L11 + L12 + L14 + L18 + L36a + L36b + L36c + L36f + L50 + COUNTRY"
        )
    ),

    tar_target(
        pooled_internet_ls_models_fullwb_weighted_summary,
        summarise_pooled_results(
            pooled_internet_ls_models_fullwb_weighted,
            c("B1", "CO21", "B1:CO21")
        )
    ),

    tar_target(
        internet_swbs_models_fullwb_weighted,
        weighted_robust_linear_models(
            imputed_data_full_wb_vars_standardised,
            "CW_SWBS",
            "B1*CO2 + ECS_SELECTED_CH_AGE + L1 + L4 + INCOME + L9 + L11 + L12 + L14 + L18 + L36a + L36b + L36c + L36f + CG_CW_SWBS"
        )
    ),

    tar_target(
        internet_swbs_models_fullwb_weighted_summary,
        summarise_results(
            internet_swbs_models_fullwb_weighted,
            c("B1", "CO21", "B1:CO21")
        )
    ),

    tar_target(
        pooled_internet_swbs_models_fullwb_weighted,
        pooled_weighted_robust_linear_models(
            imputed_data_full_wb_vars_standardised,
            "CW_SWBS",
            "B1*CO2 + ECS_SELECTED_CH_AGE + L1 + L4 + INCOME + L9 + L11 + L12 + L14 + L18 + L36a + L36b + L36c + L36f + CG_CW_SWBS + COUNTRY"
        )
    ),

    tar_target(
        pooled_internet_swbs_models_fullwb_weighted_summary,
        summarise_pooled_results(
            pooled_internet_swbs_models_fullwb_weighted,
            c("B1", "CO21", "B1:CO21")
        )
    ),

    tar_target(
        internet_anx_models_fullwb_weighted,
        weighted_robust_linear_models(
            imputed_data_full_wb_vars_standardised,
            "ANX",
            "B1*CO2 + ECS_SELECTED_CH_AGE + L1 + L4 + INCOME + L9 + L11 + L12 + L14 + L18 + L36a + L36b + L36c + L36f + CG_ANX"
        )
    ),

    tar_target(
        internet_anx_models_fullwb_weighted_summary,
        summarise_results(
            internet_anx_models_fullwb_weighted,
            c("B1", "CO21", "B1:CO21")
        )
    ),

    tar_target(
        pooled_internet_anx_models_fullwb_weighted,
        pooled_weighted_robust_linear_models(
            imputed_data_full_wb_vars_standardised,
            "ANX",
            "B1*CO2 + ECS_SELECTED_CH_AGE + L1 + L4 + INCOME + L9 + L11 + L12 + L14 + L18 + L36a + L36b + L36c + L36f + CG_ANX + COUNTRY"
        )
    ),

    tar_target(
        pooled_internet_anx_models_fullwb_weighted_summary,
        summarise_pooled_results(
            pooled_internet_anx_models_fullwb_weighted,
            c("B1", "CO21", "B1:CO21")
        )
    ),

    tar_target(
        internet_cesd_models_fullwb_weighted,
        weighted_robust_linear_models(
            imputed_data_full_wb_vars_standardised,
            "CES_D",
            "B1*CO2 + ECS_SELECTED_CH_AGE + L1 + L4 + INCOME + L9 + L11 + L12 + L14 + L18 + L36a + L36b + L36c + L36f + CG_CES_D"
        )
    ),

    tar_target(
        internet_cesd_models_fullwb_weighted_summary,
        summarise_results(
            internet_cesd_models_fullwb_weighted,
            c("B1", "CO21", "B1:CO21")
        )
    ),

    tar_target(
        pooled_internet_cesd_models_fullwb_weighted,
        pooled_weighted_robust_linear_models(
            imputed_data_full_wb_vars_standardised,
            "CES_D",
            "B1*CO2 + ECS_SELECTED_CH_AGE + L1 + L4 + INCOME + L9 + L11 + L12 + L14 + L18 + L36a + L36b + L36c + L36f + CG_CES_D + COUNTRY"
        )
    ),

    tar_target(
        pooled_internet_cesd_models_fullwb_weighted_summary,
        summarise_pooled_results(
            pooled_internet_cesd_models_fullwb_weighted,
            c("B1", "CO21", "B1:CO21")
        )
    ),

    tar_target(
        internet_sh_models_fullwb_weighted,
        weighted_logistic_models(
            imputed_data_full_wb_vars_standardised,
            "H6",
            "B1*CO2 + ECS_SELECTED_CH_AGE + L1 + L4 + INCOME + L9 + L11 + L12 + L14 + L18 + L36a + L36b + L36c + L36f + L41"
        )
    ),

    tar_target(
        internet_sh_models_fullwb_weighted_summary,
        summarise_results(
            internet_sh_models_fullwb_weighted,
            c("B1", "CO21", "B1:CO21")
        )
    ),

    tar_target(
        pooled_internet_sh_models_fullwb_weighted,
        pooled_weighted_logistic_models(
            imputed_data_full_wb_vars_standardised,
            "H6",
            "B1*CO2 + ECS_SELECTED_CH_AGE + L1 + L4 + INCOME + L9 + L11 + L12 + L14 + L18 + L36a + L36b + L36c + L36f + L41 + COUNTRY"
        )
    ),

    tar_target(
        pooled_internet_sh_models_fullwb_weighted_summary,
        summarise_pooled_results(
            pooled_internet_sh_models_fullwb_weighted,
            c("B1", "CO21", "B1:CO21")
        )
    ),

    tar_target(
        internet_paykel_models_fullwb_weighted,
        weighted_robust_linear_models(
            imputed_data_full_wb_vars_standardised,
            "PAYKEL",
            "B1*CO2 + ECS_SELECTED_CH_AGE + L1 + L4 + INCOME + L9 + L11 + L12 + L14 + L18 + L36a + L36b + L36c + L36f + CG_PAYKEL"
        )
    ),

    tar_target(
        internet_paykel_models_fullwb_weighted_summary,
        summarise_results(
            internet_paykel_models_fullwb_weighted,
            c("B1", "CO21", "B1:CO21")
        )
    ),

    tar_target(
        pooled_internet_paykel_models_fullwb_weighted,
        pooled_weighted_robust_linear_models(
            imputed_data_full_wb_vars_standardised,
            "PAYKEL",
            "B1*CO2 + ECS_SELECTED_CH_AGE + L1 + L4 + INCOME + L9 + L11 + L12 + L14 + L18 + L36a + L36b + L36c + L36f + CG_PAYKEL + COUNTRY"
        )
    ),

    tar_target(
        pooled_internet_paykel_models_fullwb_weighted_summary,
        summarise_pooled_results(
            pooled_internet_paykel_models_fullwb_weighted,
            c("B1", "CO21", "B1:CO21")
        )
    ),

    # Producing report of sensitivity check results
    tar_quarto(
        sensitivity_report,
            "sensitivity_report.Qmd"
    )
)