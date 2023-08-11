library(targets)
library(tarchetypes)
library(data.table)
library(dplyr)
library(ggplot2)
library(lavaan)
library(magrittr)
library(quarto)
library(reshape2)
library(scales)
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

    tar_target(
        int_lit_country_cfa_model,
        conf_fact_analysis(
            data_clean,
            "lf =~ L23a + L23b + L23c + L23d + L23e + L23f + L23g + L23h + L23i",
            group = "COUNTRY"
        )
    ),

    tar_target(
        int_lit_country_metric_model,
        conf_fact_analysis(
            data_clean,
            "lf =~ L23a + L23b + L23c + L23d + L23e + L23f + L23g + L23h + L23i",
            group = "COUNTRY",
            TRUE
        )
    ),

    tar_quarto(
        cfa_report,
        "cfa_report.Qmd"
    ),

    tar_target(
        swbs_cfa_model,
        conf_fact_analysis(
            data_clean,
            "lf =~ H4a + H4b + H4c + H4d + H4e + H4f"
        )
    ),

    tar_target(
        data_calc1,
        cfa_calc(
            data_clean,
            swbs_cfa_model,
            "CW_SWBS",
            c("H4a", "H4b", "H4c", "H4d", "H4e", "H4f")
        )
    ),

    tar_target(
        anx_cfa_model,
        conf_fact_analysis(
            data_calc1,
            "lf =~ H4_NEW_a + H4_NEW_b + H4_NEW_c + H4_NEW_d + H4_NEW_e + H4_NEW_f + H4_NEW_g"
        )
    ),

    tar_target(
        data_calc2,
        cfa_calc(
            data_calc1,
            anx_cfa_model,
            "ANX",
            c("H4_NEW_a", "H4_NEW_b", "H4_NEW_c", "H4_NEW_d", "H4_NEW_e", "H4_NEW_f", "H4_NEW_g")
        )
    ),

    tar_target(
        cesd_cfa_model,
        conf_fact_analysis(
            data_calc2,
            "lf =~ H5a + H5b + H5f + H5g"
        )
    ),

    tar_target(
        data_calc3,
        cfa_calc(
            data_calc2,
            cesd_cfa_model,
            "CES_D",
            c("H5a", "H5b", "H5f", "H5g")
        )
    ),

    tar_target(
        paykel_cfa_model,
        conf_fact_analysis(
            data_calc3,
            "lf =~ H7a + H7b + H7c + H7d + H7e"
        )
    ),

    tar_target(
        data_calc4,
        cfa_calc(
            data_calc3,
            paykel_cfa_model,
            "PAYKEL",
            c("H7a", "H7b", "H7c", "H7d", "H7e")
        )
    ),

    tar_target(
        data_calc5,
        data_calc4 %>%
            rowwise() %>%
            mutate(INCOME = sum(L52a, L52b, L52c),
                   DIFFS = if_else(if_any(starts_with("L16"), ~. == 1), 1, 0)) %>%
            select(-contains("L52"), -contains("L16"))
    ),

    tar_target(
        int_lit_cfa_model,
        conf_fact_analysis(
            data_calc5,
            "lf =~ L23a + L23b + L23c + L23d + L23e + L23f + L23g + L23h + L23i"
        )
    ),

    tar_target(
        data_calc6,
        cfa_calc(
            data_calc5,
            int_lit_cfa_model,
            "CG_INT_LIT",
            c("L23a", "L23b", "L23c", "L23d", "L23e", "L23f", "L23g", "L23h", "L23i")
        )
    ),

    tar_target(
        cg_swbs_cfa_model,
        conf_fact_analysis(
            data_calc6,
            "lf =~ L39a + L39b + L39c + L39d + L39e + L39f"
        )
    ),
    
    tar_target(
        data_calc7,
        cfa_calc(
            data_calc6,
            cg_swbs_cfa_model,
            "CG_CW_SWBS",
            c("L39a", "L39b", "L39c", "L39d", "L39e", "L39f")
        )
    ),

    tar_target(
        cg_anx_cfa_model,
        conf_fact_analysis(
            data_calc7,
            "lf =~ L51a + L51b + L51c + L51d + L51e + L51f + L51g"
        )
    ),

    tar_target(
        data_calc8,
        cfa_calc(
            data_calc7,
            cg_anx_cfa_model,
            "CG_ANX",
            c("L51a", "L51b", "L51c", "L51d", "L51e", "L51f", "L51g")
        )
    ),

    tar_target(
        cg_cesd_cfa_model,
        conf_fact_analysis(
            data_calc8,
            "lf =~ L40a + L40b + L40f + L40g"
        )
    ),

    tar_target(
        data_calc9,
        cfa_calc(
            data_calc8,
            cg_cesd_cfa_model,
            "CG_CES_D",
            c("L40a", "L40b", "L40f", "L40g")
        )
    ),

    tar_target(
        cg_paykel_cfa_model,
        conf_fact_analysis(
            data_calc9,
            "lf =~ L42a + L42b + L42c + L42d + L42e"
        )
    ),

    tar_target(
        data_vars_calc,
        cfa_calc(
            data_calc9,
            cg_paykel_cfa_model,
            "CG_PAYKEL",
            c("L42a", "L42b", "L42c", "L42d", "L42e")
        )
    )#,

    #tar_target(
    #    data_mi,
    #    multiple_imputation(
    #        data_swbs_calc
    #    )
    #)
)