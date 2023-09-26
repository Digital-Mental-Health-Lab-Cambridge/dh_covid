clean_data <- function(data){
    # Initialise new participant ID, as the original ID is not unique
    data$id_new <- seq_len(nrow(data))

    # Select relevant variables (eventual variable types: [B] = boolean, [N] = numeric, [UF] = unordered factor, [OF] = ordered factor)
    data_clean <- data %>% select(
        id_new, # New participant ID [UF]
        COUNTRY, # [UF]
        Language, # [UF]
        intStartTime, # Interview start time [N]
        ECS_SELECTED_CH, # Selected child [N]
        ECS_SELECTED_CH_GENDER, # Selected child's gender [B]
        ECS_SELECTED_CH_AGE, # Selected child's age [N]
        A1, # Classification of local area/neighbourhood [UF]
        B1, # Frequency of internet usage [N]
        CO2, # Occurrence of lockdown [B]
        CO3, # Ability to stay in touch with others during lockdown [B]
        H1, # Life satisfaction ladder [N]
        contains("H4") & !contains("CH4"), # CW-SWBS and anxiety scale [N, N]
        contains("H5") & !contains("H5_") & !contains("CH5"), # Abridged CES-D [N]
        H6, # Self-harm [B]
        contains("H7") & !contains("CH7"), # Paykel suicide scale [N]
        L1, # Number of people in household [N]
        L4, # Sole caregiver status [B]
        contains("L52"), # Contextual household income [OF]
        L8, # Caregiver marital status [B]
        L9, # Caregiver's relationship to child [UF]
        L11, # Caregiver's highest level of education [OF]
        L12, # Caregiver's employment [B]
        L14, # Type of school attended [B]
        contains("L16"), # Child's difficulties [B]
        L18, # Frequency of caregiver's internet use [N]
        contains("L36"), # Caregiver's societal attitudes [B]
        L50, # Caregiver life satisfaction ladder [N]
        contains("L39"), # Caregiver CW-SWBS [N]
        contains("L51"), # Caregiver anxiety scale [N]
        contains("L40"), # Caregiver abridged CES-D [N]
        L41, # Caregiver self-harm [B]
        contains("L42"), # Caregiver Paykel suicide scale [N]
        L43, # Caregiver's alcohol intake [N]
        L45_NEW # Caregiver's corporal punishment attitude [OF]
    )

    # Set 888 and 999 values to NA
    is.na(data_clean[, !(names(data_clean) %in% c("id_new", "intStartTime"))]) <- data_clean[, !(names(data_clean) %in% c("id_new", "intStartTime"))] > 887

    # Delete all responses for H4f and L39f in Mozambique (because of a translation error)
    data_clean[data_clean$COUNTRY == 3, c("H4f", "L39f")] <- NA

    # Set CO2 to 0 for all responses in Tanzania (there were no lockdowns in the country)
    data_clean[data_clean$COUNTRY == 6, "CO2"] <- 0

    data_clean %<>% 
        mutate(
            # Set CO3 to 1 for all rows where "CO2" is 0 (i.e. normal connections were maintained by default if no lockdown occurred)
            CO3 = case_when(
                CO2 == 0 ~ 1L,
                TRUE ~ CO3
            ),

            # Merging sparse categories in marital status (L8), caregiver's relationship to study child (L9) and type of school attended (L14)
            L8 = case_match(L8,
                3:6 ~ 1L,
                .default = L8
            ),
            L9 = case_match(L9,
                4:6 ~ 3L,
                .default = L9
            ),
            L14 = case_match(L14,
                3:5 ~ 1L,
                .default = L14
            ),

            # Setting "other" to NA in caregiver's highest level of education (L11)
            L11 = case_match(L11,
                7 ~ NA,
                .default = L11
            ),

            # Combining income and difficulties variables
            INCOME = rowSums(select(., L52a, L52b, L52c)),
            DIFFS = if_else(
                if_any(starts_with("L16"), ~. == 1),
                1,
                0
            ),

            # Recoding binary variables to 0/1
            ECS_SELECTED_CH_GENDER = case_match(ECS_SELECTED_CH_GENDER,
                1 ~ 0L,
                2 ~ 1L,
                .default = ECS_SELECTED_CH_GENDER
            ),
            L8 = case_match(L8,
                1 ~ 0L,
                2 ~ 1L,
                .default = L8
            ),
            L14 = case_match(L14,
                1 ~ 0L,
                2 ~ 1L,
                .default = L14
            )
        ) %>%

        # Removing unnecessary variables
        select(-contains("L52"), -contains("L16"))

    # Making factor variables as necessary
    ## Unordered
    for(var in c("id_new",  "COUNTRY", "Language", "ECS_SELECTED_CH_GENDER", "A1", "L9")){
        data_clean[, var] <- as.factor(data_clean[, var])
    }

    ### Ordered
    data_clean$L11 %<>% ordered(levels = c(1, 2, 3))
    data_clean$L45_NEW %<>% ordered(levels = c(1, 3, 2))
    data_clean$INCOME %<>% ordered(levels = c(0, 1, 2, 3))

    return(data_clean)
}

plot_overall_NA <- function(data){
    plot <- ggplot(data, aes(x = var, y = missingness)) +
        geom_col() +
        labs(x = "Variable", y = "Missingness", title = "Overall missingness") +
        ylim(0, 1) +
        theme_linedraw() +
        theme(axis.text.x=element_text(angle = 45, hjust = 1))

    return(plot)
}

country_missingness <- function(data){
    NA_by_country <- lapply(
        seq_len(length(names(data))),
        function(x){
            data.frame(
                country = unique(data$COUNTRY), 
                missingness = rep(NA, length(unique(data$COUNTRY))),
                proportion = rep(NA, length(unique(data$COUNTRY)))
            ) %>% mutate(
                country = case_match(country,
                    "1" ~ "Ethiopia",
                    "2" ~ "Kenya",
                    "3" ~ "Mozambique",
                    "4" ~ "Namibia",
                    "6" ~ "Tanzania",
                    "7" ~ "Uganda",
                    "8" ~ "Cambodia",
                    "9" ~ "Indonesia",
                    "10" ~ "Malaysia",
                    "11" ~ "Philippines",
                    "12" ~ "Thailand",
                    "13" ~ "Vietnam"
                )
            )
        }
    )

    for(i in seq_len(length(names(data)))){
        for(j in seq_len(length(unique(data$COUNTRY)))){
            NA_by_country[[i]]$missingness[j] <- 
                sum(is.na(data[data$COUNTRY == unique(data$COUNTRY)[j], i])) /
                length(data[data$COUNTRY == unique(data$COUNTRY)[j], i])

            NA_by_country[[i]]$proportion[j] <-
                length(data[data$COUNTRY == unique(data$COUNTRY)[j], i]) /
                nrow(data)
        }
    }

    names(NA_by_country) <- names(data)

    return(NA_by_country)
}

plot_country_NA <- function(NA_by_country){
    country_NA_plots <- lapply(
        seq_len(length(NA_by_country)),
        function(x){
            NA_by_country[[x]] %<>% melt(id.vars = "country")

            ggplot(NA_by_country[[x]], aes(x = country, y = value, fill = variable)) +
                geom_col(position = "dodge") +
                labs(x = "Country", y = NULL, title = names(NA_by_country)[x]) +
                ylim(0, 1) +
                scale_fill_manual(values = c("black", "red")) +
                theme_linedraw() +
                theme(axis.text.x=element_text(angle = 45, hjust = 1))
        }
    )

    names(country_NA_plots) <- names(NA_by_country)

    return(country_NA_plots)
}

language_missingness <- function(data){
    NA_by_language <- lapply(
        seq_len(length(names(data))),
        function(x){
            data.frame(
                language = unique(data$Language), 
                missingness = rep(NA, length(unique(data$Language))),
                proportion = rep(NA, length(unique(data$Language)))
            )
        }
    )

    for(i in seq_len(length(names(data)))){
        for(j in seq_len(length(unique(data$Language)))){
            NA_by_language[[i]]$missingness[j] <- 
                sum(is.na(data[data$Language == unique(data$Language)[j], i])) /
                length(data[data$Language == unique(data$Language)[j], i])

            NA_by_language[[i]]$proportion[j] <-
                length(data[data$Language == unique(data$Language)[j], i]) /
                nrow(data)
        }
    }

    names(NA_by_language) <- names(data)

    return(NA_by_language)
}

plot_language_NA <- function(NA_by_language){
    language_NA_plots <- lapply(
        seq_len(length(NA_by_language)),
        function(x){
            NA_by_language[[x]] %<>% melt(id.vars = "language")

            ggplot(NA_by_language[[x]], aes(x = language, y = value, fill = variable)) +
                geom_col(position = "dodge") +
                labs(y = NULL, title = names(NA_by_language)[x]) +
                ylim(0, 1) +
                scale_x_discrete("Language") +
                scale_fill_manual(values = c("black", "red")) +
                theme_linedraw() +
                theme(axis.text.x=element_text(angle = 45, hjust = 1))
        }
    )

    names(language_NA_plots) <- names(NA_by_language)

    return(language_NA_plots)
}

conf_fact_analysis <- function(data, model, group = NULL, groups_constrained = FALSE){
    if(groups_constrained == TRUE){
        cfa_model <- cfa(
            model,
            data,
            missing = "ml",
            group = group,
            group.equal = "loadings"
        )
    } else if(groups_constrained == FALSE) {
        cfa_model <- cfa(
            model,
            data,
            missing = "ml",
            group = group
        )
    }

    if(is.null(group)){
        cfa_prediction <- rescale(as.vector(lavPredict(cfa_model)), c(0, 1))
        return(cfa_prediction)
    } else {
        return(cfa_model)
    }
}

cfa_calc <- function(data, prediction, new_var, old_vars){
    data[, new_var] <- prediction

    data %<>% select(-all_of(old_vars))

    return(data)
}

multiple_imputation <- function(data){
    varnames <- names(data)
    varnames <- varnames[! varnames %in% c("id_new", "COUNTRY", "Language", "intStartTime", "ECS_SELECTED_CH", "ECS_SELECTED_CH_GENDER", "ECS_SELECTED_CH_AGE")]
    
    for(i in varnames){
        if(is.numeric(unlist(data[, i]))){
            data[, i] <- rescale(unlist(data[, i]), c(0, 1))
        }
    }

    methods <- make.method(data)
    methods[] <- "pmm"
    methods[c("id_new", "COUNTRY", "Language", "intStartTime", "ECS_SELECTED_CH", "ECS_SELECTED_CH_GENDER", "ECS_SELECTED_CH_AGE")] <- ""

    predictorMatrix <- make.predictorMatrix(data)
    predictorMatrix[, c("id_new", "COUNTRY", "CO2", "CO3", "Language", "intStartTime")] <- 0
    predictorMatrix[c("id_new", "COUNTRY", "Language", "intStartTime", "ECS_SELECTED_CH", "ECS_SELECTED_CH_GENDER", "ECS_SELECTED_CH_AGE"), ] <- 0

    list_imputed_data <- list()

    for(countryNo in 1:length(unique(data$COUNTRY))){
        country_data <- data[data$COUNTRY == unique(data$COUNTRY)[countryNo], ]

        cl <- makeCluster(detectCores() - 2)

        clusterSetRNGStream(cl, 2903)

        clusterExport(cl, c("country_data", "methods", "predictorMatrix"), envir = environment())

        clusterEvalQ(cl, library(mice))
        clusterEvalQ(cl, library(miceadds))

        imp_pars <- parLapply(cl = cl, X = 1:(detectCores() - 2), fun = function(no){ 
            mice(country_data, vis = rev(names(country_data)), method = methods, predictorMatrix = predictorMatrix, m = 1, maxit = 20)
        })

        data_imputed <- imp_pars[[1]]
        for(i in 2:length(imp_pars)){
            data_imputed <- ibind(data_imputed, imp_pars[[i]])
        }

        list_imputed_data[[countryNo]] <- data_imputed
    }

    return(list_imputed_data)
}

imputations_delist <- function(mids_list){
    data_imputed <- mids_list[[1]]
    for(i in 2:length(mids_list)){
        data_imputed <- rbind(data_imputed, mids_list[[i]])
    }

    return(data_imputed)
}

logistic_mixed_model <- function(data, y, formula_RHS){
    plan(multisession, workers = availableCores() - 2)

    model_fit <- brm_multiple(bf(as.formula(paste(y, "~", formula_RHS, "+ (1 | COUNTRY)"))), data, family = bernoulli(), chains = 1)

    return(model_fit)
}

zoib_mixed_model <- function(data, y, formula_RHS){
    plan(multisession, workers = availableCores() - 2)

    model_fit <- brm_multiple(bf(as.formula(paste(y, "~", formula_RHS, "+ (1 | COUNTRY)"))), data, family = zero_one_inflated_beta(), chains = 1)

    return(model_fit)
}