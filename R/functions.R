clean_data <- function(data){
    # Initialise new participant ID, as the original ID is not unique
    data$id_new <- seq_len(nrow(data))

    # Select relevant variables (eventual variable types: [B] = boolean, [N] = numeric, [UF] = unordered factor, [OF] = ordered factor)
    data_clean <- data %>% dplyr::select(
        id_new, # New participant ID [UF]
        ECS_AddressID, # Address ID (from which PSU will be extracted) [UF]
        COUNTRY, # [UF]
        Language, # [UF]
        intStartTime, # Interview start time [N]
        ECS_SELECTED_CH, # Selected child [N]
        ECS_SELECTED_CH_GENDER, # Selected child's gender [B]
        ECS_SELECTED_CH_AGE, # Selected child's age [N]
        Urbanity, # Classification of local area/neighbourhood [UF]
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
        L45_NEW, # Caregiver's corporal punishment attitude [OF]
        wgt_scaled # Survey weights scaled by country [N]
    )

    # Set 888 and 999 values to NA
    is.na(data_clean[, !(names(data_clean) %in% c("id_new", "ECS_AddressID", "intStartTime", "Urbanity"))]) <- data_clean[, !(names(data_clean) %in% c("id_new", "ECS_AddressID", "intStartTime", "Urbanity"))] > 887

    # Delete all responses for H4f and L39f in Mozambique (because of a translation error)
    data_clean[data_clean$COUNTRY == 3, c("H4f", "L39f")] <- NA

    # Set CO2 to 0 for all responses in Tanzania (there were no lockdowns in the country)
    data_clean[data_clean$COUNTRY == 6, "CO2"] <- 0

    data_clean %<>% 
        mutate(
            # Derive lockdown status & connection variable
            dv_covid_status = case_when(
                CO2 == 0 ~ "No lockdown",
                CO2 == 1 & CO3 == 0 ~ "Lockdown, disconnected",
                CO2 == 1 & CO3 == 1 ~ "Lockdown, connected"
            ),

            # Merging sparse categories in marital status (L8), caregiver's relationship to study child (L9), caregiver's education (L11) and type of school attended (L14)
            L8 = case_match(L8,
                3:6 ~ 1L,
                .default = L8
            ),
            L9 = case_match(L9,
                4:6 ~ 3L,
                .default = L9
            ),
            L11 = case_match(L11,
                3 ~ 2L,
                .default = L11
            ),
            L14 = case_match(L14,
                3:5 ~ 1L,
                .default = L14
            ),

            # Combining income and difficulties variables
            INCOME = rowSums(dplyr::select(., L52a, L52b, L52c)),
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
            L11 = case_match(L11,
                1 ~ 0L,
                2 ~ 1L,
                .default = L11  
            ),
            L14 = case_match(L14,
                1 ~ 0L,
                2 ~ 1L,
                .default = L14
            ),

            # Recoding urbanity
            Urbanity = case_match(Urbanity,
                "Rural     " ~ "Rural",
                "Urban     " ~ "Urban",
                .default = Urbanity
            )
        ) %>%

        # Removing unnecessary variables
        dplyr::select(-contains("L52"), -contains("L16"), -CO3) %>%

        # Renaming PSU variable
        rename(PSU = ECS_AddressID)

    # Extract PSU from ECS_AddressID
    data_clean$PSU <- sapply(data_clean$PSU, function(x) gsub("(.{3,})(Z[01])+?.*", "\\1", x))

    # Making factor variables as necessary
    ## Unordered
    for(var in c("id_new", "PSU", "COUNTRY", "Language", "ECS_SELECTED_CH", "ECS_SELECTED_CH_GENDER", "Urbanity", "CO2", "H6", "L4", "L8", "L9", "L12", "L14", "DIFFS", "L36a", "L36b", "L36c", "L36f", "L41", "dv_covid_status")){
        data_clean[, var] <- as.factor(data_clean[, var])
    }

    ### Ordered
    data_clean$L45_NEW %<>% ordered(levels = c(1, 3, 2))
    data_clean$INCOME %<>% ordered(levels = c(0, 1, 2, 3))

    # Setting "no lockdown" as the reference category for the derived lockdown status variable
    data_clean$dv_covid_status %<>% relevel("No lockdown")

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
        # For each variable
        seq_len(length(names(data))),

        # Preparing a data frame to store the amount of missingness by country
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

    # For each variable
    for(i in seq_len(length(names(data)))){

        # For each country
        for(j in seq_len(length(unique(data$COUNTRY)))){

            # Calculating the number and proportion of missing responses
            NA_by_country[[i]]$missingness[j] <- 
                sum(is.na(data[data$COUNTRY == unique(data$COUNTRY)[j], i])) /
                length(data[data$COUNTRY == unique(data$COUNTRY)[j], i])

            NA_by_country[[i]]$proportion[j] <-
                length(data[data$COUNTRY == unique(data$COUNTRY)[j], i]) /
                nrow(data)
        }
    }

    # Renaming columns in missingness data frame
    names(NA_by_country) <- names(data)

    return(NA_by_country)
}

plot_country_NA <- function(NA_by_country){
    country_NA_plots <- lapply(

        # For each variable
        seq_len(length(NA_by_country)),

        # Plotting the missingness in each country
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

    # Renaming each item in the list
    names(country_NA_plots) <- names(NA_by_country)

    return(country_NA_plots)
}

language_missingness <- function(data){
    NA_by_language <- lapply(

        # For each variable
        seq_len(length(names(data))),

        # Preparing a data frame to store the amount of missingness by language
        function(x){
            data.frame(
                language = unique(data$Language), 
                missingness = rep(NA, length(unique(data$Language))),
                proportion = rep(NA, length(unique(data$Language)))
            )
        }
    )

    # For each variable
    for(i in seq_len(length(names(data)))){

        # For each language
        for(j in seq_len(length(unique(data$Language)))){

            # Calculating the number and proportion of missing responses
            NA_by_language[[i]]$missingness[j] <- 
                sum(is.na(data[data$Language == unique(data$Language)[j], i])) /
                length(data[data$Language == unique(data$Language)[j], i])

            NA_by_language[[i]]$proportion[j] <-
                length(data[data$Language == unique(data$Language)[j], i]) /
                nrow(data)
        }
    }

    # Renaming each item in the list
    names(NA_by_language) <- names(data)

    return(NA_by_language)
}

plot_language_NA <- function(NA_by_language){
    language_NA_plots <- lapply(

        # For each variable
        seq_len(length(NA_by_language)),

        # Plotting the missingness in each language
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

    # Renaming each item in the list
    names(language_NA_plots) <- names(NA_by_language)

    return(language_NA_plots)
}

conf_fact_analysis <- function(data, model, group = NULL, constraints = ""){
    # CFA model with loadings constrained to be equal across countries
    cfa_model <- cfa(
        model,
        data,
        missing = "ml",
        group = group,
        group.equal = constraints
    )

    # Rescale and return CFA output
    if(is.null(group)){
        cfa_prediction <- rescale(as.vector(lavPredict(cfa_model)), c(0, 1))
        return(cfa_prediction)
    } else {
        return(cfa_model)
    }
}

cfa_calc <- function(data, prediction, new_var, old_vars){
    # Add CFA output to data frame
    data[, new_var] <- prediction

    # Remove original variables
    data %<>% dplyr::select(-all_of(old_vars))

    return(data)
}

multiple_imputation <- function(data){
    # Listing variables needing to be rescaled
    varnames <- names(data)
    varnames <- varnames[! varnames %in% c("id_new", "PSU", "COUNTRY", "Language", "intStartTime", "ECS_SELECTED_CH", "ECS_SELECTED_CH_GENDER", "ECS_SELECTED_CH_AGE", "Urbanity", "wgt_scaled")]
    
    # Rescaling variables as necessary
    for(i in varnames){
        if(is.numeric(unlist(data[, i]))){
            data[, i] <- rescale(unlist(data[, i]), c(0, 1))
        }
    }

    # Setting method to pmm, except for those variables that should not be imputed
    methods <- make.method(data)
    methods[] <- "pmm"
    methods[c("id_new", "PSU", "COUNTRY", "Language", "intStartTime", "ECS_SELECTED_CH", "ECS_SELECTED_CH_GENDER", "ECS_SELECTED_CH_AGE", "Urbanity", "wgt_scaled")] <- ""

    # Setting certain variables to not predict the rest, and again specifying which variables are not to be imputed at all
    predictorMatrix <- make.predictorMatrix(data)
    predictorMatrix[, c("id_new", "PSU", "COUNTRY", "Language", "intStartTime", "CO2", "wgt_scaled")] <- 0
    predictorMatrix[c("id_new", "PSU", "COUNTRY", "Language", "intStartTime", "ECS_SELECTED_CH", "ECS_SELECTED_CH_GENDER", "ECS_SELECTED_CH_AGE", "Urbanity", "wgt_scaled"), ] <- 0

    imputed_data <- mice(data, vis = "monotone", method = methods, predictorMatrix = predictorMatrix, m = 20, maxit = 20)

    return(imputed_data)
}

# Setting "lockdown, disconnected" to be the reference level of the derived lockdown variable for relevelled datasets
covid_relevel <- function(data){
    lapply(data[[1]], function(x){
        x$dv_covid_status %<>% relevel("Lockdown, disconnected")

        return(x)
    })
}

standardise_vars <- function(data){
    # Converting imputed data to imputationList object
    data_list <- imputationList(lapply(1:data$m, function(x) complete(data, x)))

    # Removing rows with no survey weight (excluded participants)
    data_list <- filter(data_list, !is.na(wgt_scaled))

    # Standardising mental health, internet use and lockdown variables
    for(i in c("H1", "CW_SWBS", "ANX", "CES_D", "PAYKEL", "B1", "CO2")){
        for(j in seq_along(data_list[[1]])){
            data_list[[1]][[j]][, i] <- scale(as.numeric(data_list[[1]][[j]][, i]))
        }
    }

    return(data_list)
}

# Defining filter function for imputationList objects
filter.imputationList <- function(data, subset){
    imputationList(lapply(seq_along(data[[1]]), function(x) filter(data[[1]][[x]], {{subset}})))
}

logistic_models <- function(data, y, formula_RHS){
    # Initialising lists of model fits
    fitlist <- list()
    boys_fitlist <- list()
    girls_fitlist <- list()
    rural_fitlist <- list()
    urban_fitlist <- list()
    boys_rural_fitlist <- list()
    boys_urban_fitlist <- list()
    girls_rural_fitlist <- list()
    girls_urban_fitlist <- list()

    # For each country
    for(i in unique(data[[1]][[1]]$COUNTRY)){

        # Skipping Tanzania
        if(i == 6){
            next
        }

        # Selecting relevant dataset
        these_data <- filter(data, COUNTRY == i)

        # Creating sub-datasets
        boys_data <- filter(these_data, ECS_SELECTED_CH_GENDER == 0)
        girls_data <- filter(these_data, ECS_SELECTED_CH_GENDER == 1)
        rural_data <- filter(these_data, Urbanity == "Rural")
        urban_data <- filter(these_data, Urbanity == "Urban")
        boys_rural_data <- filter(boys_data, Urbanity == "Rural")
        boys_urban_data <- filter(boys_data, Urbanity == "Urban")
        girls_rural_data <- filter(girls_data, Urbanity == "Rural")
        girls_urban_data <- filter(girls_data, Urbanity == "Urban")

        # Fitting logistic regression models
        fitlist[[i]] <- with(these_data, glm(as.formula(paste(y, "~", formula_RHS, "+ ECS_SELECTED_CH_GENDER + Urbanity")), family = "binomial"))
        try(boys_fitlist[[i]] <- with(boys_data, glm(as.formula(paste(y, "~", formula_RHS, "+ Urbanity")), family = "binomial")), silent = TRUE)
        try(girls_fitlist[[i]] <- with(girls_data, glm(as.formula(paste(y, "~", formula_RHS, "+ Urbanity")), family = "binomial")), silent = TRUE)
        try(rural_fitlist[[i]] <- with(rural_data, glm(as.formula(paste(y, "~", formula_RHS, "+ ECS_SELECTED_CH_GENDER")), family = "binomial")), silent = TRUE)
        try(urban_fitlist[[i]] <- with(urban_data, glm(as.formula(paste(y, "~", formula_RHS, "+ ECS_SELECTED_CH_GENDER")), family = "binomial")), silent = TRUE)
        try(boys_rural_fitlist[[i]] <- with(boys_rural_data, glm(as.formula(paste(y, "~", formula_RHS)), family = "binomial")), silent = TRUE)
        try(boys_urban_fitlist[[i]] <- with(boys_urban_data, glm(as.formula(paste(y, "~", formula_RHS)), family = "binomial")), silent = TRUE)
        try(girls_rural_fitlist[[i]] <- with(girls_rural_data, glm(as.formula(paste(y, "~", formula_RHS)), family = "binomial")), silent = TRUE)
        try(girls_urban_fitlist[[i]] <- with(girls_urban_data, glm(as.formula(paste(y, "~", formula_RHS)), family = "binomial")), silent = TRUE)
    }

    return(list(fitlist, boys_fitlist, girls_fitlist, rural_fitlist, urban_fitlist, boys_rural_fitlist, boys_urban_fitlist, girls_rural_fitlist, girls_urban_fitlist))
}

pooled_logistic_models <- function(data, y, formula_RHS){
    # Removing Tanzania rows
    data <- filter(data, COUNTRY != 6)
    for(i in seq_along(data[[1]])){
        data[[1]][[i]]$COUNTRY <- droplevels(data[[1]][[i]]$COUNTRY)
    }

    # Creating sub-datasets
    boys_data <- filter(data, ECS_SELECTED_CH_GENDER == 0)
    girls_data <- filter(data, ECS_SELECTED_CH_GENDER == 1)
    rural_data <- filter(data, Urbanity == "Rural")
    urban_data <- filter(data, Urbanity == "Urban")
    boys_rural_data <- filter(boys_data, Urbanity == "Rural")
    boys_urban_data <- filter(boys_data, Urbanity == "Urban")
    girls_rural_data <- filter(girls_data, Urbanity == "Rural")
    girls_urban_data <- filter(girls_data, Urbanity == "Urban")

    # Fitting logistic regression models
    model_fit <- with(data, glm(as.formula(paste(y, "~", formula_RHS, "+ ECS_SELECTED_CH_GENDER + Urbanity")), family = "binomial"))
    boys_model_fit <- with(boys_data, glm(as.formula(paste(y, "~", formula_RHS, "+ Urbanity")), family = "binomial"))
    girls_model_fit <- with(girls_data, glm(as.formula(paste(y, "~", formula_RHS, "+ Urbanity")), family = "binomial"))
    rural_model_fit <- with(rural_data, glm(as.formula(paste(y, "~", formula_RHS, "+ ECS_SELECTED_CH_GENDER")), family = "binomial"))
    urban_model_fit <- with(urban_data, glm(as.formula(paste(y, "~", formula_RHS, "+ ECS_SELECTED_CH_GENDER")), family = "binomial"))
    boys_rural_model_fit <- with(boys_rural_data, glm(as.formula(paste(y, "~", formula_RHS)), family = "binomial"))
    boys_urban_model_fit <- with(boys_urban_data, glm(as.formula(paste(y, "~", formula_RHS)), family = "binomial"))
    girls_rural_model_fit <- with(girls_rural_data, glm(as.formula(paste(y, "~", formula_RHS)), family = "binomial"))
    girls_urban_model_fit <- with(girls_urban_data, glm(as.formula(paste(y, "~", formula_RHS)), family = "binomial"))

    return(list(model_fit, boys_model_fit, girls_model_fit, rural_model_fit, urban_model_fit, boys_rural_model_fit, boys_urban_model_fit, girls_rural_model_fit, girls_urban_model_fit))
}

robust_linear_models <- function(data, y, formula_RHS){
    # Initialising lists of model fits
    fitlist <- list()
    boys_fitlist <- list()
    girls_fitlist <- list()
    rural_fitlist <- list()
    urban_fitlist <- list()
    boys_rural_fitlist <- list()
    boys_urban_fitlist <- list()
    girls_rural_fitlist <- list()
    girls_urban_fitlist <- list()

    for(i in unique(complete(data, 1)$COUNTRY)){

        # Skipping Tanzania
        if(i == 6){
            next
        }

        # Selecting relevant dataset
        these_data <- filter(data, COUNTRY == i)

        # Creating sub-datasets
        boys_data <- filter(these_data, ECS_SELECTED_CH_GENDER == 0)
        girls_data <- filter(these_data, ECS_SELECTED_CH_GENDER == 1)
        rural_data <- filter(these_data, Urbanity == "Rural")
        urban_data <- filter(these_data, Urbanity == "Urban")
        boys_rural_data <- filter(boys_data, Urbanity == "Rural")
        boys_urban_data <- filter(boys_data, Urbanity == "Urban")
        girls_rural_data <- filter(girls_data, Urbanity == "Rural")
        girls_urban_data <- filter(girls_data, Urbanity == "Urban")

        # Fitting robust linear regression models
        fitlist[[i]] <- with(these_data, rlm(as.formula(paste(y, "~", formula_RHS, "+ ECS_SELECTED_CH_GENDER + Urbanity"))))
        try(boys_fitlist[[i]] <- with(boys_data, rlm(as.formula(paste(y, "~", formula_RHS, "+ Urbanity")))), silent = TRUE)
        try(girls_fitlist[[i]] <- with(girls_data, rlm(as.formula(paste(y, "~", formula_RHS, "+ Urbanity")))), silent = TRUE)
        try(rural_fitlist[[i]] <- with(rural_data, rlm(as.formula(paste(y, "~", formula_RHS, "+ ECS_SELECTED_CH_GENDER")))), silent = TRUE)
        try(urban_fitlist[[i]] <- with(urban_data, rlm(as.formula(paste(y, "~", formula_RHS, "+ ECS_SELECTED_CH_GENDER")))), silent = TRUE)
        try(boys_rural_fitlist[[i]] <- with(boys_rural_data, rlm(as.formula(paste(y, "~", formula_RHS)))), silent = TRUE)
        try(boys_urban_fitlist[[i]] <- with(boys_urban_data, rlm(as.formula(paste(y, "~", formula_RHS)))), silent = TRUE)
        try(girls_rural_fitlist[[i]] <- with(girls_rural_data, rlm(as.formula(paste(y, "~", formula_RHS)))), silent = TRUE)
        try(girls_urban_fitlist[[i]] <- with(girls_urban_data, rlm(as.formula(paste(y, "~", formula_RHS)))), silent = TRUE)
    }

    return(list(fitlist, boys_fitlist, girls_fitlist, rural_fitlist, urban_fitlist, boys_rural_fitlist, boys_urban_fitlist, girls_rural_fitlist, girls_urban_fitlist))
}

pooled_robust_linear_models <- function(data, y, formula_RHS){
    # Removing Tanzania rows
    data <- filter(data, COUNTRY != 6)
    for(i in seq_along(data[[1]])){
        data[[1]][[x]]$COUNTRY <- droplevels(data[[1]][[x]]$COUNTRY)
    }
    
    # Creating sub-datasets
    boys_data <- filter(data, ECS_SELECTED_CH_GENDER == 0)
    girls_data <- filter(data, ECS_SELECTED_CH_GENDER == 1)
    rural_data <- filter(data, Urbanity == "Rural")
    urban_data <- filter(data, Urbanity == "Urban")
    boys_rural_data <- filter(boys_data, Urbanity == "Rural")
    boys_urban_data <- filter(boys_data, Urbanity == "Urban")
    girls_rural_data <- filter(girls_data, Urbanity == "Rural")
    girls_urban_data <- filter(girls_data, Urbanity == "Urban")

    # Fitting robust linear regression models
    model_fit <- with(data, rlm(as.formula(paste(y, "~", formula_RHS, "+ ECS_SELECTED_CH_GENDER + Urbanity"))))
    boys_model_fit <- with(boys_data, rlm(as.formula(paste(y, "~", formula_RHS, "+ Urbanity"))))
    girls_model_fit <- with(girls_data, rlm(as.formula(paste(y, "~", formula_RHS, "+ Urbanity"))))
    rural_model_fit <- with(rural_data, rlm(as.formula(paste(y, "~", formula_RHS, "+ ECS_SELECTED_CH_GENDER"))))
    urban_model_fit <- with(urban_data, rlm(as.formula(paste(y, "~", formula_RHS, "+ ECS_SELECTED_CH_GENDER"))))
    boys_rural_model_fit <- with(boys_rural_data, rlm(as.formula(paste(y, "~", formula_RHS))))
    boys_urban_model_fit <- with(boys_urban_data, rlm(as.formula(paste(y, "~", formula_RHS))))
    girls_rural_model_fit <- with(girls_rural_data, rlm(as.formula(paste(y, "~", formula_RHS))))
    girls_urban_model_fit <- with(girls_urban_data, rlm(as.formula(paste(y, "~", formula_RHS))))

    return(list(model_fit, boys_model_fit, girls_model_fit, rural_model_fit, urban_model_fit, boys_rural_model_fit, boys_urban_model_fit, girls_rural_model_fit, girls_urban_model_fit))
}

weighted_logistic_models <- function(data, y, formula_RHS){
    # Initialising lists of model fits
    fitlist <- list()
    boys_fitlist <- list()
    girls_fitlist <- list()
    rural_fitlist <- list()
    urban_fitlist <- list()
    boys_rural_fitlist <- list()
    boys_urban_fitlist <- list()
    girls_rural_fitlist <- list()
    girls_urban_fitlist <- list()

    # For each country
    for(i in unique(data[[1]][[1]]$COUNTRY)){

        # Skipping Tanzania
        if(i == 6){
            next
        }

        # Selecting relevant dataset
        these_data <- filter(data, COUNTRY == i)

        # Creating sub-datasets
        boys_data <- filter(these_data, ECS_SELECTED_CH_GENDER == 0)
        girls_data <- filter(these_data, ECS_SELECTED_CH_GENDER == 1)
        rural_data <- filter(these_data, Urbanity == "Rural")
        urban_data <- filter(these_data, Urbanity == "Urban")
        boys_rural_data <- filter(boys_data, Urbanity == "Rural")
        boys_urban_data <- filter(boys_data, Urbanity == "Urban")
        girls_rural_data <- filter(girls_data, Urbanity == "Rural")
        girls_urban_data <- filter(girls_data, Urbanity == "Urban")

        # Creating survey design objects
        designs <- svydesign(ids = ~PSU, weights = ~wgt_scaled, data = these_data)
        boys_designs <- svydesign(ids = ~PSU, weights = ~wgt_scaled, data = boys_data)
        girls_designs <- svydesign(ids = ~PSU, weights = ~wgt_scaled, data = girls_data)
        rural_designs <- svydesign(ids = ~PSU, weights = ~wgt_scaled, data = rural_data)
        urban_designs <- svydesign(ids = ~PSU, weights = ~wgt_scaled, data = urban_data)
        boys_rural_designs <- svydesign(ids = ~PSU, weights = ~wgt_scaled, data = boys_rural_data)
        boys_urban_designs <- svydesign(ids = ~PSU, weights = ~wgt_scaled, data = boys_urban_data)
        girls_rural_designs <- svydesign(ids = ~PSU, weights = ~wgt_scaled, data = girls_rural_data)
        girls_urban_designs <- svydesign(ids = ~PSU, weights = ~wgt_scaled, data = girls_urban_data)

        # Fitting weighted logistic regression models
        fitlist[[i]] <- with(designs, svyglm(as.formula(paste(y, "~", formula_RHS, "+ ECS_SELECTED_CH_GENDER + Urbanity")), family = "quasibinomial"))
        try(boys_fitlist[[i]] <- with(boys_designs, svyglm(as.formula(paste(y, "~", formula_RHS, "+ Urbanity")), family = "quasibinomial")), silent = TRUE)
        try(girls_fitlist[[i]] <- with(girls_designs, svyglm(as.formula(paste(y, "~", formula_RHS, "+ Urbanity")), family = "quasibinomial")), silent = TRUE)
        try(rural_fitlist[[i]] <- with(rural_designs, svyglm(as.formula(paste(y, "~", formula_RHS, "+ ECS_SELECTED_CH_GENDER")), family = "quasibinomial")), silent = TRUE)
        try(urban_fitlist[[i]] <- with(urban_designs, svyglm(as.formula(paste(y, "~", formula_RHS, "+ ECS_SELECTED_CH_GENDER")), family = "quasibinomial")), silent = TRUE)
        try(boys_rural_fitlist[[i]] <- with(boys_rural_designs, svyglm(as.formula(paste(y, "~", formula_RHS)), family = "quasibinomial")), silent = TRUE)
        try(boys_urban_fitlist[[i]] <- with(boys_urban_designs, svyglm(as.formula(paste(y, "~", formula_RHS)), family = "quasibinomial")), silent = TRUE)
        try(girls_rural_fitlist[[i]] <- with(girls_rural_designs, svyglm(as.formula(paste(y, "~", formula_RHS)), family = "quasibinomial")), silent = TRUE)
        try(girls_urban_fitlist[[i]] <- with(girls_urban_designs, svyglm(as.formula(paste(y, "~", formula_RHS)), family = "quasibinomial")), silent = TRUE)
    }

    return(list(fitlist, boys_fitlist, girls_fitlist, rural_fitlist, urban_fitlist, boys_rural_fitlist, boys_urban_fitlist, girls_rural_fitlist, girls_urban_fitlist))
}

pooled_weighted_logistic_models <- function(data, y, formula_RHS){
    # Removing Tanzania rows
    data <- filter(data, COUNTRY != 6)
    for(i in seq_along(data[[1]])){
        data[[1]][[i]]$COUNTRY <- droplevels(data[[1]][[i]]$COUNTRY)
    }

    # Creating sub-datasets
    boys_data <- filter(data, ECS_SELECTED_CH_GENDER == 0)
    girls_data <- filter(data, ECS_SELECTED_CH_GENDER == 1)
    rural_data <- filter(data, Urbanity == "Rural")
    urban_data <- filter(data, Urbanity == "Urban")
    boys_rural_data <- filter(boys_data, Urbanity == "Rural")
    boys_urban_data <- filter(boys_data, Urbanity == "Urban")
    girls_rural_data <- filter(girls_data, Urbanity == "Rural")
    girls_urban_data <- filter(girls_data, Urbanity == "Urban")

    # Creating survey design objects
    designs <- svydesign(ids = ~PSU, weights = ~wgt_scaled, data = data)
    boys_designs <- svydesign(ids = ~PSU, weights = ~wgt_scaled, data = boys_data)
    girls_designs <- svydesign(ids = ~PSU, weights = ~wgt_scaled, data = girls_data)
    rural_designs <- svydesign(ids = ~PSU, weights = ~wgt_scaled, data = rural_data)
    urban_designs <- svydesign(ids = ~PSU, weights = ~wgt_scaled, data = urban_data)
    boys_rural_designs <- svydesign(ids = ~PSU, weights = ~wgt_scaled, data = boys_rural_data)
    boys_urban_designs <- svydesign(ids = ~PSU, weights = ~wgt_scaled, data = boys_urban_data)
    girls_rural_designs <- svydesign(ids = ~PSU, weights = ~wgt_scaled, data = girls_rural_data)
    girls_urban_designs <- svydesign(ids = ~PSU, weights = ~wgt_scaled, data = girls_urban_data)

    # Fitting weighted logistic regression models
    model_fit <- with(designs, svyglm(as.formula(paste(y, "~", formula_RHS, "+ ECS_SELECTED_CH_GENDER + Urbanity")), family = "quasibinomial"))
    boys_model_fit <- with(boys_designs, svyglm(as.formula(paste(y, "~", formula_RHS, "+ Urbanity")), family = "quasibinomial"))
    girls_model_fit <- with(girls_designs, svyglm(as.formula(paste(y, "~", formula_RHS, "+ Urbanity")), family = "quasibinomial"))
    rural_model_fit <- with(rural_designs, svyglm(as.formula(paste(y, "~", formula_RHS, "+ ECS_SELECTED_CH_GENDER")), family = "quasibinomial"))
    urban_model_fit <- with(urban_designs, svyglm(as.formula(paste(y, "~", formula_RHS, "+ ECS_SELECTED_CH_GENDER")), family = "quasibinomial"))
    boys_rural_model_fit <- with(boys_rural_designs, svyglm(as.formula(paste(y, "~", formula_RHS)), family = "quasibinomial"))
    boys_urban_model_fit <- with(boys_urban_designs, svyglm(as.formula(paste(y, "~", formula_RHS)), family = "quasibinomial"))
    girls_rural_model_fit <- with(girls_rural_designs, svyglm(as.formula(paste(y, "~", formula_RHS)), family = "quasibinomial"))
    girls_urban_model_fit <- with(girls_urban_designs, svyglm(as.formula(paste(y, "~", formula_RHS)), family = "quasibinomial"))

    return(list(model_fit, boys_model_fit, girls_model_fit, rural_model_fit, urban_model_fit, boys_rural_model_fit, boys_urban_model_fit, girls_rural_model_fit, girls_urban_model_fit))
}

weighted_robust_linear_models <- function(data, y, formula_RHS){
    # Initialising lists of model fits
    fitlist <- list()
    boys_fitlist <- list()
    girls_fitlist <- list()
    rural_fitlist <- list()
    urban_fitlist <- list()
    boys_rural_fitlist <- list()
    boys_urban_fitlist <- list()
    girls_rural_fitlist <- list()
    girls_urban_fitlist <- list()

    # For each country
    for(i in unique(data[[1]][[1]]$COUNTRY)){

        # Skipping Tanzania
        if(i == 6){
            next
        }

        # Selecting relevant dataset
        these_data <- filter(data, COUNTRY == i)

        # Creating sub-datasets
        boys_data <- filter(these_data, ECS_SELECTED_CH_GENDER == 0)
        girls_data <- filter(these_data, ECS_SELECTED_CH_GENDER == 1)
        rural_data <- filter(these_data, Urbanity == "Rural")
        urban_data <- filter(these_data, Urbanity == "Urban")
        boys_rural_data <- filter(boys_data, Urbanity == "Rural")
        boys_urban_data <- filter(boys_data, Urbanity == "Urban")
        girls_rural_data <- filter(girls_data, Urbanity == "Rural")
        girls_urban_data <- filter(girls_data, Urbanity == "Urban")

        # Creating survey design objects
        designs <- svydesign(ids = ~PSU, weights = ~wgt_scaled, data = these_data)
        boys_designs <- svydesign(ids = ~PSU, weights = ~wgt_scaled, data = boys_data)
        girls_designs <- svydesign(ids = ~PSU, weights = ~wgt_scaled, data = girls_data)
        rural_designs <- svydesign(ids = ~PSU, weights = ~wgt_scaled, data = rural_data)
        urban_designs <- svydesign(ids = ~PSU, weights = ~wgt_scaled, data = urban_data)
        boys_rural_designs <- svydesign(ids = ~PSU, weights = ~wgt_scaled, data = boys_rural_data)
        boys_urban_designs <- svydesign(ids = ~PSU, weights = ~wgt_scaled, data = boys_urban_data)
        girls_rural_designs <- svydesign(ids = ~PSU, weights = ~wgt_scaled, data = girls_rural_data)
        girls_urban_designs <- svydesign(ids = ~PSU, weights = ~wgt_scaled, data = girls_urban_data)

        # Fitting weighted logistic regression models
        fitlist[[i]] <- with(designs, svyreg_huberM(as.formula(paste(y, "~", formula_RHS, "+ ECS_SELECTED_CH_GENDER + Urbanity")), k = 1.345))
        try(boys_fitlist[[i]] <- with(boys_designs, svyreg_huberM(as.formula(paste(y, "~", formula_RHS, "+ Urbanity")), k = 1.345)), silent = TRUE)
        try(girls_fitlist[[i]] <- with(girls_designs, svyreg_huberM(as.formula(paste(y, "~", formula_RHS, "+ Urbanity")), k = 1.345)), silent = TRUE)
        try(rural_fitlist[[i]] <- with(rural_designs, svyreg_huberM(as.formula(paste(y, "~", formula_RHS, "+ ECS_SELECTED_CH_GENDER")), k = 1.345)), silent = TRUE)
        try(urban_fitlist[[i]] <- with(urban_designs, svyreg_huberM(as.formula(paste(y, "~", formula_RHS, "+ ECS_SELECTED_CH_GENDER")), k = 1.345)), silent = TRUE)
        try(boys_rural_fitlist[[i]] <- with(boys_rural_designs, svyreg_huberM(as.formula(paste(y, "~", formula_RHS)), k = 1.345)), silent = TRUE)
        try(boys_urban_fitlist[[i]] <- with(boys_urban_designs, svyreg_huberM(as.formula(paste(y, "~", formula_RHS)), k = 1.345)), silent = TRUE)
        try(girls_rural_fitlist[[i]] <- with(girls_rural_designs, svyreg_huberM(as.formula(paste(y, "~", formula_RHS)), k = 1.345)), silent = TRUE)
        try(girls_urban_fitlist[[i]] <- with(girls_urban_designs, svyreg_huberM(as.formula(paste(y, "~", formula_RHS)), k = 1.345)), silent = TRUE)
    }

    return(list(fitlist, boys_fitlist, girls_fitlist, rural_fitlist, urban_fitlist, boys_rural_fitlist, boys_urban_fitlist, girls_rural_fitlist, girls_urban_fitlist))
}

pooled_weighted_robust_linear_models <- function(data, y, formula_RHS){
    # Removing Tanzania rows
    data <- filter(data, COUNTRY != 6)
    for(i in seq_along(data[[1]])){
        data[[1]][[i]]$COUNTRY <- droplevels(data[[1]][[i]]$COUNTRY)
    }

    # Creating sub-datasets
    boys_data <- filter(data, ECS_SELECTED_CH_GENDER == 0)
    girls_data <- filter(data, ECS_SELECTED_CH_GENDER == 1)
    rural_data <- filter(data, Urbanity == "Rural")
    urban_data <- filter(data, Urbanity == "Urban")
    boys_rural_data <- filter(boys_data, Urbanity == "Rural")
    boys_urban_data <- filter(boys_data, Urbanity == "Urban")
    girls_rural_data <- filter(girls_data, Urbanity == "Rural")
    girls_urban_data <- filter(girls_data, Urbanity == "Urban")

    # Creating survey design objects
    designs <- svydesign(ids = ~PSU, weights = ~wgt_scaled, data = data)
    boys_designs <- svydesign(ids = ~PSU, weights = ~wgt_scaled, data = boys_data)
    girls_designs <- svydesign(ids = ~PSU, weights = ~wgt_scaled, data = girls_data)
    rural_designs <- svydesign(ids = ~PSU, weights = ~wgt_scaled, data = rural_data)
    urban_designs <- svydesign(ids = ~PSU, weights = ~wgt_scaled, data = urban_data)
    boys_rural_designs <- svydesign(ids = ~PSU, weights = ~wgt_scaled, data = boys_rural_data)
    boys_urban_designs <- svydesign(ids = ~PSU, weights = ~wgt_scaled, data = boys_urban_data)
    girls_rural_designs <- svydesign(ids = ~PSU, weights = ~wgt_scaled, data = girls_rural_data)
    girls_urban_designs <- svydesign(ids = ~PSU, weights = ~wgt_scaled, data = girls_urban_data)

    # Fitting weighted logistic regression models
    model_fit <- with(designs, svyreg_huberM(as.formula(paste(y, "~", formula_RHS, "+ ECS_SELECTED_CH_GENDER + Urbanity")), k = 1.345))
    boys_model_fit <- with(boys_designs, svyreg_huberM(as.formula(paste(y, "~", formula_RHS, "+ Urbanity")), k = 1.345))
    girls_model_fit <- with(girls_designs, svyreg_huberM(as.formula(paste(y, "~", formula_RHS, "+ Urbanity")), k = 1.345))
    rural_model_fit <- with(rural_designs, svyreg_huberM(as.formula(paste(y, "~", formula_RHS, "+ ECS_SELECTED_CH_GENDER")), k = 1.345))
    urban_model_fit <- with(urban_designs, svyreg_huberM(as.formula(paste(y, "~", formula_RHS, "+ ECS_SELECTED_CH_GENDER")), k = 1.345))
    boys_rural_model_fit <- with(boys_rural_designs, svyreg_huberM(as.formula(paste(y, "~", formula_RHS)), k = 1.345))
    boys_urban_model_fit <- with(boys_urban_designs, svyreg_huberM(as.formula(paste(y, "~", formula_RHS)), k = 1.345))
    girls_rural_model_fit <- with(girls_rural_designs, svyreg_huberM(as.formula(paste(y, "~", formula_RHS)), k = 1.345))
    girls_urban_model_fit <- with(girls_urban_designs, svyreg_huberM(as.formula(paste(y, "~", formula_RHS)), k = 1.345))

    return(list(model_fit, boys_model_fit, girls_model_fit, rural_model_fit, urban_model_fit, boys_rural_model_fit, boys_urban_model_fit, girls_rural_model_fit, girls_urban_model_fit))
}

summarise_results <- function(results, terms){
    summary_list <- list()
    country_list <- c("Ethiopia", "Kenya", "Mozambique", "Namibia", "Uganda", "Cambodia", "Indonesia", "Malaysia", "Philippines", "Thailand", "Vietnam")

    for(i in seq_along(results)){
        results_list <- list()

        for(j in seq_along(results[[i]])){
            try(these_results_abridged <- summary(MIcombine(results[[i]][[j]]))[terms,], silent = TRUE)
            if(!exists("these_results_abridged")){
                next
            }
            these_results_abridged <- cbind(term = rownames(these_results_abridged), these_results_abridged)
            rownames(these_results_abridged) <- NULL
            these_results_abridged %<>% 
                rename(estimate = "results", std.error = se) %>%
                dplyr::select(-`(lower`, -`upper)`, -missInfo)

            results_list[[j]] <- as.data.frame(cbind(country = country_list[j], these_results_abridged))
            rm(these_results_abridged)

            results_list[[j]] %<>% mutate(
                statistic = estimate / std.error
            )
            results_list[[j]]$df <- MIcombine(results[[i]][[j]])$df[terms]
            results_list[[j]] %<>% mutate(
                p.value = 2 * pt(abs(statistic), df, lower.tail = FALSE)
            )
        }

        try(summary_list[[i]] <- reduce(results_list, rbind), silent = TRUE)
    }

    return(summary_list)
}

summarise_pooled_results <- function(results, terms){
    results_list <- list()
    
    for(i in seq_along(results)){
        results_abridged <- summary(MIcombine(results[[i]]))[terms,]
        results_abridged <- cbind(term = rownames(results_abridged), results_abridged)
        rownames(results_abridged) <- NULL
        results_abridged %<>% 
            rename(estimate = "results", std.error = se) %>%
            dplyr::select(-`(lower`, -`upper)`, -missInfo)

        results_abridged %<>% mutate(
            statistic = estimate / std.error
        )
        results_abridged$df <- MIcombine(results[[i]])$df[terms]
        results_abridged %<>% mutate(
            p.value = 2 * pt(abs(statistic), df, lower.tail = FALSE)
        )

        results_list[[i]] <- results_abridged
    }

    return(results_list)
}