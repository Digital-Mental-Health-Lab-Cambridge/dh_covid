clean_data <- function(data){
    # Initialise new participant ID, as the original ID is not unique
    data$id_new <- seq_len(nrow(data))

    # Select relevant variables
    data_clean <- data %>% select(
        id_new, # New participant ID
        COUNTRY,
        Language,
        intStartTime, # Interview start time
        ECS_SELECTED_CH, # Selected child
        ECS_SELECTED_CH_GENDER, # Selected child's gender
        ECS_SELECTED_CH_AGE, # Selected child's age
        A1, # Classification of local area/neighbourhood
        A4, # Highest level of education completed
        B1, # Frequency of internet usage
        contains("B2") & !contains("B2x"), # Frequency of internet usage in different locations
        contains("B3"), # Reasons for inability to use internet
        CO2, # Occurrence of lockdown
        CO3, # Ability to stay in touch with others during lockdown
        H1, # Life satisfaction ladder
        contains("H4") & !contains("CH4"), # CW-SWBS and anxiety scale
        contains("H5") & !contains("H5_") & !contains("CH5"), # Abridged CES-D
        H6, # Self-harm
        contains("H7") & !contains("CH7"), # Paykel suicide scale
        L1, # Number of people in household
        L4, # Sole caregiver status
        contains("L52"), # Contextual household income
        L8, # Caregiver marital status
        L9, # Caregiver's relationship to child
        L11, # Caregiver's highest level of education
        L12, # Caregiver's employment
        L14, # Type of school attended
        contains("L16"), # Child's difficulties
        L18, # Frequency of caregiver's internet use
        contains("L23"), # Caregiver's internet literacy
        contains("L36"), # Caregiver's societal attitudes
        L50, # Caregiver life satisfaction ladder
        contains("L39"), # Caregiver CW-SWBS
        contains("L51"), # Caregiver anxiety scale
        contains("L40"), # Caregiver abridged CES-D
        L41, # Caregiver self-harm
        contains("L42"), # Caregiver Paykel suicide scale
        L43, # Caregiver's alcohol intake
        L45_NEW # Caregiver's corporal punishment attitude
    )

    # Set 888 and 999 values to NA
    is.na(data_clean[, !(names(data_clean) %in% c("id_new", "intStartTime"))]) <- data_clean[, !(names(data_clean) %in% c("id_new", "intStartTime"))] > 887

    # Recode country names
    data_clean %<>% mutate(COUNTRY = recode(COUNTRY,
        `1` = "Ethiopia", 
        `2` = "Kenya", 
        `3` = "Mozambique", 
        `4` = "Namibia",
        `6` = "Tanzania", 
        `7` = "Uganda", 
        `8` = "Cambodia", 
        `9` = "Indonesia",
        `10` = "Malaysia", 
        `11` = "Philippines",
        `12` = "Thailand", 
        `13` = "Vietnam"
    ))

    # Convert variables to factors as necessary
    for(var in c("COUNTRY", "Language", "ECS_SELECTED_CH", "ECS_SELECTED_CH_GENDER", "A1", "A4", "B3_1", "B3_2", "B3_3", "B3_4", "B3_6", "B3_7", "B3_8", "B3_9", "B3_10", "B3_other", "B3_999", "B3_888", "CO2", "CO3", "H6", "L4", "L8", "L9", "L11", "L12", "L14", "L36a", "L36b", "L36c", "L36f", "L41")){
        data_clean[, var] <- as.factor(data_clean[, var])
    }

    # Delete all responses for H4f and L39f in Mozambique (because of a translation error)
    data_clean[data_clean$COUNTRY == "Mozambique", c("H4f", "L39f")] <- NA

    # Setting all B3_... variables to NA if B3_888 or B3_999 is 1
    data_clean %<>% mutate(
        B3_1 = if_else(B3_888 == 1 | B3_999 == 1, NA, B3_1),
        B3_2 = if_else(B3_888 == 1 | B3_999 == 1, NA, B3_2),
        B3_3 = if_else(B3_888 == 1 | B3_999 == 1, NA, B3_3),
        B3_4 = if_else(B3_888 == 1 | B3_999 == 1, NA, B3_4),
        B3_6 = if_else(B3_888 == 1 | B3_999 == 1, NA, B3_6),
        B3_7 = if_else(B3_888 == 1 | B3_999 == 1, NA, B3_7),
        B3_8 = if_else(B3_888 == 1 | B3_999 == 1, NA, B3_8),
        B3_9 = if_else(B3_888 == 1 | B3_999 == 1, NA, B3_9),
        B3_10 = if_else(B3_888 == 1 | B3_999 == 1, NA, B3_10),
        B3_other = if_else(B3_888 == 1 | B3_999 == 1, NA, B3_other)
    )

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

multinomial_data_prep <- function(data){
    data %<>%
        mutate(
            CONNECTION = case_when(
                CO2 == 0 ~ "No lockdown",
                CO2 == 1 & CO3 == 0 ~ "Lockdown, no connection",
                CO2 == 1 & CO3 == 1 ~ "Lockdown, connection maintained"
            )
        ) %>%
        select(-CO2, -CO3)

    data$CONNECTION %<>% as.factor() %>% relevel("No lockdown")
    
    return(data)
}

multiple_imputation <- function(data){
    varnames <- names(data)
    varnames <- varnames[! varnames %in% c("id_new", "COUNTRY", "Language", "intStartTime", "ECS_SELECTED_CH", "ECS_SELECTED_CH_GENDER", "ECS_SELECTED_CH_AGE")]
    

    data %<>% group_by(COUNTRY)
    for(i in varnames){
        data %<>% mutate(
            "{i}_GRP" := mean(get(!!i), na.rm = TRUE)
        )
    }
    data %<>% ungroup()

    methods <- make.method(data)
    methods[] <- "2l.pmm"
    methods[c("id_new", "COUNTRY", "Language", "intStartTime", "ECS_SELECTED_CH", "ECS_SELECTED_CH_GENDER", "ECS_SELECTED_CH_AGE")] <- ""

    predictorMatrix <- make.predictorMatrix(data)
    predictorMatrix[, "id_new"] <- 0
    predictorMatrix[, "COUNTRY"] <- -2
    predictorMatrix[predictorMatrix == 1] <- 3

    cl <- makeCluster(detectCores() - 2)

    clusterSetRNGStream(cl, 2903)

    clusterExport(cl, c("data", "methods", "predictorMatrix"), envir = environment())

    clusterEvalQ(cl, library(mice))

    imp_pars <- parLapply(cl = cl, X = 1:(detectCores() - 2), fun = function(no){ 
        mice(data, vis = "monotone", method = methods, predictorMatrix = predictorMatrix, m = 1, maxit = 20)
    })

    data_imputed <- imp_pars[[1]]
    for(i in 2:length(imp_pars)){
        data_imputed <- ibind(data_imputed, imp_pars[[i]])
    }

    return(data_imputed)
}

linear_mixed_model <- function(data, dep_var, ind_vars){
    model_fit <- with(data, lmer(as.formula(paste0(dep_var, " ~ (", ind_vars, " | COUNTRY)"))))
    results <- summary(pool(model_fit))

    return(results)
}

logistic_mixed_model <- function(data, dep_var, ind_vars){
    model_fit <- with(data, glmer(as.formula(paste0(dep_var, "~ (", ind_vars, " | COUNTRY)")), family = "binomial"))
    results <- summary(pool(model_fit))

    return(results)
}

multinomial_mixed_model <- function(data, dep_var, ind_vars){
    model_fit <- with(data, mclogit(as.formula(paste0(dep_var, " ~ (", ind_vars, " | COUNTRY)"))))
    results <- summary(pool(model_fit))

    return(results)
}