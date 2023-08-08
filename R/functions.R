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
        ECS_SELECTED_CH_INTUSE, # Selected child's internet usage
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
        contains("L39"), # Caregiver CW-SWBS
        contains("L51"), # Caregiver anxiety scale
        contains("L40"), # Caregiver abridged CES-D
        L41, # Caregiver self-harm
        contains("L42"), # Caregiver Paykel suicide scale
        L43, # Caregiver's alcohol intake
        L45_NEW, # Caregiver's corporal punishment attitude
        contains("L46") # Violent behaviours
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

    return(data_clean)
}

country_missingness <- function(data){
    NA_by_country <- lapply(
        seq_len(length(names(data))),
        function(x){
            data.frame(country = unique(data$COUNTRY), missingness = rep(NA, length(unique(data$COUNTRY))))
        }
    )

    for(i in seq_len(length(names(data)))){
        for(j in seq_len(length(unique(data$COUNTRY)))){
            NA_by_country[[i]]$missingness[j] <- 
            sum(is.na(data[data$COUNTRY == unique(data$COUNTRY)[j], i])) /
            length(data[data$COUNTRY == unique(data$COUNTRY)[j], i])
        }
    }

    names(NA_by_country) <- names(data)

    return(NA_by_country)
}

language_missingness <- function(data){
    NA_by_language <- lapply(
        seq_len(length(names(data))),
        function(x){
            data.frame(language = unique(data$Language), missingness = rep(NA, length(unique(data$Language))))
        }
    )

    for(i in seq_len(length(names(data)))){
        for(j in seq_len(length(unique(data$Language)))){
            NA_by_language[[i]]$missingness[j] <- 
            sum(is.na(data[data$Language == unique(data$Language)[j], i])) /
            length(data[data$Language == unique(data$Language)[j], i])
        }
    }

    names(NA_by_language) <- names(data)

    return(NA_by_language)
}