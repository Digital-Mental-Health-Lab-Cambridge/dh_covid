clean_data <- function(data){
    # Initialise new participant ID, as the original ID is not unique
    data$id_new <- 1:nrow(data)

    # Select relevant variables
    data_clean <- data %>% select(
        id_new, # New participant ID
        
    )
}