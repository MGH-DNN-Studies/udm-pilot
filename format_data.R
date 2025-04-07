format_data <- function(path_data, outcome){
  
  # load data
  data <- read.csv(paste0(path_data, '6.synthetic_data_synthpop.csv'))
  
  # drop unwanted columns
  data <- data %>%
    select(-comorbidities)
  
  # dummy variable encoding
  data <- fastDummies::dummy_cols(data, select_columns = c("tx_protocol_machine", "tx_protocol_raw", "tx_protocol", "stim_site", "bobine", "dx_principal"),
                                  remove_selected_columns=TRUE, ignore_na=TRUE)
  
  # temporary handling of episode_passe_bl: consider bins later
  data$episode_passe_bl <- as.numeric(gsub('>=', '', data$episode_passe_bl))
  
  # clean up names
  data <- data %>%
    clean_names()
  
  # remove observations with missing outcomes
  data_ss <- data[-which(is.na(data[[outcome]])), ]
  
  # set outcome 
  y <- data_ss[[outcome]]
  
  # drop follow-up variables
  data_ss <- data_ss[, !names(data) %in% c(grep('_fu', names(data), value = T), 'id')]
  
  # reset outcome
  data_clf <- data.frame(
    outcome=y,
    data_ss
  )
  
  return(data_clf)
  
}