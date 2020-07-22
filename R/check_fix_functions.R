#' parameter_check
#'
#' \code{parameter_check} checks for three flags of temperature parameters
#'
#' @param data dataframe output of procFulcrum function
#' @param save set to TRUE if flagged rows are to be saved
#' @param return set to TRUE if flagged rows are to be returned in list format
#' @return a list of flagged rows in dataframe for each of three flags of interest
#' @import tidyr
#' @export
#'

parameter_check <- function(data, save = FALSE, return = FALSE) {
  nematode_field_sampling_proc <- data[[1]]

  message(">>> Checking substrate temperature")
  substrate_temperature <- nematode_field_sampling_proc %>%
    dplyr::filter(flag_substrate_temperature == TRUE)
  print(paste("There are", nrow(substrate_temperature),
              "rows with flagged substrate temperature:", sep = " "))
  if(nrow(substrate_temperature) > 0){to_return <- substrate_temperature %>%
    dplyr::select(fulcrum_id ,raw_substrate_temperature, proc_substrate_temperature)
    return(as.data.frame(to_return))}

  message(">>> Checking ambient temperature")
  ambient_temperature <- nematode_field_sampling_proc %>%
    dplyr::filter(flag_ambient_temperature == TRUE)
  print(paste("There are", nrow(ambient_temperature),
              "rows with flagged ambient temperature:", sep = " "))
  if(nrow(ambient_temperature) > 0){to_return <- ambient_temperature %>%
    dplyr::select(fulcrum_id ,raw_ambient_temperature, proc_ambient_temperature)
    return(as.data.frame(to_return))}

  message(">>> Checking ambient run temperature")
  #arrange by collection_datetime
  ambient_temperature_run <- nematode_field_sampling_proc %>%
    dplyr::arrange(collection_datetime_UTC)
  #replace NA values in the flag with FALSE, doesn't matter for this purpose
  ambient_temperature_run$flag_ambient_temperature_run <-
    tidyr::replace_na(ambient_temperature_run$flag_ambient_temperature_run, FALSE)
  #add another logical for adjacent rows, will later delete
  ambient_temperature_run$temp <- rep(FALSE, nrow(ambient_temperature_run))
  for(i in 1:10){
    if(ambient_temperature_run$flag_ambient_temperature_run[i]==TRUE){
      ambient_temperature_run$temp[1:i] <- TRUE
      ambient_temperature_run$temp[i+1:10] <- TRUE
    }
  }
  for(i in 11:(nrow(ambient_temperature_run)-11)){
    if(ambient_temperature_run$flag_ambient_temperature_run[i]==TRUE){
      ambient_temperature_run$temp[i] <- TRUE
      ambient_temperature_run$temp[i+1:10] <- TRUE
      ambient_temperature_run$temp[i-1:10] <- TRUE
    }
  }
  for(i in (nrow(ambient_temperature_run)-11):nrow(ambient_temperature_run)){
    if(ambient_temperature_run$flag_ambient_temperature_run[i]==TRUE){
      ambient_temperature_run$temp[i:nrow(ambient_temperature_run)] <- TRUE
      ambient_temperature_run$temp[i-1:10] <- TRUE
    }
  }
  ambient_temperature_run <- ambient_temperature_run %>%
    dplyr::filter(temp == TRUE) %>% dplyr::select(-matches("drop"))
  print(paste("There are",
              sum(ambient_temperature_run$flag_ambient_temperature_run),
              "rows with flagged ambient run temperature:", sep = " "))
  if(sum(ambient_temperature_run$flag_ambient_temperature_run) > 0){
    to_return <- ambient_temperature_run %>%
    dplyr::select(raw_ambient_temperature, ambient_humidity,
                  flag_ambient_temperature_run, collection_local_time, collection_datetime_UTC)
    return(as.data.frame(to_return))}

  if(save){
    dir.create("parameter_check", showWarnings = FALSE)
    write.csv(substrate_temperature, "parameter_check/substrate_temperature.csv")
    write.csv(ambient_temperature, "parameter_check/ambient_temperature.csv")
    write.csv(ambient_temperature_run, "parameter_check/ambient_temperature_run.csv")
    message(">>> Examine parameter_check folder for full records.")
  }
  else{
    message(">>> Flagged values unsaved (default is FALSE).")
  }
  if(return){
    return(list("substrate_temperature" = substrate_temperature, "ambient_temperature" = ambient_temperature,
                "ambient_temperature_run" = ambient_temperature_run))
  }
}

#' parameter_fix
#'
#' \code{parameter_fix} fixes three flags of temperature parameters
#'
#' @param data dataframe output of procFulcrum function
#' @param substrate_temperature_ids vector of ids of substrate_temperature to edit
#' @param ambient_temperature_ids vector of ids of ambient_temperature to edit
#' @param ambient_temperature_run_ids vector of ids of ambient_temperature_run to edit
#' @return an edited procFulcrum output (list of dataframes)
#' @export
#'

parameter_fix <- function(data,
                            substrate_temperature_ids = NULL,
                            ambient_temperature_ids = NULL,
                            ambient_temperature_run_ids = NULL) {
  for(i in 1:nrow(data[[1]])){
    #replace proc_substrate_temperature with raw_substrate_temperature
    if(data[[1]]$fulcrum_id[i] %in% substrate_temperature_ids){
      data[[1]]$proc_substrate_temperature[i] = data[[1]]$raw_substrate_temperature[i]}
    #replace proc_ambient_temperature with raw_ambient_temperature
    if(data[[1]]$fulcrum_id[i] %in% ambient_temperature_ids){
      data[[1]]$proc_ambient_temperature[i] = data[[1]]$raw_ambient_temperature[i]}

    ## fix below--what columns do you want changed???

    #replace proc_ambient_temperature with NA
    if(data[[1]]$fulcrum_id[i] %in% ambient_temperature_run_ids){
      data[[1]]$proc_ambient_temperature[i] = NA}
  }
  return(data)
}

#' initial_data_check
#'
#' \code{initial_data_check} checks for four flags of duplicated and missing parameters in joined data
#'
#' @param data dataframe output of joinFulcrum function
#' @param save set to TRUE if flagged rows are to be saved
#' @param return set to TRUE if flagged rows are to be returned in list format
#' @return a list of flagged rows in dataframe for each of four flags of interest
#' @export
#'

initial_data_check <- function(data, save = FALSE, return = FALSE) {
  message(">>> Checking duplicated c labels")
  duplicated_c_label <- data %>% dplyr::filter(flag_duplicated_c_label_field_sampling == TRUE)
  print(paste("There are", nrow(duplicated_c_label), "rows with duplicated c labels:", sep = " "))
  if(nrow(duplicated_c_label) > 0){print(duplicated_c_label$c_label)}

  message(">>> Checking duplicated s labels")
  duplicated_s_label <- data %>% dplyr::filter(flag_duplicated_s_label_isolation_s_labeled_plates == TRUE)
  print(paste("There are", nrow(duplicated_s_label), "rows with duplicated s labels", sep = " "))
  if(nrow(duplicated_s_label) > 0){print(duplicated_s_label$s_label)}

  message(">>> Checking missing s labels")
  missing_s_label <- data %>% dplyr::filter(flag_missing_s_label_isolation_s_labeled_plates == TRUE)
  print(paste("There are", nrow(missing_s_label), "rows with missing s labels", sep = " "))
  if(nrow(missing_s_label) > 0){print(missing_s_label$s_label)}

  message(">>> Checking missing isolation records")
  missing_isolation_record <- data %>% dplyr::filter(flag_missing_isolation_record == TRUE)
  print(paste("There are", nrow(missing_isolation_record), "rows with missing isolation records", sep = " "))
  if(nrow(missing_isolation_record) > 0){print(missing_isolation_record$isolation_by)}

  if(save){
    dir.create("initial_data_check", showWarnings = FALSE)
    write.csv(duplicated_c_label, "initial_data_check/duplicated_c_label.csv")
    write.csv(duplicated_s_label, "initial_data_check/duplicated_s_label.csv")
    write.csv(missing_s_label, "initial_data_check/missing_s_label.csv")
    write.csv(missing_isolation_record, "initial_data_check/missing_isolation_record.csv")
    message(">>> Examine initial_data_check folder for full records, consider fixing flagged issues in primary data input and rerunning.")
  }
  else{
    message(">>> Flagged values unsaved, (default is FALSE). Consider fixing flagged issues in primary data input and rerunning.")
  }
  if(return){
    return(list("duplicated_c_label" = duplicated_c_label, "duplicated_s_label" = duplicated_s_label,
                "missing_s_label" = missing_s_label, "missing_isolation_record" = missing_isolation_record))
  }
}

