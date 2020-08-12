#' checkParameters
#'
#' \code{checkParameters} checks for three flags of raw temperature parameters
#'
#' @param data dataframe output of procFulcrum function
#' @param return set to TRUE if flagged rows are to be returned in list format
#' @return a list of flagged rows in dataframe for each of three flags of interest
#' @import tidyr
#' @export
#'

checkParameters <- function(data, return = FALSE) {

  message(">>> Checking substrate temperature")
  substrate_temperature <- data[[1]] %>% dplyr::filter(flag_substrate_temperature == TRUE)
  print(paste("There are", nrow(substrate_temperature), "rows with flagged substrate temperature:", sep = " "))

  if(nrow(substrate_temperature) > 0){to_return <- substrate_temperature %>%
    dplyr::select(fulcrum_id ,raw_substrate_temperature, proc_substrate_temperature)
  print.data.frame(as.data.frame(to_return))}

  message(">>> Checking ambient temperature")
  ambient_temperature <- data[[1]] %>% dplyr::filter(flag_ambient_temperature == TRUE)
  print(paste("There are", nrow(ambient_temperature), "rows with flagged ambient temperature:", sep = " "))

  if(nrow(ambient_temperature) > 0){to_return <- ambient_temperature %>%
    dplyr::select(fulcrum_id ,raw_ambient_temperature, proc_ambient_temperature)
  print.data.frame(as.data.frame(to_return))}

  message(">>> Checking ambient run temperature")
  #arrange by collection_datetime
  ambient_temperature_run <- data[[1]] %>%
    dplyr::arrange(collection_datetime_UTC)
  #replace NA values in the flag with FALSE, doesn't matter for this purpose
  ambient_temperature_run$flag_ambient_temperature_run <-
    tidyr::replace_na(ambient_temperature_run$flag_ambient_temperature_run, FALSE)
  #add another logical for adjacent rows, will later delete
  ambient_temperature_run$temp <- rep(FALSE, nrow(ambient_temperature_run))
  for(i in 1:5){
    if(ambient_temperature_run$flag_ambient_temperature_run[i]==TRUE){
      ambient_temperature_run$temp[1:i] <- TRUE
      ambient_temperature_run$temp[i+1:5] <- TRUE
    }
  }
  for(i in 6:(nrow(ambient_temperature_run)-6)){
    if(ambient_temperature_run$flag_ambient_temperature_run[i]==TRUE){
      ambient_temperature_run$temp[i] <- TRUE
      ambient_temperature_run$temp[i+1:5] <- TRUE
      ambient_temperature_run$temp[i-1:5] <- TRUE
    }
  }
  for(i in (nrow(ambient_temperature_run)-5):nrow(ambient_temperature_run)){
    if(ambient_temperature_run$flag_ambient_temperature_run[i]==TRUE){
      ambient_temperature_run$temp[i:nrow(ambient_temperature_run)] <- TRUE
      ambient_temperature_run$temp[i-1:5] <- TRUE
    }
  }
  ambient_temperature_run <- ambient_temperature_run %>%
    dplyr::filter(temp == TRUE) %>% dplyr::select(-temp)
  print(paste("There are", sum(ambient_temperature_run$flag_ambient_temperature_run),
              "rows with flagged ambient run temperature:", sep = " "))

  if(sum(ambient_temperature_run$flag_ambient_temperature_run) > 0){
    to_return <- ambient_temperature_run %>%
    dplyr::select(fulcrum_id,
                  c_label,
                  raw_ambient_temperature,
                  proc_ambient_temperature,
                  ambient_humidity,
                  flag_ambient_temperature_run,
                  collection_local_time,
                  collection_datetime_UTC)
    print.data.frame(as.data.frame(to_return))}

  if(return){
    return(list("substrate_temperature" = substrate_temperature,
                "ambient_temperature" = ambient_temperature,
                "ambient_temperature_run" = ambient_temperature_run))
  }
}

#' fixParameters
#'
#' \code{fixParameters} fixes three flags of temperature parameters
#'
#' @param data dataframe output of procFulcrum function
#' @param substrate_temperature_ids vector of ids of substrate_temperature to edit
#' @param ambient_temperature_ids vector of ids of ambient_temperature to edit
#' @param ambient_temperature_run_ids vector of ids of ambient_temperature_run to edit
#' @return an edited procFulcrum output (list of dataframes)
#' @export
#'

fixParameters <- function(data,
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
    #replace proc_ambient_temperature with NA
    if(data[[1]]$fulcrum_id[i] %in% ambient_temperature_run_ids){
      data[[1]]$proc_ambient_temperature[i] = NA
      data[[1]]$ambient_humidity[i] = NA}
  }
  return(data)
}

#' checkProc
#'
#' \code{checkProc} checks for six flags of duplicated and missing parameters in processed data
#'
#' @param data dataframe output of procFulcrum function
#' @param return set to TRUE if flagged rows are to be returned in list format
#' @return a list of dataframes of flagged rows for each of six flags of interest
#' @export
#'

checkProc <- function(data, return = FALSE) {
  message(">>> Checking duplicated c labels")
  duplicated_c_label <- data[[1]] %>% dplyr::filter(flag_duplicated_c_label_field_sampling == TRUE)
  print(paste("There are", nrow(duplicated_c_label), "rows with duplicated c labels, these c labels are:", sep = " "))
  if(nrow(duplicated_c_label) > 0){
    print(duplicated_c_label$c_label)
    print("Duplicated c labels are found in nematode_field_sampling.csv")}

  message(">>> Checking unusual sample photo number")
  unusual_sample_photo_num <- data[[1]] %>% dplyr::filter(flag_unusual_sample_photo_num == TRUE)
  print(paste("There are", nrow(unusual_sample_photo_num), "rows with unusual sample photo numbers, their c labels are:", sep = " "))
  if(nrow(unusual_sample_photo_num) > 0){
    print(unusual_sample_photo_num$c_label)
    print("Unusual sample photo number are found in nematode_field_sampling.csv")}

  message(">>> Checking duplicated isolation for c label")
  duplicated_isolation_for_c_label <- data[[3]] %>% dplyr::filter(flag_duplicated_isolation_for_c_label == TRUE)
  print(paste("There are", nrow(duplicated_isolation_for_c_label), "rows with duplicated isolation for c label, their c labels ids are:", sep = " "))
  if(nrow(duplicated_isolation_for_c_label) > 0){
    print(duplicated_isolation_for_c_label$c_label_id)
    print("Duplicated isolation for c label are found in nematode_isolation.csv")}

  message(">>> Checking duplicated s labels")
  duplicated_s_label <- data[[4]] %>% dplyr::filter(flag_duplicated_s_label_isolation_s_labeled_plates == TRUE)
  print(paste("There are", nrow(duplicated_s_label), "rows with duplicated s labels, their s labels are:", sep = " "))
  if(nrow(duplicated_s_label) > 0){
    print(duplicated_s_label$s_label)
    print("Duplicated s labels are found in nematode_isolation_s_labeled_plates.csv")}

  message(">>> Checking missing s labels")
  missing_s_label <- data[[4]] %>% dplyr::filter(flag_missing_s_label_isolation_s_labeled_plates == TRUE)
  print(paste("There are", nrow(missing_s_label), "rows with missing s labels, their fulcrum parent ids are:", sep = " "))
  if(nrow(missing_s_label) > 0){
    print(missing_s_label$fulcrum_parent_id)
    print("Missing s labels are found in nematode_isolation_s_labeled_plates.csv")}

  if(return){
    return(list("duplicated_c_label" = duplicated_c_label,
                "unusual_sample_photo_num" = unusual_sample_photo_num,
                "duplicated_isolation_for_c_label" = duplicated_isolation_for_c_label,
                "duplicated_s_label" = duplicated_s_label,
                "missing_s_label" = missing_s_label))
  }
}

#' checkJoin
#'
#' \code{checkJoin} checks for one flag in joined data
#'
#' @param data dataframe output of joinFulcrum function
#' @param return set to TRUE if flagged rows are to be returned as a dataframe
#' @return a dataframe flagged rows in a dataframe for flag of interest
#' @export
#'

checkJoin <- function(data, return = FALSE) {
  message(">>> Checking missing isolation records")
  missing_isolation_record <- data %>% dplyr::filter(flag_missing_isolation_record == TRUE)
  print(paste("There are", nrow(missing_isolation_record), "rows with missing isolation records, their c labels are:", sep = " "))
  if(nrow(missing_isolation_record) > 0){print(missing_isolation_record$c_label)}

  if(return){return(missing_isolation_record)}
}
