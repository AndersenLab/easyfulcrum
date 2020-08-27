#' checkParameters
#'
#' \code{checkParameters} checks for flags (3) regarding raw temperature parameters,
#'
#' Will return the flagged rows (and neighboring rows if appropriate), and necessary columns for flag visualization on \code{procFulcrum} output
#' The returned \emph{fulcrum_id} can be used in \code{fixParameters} to make changes to these raw temperature parameters
#' See \code{fixParameters} documentation for details on how to make these changes
#' flagged substrate temperature: occurs when substrate temperature > 40, \code{procFulcrum} will automatically convert to celsius (assumes temperature incorrectly in fahrenheit)
#' flagged ambient temperature: occurs when ambient temperature > 40, \code{procFulcrum} will automatically convert to celsius (assumes temperature incorrectly in fahrenheit)
#' flagged ambient run temperature: occurs when ambient humidity and temperature are repeated in subsequent measurements, this is meant to flag values if probe is stuck, \code{procFulcrum} does not modify anything for this flag
#'
#' @param data list format output of \code{procFulcrum}
#' @param return a logical, default FALSE, set to TRUE if flagged rows in \code{procFulcrum} dataframes are to be returned and saved in list format
#' @return rows with flagged substrate temperature, flagged ambient temperature, flagged ambient run temperature (and surrounding rows that cause flag to be triggered)
#'  these returns will also contain relevant columns for understanding flagged values and how they arose
#'  these dataframes can be saved as a list of three dataframes when return is set to TRUE
#' @import tidyr
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
#' \code{fixParameters} fixes flags (3) regarding raw temperature parameters
#'
#' To be run after checkParameters, user is advised to select the returned \emph{fulcrum_id} for changing (upon inspection) from \code{checkParameters()} to pass into this function
#' flagged substrate temperature: occurs when substrate temperature > 40, \code{procFulcrum} will automatically convert to celsius (assumes temperature incorrectly in fahrenheit)
#' flagged ambient temperature: occurs when ambient temperature > 40, \code{procFulcrum} will automatically convert to celsius (assumes temperature incorrectly in fahrenheit)
#' flagged ambient run temperature: occurs when ambient humidity and temperature are repeated in subsequent measurements, this is meant to flag values if probe is stuck, \code{procFulcrum} does not modify anything for this flag
#'
#' We advise re-running \code{checkParameters()} on the saved output of \code{fixParameters()} to ensure that wanted edits have appropriately occured
#'
#' @param data list format output of \code{procFulcrum}
#' @param substrate_temperature_ids vector of \emph{fulcrum_id} of rows of \emph{substrate_temperature} to revert to original/raw temperature
#' @param ambient_temperature_ids vector of \emph{fulcrum_id} of rows of \emph{ambient_temperature} to revert to original/raw temperature
#' @param ambient_temperature_run_ids vector of \emph{fulcrum_id} of rows of \emph{ambient_temperature_run} to set \emph{proc_ambient_temperature}, \emph{ambient_humidity} to NA
#' @return an edited (per passed \emph{fulcrum_id} values) list format output of procFulcrum to be used in easyfulcrum workflow (passed into \code{joinFulcrum()})
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
    #replace proc_ambient_temperature, ambient_humidity with NA
    if(data[[1]]$fulcrum_id[i] %in% ambient_temperature_run_ids){
      data[[1]]$proc_ambient_temperature[i] = NA
      data[[1]]$ambient_humidity[i] = NA}
  }
  return(data)
}

#' checkJoin
#'
#' \code{checkJoin} checks for flags (6) in \code{joinFulcrum} output
#'
#' This check function will return c_labels and s_labels for various flags relating to missing or duplicated data
#' Messages regarding what checks are being done, and where they lie in the raw dataframes is also provided
#' We advise returning to the raw dataframes using the information returned in these checks to better understand issues that may have occurred during data collection, before re-running
#'
#' @param data dataframe output of \code{joinFulcrum}
#' @param return set to TRUE if flagged rows are to be returned as a dataframe
#' @return c_labels and s_labels (as appropriate) for rows with each of the six flags, and the origin of the location of the raw data that triggered the flag
#'  the rows corresponding to these labels can be saved as a list of six dataframes when return is set to TRUE
#' @export
#'

checkJoin <- function(data, return = FALSE) {
  message(">>> Checking duplicated c labels")
  duplicated_c_label <- data %>% dplyr::filter(flag_duplicated_c_label_field_sampling == TRUE)
  print(paste("There are", nrow(duplicated_c_label), "rows with duplicated c labels, these c labels are:", sep = " "))
  if(nrow(duplicated_c_label) > 0){
    print(duplicated_c_label$c_label)
    print("Duplicated c labels are found in nematode_field_sampling.csv")}

  message(">>> Checking unusual sample photo number")
  unusual_sample_photo_num <- data %>% dplyr::filter(flag_unusual_sample_photo_num == TRUE)
  print(paste("There are", nrow(unusual_sample_photo_num), "rows with unusual sample photo numbers, their c labels are:", sep = " "))
  if(nrow(unusual_sample_photo_num) > 0){
    print(unusual_sample_photo_num$c_label)
    print("Unusual sample photo number are found in nematode_field_sampling.csv")}

  message(">>> Checking duplicated isolation for c label")
  duplicated_isolation_for_c_label <- data %>% dplyr::filter(flag_duplicated_isolation_for_c_label == TRUE)
  print(paste("There are", nrow(duplicated_isolation_for_c_label), "rows with duplicated isolation for c label, their c labels are:", sep = " "))
  if(nrow(duplicated_isolation_for_c_label) > 0){
    print(duplicated_isolation_for_c_label$c_label)
    print("Duplicated isolation for c label are found in nematode_isolation.csv")}

  message(">>> Checking missing isolation records")
  missing_isolation_record <- data %>% dplyr::filter(flag_missing_isolation_record == TRUE)
  print(paste("There are", nrow(missing_isolation_record), "rows with missing isolation records, their c labels are:", sep = " "))
  if(nrow(missing_isolation_record) > 0){
    print(missing_isolation_record$c_label)
    print("Missing isolation records are found in nematode_isolation.csv")}

  message(">>> Checking missing s labels")
  missing_s_label <- data %>% dplyr::filter(flag_missing_s_label_isolation_s_labeled_plates == TRUE)
  print(paste("There are", nrow(missing_s_label), "rows with missing s labels, their c labels are:", sep = " "))
  if(nrow(missing_s_label) > 0){
    print(missing_s_label$c_label)
    print("Missing s labels are found in nematode_isolation_s_labeled_plates.csv")}

  message(">>> Checking duplicated s labels")
  duplicated_s_label <- data %>% dplyr::filter(flag_duplicated_s_label_isolation_s_labeled_plates == TRUE)
  print(paste("There are", nrow(duplicated_s_label), "rows with duplicated s labels, their s labels are:", sep = " "))
  if(nrow(duplicated_s_label) > 0){
    print(duplicated_s_label$s_label)
    print("Duplicated s labels are found in nematode_isolation_s_labeled_plates.csv")}

  if(return){
    return(list("duplicated_c_label" = duplicated_c_label,
                "unusual_sample_photo_num" = unusual_sample_photo_num,
                "duplicated_isolation_for_c_label" = duplicated_isolation_for_c_label,
                "missing_isolation_record" = missing_isolation_record,
                "missing_s_label" = missing_s_label,
                "duplicated_s_label" = duplicated_s_label))
  }
}
