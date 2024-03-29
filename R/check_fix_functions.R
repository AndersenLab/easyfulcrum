#' checkTemperatures
#'
#' \code{checkTemperatures} checks for flags (3) regarding raw temperature parameters.
#'
#' Will return the flagged rows (and neighboring rows if appropriate), and necessary columns for flag visualization on \code{procFulcrum} output.
#' The returned \code{fulcrum_id} can be used in \code{fixTemperatures} to make changes to these raw temperature parameters.
#' See \code{fixTemperatures} documentation for information on how to make these changes.
#' flagged substrate temperature: occurs when substrate temperature > 40,
#' \code{procFulcrum} will automatically convert to celsius (assumes temperature incorrectly in fahrenheit).
#' flagged ambient temperature: occurs when ambient temperature > 40,
#' \code{procFulcrum} will automatically convert to celsius (assumes temperature incorrectly in fahrenheit).
#' flagged ambient run temperature: occurs when ambient humidity and temperature are repeated five times or more in subsequent measurements,
#' this is meant to flag values if probe is stuck, \code{procFulcrum} does not automatically modify anything for this flag.
#'
#' @param data list format output of \code{procFulcrum}.
#' @param return_flags a logical, default FALSE, set to TRUE if flagged rows in \code{procFulcrum} dataframes are to be returned and saved in list format.
#' @return rows with flagged substrate temperature, flagged ambient temperature,
#' flagged ambient run temperature (and surrounding rows that cause flag to be triggered)
#'  these returns will also contain relevant columns for understanding flagged values and how they arose
#'  these dataframes can be saved as a list of three dataframes when \code{return_flags} is set to TRUE
#' @import tidyr
#' @import dplyr
#' @export
#'

checkTemperatures <- function(data, return_flags = FALSE) {
  # check substrate temperature
  message(">>> Checking substrate temperature")
  substrate_temperature <- data$field_sampling_proc %>% dplyr::filter(flag_substrate_temperature == TRUE)
  print(paste("There are", nrow(substrate_temperature), "rows with flagged substrate temperature:", sep = " "))

  if(nrow(substrate_temperature) > 0){to_return <- substrate_temperature %>%
    dplyr::select(fulcrum_id ,raw_substrate_temperature, proc_substrate_temperature)
  print.data.frame(as.data.frame(to_return))}
  # check ambient temperature
  message(">>> Checking ambient temperature")
  ambient_temperature <- data$field_sampling_proc %>% dplyr::filter(flag_ambient_temperature == TRUE)
  print(paste("There are", nrow(ambient_temperature), "rows with flagged ambient temperature:", sep = " "))

  if(nrow(ambient_temperature) > 0){to_return <- ambient_temperature %>%
    dplyr::select(fulcrum_id ,raw_ambient_temperature, proc_ambient_temperature)
  print.data.frame(as.data.frame(to_return))}

  if(nrow(data$field_sampling_proc) > 10){
    # check ambient run temperature
    message(">>> Checking ambient run temperature")
    #arrange by collection_datetime
    ambient_temperature_run <- data$field_sampling_proc %>%
      dplyr::arrange(collection_datetime_UTC)
    #replace NA values in the flag with FALSE, doesn't matter for this purpose
    ambient_temperature_run$flag_ambient_temperature_run <-
      tidyr::replace_na(ambient_temperature_run$flag_ambient_temperature_run, FALSE)
    #add another logical for adjacent rows, will later delete
    ambient_temperature_run$temp <- rep(FALSE, nrow(ambient_temperature_run))
    # take care of first five cases
    for(i in 1:5){
      if(ambient_temperature_run$flag_ambient_temperature_run[i]==TRUE){
        ambient_temperature_run$temp[1:i] <- TRUE
        ambient_temperature_run$temp[i+1:5] <- TRUE
      }
    }
    # values imbetween
    for(i in 6:(nrow(ambient_temperature_run)-6)){
      if(ambient_temperature_run$flag_ambient_temperature_run[i]==TRUE){
        ambient_temperature_run$temp[i] <- TRUE
        ambient_temperature_run$temp[i+1:5] <- TRUE
        ambient_temperature_run$temp[i-1:5] <- TRUE
      }
    }
    # take care of last five cases
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
  }
  else{# check ambient run temperature
    message(">>> Less than 10 collections, NOT checking ambient run temperature")
  }
  if(nrow(data$field_sampling_proc) > 10){
    if(return_flags){
      return(list("substrate_temperature" = substrate_temperature,
                  "ambient_temperature" = ambient_temperature,
                  "ambient_temperature_run" = ambient_temperature_run))
    }
  }
  else{
    if(return_flags){
      return(list("substrate_temperature" = substrate_temperature,
                  "ambient_temperature" = ambient_temperature))
    }
  }
}

#' fixTemperatures
#'
#' \code{fixTemperatures} fixes flags (3) regarding raw temperature parameters.
#'
#' To be run after checkTemperatures, user is advised to select the returned \emph{fulcrum_id} for changing (upon inspection)
#' from \code{checkTemperatures()} to pass into this function.
#' flagged substrate temperature: occurs when substrate temperature > 40, \code{procFulcrum}
#' will automatically convert to celsius (assumes temperature incorrectly in fahrenheit).
#' flagged ambient temperature: occurs when ambient temperature > 40, \code{procFulcrum}
#' will automatically convert to celsius (assumes temperature incorrectly in fahrenheit).
#' flagged ambient run temperature: occurs when ambient humidity and temperature are repeated in subsequent measurements,
#' this is meant to flag values if probe is stuck, \code{procFulcrum} does not automatically modify anything for this flag.
#' We advise re-running \code{checkTemperatures()} on the saved output of \code{fixTemperatures()} to ensure that wanted edits have appropriately occurred.
#'
#' @param data list format output of \code{procFulcrum}.
#' @param substrate_temperature_ids vector of \emph{fulcrum_id} of rows of \emph{substrate_temperature} to revert to original/raw temperature.
#' @param ambient_temperature_ids vector of \emph{fulcrum_id} of rows of \emph{ambient_temperature} to revert to original/raw temperature.
#' @param ambient_temperature_run_ids vector of \emph{fulcrum_id} of rows of \emph{ambient_temperature_run} to set \emph{proc_ambient_temperature}, \emph{ambient_humidity} to NA.
#' @return an edited (per passed \emph{fulcrum_id} values) list format output of \code{procFulcrum} to be used in easyfulcrum workflow (passed into \code{joinFulcrum()}).
#' @export
#'

fixTemperatures <- function(data,
                            substrate_temperature_ids = NULL,
                            ambient_temperature_ids = NULL,
                            ambient_temperature_run_ids = NULL) {
  for(i in 1:nrow(data$field_sampling_proc)){
    # replace proc_substrate_temperature with raw_substrate_temperature
    if(data$field_sampling_proc$fulcrum_id[i] %in% substrate_temperature_ids){
      data$field_sampling_proc$proc_substrate_temperature[i] = data$field_sampling_proc$raw_substrate_temperature[i]}
    # replace proc_ambient_temperature with raw_ambient_temperature
    if(data$field_sampling_proc$fulcrum_id[i] %in% ambient_temperature_ids){
      data$field_sampling_proc$proc_ambient_temperature[i] = data$field_sampling_proc$raw_ambient_temperature[i]}
    # replace proc_ambient_temperature, ambient_humidity with NA
    if(data$field_sampling_proc$fulcrum_id[i] %in% ambient_temperature_run_ids){
      data$field_sampling_proc$proc_ambient_temperature[i] = NA
      data$field_sampling_proc$ambient_humidity[i] = NA}
  }
  return(data)
}

#' checkJoin
#'
#' \code{checkJoin} checks for flags (9) in \code{joinFulcrum} output.
#'
#' This check function will return \emph{c_labels} and \emph{s_labels} for various flags relating to missing or duplicated data.
#' Messages regarding what checks are being done, and where they might lie in the raw dataframes are also provided.
#' We advise returning to the raw dataframes using the information returned in these checks to better understand
#' issues that might have occurred during data collection, before re-running.
#'
#' @param data dataframe output from \code{joinFulcrum}.
#' @param return_flags set to TRUE if flagged rows are to be returned as a dataframe.
#' @return \emph{c_labels} and \emph{s_label}s (as appropriate) for rows with each of the six flags,
#' and the origin of the location of the raw data that triggered the flag
#'  the rows corresponding to these labels can be saved as a list of six dataframes when return is set to TRUE
#' @import dplyr
#' @export
#'

checkJoin <- function(data, return_flags = FALSE) {
  # check data classes based on a reference dataframe (fulcrumTypes)
  message(">>> Checking data classes")
  types <- as.data.frame(unlist(sapply(data, class)))
  types[,2] <- rownames(types)
  rownames(types) <- NULL
  names(types) <- c("class", "variable")
  check_classes <- dplyr::left_join(easyfulcrum::fulcrumTypes, types, by = c("variable" = "variable")) %>%
    dplyr::filter(class != classExpected)
  print(paste("There are", nrow(check_classes), "improperly classified variables", sep = " "))
  if(nrow(check_classes) > 0){
    print(check_classes$variable)
    writeLines("Improperly classified variables may require manipulation after read-in.\nSee easyfulcrum::fulcrumTypes for expected classes")}
  # check for duplicated c labels
  message(">>> Checking duplicated c labels")
  duplicated_c_label <- data %>% dplyr::filter(flag_duplicated_c_label_field_sampling == TRUE)
  print(paste("There are", nrow(duplicated_c_label), "rows with duplicated c labels, these c labels are:", sep = " "))
  if(nrow(duplicated_c_label) > 0){
    print(duplicated_c_label$c_label)
    print("Duplicated c labels are found in the ...field_sampling.csv")}
  # check for unusual sample photo numbers
  message(">>> Checking unusual sample photo number")
  unusual_sample_photo_num <- data %>% dplyr::filter(flag_unusual_sample_photo_num == TRUE)
  print(paste("There are", nrow(unusual_sample_photo_num), "rows with unusual sample photo numbers, their c labels are:", sep = " "))
  if(nrow(unusual_sample_photo_num) > 0){
    print(unusual_sample_photo_num$c_label)
    print("Unusual sample photo number are found in the ...field_sampling.csv")}
  # check for unusual distances between photo and record locations
  message(">>> Checking for >500 meter distance between location methods") # TAC
  collection_lat_long_method_diff_extreme <- data %>% dplyr::filter(flag_collection_lat_long_method_diff_extreme == TRUE)
  print(paste("There are", nrow(collection_lat_long_method_diff_extreme), "rows with a >500 meter distance between the record creation location and photo location, their c labels are:", sep = " "))
  if(nrow(collection_lat_long_method_diff_extreme) > 0){
    print(collection_lat_long_method_diff_extreme$c_label)
    print("The unusual location c labels are found in the ...field_sampling.csv and the ...field_sampling_sample_photo.csv")}
  # check for unusual isolation photo numbers
  message(">>> Checking unusual isolation photo number") # TAC
  unusual_isolation_photo_num <- data %>% dplyr::filter(flag_unusual_isolation_photo_num == TRUE)
  print(paste("There are", nrow(unusual_isolation_photo_num), "rows with unusual isolation photo numbers, their c labels are:", sep = " "))
  if(nrow(unusual_isolation_photo_num) > 0){
    print(unusual_isolation_photo_num$c_label)
    print("Unusual isolation photo number are found in the ...isolation.csv")}
  # check for duplicated isolation for c labels
  message(">>> Checking duplicated isolation for c label")
  duplicated_isolation_for_c_label <- data %>% dplyr::filter(flag_duplicated_isolation_for_c_label == TRUE)
  print(paste("There are", nrow(duplicated_isolation_for_c_label), "rows with duplicated isolation for c label, their c labels are:", sep = " "))
  if(nrow(duplicated_isolation_for_c_label) > 0){
    print(duplicated_isolation_for_c_label$c_label)
    print("Duplicated isolation for c label are found in the ...isolation.csv")}
  # check for missing isolation records
  message(">>> Checking missing isolation records")
  missing_isolation_record <- data %>% dplyr::filter(flag_missing_isolation_record == TRUE)
  print(paste("There are", nrow(missing_isolation_record), "rows with missing isolation records, their c labels are:", sep = " "))
  if(nrow(missing_isolation_record) > 0){
    print(missing_isolation_record$c_label)
    print("Missing isolation records are found in the ...isolation.csv")}
  # check for extreme substrate temperature
  message(">>> Checking extreme substrate temperatures")
  extreme_substrate_temperature <- data %>% dplyr::filter(flag_substrate_temperature_extreme == TRUE)
  print(paste("There are", nrow(extreme_substrate_temperature), "rows with extreme substrate temperatures, their c labels are:", sep = " "))
  if(nrow(extreme_substrate_temperature) > 0){
    print(extreme_substrate_temperature$c_label)
    print("Extreme substrate temperatures are found in the ...field_sampling.csv")}
  # check for extreme ambient temperatures
  message(">>> Checking extreme ambient temperatures")
  extreme_ambient_temperature <- data %>% dplyr::filter(flag_ambient_temperature_extreme == TRUE)
  print(paste("There are", nrow(extreme_ambient_temperature), "rows with extreme ambient temperatures, their c labels are:", sep = " "))
  if(nrow(extreme_ambient_temperature) > 0){
    print(extreme_ambient_temperature$c_label)
    print("Extreme ambient temperatures are found in nematode_field_sampling.csv")}
  # check for extreme collection altitude
  message(">>> Checking extreme collection altitude")
  extreme_collection_altitude <- data %>% dplyr::filter(flag_collection_altitude_extreme == TRUE)
  print(paste("There are", nrow(extreme_collection_altitude), "rows with extreme collection altitudes, their c labels are:", sep = " "))
  if(nrow(extreme_collection_altitude) > 0){
    print(extreme_collection_altitude$c_label)
    print("Extreme collection altitudes are found in nematode_field_sampling.csv")}
  # check for missing s labels
  message(">>> Checking missing s labels")
  missing_s_label <- data %>% dplyr::filter(flag_missing_s_label_isolation_s_labeled_plates == TRUE)
  print(paste("There are", nrow(missing_s_label), "rows with missing s labels, their c labels are:", sep = " "))
  if(nrow(missing_s_label) > 0){
    print(missing_s_label$c_label)
    print("Missing s labels are found in nematode_isolation_s_labeled_plates.csv")}
  # check for duplicated s labels
  message(">>> Checking duplicated s labels")
  duplicated_s_label <- data %>% dplyr::filter(flag_duplicated_s_label_isolation_s_labeled_plates == TRUE)
  print(paste("There are", nrow(duplicated_s_label), "rows with duplicated s labels, their s labels are:", sep = " "))
  if(nrow(duplicated_s_label) > 0){
    print(duplicated_s_label$s_label)
    print("Duplicated s labels are found in nematode_isolation_s_labeled_plates.csv")}

  if(return_flags){
    return(list("check_classes" = check_classes,
                "duplicated_c_label" = duplicated_c_label,
                "unusual_sample_photo_num" = unusual_sample_photo_num,
                "duplicated_isolation_for_c_label" = duplicated_isolation_for_c_label,
                "collection_lat_long_method_diff_extreme" = collection_lat_long_method_diff_extreme,
                "missing_isolation_record" = missing_isolation_record,
                "unusual_isolation_photo_num" = unusual_isolation_photo_num,
                "extreme_substrate_temperature" = extreme_substrate_temperature,
                "extreme_ambient_temperature" = extreme_ambient_temperature,
                "extreme_collection_altitude" = extreme_collection_altitude,
                "missing_s_label" = missing_s_label,
                "duplicated_s_label" = duplicated_s_label))
  }
}

#' checkGenotypes
#'
#' \code{checkGenotypes} checks genotyping data for common errors in
#' \code{readGenotypes} output.
#'
#' @param geno_data a genotyping dataframe generated from the
#'   \code{readGenotypes} function.
#' @param fulc_data a single, joined fulcrum dataframe with all collection data.
#' @param return_geno logical, if \code{TRUE} the genotyping data is returned.
#' @param return_flags logical, if \code{TRUE} the rows of data for specific
#'   flags are returned.
#' @param target_sp vector of target species for species id checks. Default
#'   target species names are: Caenorhabditis briggsae, Caenorhabditis elegans,
#'   Caenorhabditis tropicalis. \code{target_sp} parameter is only required when
#'   the \code{profile} parameter is set to \code{"nematode"}.
#' @param profile set to \code{"general"} to use a basic genotyping sheet with
#'   basic functionality. Set to \code{"nematode"} to use the nematode specific
#'   genotyping sheet and the standard Andersen Lab functionality.
#' @return a list of flagged rows in genotyping and fulcrum dataframes for each
#'   flag.
#' @importFrom rebus ALPHA one_or_more %R% DGT WRD optional
#' @import dplyr
#' @export
#'

checkGenotypes <- function(geno_data, fulc_data, return_geno = TRUE, return_flags = FALSE,
                           target_sp = c("Caenorhabditis briggsae", "Caenorhabditis elegans", "Caenorhabditis tropicalis"),
                           profile = "general") {
  if(return_geno == TRUE & return_flags == TRUE){
    message("Both return_geno and return_flags cannnot be set to true, nothing will be returned")
  }
  if("list" %in% class(geno_data)){
    message("geno_data is in list form when dataframe is expected")
  }
  if(!(profile %in% c("general", "nematode"))){
    message('profile parameter must be set to either "general" or "nematode"')
  }

  if(profile == "nematode"){
    message('Using "nematode" profile:')

  message(">>> Checking data classes")
  types <- as.data.frame(unlist(sapply(geno_data, class)))
  types[,2] <- rownames(types)
  rownames(types) <- NULL
  names(types) <- c("class", "variable")
  check_classes <- dplyr::left_join(easyfulcrum::genotypeTypes, types, by = c("variable" = "variable")) %>%
    dplyr::filter(class != classExpected)
  print(paste("There are", nrow(check_classes), "improperly classified variables", sep = " "))
  if(nrow(check_classes) > 0){print(check_classes$variable)}

  # Find usual S-labels in genotyping dataframe
  usual_s_labels <- stringr::str_subset(geno_data$s_label, pattern = "S-" %R% DGT %R% DGT %R% DGT %R% DGT %R% optional(DGT) %R% optional(DGT))

  # add s_label flags to genotyping dataframe
  geno_data_flagged <- geno_data %>%
    dplyr::mutate(flag_unusual_s_label_genotyping = ifelse(!(s_label %in% c(usual_s_labels,NA)), TRUE, FALSE),
                  flag_missing_s_label_genotyping = ifelse(is.na(s_label), TRUE, FALSE),
                  flag_s_label_not_in_fulcrum = ifelse(!(s_label %in% fulc_data$s_label), TRUE, FALSE)) %>%
    dplyr::group_by(s_label) %>%
    dplyr::mutate(flag_duplicated_s_label_genotyping = ifelse(dplyr::n() > 1, TRUE, FALSE)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(flag_unusual_target_species_name = ifelse(make_strain_name == 1 & !(is.na(strain_name)) & is.na(possible_new_caeno_sp) &
                                                              !(species_id %in% target_sp), TRUE, FALSE),
                  flag_proliferation_missing = ifelse(is.na(proliferating), TRUE, FALSE),
                  flag_its2_genotype_expected = ifelse(proliferating == 1 & is.na(pcr_product_its2), TRUE, FALSE),
                  flag_species_id_expected = ifelse(pcr_product_its2 == 1 & is.na(species_id), TRUE, FALSE),
                  flag_species_id_expected = ifelse(is.na(flag_species_id_expected), FALSE, flag_species_id_expected),
                  flag_strain_name_expected = ifelse((make_strain_name == 1 & is.na(reason_strain_not_named) & is.na(strain_name)) | ((species_id %in% target_sp) & is.na(strain_name)), TRUE, FALSE),
                  flag_strain_name_expected = ifelse(is.na(flag_strain_name_expected), FALSE, flag_strain_name_expected))

  # report s_label check
  message(">>> Checking s labels")

  # check for missing s_labels
  missing_s_label_genotyping <- geno_data_flagged %>% dplyr::filter(flag_missing_s_label_genotyping == TRUE)
  print(paste("There are", nrow(missing_s_label_genotyping), "rows with missing s labels, these s labels are:", sep = " "))
  if(nrow(missing_s_label_genotyping) > 0){print(missing_s_label_genotyping$s_label)}

  # check for duplicated s_labels
  duplicated_s_label_genotyping <- geno_data_flagged %>% dplyr::filter(flag_duplicated_s_label_genotyping == TRUE)
  print(paste("There are", nrow(duplicated_s_label_genotyping), "rows with duplicated s labels, these s labels are:", sep = " "))
  if(nrow(duplicated_s_label_genotyping) > 0){print(duplicated_s_label_genotyping$s_label)}

  # check for unusual s_labels
  unusual_s_label_genotyping <- geno_data_flagged %>% dplyr::filter(flag_unusual_s_label_genotyping == TRUE)
  print(paste("There are", nrow(unusual_s_label_genotyping), "rows with unusual s labels, these s labels are:", sep = " "))
  if(nrow(unusual_s_label_genotyping) > 0){print(unusual_s_label_genotyping$s_label)}

  # check for s_labels not in fulcrum
  s_label_not_in_fulcrum <- geno_data_flagged %>% dplyr::filter(flag_s_label_not_in_fulcrum == TRUE)
  print(paste("There are", nrow(s_label_not_in_fulcrum), "rows with s labels not found in the Fulcrum data, these s labels are:", sep = " "))
  if(nrow(s_label_not_in_fulcrum) > 0){print(s_label_not_in_fulcrum$s_label)}

  # make a dataframe for s_labels in Fulcrum but not in the genotyping sheet
  s_label_in_fulcrum_not_in_genotyping <- fulc_data %>%
    dplyr::filter(!is.na(s_label)) %>%
    dplyr::filter(!(s_label %in% geno_data$s_label))
  print(paste("There are", nrow(s_label_in_fulcrum_not_in_genotyping), "s labels in the Fulcrum data not in the genotyping data, these s labels are:", sep = " "))
  if(nrow(s_label_in_fulcrum_not_in_genotyping) > 0){print(s_label_in_fulcrum_not_in_genotyping$s_label)}

  message(">>> Checking genotyping process")

  # check for expected but missing proliferation data
  proliferation_missing <- geno_data_flagged %>% dplyr::filter(flag_proliferation_missing == TRUE)
  print(paste("There are", nrow(proliferation_missing), "rows missing expected proliferation data, these s labels are:", sep = " "))
  if(nrow(proliferation_missing) > 0){print(proliferation_missing$s_label)}

  # check for its2
  its2_genotype_expected <- geno_data_flagged %>% dplyr::filter(flag_its2_genotype_expected == TRUE)
  print(paste("There are", nrow(its2_genotype_expected), "rows missing expected its2 genotype, these s labels are:", sep = " "))
  if(nrow(its2_genotype_expected) > 0){print(its2_genotype_expected$s_label)}

  # check for species_id
  species_id_expected <- geno_data_flagged %>% dplyr::filter(flag_species_id_expected == TRUE)
  print(paste("There are", nrow(species_id_expected), "rows missing expected species_id, these s labels are:", sep = " "))
  if(nrow(species_id_expected) > 0){print(species_id_expected$s_label)}

  # check for unusual target name
  unusual_target_species_name <- geno_data_flagged %>% dplyr::filter(flag_unusual_target_species_name == TRUE)
  print(paste("There are", nrow(unusual_target_species_name), "rows with unusual target species names, these names are:", sep = " "))
  if(nrow(unusual_target_species_name) > 0){print(unusual_target_species_name$species_id)}

  # check for expected strain_name
  strain_name_expected <- geno_data_flagged %>% dplyr::filter(flag_strain_name_expected == TRUE)
  print(paste("There are", nrow(strain_name_expected), "rows missing expected strain_name, these s labels are:", sep = " "))
  if(nrow(strain_name_expected) > 0){print(strain_name_expected$s_label)}

  # return geno_data with added flags
  if(return_flags == FALSE){
    if(return_geno == TRUE){return(geno_data_flagged)}}

  # return data frames with appropriate missing flags
  if(return_geno == FALSE){
    if(return_flags == TRUE){
      return(list("check_classes" = check_classes,
                  "missing_s_label_genotyping" = missing_s_label_genotyping,
                  "duplicated_s_label_genotyping" = duplicated_s_label_genotyping,
                  "unusual_s_label_genotyping" = unusual_s_label_genotyping,
                  "s_label_not_in_fulcrum" = s_label_not_in_fulcrum,
                  "s_label_in_fulcrum_not_in_genotyping" = s_label_in_fulcrum_not_in_genotyping,
                  "proliferation_missing" = proliferation_missing,
                  "its2_genotype_expected" = its2_genotype_expected,
                  "species_id_expected" = species_id_expected,
                  "unusual_target_species_name" = unusual_target_species_name,
                  "strain_name_expected" = strain_name_expected))
    }
  }
}
if(profile == "general"){
  message('Using "general" profile, target_sp parameter will not be used:')

  # Find usual S-labels in genotyping dataframe
  usual_s_labels <- stringr::str_subset(geno_data$s_label, pattern = "S-" %R% DGT %R% DGT %R% DGT %R% DGT %R% optional(DGT) %R% optional(DGT))

  # add s_label flags to genotyping dataframe
  geno_data_flagged <- geno_data %>%
    dplyr::mutate(flag_unusual_s_label_genotyping = ifelse(!(s_label %in% c(usual_s_labels,NA)), TRUE, FALSE),
                  flag_missing_s_label_genotyping = ifelse(is.na(s_label), TRUE, FALSE),
                  flag_s_label_not_in_fulcrum = ifelse(!(s_label %in% fulc_data$s_label), TRUE, FALSE)) %>%
    dplyr::group_by(s_label) %>%
    dplyr::mutate(flag_duplicated_s_label_genotyping = ifelse(dplyr::n() > 1, TRUE, FALSE)) %>%
    dplyr::ungroup()

  # report s_label check
  message(">>> Checking s labels")

  # check for missing s_labels
  missing_s_label_genotyping <- geno_data_flagged %>% dplyr::filter(flag_missing_s_label_genotyping == TRUE)
  print(paste("There are", nrow(missing_s_label_genotyping), "rows with missing s labels, these s labels are:", sep = " "))
  if(nrow(missing_s_label_genotyping) > 0){print(missing_s_label_genotyping$s_label)}

  # check for duplicated s_labels
  duplicated_s_label_genotyping <- geno_data_flagged %>% dplyr::filter(flag_duplicated_s_label_genotyping == TRUE)
  print(paste("There are", nrow(duplicated_s_label_genotyping), "rows with duplicated s labels, these s labels are:", sep = " "))
  if(nrow(duplicated_s_label_genotyping) > 0){print(duplicated_s_label_genotyping$s_label)}

  # check for unusual s_labels
  unusual_s_label_genotyping <- geno_data_flagged %>% dplyr::filter(flag_unusual_s_label_genotyping == TRUE)
  print(paste("There are", nrow(unusual_s_label_genotyping), "rows with unusual s labels, these s labels are:", sep = " "))
  if(nrow(unusual_s_label_genotyping) > 0){print(unusual_s_label_genotyping$s_label)}

  # check for s_labels not in fulcrum
  s_label_not_in_fulcrum <- geno_data_flagged %>% dplyr::filter(flag_s_label_not_in_fulcrum == TRUE)
  print(paste("There are", nrow(s_label_not_in_fulcrum), "rows with s labels not found in the Fulcrum data, these s labels are:", sep = " "))
  if(nrow(s_label_not_in_fulcrum) > 0){print(s_label_not_in_fulcrum$s_label)}

  # make a dataframe for s_labels in Fulcrum but not in the genotyping sheet
  s_label_in_fulcrum_not_in_genotyping <- fulc_data %>%
    dplyr::filter(!is.na(s_label)) %>%
    dplyr::filter(!(s_label %in% geno_data$s_label))
  print(paste("There are", nrow(s_label_in_fulcrum_not_in_genotyping), "s labels in the Fulcrum data not in the genotyping data, these s labels are:", sep = " "))
  if(nrow(s_label_in_fulcrum_not_in_genotyping) > 0){print(s_label_in_fulcrum_not_in_genotyping$s_label)}


  # return geno_data with added flags
  if(return_flags == FALSE){
    if(return_geno == TRUE){return(geno_data_flagged)}}

  # return data frames with appropriate missing flags
  if(return_geno == FALSE){
    if(return_flags == TRUE){
      return(list("missing_s_label_genotyping" = missing_s_label_genotyping,
                  "duplicated_s_label_genotyping" = duplicated_s_label_genotyping,
                  "unusual_s_label_genotyping" = unusual_s_label_genotyping,
                  "s_label_not_in_fulcrum" = s_label_not_in_fulcrum,
                  "s_label_in_fulcrum_not_in_genotyping" = s_label_in_fulcrum_not_in_genotyping))
    }
  }
}
}
