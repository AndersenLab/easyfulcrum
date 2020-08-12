#' makeSpSheet
#'
#' \code{makeSpSheet} produces a species sheet .csv file for target species
#'
#' @param data a checked dataframe generated from the \code{joinGenoFulc} function.
#' @param target_sp a vector of target species names with full genus and species names. Default target species names are:
#' Caenorhabditis briggsae, Caenorhabditis elegans, Caenorhabditis tropicalis.
#' @return a list of dataframes containing species sheets for target species with flags for identified issues
#' @export
#'

makeSpSheet <- function(data, target_sp = c("Caenorhabditis briggsae", "Caenorhabditis elegans", "Caenorhabditis tropicalis")) {
  # select and rename data
  sp_data <- data %>%
    dplyr::filter(!is.na(ECA_name)) %>%
    dplyr::distinct(ECA_name, .keep_all = TRUE) %>%
    dplyr::select(species_id,
                  ECA_name,
                  collection_latitude,
                  collection_longitude,
                  landscape,
                  collection_location,
                  collection_island,
                  collection_trail,
                  substrate,
                  substrate_notes,
                  substrate_other,
                  proc_substrate_temperature,
                  proc_ambient_temperature,
                  ambient_humidity,
                  collection_by,
                  isolation_by,
                  collection_date_UTC) %>%
    dplyr::mutate(species_id_method = "ITS2",
                  isotype = NA_character_,
                  previous_names = NA_character_,
                  release = as.numeric(NA),
                  source_lab = stringr::str_extract(ECA_name, pattern = "^[A-Z]{2,4}"),
                  associated_organism = NA_character_,
                  inbreeding_state = NA_character_,
                  sampling_date_comments = NA_character_,
                  notes = NA_character_,
                  set = as.numeric(NA),
                  issues = NA_character_,
                  issue_notes = NA_character_,
                  isotype_ref_strain = NA_character_,
                  wgs_seq = NA_character_,
                  substrate_comments = glue::glue("{substrate_notes}{substrate_other}", .na = "", .sep = ","),
                  locality_description = glue::glue("{collection_location}{collection_island}{collection_trail}", .na = "", .sep = ",")) %>%
    dplyr::select(species = species_id,
                  species_id_method,
                  strain = ECA_name,
                  isotype,
                  previous_names,
                  release,
                  source_lab,
                  latitude = collection_latitude,
                  longitude = collection_longitude,
                  landscape,
                  locality_description,
                  substrate,
                  substrate_comments,
                  substrate_temp = proc_substrate_temperature,
                  ambient_temp = proc_ambient_temperature,
                  ambient_humidity = ambient_humidity,
                  associated_organism,
                  inbreeding_state,
                  sampled_by = collection_by,
                  isolated_by = isolation_by,
                  sampling_date = collection_date_UTC,
                  sampling_date_comments,
                  notes,
                  set,
                  issues,
                  issue_notes,
                  isotype_ref_strain,
                  wgs_seq)

  # make flags
  flag_sp <- sp_data %>%
    dplyr::mutate(flag_sampled_by_is_email_address = ifelse(stringr::str_detect(sampled_by, pattern = "@") == TRUE, TRUE, FALSE),
                  flag_isolated_by_is_email_address = ifelse(stringr::str_detect(isolated_by, pattern = "@") == TRUE, TRUE, FALSE),
                  flag_species_not_in_target_species = ifelse(!(species %in% target_sp), TRUE, FALSE))

  # message about flags
  sampled_by_is_email_address <- flag_sp %>% dplyr::filter(flag_sampled_by_is_email_address == TRUE)
  print(paste("There are", nrow(sampled_by_is_email_address), "strains with an email address for sampled_by:", sep = " "))
  if(nrow(sampled_by_is_email_address) > 0){print(sampled_by_is_email_address$strain)}

  isolated_by_is_email_address <- flag_sp %>% dplyr::filter(flag_isolated_by_is_email_address == TRUE)
  print(paste("There are", nrow(isolated_by_is_email_address), "strains with an email address for isolated_by:", sep = " "))
  if(nrow(isolated_by_is_email_address) > 0){print(isolated_by_is_email_address$strain)}

  species_not_in_target_species <- flag_sp %>% dplyr::filter(flag_species_not_in_target_species == TRUE)
  print(paste("There are", nrow(species_not_in_target_species), "strains with a species name not in the target species list:", sep = " "))
  if(nrow(species_not_in_target_species) > 0){print(species_not_in_target_species$strain)}

  # return
  return(flag_sp)
}