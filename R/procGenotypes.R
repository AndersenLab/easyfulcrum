#' procGenotypes
#'
#' \code{procGenotypes} adds flags to genotyping dataframe
#'
#' @param data a dataframe resulting from \code{readGenotypes} function.
#' @param target_sp a vector of target species names with full genus and species names. Default target species names are:
#' Caenorhabditis briggsae, Caenorhabditis elegans, Caenorhabditis tropicalis.
#' @return A dataframe with flags for common errors in Genotyping dataframe.
#' @importFrom rebus %R% DGT optional
#' @export
#'

procGenotypes <- function(data, target_sp = c("Caenorhabditis briggsae", "Caenorhabditis elegans", "Caenorhabditis tropicalis")) {
  # Find usual S-labels in genotyping dataframe
  usual_s_labels <- stringr::str_subset(data$s_label, pattern = "S-" %R% DGT %R% DGT %R% DGT %R% DGT %R% optional(DGT) %R% optional(DGT))

  # add s_label flags to genotyping dataframe
  flag <- data %>%
    dplyr::mutate(flag_unusual_s_label_genotyping = ifelse(!(s_label %in% usual_s_labels), TRUE, FALSE),
                  flag_missing_s_label_genotyping = ifelse(is.na(s_label), TRUE, FALSE)) %>%
    dplyr::group_by(s_label) %>%
    dplyr::mutate(flag_duplicated_s_label_genotyping = ifelse(dplyr::n() > 1, TRUE, FALSE)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(flag_unusual_target_species_name = ifelse(make_strain_name == 1 & !(is.na(ECA_name)) & is.na(possible_new_caeno_sp) &
                                                              !(species_id %in% c("Caenorhabditis elegans", "Caenorhabditis briggsae", "Caenorhabditis tropicalis")), TRUE, FALSE),
                  flag_proliferation_missing = ifelse(is.na(proliferating), TRUE, FALSE),
                  flag_its2_genotype_expected = ifelse(proliferating == 1 & is.na(pcr_product_its2), TRUE, FALSE),
                  flag_species_id_expected = ifelse(pcr_product_its2 == 1 & is.na(species_id), TRUE, FALSE),
                  flag_species_id_expected = ifelse(is.na(flag_species_id_expected), FALSE, flag_species_id_expected),
                  flag_ECA_name_expected = ifelse((make_strain_name == 1 & is.na(reason_strain_not_named) & is.na(ECA_name)) | ((species_id %in% target_sp) & is.na(ECA_name)), TRUE, FALSE),
                  flag_ECA_name_expected = ifelse(is.na(flag_ECA_name_expected), FALSE, flag_ECA_name_expected))

  # return raw genotyping sheet
  message(glue::glue("processed {unique(data$project_id)} genotyping data"))
  return(flag)
}
