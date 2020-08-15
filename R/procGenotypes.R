#' procGenotypes
#'
#' \code{procGenotypes} adds flags to genotyping dataframe
#'
#' @param geno_data a dataframe resulting from the \code{readGenotypes} function.
#' @param fulc_data a checked dataframe resulting from the \code{joinFulcrum} function.
#' @param target_sp a vector of target species names with full genus and species names. Default target species names are:
#' Caenorhabditis briggsae, Caenorhabditis elegans, Caenorhabditis tropicalis.
#' @return A dataframe with flags for common errors in Genotyping dataframe.
#' @importFrom rebus %R% DGT optional
#' @export
#'

procGenotypes <- function(geno_data, fulc_data, target_sp = c("Caenorhabditis briggsae", "Caenorhabditis elegans", "Caenorhabditis tropicalis")) {
  # Find usual S-labels in genotyping dataframe
  usual_s_labels <- stringr::str_subset(geno_data$s_label, pattern = "S-" %R% DGT %R% DGT %R% DGT %R% DGT %R% optional(DGT) %R% optional(DGT))

  # add s_label flags to genotyping dataframe
  flag <- geno_data %>%
    dplyr::mutate(flag_unusual_s_label_genotyping = ifelse(!(s_label %in% usual_s_labels), TRUE, FALSE),
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

  # return raw genotyping sheet
  message(glue::glue("processed {unique(geno_data$project_id)} genotyping data"))
  return(flag)
}
