#' checkGenotypes
#'
#' \code{checkGenotypes} checks genotyping data for common errors
#'
#' @param geno_data a genotyping dataframe generated from the \code{procGenotypes} function.
#' @param fulc_data a single, joined fulcrum dataframe with all collection data.
#' @param return logical, if \code{TRUE} the rows of data for specific flags are returned.
#' @return a list of flagged rows in genptyping and fulcrum dataframes for each flag.
#' @export
#'

checkGenotypes <- function(geno_data, fulc_data, return = FALSE) {
  # report s_label check
  message(">>> Checking s labels")
  # missing s_labels
  missing_s_label_genotyping <- geno_data %>% dplyr::filter(flag_missing_s_label_genotyping == TRUE)
  print(paste("There are", nrow(missing_s_label_genotyping), "rows with missing s labels, these s labels are:", sep = " "))
  if(nrow(missing_s_label_genotyping) > 0){print(missing_s_label_genotyping$s_label)}
  # dupicated s_labels
  duplicated_s_label_genotyping <- geno_data %>% dplyr::filter(flag_duplicated_s_label_genotyping == TRUE)
  print(paste("There are", nrow(duplicated_s_label_genotyping), "rows with duplicated s labels, these s labels are:", sep = " "))
  if(nrow(duplicated_s_label_genotyping) > 0){print(duplicated_s_label_genotyping$s_label)}
  # unusual s_labels
  unusual_s_label_genotyping <- geno_data %>% dplyr::filter(flag_unusual_s_label_genotyping == TRUE)
  print(paste("There are", nrow(unusual_s_label_genotyping), "rows with unusual s labels, these s labels are:", sep = " "))
  if(nrow(unusual_s_label_genotyping) > 0){print(unusual_s_label_genotyping$s_label)}
  # s_labels not in fulcrum
  s_label_not_in_fulcrum <- geno_data %>% dplyr::filter(flag_s_label_not_in_fulcrum == TRUE)
  print(paste("There are", nrow(s_label_not_in_fulcrum), "rows with s labels not found in the Fulcrum data, these s labels are:", sep = " "))
  if(nrow(s_label_not_in_fulcrum) > 0){print(s_label_not_in_fulcrum$s_label)}
  # report unusual target species names
  message(">>> Checking genotyping process")
  proliferation_missing <- geno_data %>% dplyr::filter(flag_proliferation_missing == TRUE)
  print(paste("There are", nrow(proliferation_missing), "rows missing expected proliferation data, these s labels are:", sep = " "))
  if(nrow(proliferation_missing) > 0){print(proliferation_missing$s_label)}
  # its2 check
  its2_genotype_expected <- geno_data %>% dplyr::filter(flag_its2_genotype_expected == TRUE)
  print(paste("There are", nrow(its2_genotype_expected), "rows missing expected its2 genotype, these s labels are:", sep = " "))
  if(nrow(its2_genotype_expected) > 0){print(its2_genotype_expected$s_label)}
  # species_id check
  species_id_expected <- geno_data %>% dplyr::filter(flag_species_id_expected == TRUE)
  print(paste("There are", nrow(species_id_expected), "rows missing expected species_id, these s labels are:", sep = " "))
  if(nrow(species_id_expected) > 0){print(species_id_expected$s_label)}
  # unusual target name
  unusual_target_species_name <- geno_data %>% dplyr::filter(flag_unusual_target_species_name == TRUE)
  print(paste("There are", nrow(unusual_target_species_name), "rows with unusual target species names, these names are:", sep = " "))
  if(nrow(unusual_target_species_name) > 0){print(unusual_target_species_name$species_id)}
  # expected strain_name
  strain_name_expected <- geno_data %>% dplyr::filter(flag_strain_name_expected == TRUE)
  print(paste("There are", nrow(strain_name_expected), "rows missing expected strain_name, these s labels are:", sep = " "))
  if(nrow(strain_name_expected) > 0){print(strain_name_expected$s_label)}
  # Make a dataframe for s_labels in Fulcrum but not in the genotyping sheet
  s_label_in_geno_not_in_fulcrum <- fulc_data %>%
    dplyr::filter(!is.na(s_label)) %>%
    dplyr::filter(!(s_label %in% geno_data$s_label))
  print(paste("There are", nrow(s_label_in_geno_not_in_fulcrum), "s labels in the fulc_data not in the geno_data, these s labels are:", sep = " "))
  if(nrow(s_label_in_geno_not_in_fulcrum) > 0){print(s_label_in_geno_not_in_fulcrum$s_label)}

  # return
  if(return){
    return(list("missing_s_label_genotyping" = missing_s_label_genotyping,
                "duplicated_s_label_genotyping" = duplicated_s_label_genotyping,
                "unusual_s_label_genotyping" = unusual_s_label_genotyping,
                "s_label_not_in_fulcrum" = s_label_not_in_fulcrum,
                "proliferation_missing" = proliferation_missing,
                "its2_genotype_expected" = its2_genotype_expected,
                "species_id_expected" = species_id_expected,
                "unusual_target_species_name" = unusual_target_species_name,
                "strain_name_expected" = strain_name_expected,
                "s_label_in_geno_not_in_fulcrum" = s_label_in_geno_not_in_fulcrum))
  }
}
