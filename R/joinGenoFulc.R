#' joinGenoFulc
#'
#' \code{joinGenoFulc} joins the collection data output from the \code{procFulcrum} function
#' with the genotyping data output from the \code{readGenotypes} function.
#' Blast data output from the \code{procSanger} function can also be joined if desired.
#'
#' @param fulc a collection data frame output from the \code{procFulcrum} function.
#' @param geno a genotyping data frame output from the \code{loadGenotypes} function.
#' @param blast OPTIONAL, a blast results data frame output from the \code{procSanger} function.
#'
#' @return A single collection dataframe with variables descriped in the data dictionary.
#' @export
#'

joinGenoFulc <- function(geno, fulc, blast = NULL, dir = NULL) {
  # Save the finalized genotyping sheet if given
  if(is.character(dir)){
    save(data = geno,
         file = glue::glue("{dir}","/data/processed/genotypes/",
                           as.character(Sys.Date()),"_",
                           tail(strsplit(dir,"/")[[1]],1),".rds"), version = 3)}
  if(is.null(blast)){
    # Join genotyping sheet with collection and isolation data
    out_dat <- fulc %>%
      dplyr::full_join(geno) %>%
      # Reorder variables
      dplyr::select(project,
                    c_label,
                    s_label,
                    species_id,
                    strain_name,
                    collection_by,
                    collection_datetime_UTC,
                    collection_date_UTC,
                    collection_local_time,
                    collection_fulcrum_latitude,
                    collection_fulcrum_longitude,
                    exif_gps_latitude,
                    exif_gps_longitude,
                    collection_latitude,
                    collection_longitude,
                    collection_lat_long_method,
                    collection_lat_long_method_diff,
                    fulcrum_altitude,
                    exif_gps_altitude,
                    collection_altitude,
                    collection_altitude_method,
                    collection_location,
                    collection_island,
                    collection_trail,
                    landscape,
                    sky_view,
                    ambient_humidity,
                    substrate,
                    substrate_notes,
                    substrate_other,
                    raw_ambient_temperature,
                    proc_ambient_temperature,
                    raw_substrate_temperature,
                    proc_substrate_temperature,
                    gridsect,
                    gridsect_index,
                    gridsect_radius,
                    grid_sect_direction,
                    sample_photo1,
                    sample_photo2,
                    sample_photo3,
                    best_exif_dop_photo,
                    best_sample_photo_caption,
                    isolation_by,
                    isolation_datetime_UTC,
                    isolation_date_UTC,
                    isolation_local_time,
                    isolation_latitude,
                    isolation_longitude,
                    worms_on_sample,
                    approximate_number_of_worms,
                    shipment_sent_date,
                    shipment_received_date,
                    proliferation_48,
                    proliferation_168,
                    proliferating,
                    lysis_date,
                    pcr_product_its2,
                    pcr_product_ssu,
                    general_notes,
                    manual_blast_notes,
                    possible_new_caeno_sp,
                    make_strain_name,
                    reason_strain_not_named,
                    flag_ambient_temperature,
                    flag_ambient_temperature_run,
                    flag_substrate_temperature,
                    flag_unusual_sample_photo_num,
                    flag_duplicated_c_label_field_sampling,
                    flag_duplicated_isolation_for_c_label,
                    flag_duplicated_s_label_isolation_s_labeled_plates,
                    flag_missing_s_label_isolation_s_labeled_plates,
                    flag_missing_isolation_record,
                    flag_unusual_s_label_genotyping,
                    flag_missing_s_label_genotyping,
                    flag_duplicated_s_label_genotyping,
                    flag_unusual_target_species_name)
  }
  else{
    # load blast results
    blast_results <- readr::read_tsv(blast)
    message(glue::glue("loading blast results from {blast}"))

    # Join genotyping sheet with collection and isolation data
    out_dat <- fulc %>%
      dplyr::full_join(geno)
  }
  return(out_dat)
}
