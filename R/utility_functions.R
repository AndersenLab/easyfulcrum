#' filter_box
#'
#' \code{filter_box} finds lat and long inside bounding box
#'
#' @param longitude longitude of collection
#' @param latitude latitude of collection
#' @param coords coordinates of bounding box
#'

filter_box <- function(longitude, latitude, coords) {
  between(longitude, coords[1], coords[3]) &
    between(latitude, coords[2], coords[4]) &
    !is.na(longitude)
}

#' FtoC
#'
#' \code{FtoC} Converts fahrenheit measurement to celsius
#'
#' @param F fahrenheit measurement to convert
#'

FtoC <- function(F) {
  (F - 32) * (5 / 9)
}

generateReport <- function(data){
  data <- data %>% dplyr::select(project,
                        c_label,
                        s_label,
                        species_id,
                        strain_name,
                        flag_ambient_temperature,
                        flag_ambient_temperature_run,
                        flag_substrate_temperature,
                        flag_unusual_sample_photo_num,
                        flag_duplicated_c_label_field_sampling,
                        flag_duplicated_isolation_for_c_label,
                        flag_duplicated_s_label_isolation_s_labeled_plates,
                        flag_missing_s_label_isolation_s_labeled_plates,
                        flag_missing_isolation_record,
                        collection_by,
                        collection_datetime_UTC,
                        collection_date_UTC,
                        collection_local_time,
                        collection_fulcrum_latitude,
                        collection_fulcrum_longitude,
                        exif_gps_latitude,
                        exif_gps_longitude,
                        latitude = collection_latitude,
                        longitude = collection_longitude,
                        collection_lat_long_method,
                        collection_lat_long_method_diff,
                        fulcrum_altitude,
                        exif_gps_altitude,
                        collection_altitude,
                        collection_altitude_method,
                        collection_location,
                        collection_island,
                        collection_trail,
                        landscape = landscape,
                        sky_view,
                        ambient_humidity = ambient_humidity,
                        substrate = substrate,
                        substrate_comments = substrate_notes,
                        substrate_other,
                        ambient_temp = proc_ambient_temperature,
                        substrate_temp = proc_substrate_temperature,
                        gridsect,
                        gridsect_index,
                        gridsect_radius,
                        grid_sect_direction,
                        sample_photo1,
                        sample_photo2,
                        sample_photo3,
                        best_exif_dop_photo,
                        best_sample_photo_caption,
                        isolated_by = isolation_by,
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
                        notes = general_notes,
                        manual_blast_notes,
                        possible_new_caeno_sp,
                        make_strain_name,
                        reason_strain_not_named)
  write.csv(data, paste(data$project[1],".csv", sep = ""))
}

