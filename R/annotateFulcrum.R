#' annotateFulcrum
#'
#' \code{annotateFulcrum} Adds additional collection location information to the
#' final Fulcrum dataframe
#'
#' @param data A single dataframe generated with the joinFulcrum function.
#' @param dir The path to the base fulcrum directory. Within this directory,
#'  data/raw/fulcrum should contain: island.csv, location.csv, trails.csv
#' @param select_vars Logical, TRUE  will return only the default variables,
#'   FALSE will return all variables. FALSE is recommended if using customized
#'   Fulcrum applications other than "Nematode field sampling" and "Nematode
#'   isolation". FALSE is default.
#' @return A single dataframe containing all Fulcrum data sources. This data
#'   frame contains all necessary variables from Fulcrum. It also contains data
#'   quality flags. The variable names match the data dictionary.
#' @importFrom glue glue
#' @import tibble
#' @import dplyr
#' @import sp
#' @export
#'

annotateFulcrum <- function(data, dir = NULL, select_vars = F) {
  # import island csv
  island <- easyfulcrum::hawaii_islands
  # import location csv
  location <- easyfulcrum::hawaii_locations

  # Generate list of trails and geojson polygon points from geojson output of https://boundingbox.klokantech.com/.
  # These polygons are manually curated by using the polygon tool.

  # import trails csv
  trails_df <- easyfulcrum::hawaii_trails

  if(!is.null(dir)){
    dir <- glue::glue("{dir}","/data/raw/annotate")
    message(">>> Checking for homemade island.csv, location.csv, trails.csv files in data/raw/annotate")
    island <- read.csv(glue::glue("{dir}","/island.csv"))
    location <- read.csv(glue::glue("{dir}","/location.csv"))
    trails_df <- read.csv(glue::glue("{dir}","/trails.csv"))
  }

  # assign data to joined_data
  joined_data <- data

  # Create Island Column
  joined_data$collection_island <- NA_character_

  # Loop through imported island longitude and latitudes to assign values to Island Column
  for (i in 1:nrow(island)){
    joined_data[filter_box(joined_data$collection_longitude, joined_data$collection_latitude, c(island$long_start[i], island$lat_start[i], island$long_end[i], island$lat_end[i])), "collection_island"] <- island$island_name[i]
  }

  # Create location Column
  joined_data$collection_location <- NA_character_

  for (i in 1:nrow(location)){
    joined_data[filter_box(joined_data$collection_longitude, joined_data$collection_latitude, c(location$long_start[i], location$lat_start[i], location$long_end[i], location$lat_end[i])), "collection_location"] <- location$location_name[i]
  }

  trails <- as.list(trails_df$coordinates)
  names(trails) <- trails_df$trail_name

  # Make dataframe with trail specific polygon points
  trail_coordinates <- NULL

  for(i in 1:length(trails)){
    longs <- tibble::as_tibble(stringr::str_match_all(trails,  "(?<=\\[).+?(?=,)")[[i]], .name_repair = ~ vctrs::vec_as_names(..., repair = "unique", quiet = TRUE)) %>%
      dplyr::rename(longitudes = `...1`) %>%
      dplyr::mutate(longitudes = as.numeric(longitudes))

    lats <- tibble::as_tibble(stringr::str_match_all(trails,  "(?<=[0-9],).+?(?=\\])")[[i]], .name_repair = ~ vctrs::vec_as_names(..., repair = "unique", quiet = TRUE)) %>%
      dplyr::rename(latitudes = `...1`) %>%
      dplyr::mutate(latitudes = as.numeric(latitudes))

    long_lats <- dplyr::bind_cols(longs, lats) %>%
      dplyr::mutate(trail = names(trails)[i])

    trail_coordinates <- rbind(trail_coordinates, long_lats)
  }

  # Create trail polygon object from trail_coordinates dataframe
  trail_polygons_list <-  vector('list', length = length(trails))

  for(i in 1:length(trail_polygons_list)){
    trail_polygons_list[[i]] = sp::Polygons(list(sp::Polygon(cbind(trail_coordinates %>% dplyr::filter(trail == glue::glue("{list(unique(trail_coordinates$trail))[[1]][i]}")) %>% .$longitudes,
                                                                   trail_coordinates %>% dplyr::filter(trail == glue::glue("{list(unique(trail_coordinates$trail))[[1]][i]}")) %>% .$latitudes))),
                                            ID = glue::glue("{list(unique(trail_coordinates$trail))[[1]][i]}"))
  }
  trail_polygons <- sp::SpatialPolygons(trail_polygons_list)

  # find if collection locations fall within trail polygons
  pts <- sp::SpatialPoints(cbind(joined_data %>% dplyr::filter(!is.na(collection_longitude)) %>% .$collection_longitude,
                                 joined_data %>% dplyr::filter(!is.na(collection_latitude)) %>% .$collection_latitude))

  # bind trails
  fulc_data <- cbind(unname(sp::over(pts, trail_polygons)), joined_data %>%
                       dplyr::filter(!is.na(collection_longitude) & !is.na(collection_latitude))) %>%
    dplyr::rename(collection_trail = 1) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(collection_trail = glue::glue("{list(unique(trail_coordinates$trail))[[1]][{collection_trail}]}")) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(collection_trail = ifelse(collection_trail == "NA", NA_character_, collection_trail)) %>% # fix NAs
    dplyr::full_join(joined_data) # add back joined_data with NAs for complete collection

  # chose the selected data or not
  if(select_vars == TRUE) {
    fulc_data_selected <- fulc_data %>%
      dplyr::select(project,
                    c_label,
                    s_label,
                    flag_ambient_temperature,
                    flag_ambient_temperature_extreme,
                    flag_ambient_temperature_run,
                    flag_substrate_temperature,
                    flag_substrate_temperature_extreme,
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
                    flag_collection_altitude_extreme,
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
                    best_photo,
                    best_photo_gps_dop,
                    best_photo_caption,
                    gps_course,
                    gps_horizontal_accuracy,
                    gps_speed,
                    gps_vertical_accuracy,
                    isolation_by,
                    isolation_datetime_UTC,
                    isolation_date_UTC,
                    isolation_local_time,
                    isolation_latitude,
                    isolation_longitude,
                    worms_on_sample)
  }
  # return data
  if(select_vars == TRUE){
    message("returning selected data, set select_vars to FALSE if variables are missing")
    return(fulc_data_selected)
  }
  else{
    message("returning all data, set select_vars to TRUE if you want to select default variables")
    return(fulc_data)
  }
}

