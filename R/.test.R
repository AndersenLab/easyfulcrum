# set working directory
setwd(glue::glue("{dirname(rstudioapi::getActiveDocumentContext()$path)}/.."))

# setup test directory for multi project Fulcrum export
dir1 <- "test_data/2020FebruaryAustralia/data/fulcrum"

# test readFulcrum function
raw_data1 <- readFulcrum(dir1)

# test procFulcrum function
proc_data1 <- procFulcrum(raw_data1)

# test parameter_check function
parameter_check(proc_data1)

# test parameter_check function with optional parameters set to TRUE
test1 <- parameter_check(proc_data1, save = TRUE, return = TRUE)

#test parameter_fix function (there are 5 flagged ambient_temperature run values and their corresponding fulcrum ids are: test1[["ambient_temperature_run"]]$fulcrum_id)
proc_data2 <- parameter_fix(proc_data1, ambient_temperature_run_ids = test1[["ambient_temperature_run"]]$fulcrum_id)

# test joinFulcrum function
join_data1 <- joinFulcrum(proc_data1)
join_data2 <- joinFulcrum2(proc_data1)

# test readGenotypes function
geno_data1 <- readGenotypes(gsKey = c("1CxKJHM6mEu4VvnN2T1ioXiJNZmmmpeosmECP2zeAPmY"))

# test joinGenoFulc function to join genotype data to fulcrum data
joingeno_data1 <- joinGenoFulc(geno = geno_data1, fulc = anno_data1,)

# test the procPhotos function. Output is final dataframe
photodir <- "test_data/2020FebruaryAustralia/data/fulcrum/photos"
final_data1 <- procPhotos(photodir, joingeno_data1)



##############
# test procFulcrum function
proc_data2 <- procFulcrum2(raw_data2)
data <- raw_data2

procFulcrum2 <- function(data) {
  # find names of data
  data_names <- names(data)

  # make processed data list
  proc_data <- list()

  # process nematode_field_sampling
  if("nematode_field_sampling" %in% data_names) {
  message("Processing nematode_field_sampling")
  nematode_field_sampling_proc <- data$nematode_field_sampling %>%
    dplyr::mutate(c_label = stringr::str_to_upper(c_label)) %>%
    # name created_by to specify who picked up the sample
    dplyr::rename(collection_by = created_by,
                  collection_fulcrum_latitude = latitude,
                  collection_fulcrum_longitude = longitude,
                  fulcrum_altitude = gps_altitude,
                  collection_local_time = time) %>%
    dplyr::select(-updated_at,
                  -system_created_at,
                  -system_updated_at,
                  -date) %>%
    # choose one sample photo only. This takes the first sample photo and warns if additional photos are discarded
    tidyr::separate(col = sample_photo, into = "sample_photo", sep = ",", extra = "warn") %>%
    # this is UTC time (very important if you want to convert to local time)
    dplyr::mutate(collection_datetime_UTC = lubridate::ymd_hms(created_at, tz = "UTC")) %>%
    # again this is UTC date (very important if you want to convert to local date)
    dplyr::mutate(collection_date_UTC = lubridate::date(created_at)) %>%
    dplyr::select(-created_at) %>%
    # Flag Fahrenheit observations and fix in proc
    dplyr::mutate(flag_substrate_temperature = ifelse(substrate_temperature > 40, TRUE, FALSE),
                  proc_substrate_temperature = ifelse(substrate_temperature > 40,
                                                      FtoC(substrate_temperature),
                                                      substrate_temperature)) %>%
    # Rename sub_temp with raw prefix
    dplyr::rename(raw_substrate_temperature = substrate_temperature) %>%
    # Fix ambient temp F to C
    dplyr::mutate(flag_ambient_temperature = ifelse(ambient_temperature_c > 40, TRUE, FALSE),
                  proc_ambient_temperature = ifelse(ambient_temperature_c > 40,
                                                    FtoC(ambient_temperature_c),
                                                    ambient_temperature_c)) %>%
    # Rename ambient_temp with raw prefix
    dplyr::rename(raw_ambient_temperature = ambient_temperature_c) %>%
    # force ambient temp to numeric
    dplyr::mutate(raw_ambient_temperature = as.numeric(raw_ambient_temperature)) %>%
    # add flags for runs of temperature data
    dplyr::arrange(collection_datetime_UTC) %>%
    dplyr::mutate(flag_ambient_temperature_run = (ambient_humidity == dplyr::lag(ambient_humidity)) &
                    (raw_ambient_temperature == dplyr::lag(raw_ambient_temperature))
                  & (gridsect == "no")) %>%
    # flag duplicated C-labels
    dplyr::group_by(c_label) %>%
    dplyr::mutate(flag_duplicated_c_label_field_sampling = ifelse(dplyr::n() > 1, TRUE, FALSE)) %>%
    dplyr::ungroup()

    # add to processed list
    proc_data["nematode_field_sampling_proc"] <- list(nematode_field_sampling_proc)
  }

  # Process nematode_field_sampling_sample_photo
  if("nematode_field_sampling_sample_photo" %in% data_names) {
    message("Processing nematode_field_sampling_sample_photo")
    nematode_field_sampling_sample_photo_proc <- data$nematode_field_sampling_sample_photo %>%
      dplyr::select(fulcrum_id, exif_gps_latitude, exif_gps_longitude, exif_gps_altitude)

    # add to list
    proc_data["nematode_field_sampling_sample_photo_proc"] <- list(nematode_field_sampling_sample_photo_proc)
  }

  # Process nematode_isolation
  if("nematode_isolation" %in% data_names) {
    message("Processing nematode_isolation")
    nematode_isolation_proc <- data$nematode_isolation %>%
      dplyr::select(c_label_id = c_label,
                    isolation_id = fulcrum_id,
                    isolation_datetime_UTC = system_created_at,
                    isolation_by = created_by,
                    worms_on_sample,
                    approximate_number_of_worms,
                    isolation_date_UTC = date,
                    isolation_local_time = time, # Is this actually local time? or is it UTC?
                    isolation_latitude = latitude,
                    isolation_longitude = longitude)

    # add to list
    proc_data["nematode_isolation_proc"] <- list(nematode_isolation_proc)
  }

  if("nematode_isolation_s_labeled_plates" %in% data_names) {
    message("Processing nematode_isolation_s_labeled_plates")
    nematode_isolation_s_labeled_plates_proc <- data$nematode_isolation_s_labeled_plates %>%
      dplyr::select(fulcrum_parent_id, s_label) %>%
      # flag duplicated S-labels
      dplyr::group_by(s_label) %>%
      dplyr::mutate(flag_duplicated_s_label_isolation_s_labeled_plates = ifelse(n() > 1, TRUE, FALSE)) %>%
      dplyr::ungroup() %>%
      # flag missing S-labels
      dplyr::mutate(flag_missing_s_label_isolation_s_labeled_plates = ifelse(is.na(s_label), TRUE, FALSE))

    # add to list
    proc_data["nematode_isolation_s_labeled_plates_proc"] <- list(nematode_isolation_s_labeled_plates_proc)
  }

  if("nematode_isolation_s_labeled_plates" %in% data_names) {
    message("Processing nematode_isolation_photos")
    # read in isolation photos dataframe
    nematode_isolation_photos_proc <- data$nematode_isolation_photos

    # add to list
    proc_data["nematode_isolation_photos_proc"] <- list(nematode_isolation_photos_proc)
  }

  # return list
  return(proc_data)
}

