#' readFulcrum
#'
#' \code{readFulcrum} reads .csv files exported from Fulcrum into R
#'
#' @param dir The path to the base fulcrum directory, dir/data/raw/fulcrum should contain:
#' nematode_field_sampling.csv,
#' nematode_field_sampling_sample_photo.csv,
#' nematode_isolation.csv,
#' nematode_isolation_s_labeled_plates.csv,
#' nematode_isolation_photos.csv
#'
#' All five files are not required but it is recommended to use all exported files when possible.
#' @return A list of dataframes named from the .csv files.
#' \tabular{ll}{
#' nematode_field_sampling \tab nematode_field_sampling.csv\cr
#' nematode_field_sampling_sample_photo \tab nematode_field_sampling_sample_photo.csv\cr
#' nematode_isolation \tab nematode_isolation.csv\cr
#' nematode_isolation_s_labeled_plates \tab nematode_isolation_s_labeled_plates.csv\cr
#' nematode_isolation_photos \tab nematode_isolation_photos.csv\cr
#' }
#' @export

readFulcrum <- function(dir) {
  # make file list
  files_full <- list.files(glue::glue("{dir}","/data/raw/fulcrum"), pattern = ".csv", full.names = T)

  # get names
  file_names <- list.files(glue::glue("{dir}","/data/raw/fulcrum"), pattern = ".csv") %>%
    stringr::str_replace_all(pattern = ".csv", replacement = "")

  #read files
  fulc_data <- lapply(files_full, readr::read_csv)

  # set names
  names(fulc_data) <- file_names

  # return list
  return(fulc_data)
}
