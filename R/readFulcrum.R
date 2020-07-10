#' readFulcrum
#'
#' \code{readFulcrum} reads .csv files exported from Fulcrum into R
#'
#' @param dir The path of the directory with five Fulcrum .csv files:
#' nematode_field_sampling.csv,
#' nematode_field_sampling_sample_photo.csv,
#' nematode_isolation.csv,
#' nematode_isolation_s_labeled_plates.csv,
#' nematode_isolation_photos.csv
#' @return A list of five named data frames generated from the .csv files.
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
  files <- list(glue::glue("{dir}/nematode_field_sampling.csv"),
                glue::glue("{dir}/nematode_field_sampling_sample_photo.csv"),
                glue::glue("{dir}/nematode_isolation.csv"),
                glue::glue("{dir}/nematode_isolation_s_labeled_plates.csv"),
                glue::glue("{dir}/nematode_isolation_photos.csv"))

  #read files
  fulc_data <- lapply(files, readr::read_csv)

  # set names
  names(fulc_data) <- c("nematode_field_sampling",
                        "nematode_field_sampling_sample_photo",
                        "nematode_isolation",
                        "nematode_isolation_s_labeled_plates",
                        "nematode_isolation_photos")

  # return list
  return(fulc_data)
}
