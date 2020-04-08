#' load_fulc
#'
#' \code{load_fulc} loads .csv files exported from Fulcrum into R
#'
#' @param dir The path of the directory with five Fulcrum .csv files:
#' nematode_field_sampling.csv,
#' nematode_field_sampling_sample_photo.csv,
#' nematode_isolation.csv,
#' nematode_isolation_s_labeled_plates.csv,
#' nematode_isolation_photos.csv
#' @return a list of five data frames generated from the .csv files.
#' Each data frame has a shortened name.
#' \tabular{ll}{
#' collection \tab nematode_field_sampling.csv\cr
#' collection_photo \tab nematode_field_sampling_sample_photo.csv\cr
#' isolation \tab nematode_isolation.csv\cr
#' isolation_slab \tab nematode_isolation_s_labeled_plates.csv\cr
#' isolation_photo \tab nematode_isolation_photos.csv\cr
#' }
#' @export

load_fulc <- function(dir) {

  # Make list to hold data
  fulc_dat <- list()

  # Read collection data
  fulc_dat$collection <- readr::read_csv(glue::glue("{dir}/nematode_field_sampling.csv"))
  fulc_dat$collection_photo <- readr::read_csv(glue::glue("{dir}/nematode_field_sampling_sample_photo.csv"))

  # Read isolation data
  fulc_dat$isolation <- readr::read_csv(glue::glue("{dir}/nematode_isolation.csv"))
  fulc_dat$isolation_slab <- readr::read_csv(glue::glue("{dir}/nematode_isolation_s_labeled_plates.csv"))
  fulc_dat$isolation_photo <- readr::read_csv(glue::glue("{dir}/nematode_isolation_photos.csv"))

  # Return list with data
  return(fulc_dat)

}
