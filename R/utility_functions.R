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


