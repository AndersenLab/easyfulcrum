#' Bounding box for Islands
#'
#' A dataset containing Island names, latitudes and longitudes for bounding box
#'
#' @format A data frame with 5 rows and 5 variables:
#' \describe{
#'   \item{island_name}{Name of island}
#'   \item{long_start}{longitude of bounding box start}
#'   \item{lat_start}{latitude of bounding box start}
#'   \item{long_end}{longitude of bounding box end}
#'   \item{lat_end}{latitude of bounding box end}
#' }
#' @source \url{https://boundingbox.klokantech.com/}
#' @name island

#' Bounding box for locations
#'
#' A dataset containing location names, latitudes and longitudes for bounding box
#'
#' @format A data frame with 7 rows and 5 variables:
#' \describe{
#'   \item{location_name}{Name of location}
#'   \item{long_start}{longitude of bounding box start}
#'   \item{lat_start}{latitude of bounding box start}
#'   \item{long_end}{longitude of bounding box end}
#'   \item{lat_end}{latitude of bounding box end}
#' }
#' @source \url{https://boundingbox.klokantech.com/}
#' @name location

#' trail polygon vertices as a string of coordinates
#'
#' A dataset containing trail names amd a string of ploygon vertices. The vertices
#' are given as longitude / latitude.
#'
#' @format A data frame with 18 rows and 2 variables:
#' \describe{
#'   \item{trail_name}{Name of trail}
#'   \item{coordinates}{string of ploygon vertices. Each vertex contains long,lat}
#' }
#' @source \url{https://boundingbox.klokantech.com/}
#' @name trails

#' Expected classes in joined fulcrum object
#'
#' A dataset containing expected classes for each variable
#'
#' @format A data frame with 65 rows and 2 variables:
#' \describe{
#'   \item{classExpected}{class name}
#'   \item{variable}{variable name}
#' }
#' @name fulcrumTypes

#' Expected classes in genotype object
#'
#' A dataset containing expected classes for each variable
#'
#' @format A data frame with 38 rows and 2 variables:
#' \describe{
#'   \item{classExpected}{class name}
#'   \item{variable}{variable name}
#' }
#' @name genotypeTypes

