#' filter_box
#'
#' \code{filter_box} finds lat and long inside bounding box.
#'
#' @param longitude longitude of collection.
#' @param latitude latitude of collection.
#' @param coords coordinates of bounding box.
#'

filter_box <- function(longitude, latitude, coords) {
  between(longitude, coords[1], coords[3]) &
    between(latitude, coords[2], coords[4]) &
    !is.na(longitude)
}

#' FtoC
#'
#' \code{FtoC} Converts fahrenheit measurement to celsius.
#'
#' @param F fahrenheit measurement to convert.
#'

FtoC <- function(F) {
  round((F - 32) * (5 / 9), digits = 1)
}

#' makeDirStructure
#'
#' \code{makeDirStructure} Makes the nested folder directory for use in easyfulcrum.
#'
#' @param startdir the working folder where project files will lie.
#' @param projectdirname the name of the project, folder will be constructed under \code{startdir}.
#' @export
#'

makeDirStructure <- function(startdir, projectdirname) {
  # creates empty directory folders as required
  dir.create(file.path(startdir, projectdirname, "data", "raw", "fulcrum", "photos"), recursive = TRUE)
  dir.create(file.path(startdir, projectdirname, "data", "raw", "sanger"), recursive = TRUE)
  dir.create(file.path(startdir, projectdirname, "data", "raw", "annotate"), recursive = TRUE)
  dir.create(file.path(startdir, projectdirname, "data", "processed", "fulcrum"), recursive = TRUE)
  dir.create(file.path(startdir, projectdirname, "data", "processed", "genotypes"), recursive = TRUE)
  dir.create(file.path(startdir, projectdirname, "data", "processed", "sanger"), recursive = TRUE)
  dir.create(file.path(startdir, projectdirname, "reports"), recursive = TRUE)
  dir.create(file.path(startdir, projectdirname, "scripts"), recursive = TRUE)
}
