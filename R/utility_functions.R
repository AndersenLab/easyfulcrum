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
  dir.create(file.path(startdir, projectdirname, "data", "raw", "annotate"), recursive = TRUE)
  dir.create(file.path(startdir, projectdirname, "data", "processed", "fulcrum"), recursive = TRUE)
  dir.create(file.path(startdir, projectdirname, "data", "processed", "genotypes"), recursive = TRUE)
  dir.create(file.path(startdir, projectdirname, "reports"), recursive = TRUE)
  dir.create(file.path(startdir, projectdirname, "scripts"), recursive = TRUE)
}

#' loadExampleFiles
#'
#' \code{loadExampleFiles} Copies raw .csv and .jpg files from package into example directory.
#'
#' @param startdir the working folder where project files will lie.
#' @param projectdirname the name of the project, folder will be constructed under \code{startdir}.
#' @import stringr
#' @import fs
#' @export
#'

loadExampleFiles <- function(startdir, projectdirname) {
  # Find extdata folder path
  files_path <- stringr::str_replace(system.file("extdata", "nematode_isolation.csv", package = "easyfulcrum", mustWork = TRUE),
                                     pattern = "nematode_isolation.csv", replacement = "")

  # Make vecotr of .csv and .jpg file paths
  csv <- stringr::str_subset(list.files(files_path), pattern = ".csv")
  csv_paths <- paste0(files_path, csv)
  jpg <- stringr::str_subset(list.files(files_path), pattern = ".jpg")
  jpg_paths <- paste0(files_path, jpg)

  # make path to example project directory
  csv_example_dir <- paste0(startdir,"/", projectdirname, "/data/raw/fulcrum/")
  jpg_example_dir <- paste0(startdir,"/", projectdirname, "/data/raw/fulcrum/photos/")
  csv_new_paths <- paste0(csv_example_dir, csv)
  jpg_new_paths <- paste0(jpg_example_dir, jpg)

  # make df to pass to file_copy
  c_copy <- tibble::tibble(csv_paths, csv_new_paths)
  j_copy <- tibble::tibble(jpg_paths, jpg_new_paths)

  # copy files in package to example project folder
  fs::file_copy(c_copy$csv_paths, c_copy$csv_new_paths, overwrite = F)
  fs::file_copy(j_copy$jpg_paths, j_copy$jpg_new_paths, overwrite = F)
}
