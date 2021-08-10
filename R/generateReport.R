#' generateReport
#'
#' \code{generateReport} produces a report html for project
#'
#' @param data a checked dataframe generated from the \code{procPhotos}
#'   function.
#' @param dir The path to the base fulcrum directory, report html will be saved
#'   as reports/sampleReport_general.html or reports/sampleReport.html
#' @param target_sp vector of target species for species id checks. Default
#'   target species names are: Caenorhabditis elegans,Caenorhabditis briggsae,
#'   Caenorhabditis tropicalis. \code{generateReport} currently supports up to
#'   three target species for use with the \code{"general"} profile.
#' @param profile set to \code{"nematode"} by default. This will use nematode
#'   specific reporting standard to the Andersen Lab. Set to \code{"general"} to
#'   use the non-nematode specific reporting.
#' @return a report as an html file
#' @import rmarkdown
#' @export
#'

generateReport <- function(data, dir,
                           target_sp = c("Caenorhabditis elegans", "Caenorhabditis briggsae", "Caenorhabditis tropicalis"),
                           profile = "nematode") {
  # prepare input data for use in sampleReport.Rmd
  df <- data
  target_sp <- target_sp

  if(profile == "general"){
    message('Using "general" profile, sampleReport_general.Rmd will be used.')


    # copy sampleReport_general.Rmd to scripts
    file.copy(from = system.file("rmd", "sampleReport_general.Rmd", package = "easyfulcrum"),
              to = paste(dir,"scripts",sep = "/"),
              overwrite = TRUE)

    # render sampleReport_general.Rmd
    rmarkdown::render(paste(dir,"scripts","sampleReport_general.Rmd",sep = "/"),
                      output_dir = paste(dir,"reports",sep = "/"),
                      output_format	= "html_document")
  }
  if(profile == "nematode"){
    message('Using "nematode" profile, nematode specific sampleReport.Rmd will be used.')

    # copy sampleReport.Rmd to scripts
    file.copy(from = system.file("rmd", "sampleReport.Rmd", package = "easyfulcrum"),
              to = paste(dir,"scripts",sep = "/"),
              overwrite = TRUE)

    # render sampleReport.Rmd
    rmarkdown::render(paste(dir,"scripts","sampleReport.Rmd",sep = "/"),
                      output_dir = paste(dir,"reports",sep = "/"),
                      output_format	= "html_document")
  }
}
