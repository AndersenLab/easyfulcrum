#' generateReport
#'
#' \code{generateReport} produces a report html for project
#'
#' @param data a checked dataframe generated from the \code{procPhotos} function.
#' @param dir The path to the base fulcrum directory, report html will be saved as reports/spSheet.csv
#' @return a report as an html file
#' @import rmarkdown
#' @export
#'

generateReport <- function(data, dir) {

  # prepare input data for use in sampleReport.Rmd
  df <- data
  # copy sampleReport.Rmd to scripts
  file.copy(from = system.file("rmd", "sampleReport.Rmd", package = "easyfulcrum"),
            to = paste(dir,"scripts",sep = "/"),
            overwrite = TRUE)

  # render sampleReport.Rmd
  rmarkdown::render(paste(dir,"scripts","sampleReport.Rmd",sep = "/"),
                    output_dir = paste(dir,"reports",sep = "/"),
                    output_format	= "html_document")
}
