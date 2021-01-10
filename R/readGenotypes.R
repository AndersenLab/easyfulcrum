#' readGenotypes
#'
#' \code{readGenotypes} reads genotyping data from project specific genotyping Google sheets.
#'
#' @param gsKey a vector containing the google sheet keys for the genotype data to load.
#' Your Google Sheet keys are found in your Google sheets URL. Select the string
#' found between the slashes after spreadsheets/d in your Google Sheet URL.
#' @param col_types a single string of readr-style short codes. To let the column types be guessed
#' specify \code{col_types} to \code{"?"}. See details here:
#' https://googlesheets4.tidyverse.org/reference/read_sheet.html
#'
#' @return A dataframe generated from the Google sheets \code{gsKey} argument. If multiple Google sheets
#' are provided the data are appended using rbind. Note, the genotyping sheet must contain variable names
#' specified by the \code{wild_isolate_genotyping_template} and the data must be found on the sheet named
#' \code{genotyping template}.
#' @export
#'

readGenotypes <- function(gsKey, col_types = "cDDdcdcddddddDcDDdcdcdddddddcdcccddccc") {
  # read genotyping sheet(s)
  genotyping_sheet <- NULL

  for(i in unique(gsKey)){
    # get data from sheet
    geno <- googlesheets4::read_sheet(i, range = "genotyping template", col_types = col_types) %>%
      dplyr::filter(!is.na(s_label))

    genotyping_sheet <- rbind(genotyping_sheet, geno)
  }
  # return raw genotyping sheet
  return(genotyping_sheet)
}
