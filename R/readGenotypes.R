#' readGenotypes
#'
#' \code{readGenotypes} reads genotyping data from project specific genotyping Google sheets
#'
#' @param gsKey a vector containing the google sheet keys for the genotype data to load.
#' Your Google Sheet keys are found in your Google sheets URL. Select the string
#' found between the slashes after spreadsheets/d in your Google Sheet URL.
#'
#' @return A dataframe generated from the Google sheets \code{gsKey} argument. If multiple Google sheets
#' are provided the data are appended using rbind. Note, the genotyping sheet must contain variable names
#' specified by the \code{wild_isolate_genotyping_template} and the data must be found on the sheet named
#' \code{genotyping template}.
#' @export
#'

readGenotypes <- function(gsKey) {
  # read genotyping sheet(s)
  genotyping_sheet <- NULL

  for(i in unique(gsKey)){
    # get data from sheet
    geno <- googlesheets4::read_sheet(i, range = "genotyping template") %>%
      dplyr::filter(!is.na(s_label))

    genotyping_sheet <- rbind(genotyping_sheet, geno)
  }

  # find s_labels in genotyping sheet
  slabs <- str_subset(genotyping_sheet$s_label, pattern = "S-")

  # filter genotyping sheet by s_labels matching "S-" pattern
  unusual_slabs <- genotyping_sheet %>%
    dplyr::filter(!(s_label %in% slabs)) %>%
    dplyr::pull(s_label)

  duplicated_slabs <- genotyping_sheet %>%
    dplyr::group_by(s_label) %>%
    dplyr::mutate(slab_n = n(),
                  slab_duplicate = ifelse(slab_n > 1, 1, 0)) %>%
    dplyr::ungroup() %>%
    dplyr::distinct(s_label, .keep_all = T) %>%
    dplyr::filter(slab_duplicate == 1) %>%
    dplyr::pull(s_label)

  # print warning if duplicates or unusual names found for S-labels
  print(paste0("There are ", length(unusual_slabs), " unsual S-label names in genotyping sheet(s) ", unusual_slabs))

  # print warning if duplicates or unusual names found for strain names
  print(paste0("There are ", length(duplicated_slabs), " duplicated S-label names in genotyping sheet(s) ", duplicated_slabs))

  return(genotyping_sheet)
}
