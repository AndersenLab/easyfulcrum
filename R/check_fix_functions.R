#' initial_data_check
#'
#' \code{initial_data_check} checks for four flags of duplicated and missing parameters in joined data
#'
#' @param data dataframe output of joinFulcrum function
#' @param save set to TRUE if flagged rows are to be saved
#' @param return set to TRUE if flagged rows are to be returned in list format
#' @return a list of flagged rows in dataframe for each of four flags of interest
#' @export
#'

initial_data_check <- function(data, save = FALSE, return = FALSE) {
  message(">>> Checking duplicated c labels")
  duplicated_c_label <- data %>% filter(flag_duplicated_c_label_field_sampling == TRUE)
  print(paste("There are", nrow(duplicated_c_label), "rows with duplicated c labels:", sep = " "))
  print(duplicated_c_label$c_label)
  
  message(">>> Checking duplicated s labels")
  duplicated_s_label <- data %>% filter(flag_duplicated_s_label_isolation_s_labeled_plates == TRUE)
  print(paste("There are", nrow(duplicated_s_label), "rows with duplicated s labels", sep = " "))
  print(duplicated_s_label$s_label)
  
  message(">>> Checking missing s labels")
  missing_s_label <- data %>% filter(flag_missing_s_label_isolation_s_labeled_plates == TRUE)
  print(paste("There are", nrow(missing_s_label), "rows with missing s labels", sep = " "))
  print(missing_s_label$s_label)
  
  message(">>> Checking missing isolation records")
  missing_isolation_record <- data %>% filter(flag_missing_isolation_record == TRUE)
  print(paste("There are", nrow(missing_isolation_record), "rows with missing isolation records", sep = " "))
  print(missing_isolation_record$isolation_by)
  
  if(save){
    dir.create("initial_data_check", showWarnings = FALSE)
    write.csv(duplicated_c_label, "initial_data_check/duplicated_c_label.csv")
    write.csv(duplicated_s_label, "initial_data_check/duplicated_s_label.csv")
    write.csv(missing_s_label, "initial_data_check/missing_s_label.csv")
    write.csv(missing_isolation_record, "initial_data_check/missing_isolation_record.csv")
    message(">>> Examine initial_data_check folder for full records, consider fixing flagged issues in primary data input and rerunning.")
  }
  else{
    message(">>> Flagged values unsaved, (default is FALSE). Consider fixing flagged issues in primary data input and rerunning.")
  }
  if(return){
    return(list("duplicated_c_label" = duplicated_c_label, "duplicated_s_label" = duplicated_s_label, 
                "missing_s_label" = missing_s_label, "missing_isolation_record" = missing_isolation_record))
  }
}