# set working directory
setwd(glue::glue("{dirname(rstudioapi::getActiveDocumentContext()$path)}/.."))

# setup test directory for multi project Fulcrum export
dir1 <- "test_data/2020FebruaryAustralia/data/fulcrum"

# test readFulcrum function
raw_data1 <- readFulcrum(dir1)

# test procFulcrum function
proc_data1 <- procFulcrum(raw_data1)

# test parameter_check function
parameter_check(proc_data1)

# test parameter_check function with optional parameters set to TRUE
test1 <- parameter_check(proc_data1, save = TRUE, return = TRUE)

#test parameter_fix function (there are 5 flagged ambient_temperature run values and their corresponding fulcrum ids are: test1[["ambient_temperature_run"]]$fulcrum_id)
proc_data2 <- parameter_fix(proc_data1, ambient_temperature_run_ids = test1[["ambient_temperature_run"]]$fulcrum_id)

# test joinFulcrum function
join_data1 <- joinFulcrum(proc_data1)

# test readGenotypes function
geno_data1 <- readGenotypes(gsKey = c("1CxKJHM6mEu4VvnN2T1ioXiJNZmmmpeosmECP2zeAPmY"))

# test joinGenoFulc function to join genotype data to fulcrum data
joingeno_data1 <- joinGenoFulc(geno = geno_data1, fulc = anno_data1,)

# test the procPhotos function. Output is final dataframe
photodir <- "test_data/2020FebruaryAustralia/data/fulcrum/photos"
final_data1 <- procPhotos(photodir, joingeno_data1)
