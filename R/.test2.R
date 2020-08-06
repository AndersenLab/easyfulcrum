# setup test directory for multi project Fulcrum export
dir1 <- "test_data/2020FebruaryAustralia/data/raw/fulcrum"
dir2 <- "test_data/2018OctoberHawaii/data/fulcrum"
dir3 <- "test_data/2019DecemberHawaii/data/fulcrum"
dir4 <- "test_data/2019OctoberHawaii/data/fulcrum"

### TEST 1 ###

# test readFulcrum function
raw_data1 <- readFulcrum(dir1)

# test procFulcrum function
proc_data1 <- procFulcrum(raw_data1)

# test checkParameters function, will output rows with flags/rows related to those w flags
checkParameters(proc_data1)

# test checkParameters function, saves the output as a list of three dataframes of flagged rows
flag1.1 <- checkParameters(proc_data1, return = TRUE)

# easier to see which rows we might want to edit
View(flag1.1)

# upon further inspection we select two rows for ambient_temperature_run to be set to NA
to_change <- c("c5b0cabf-ded8-4c3d-9915-b3c94b913686", "fb975ace-0036-4a80-b073-8ff638f35786")
proc_data1_clean <- fixParameters(proc_data1, ambient_temperature_run_ids = to_change)

# Recheck to see if fixParameters worked
checkParameters(proc_data1_clean)

# test checkProc function, will output info with flags/rows related to those flags
checkProc(proc_data1)
# test checkParameters function, saves the output as a list of six dataframes of flagged rows
flag1.2 <- checkProc(proc_data1, return = TRUE)

# test joinFulcrum function
join_data1 <- joinFulcrum(proc_data1)

# test checkProc function, will output info of flags/rows related to those flags
checkJoin(join_data1)
# test checkProc function, saves the output as a dataframe of flagged rows
flag1.3 <- checkJoin(join_data1, return = TRUE)

# test annotateFulcrum fuction
anno_data1 <- annotateFulcrum(join_data1)

# test readGenotypes function
geno_data1 <- readGenotypes(gsKey = c("1CxKJHM6mEu4VvnN2T1ioXiJNZmmmpeosmECP2zeAPmY"))

# test procGenotypes function
proc_geno_data1 <- procGenotypes(geno_data = geno_data1, fulc_data = anno_data1)

#test checkGenotypes function
checkGenotypes(proc_geno_data1)
flag1.4 <- checkGenotypes(proc_geno_data1, return = TRUE)

# code to fix Genotype data
# library(tidyverse)
# proc_geno_data2 <- proc_geno_data1 %>%
#         dplyr::mutate(species_id = ifelse(species_id == "C. elegans", "Caenorhabditis elegans",
#                                           ifelse(species_id == "C. briggsae", "Caenorhabditis briggsae",
#                                                  ifelse(species_id == "C. tropicalis", "Caenorhabditis tropicalis", species_id))))

# test joinGenoFulc function to join genotype data to fulcrum data
joingeno_data1 <- joinGenoFulc(geno = proc_geno_data1, fulc = anno_data1)

# test the procPhotos function, output is final dataframe
final_data1 <- procPhotos(dir = "test_data/2020FebruaryAustralia/data/raw/fulcrum/photos", data = joingeno_data1, percentage = 20, overwrite = T)

test <- make
### TEST 2 ###

# test readFulcrum function
raw_data4 <- readFulcrum(dir4)

# test procFulcrum function
proc_data4 <- procFulcrum(raw_data4)

# test checkParameters function, will output rows with flags/rows related to those w flags
checkParameters(proc_data4)
# test checkParameters function, saves the output as a list of three dataframes
test4 <- checkParameters(proc_data4, return = TRUE)
# upon further inspection we select all rows for ambient_temperature_run to be set to NA
to_change <- test4$fulcrum_id
proc_data4_clean <- fixParameters(proc_data4, substrate_temperature_ids = to_change)

# test checkProc function, will output info of flags/rows related to those flags
checkProc(proc_data4)
# test checkParameters function, saves the output as a list of six dataframes of flagged rows
flag4.2 <- checkProc(proc_data4, return = TRUE)

# test joinFulcrum function
join_data4 <- joinFulcrum(proc_data4)

# test checkProc function, will output info of flags/rows related to those flags
checkJoin(join_data4)
# test checkProc function, saves the output as a dataframe of flagged rows
flag4.3 <- checkJoin(join_data4, return = TRUE)

# test annotateFulcrum fuction
anno_data4 <- annotateFulcrum(join_data4)

# test readGenotypes function
geno_data4 <- readGenotypes(gsKey = c("1CxKJHM6mEu4VvnN2T1ioXiJNZmmmpeosmECP2zeAPmY"))

# test joinGenoFulc function to join genotype data to fulcrum data
joingeno_data4 <- joinGenoFulc(geno = geno_data4, fulc = anno_data4)

# test the procPhotos function, output is final dataframe
#photodir <- "test_data/2020FebruaryAustralia/data/fulcrum/photos"
#final_data1 <- procPhotos(photodir, joingeno_data4)
