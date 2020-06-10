library(tidyverse)

# set working directory
setwd(glue::glue("{dirname(rstudioapi::getActiveDocumentContext()$path)}/.."))

# setup test directory for multi project Fulcrum export
test_dir1 <- "test_data/20200515_fulcrum_data"

# test functions from multi project Fulcrum export
loadFulcrum(test_dir1)

fulcrum_data1 <- procFulcrum(test_dir1)

# setup test directories for single project Fulcrum exports
test_dir2 <- "test_data/20191201_Hawaii/data/fulcrum"
test_dir3 <- "test_data/2020FebruaryAustralia/data/fulcrum"

loadFulcrum(test_dir2) # dec hawaii
loadFulcrum(test_dir3) # feb australia

# test procFulcrum function to load and process Fulcrum data
hi2019dec <- procFulcrum(test_dir2)

aus2020feb <- procFulcrum(test_dir3)

# read in genotype data for 2 single projects with loadGenotypes function
genotype_data23 <- loadGenotypes(gsKey = c("1jLmludjZX8_jy1RQ_DPMQ3k-3eZ30GtCAW6LEj_YSVw", "14xvEXbKVej7gnPPaHtCcEHDc6xLbRPfN9ESLXOSLw58")) %>%
  dplyr::mutate(project_geno = ifelse(project_geno == "test1", "2019DecemberHawaii",
                                    ifelse(project_geno == "test2", "2020FebruarayAustralia", NA)))
genotype_data3 <- loadGenotypes(gsKey = c("14xvEXbKVej7gnPPaHtCcEHDc6xLbRPfN9ESLXOSLw58")) %>%
  dplyr::mutate(project_geno = ifelse(project_geno == "test2", "2020FebruarayAustralia", NA))

# test joinFulcGeno function to join genotype data to fulcrum data
project_data3 <- joinFulcGeno(fulc = aus2020feb, geno = genotype_data3)

# test readGenotypes function
genotype_data3_2 <- readGenotypes(gsKey = c("1CxKJHM6mEu4VvnN2T1ioXiJNZmmmpeosmECP2zeAPmY"))

# test joinFulcGeno function to join genotype data to fulcrum data
project_data3_2 <- joinFulcGeno(fulc = aus2020feb, geno = genotype_data3_2)
