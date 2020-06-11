library(tidyverse)

# set working directory
setwd(glue::glue("{dirname(rstudioapi::getActiveDocumentContext()$path)}/.."))

# setup test directory for multi project Fulcrum export
test_dir1 <- "test_data/20200515_fulcrum_data"

# test functions from multi project Fulcrum export
loadFulcrum(test_dir1)
test_fulc_1 <- readFulcrum(test_dir1)


fulcrum_data1 <- procFulcrum(test_dir1)

# setup test directories for single project Fulcrum exports
test_dir2 <- "test_data/20191201_Hawaii/data/fulcrum"
test_dir3 <- "test_data/2020FebruaryAustralia/data/fulcrum"

loadFulcrum(test_dir2) # dec hawaii
loadFulcrum(test_dir3) # feb australia

# test readGenotypes function
genotype_data3_2 <- readGenotypes(gsKey = c("1CxKJHM6mEu4VvnN2T1ioXiJNZmmmpeosmECP2zeAPmY"))

# test joinFulcGeno function to join genotype data to fulcrum data
project_data3_2 <- joinFulcGeno(fulc = aus2020feb, geno = genotype_data3_2)
