## ---- include = F-------------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
# install.packages("devtools")
# devtools::install_github("AndersenLab/easyfulcrum")
# setwd("~/Desktop")

library(easyfulcrum)

## ----warning = F, message = F-------------------------------------------------
# makeDirStructure(startdir = "~/Desktop/",
#                  projectdirname = "2020JanuaryHawaii")

## -----------------------------------------------------------------------------
dir1 <- "~/Desktop/2020JanuaryHawaii"
# raw_fulc1 <- readFulcrum(dir = dir1)
raw_fulc1 <- easyfulcrum::raw_fulc1
names(raw_fulc1)

## -----------------------------------------------------------------------------
proc_fulc1 <- procFulcrum(data = raw_fulc1)

## -----------------------------------------------------------------------------
flag1.1 <- checkTemperatures(data = proc_fulc1, return_flags = TRUE)

## -----------------------------------------------------------------------------
proc_fulc1_clean <- fixTemperatures(data = proc_fulc1,
                                  substrate_temperature_ids = "a7db618d-44cc-4b4a-bc67-871306029274",
                                  ambient_temperature_ids = "b1f20ae4-c5c2-426f-894a-e1f46c2fa693",
                                  ambient_temperature_run_ids=c("dda77efe-d73c-48e9-aefb-b508e613256b",
                                                                 "93de14a0-40ab-4793-8614-ab1512ab158c"))

## -----------------------------------------------------------------------------
join_fulc1 <- joinFulcrum(data = proc_fulc1)

## -----------------------------------------------------------------------------
flag1.2 <- checkJoin(data = join_fulc1, return_flags = TRUE)

## ----message = F--------------------------------------------------------------
anno_fulc1 <- annotateFulcrum(data = join_fulc1, dir = NULL)

## -----------------------------------------------------------------------------
# raw_geno1 <- readGenotypes(gsKey = c("1_6u4sk_Zj-Hm5d_058Lg8WYWLe7BZHGTWxXcH6EsDUI"))
raw_geno1 <- easyfulcrum::raw_geno1
head(raw_geno1)

## -----------------------------------------------------------------------------
proc_geno1 <- checkGenotypes(geno_data = easyfulcrum::raw_geno1, fulc_data = anno_fulc1, 
                                  return_geno = TRUE, return_flags = FALSE)

## -----------------------------------------------------------------------------
flag1.3 <- checkGenotypes(geno_data = raw_geno1, fulc_data = anno_fulc1, 
                          return_geno = FALSE, return_flags = TRUE)

## -----------------------------------------------------------------------------
join_genofulc1 <- joinGenoFulc(geno = proc_geno1, fulc = anno_fulc1, dir = NULL)

## ----message = F, include = F-------------------------------------------------
#This chunk of code will move the photos required for this trial run into the appropriate folder for raw photos, according to what `dir1` is specified. 
#You can also find these photos for manual download at: https://github.com/AndersenLab/easyfulcrum/tree/master/vignettes/2020JanuaryHawaii_photos

# library(googledrive)
# final_directory <- paste(dir1, "data/raw/fulcrum/photos", sep = "/")
# temp_dir <- tempdir()
# photos <- googledrive::drive_ls(as_id("11T6qzszJ_yK3yRyy4eqBfplZuai20y-g"))
# photos$temp_location <- paste(temp_dir,photos$name, sep = "/")
# photos$final_location <- paste(final_directory,photos$name, sep = "/")
# 
# for(i in 1:nrow(photos)){
#   googledrive::drive_download(as_id(photos$id[i]), 
#                             path =photos$temp_location[i], 
#                             overwrite = TRUE)
#   fs::file_copy(photos$temp_location[i], photos$final_location[i], overwrite = TRUE)
# }

## ----message = FALSE----------------------------------------------------------
# final_data1 <- procPhotos(dir = dir1, data = join_genofulc1,
#                           max_dim = 500, overwrite = T, 
#                           CeNDR = TRUE)
final_data1 <- easyfulcrum::final_data1
head(final_data1)

## -----------------------------------------------------------------------------
# flag1.4 <- makeSpSheet(data = final_data1,
#                     target_sp = "Caenorhabditis briggsae", dir = dir1)

## ----message = F--------------------------------------------------------------
# file.copy("../R/sampleReport.Rmd", paste(dir1,"scripts",sep = "/"))
# 
# rmarkdown::render(paste(dir1,"scripts","sampleReport.Rmd",sep = "/"),
#                   output_dir = paste(dir1,"reports",sep = "/"),
#                   output_format	= "html_document")

