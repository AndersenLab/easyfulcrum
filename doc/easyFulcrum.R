## ---- include = F-------------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----eval = F-----------------------------------------------------------------
#  install.packages("devtools")
#  devtools::install_github("AndersenLab/easyfulcrum")

## -----------------------------------------------------------------------------
library(easyfulcrum)

## ----warning = F, message = F-------------------------------------------------
makeDirStructure(startdir = "~/Desktop/",
                 projectdirname = "2020JanuaryHawaii")

## ----message = F--------------------------------------------------------------
dir1 <- "~/Desktop/2020JanuaryHawaii"
raw_fulc1 <- readFulcrum(dir = dir1)
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
join_fulc1 <- joinFulcrum(data = proc_fulc1, select_vars = TRUE)

## -----------------------------------------------------------------------------
flag1.2 <- checkJoin(data = join_fulc1, return_flags = TRUE)

## ----message = F--------------------------------------------------------------
anno_fulc1 <- annotateFulcrum(data = join_fulc1, dir = NULL, select_vars = TRUE)

## -----------------------------------------------------------------------------
raw_geno1 <- readGenotypes(gsKey = c("1iSGkGbhoyg-uq_l83UbiNdI_g4xqEkTxf3Ap0J7JGto"))
head(raw_geno1)

## -----------------------------------------------------------------------------
proc_geno1 <- checkGenotypes(geno_data = raw_geno1, fulc_data = anno_fulc1, 
                                  return_geno = TRUE, return_flags = FALSE)

## -----------------------------------------------------------------------------
flag1.3 <- checkGenotypes(geno_data = raw_geno1, fulc_data = anno_fulc1, 
                          return_geno = FALSE, return_flags = TRUE)

## -----------------------------------------------------------------------------
raw_geno1 <- readGenotypes(gsKey = c("1WlnmujvHBc3s5jwCtRPcT3oHkRvpU-RXXIBmJOV8Qb0"))
proc_geno1 <- checkGenotypes(geno_data = raw_geno1, fulc_data = anno_fulc1, 
                                  return_geno = TRUE, return_flags = FALSE)

## -----------------------------------------------------------------------------
join_genofulc1 <- joinGenoFulc(geno = proc_geno1, fulc = anno_fulc1, dir = NULL)

## ----message = FALSE----------------------------------------------------------
final_data1 <- procPhotos(dir = dir1, data = join_genofulc1,
                          max_dim = 500, overwrite = T,
                          CeNDR = TRUE)
head(final_data1)

## -----------------------------------------------------------------------------
flag1.4 <- makeSpSheet(data = final_data1,
                       target_sp = "Caenorhabditis briggsae", dir = dir1)

## ----message = F--------------------------------------------------------------
generateReport(data = final_data1, dir = dir1)

