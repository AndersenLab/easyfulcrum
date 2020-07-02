library(tidyverse)

# set working directory
setwd(glue::glue("{dirname(rstudioapi::getActiveDocumentContext()$path)}/.."))

# setup test directory for multi project Fulcrum export
dir1 <- "test_data/2020FebruaryAustralia/data/fulcrum"

# test readFulcrum function
raw_data1 <- readFulcrum(dir1)

# test procFulcrum function
proc_data1 <- procFulcrum(raw_data1)

# test joinFulcrum function
join_data1 <- joinFulcrum(proc_data1)

# test annotateFulcrum fuction
anno_data1 <- annotateFulcrum(join_data1)

# test readGenotypes function
geno_data1 <- readGenotypes(gsKey = c("1CxKJHM6mEu4VvnN2T1ioXiJNZmmmpeosmECP2zeAPmY"))

# test joinGenoFulc function to join genotype data to fulcrum data
joingeno_data1 <- joinGenoFulc(geno = geno_data1, fulc = anno_data1,)

# test the procPhotos function
photodir <- "test_data/2020FebruaryAustralia/data/fulcrum/photos"

procPhotos(photodir, joingeno_data1)








# function for making thumb
makeThumb <- function(file, size = 0.2) {
  img <- jpeg::readJPEG(file)
  h<-dim(img)[1] # image height
  w<-dim(img)[2] # image width

  jpeg::jpeg(file = glue::glue("thumb_{file}"), height = h*size, width = w*size)
  par(mar=c(0,0,0,0), xaxs="i", yaxs="i", ann=FALSE)
  plot(1:2, type='n', xaxt = "n", yaxt = "n", xlab = "", ylab = "")
  lim <- par()
  rasterImage(img, lim$usr[1], lim$usr[3], lim$usr[2], lim$usr[4])
  dev.off()
}

makeThumb("/Users/tim/repos/easyfulcrum/test_data/2020FebruaryAustralia/data/fulcrum/photos/processed_photos/ECA2657_C-5268.jpg")

img <- jpeg::readJPEG("/Users/tim/repos/easyfulcrum/test_data/2020FebruaryAustralia/data/fulcrum/photos/processed_photos/ECA2657_C-5268.jpg")


