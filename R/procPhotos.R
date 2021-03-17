#' procPhotos
#'
#' \code{procPhotos} copies raw sample photos from the Fulcrum export and renames them to strain_name for use with CeNDR. The function also resizes images.
#'
#' @param dir The path to the base fulcrum directory, raw/fulcrum/photos should contain the photos for the project
#' @param data a data frame output from the \code{joinGenoFulc} function.
#' @param max_dim This value sets the maximum dimension of the resized images in pixels. The default value is 500.
#' @param overwrite Logical, passed to fs::file_copy. If TRUE then existing files with similar names will be written over. Default is FALSE.
#' @param CeNDR Logical, determines whether to write CeNDR criteria qualifying photos to a subdirectory
#' @return A folder named processed_photos in the data/processed/fulcrum directory. The folder contains full size sample photos renamed with strain names.
#' A thumbnails subfolder is also returned within the processed_photos folder. This folder contains resized images. A dataframe identical to input \code{data}
#' with md5 hash values and file names for all photos, also saved as an rds file in data/processed/fulcrum directory.
#' If \code{CeNDR} is set to TRUE other subfolders will be made with those images.
#' @importFrom rebus ALPHA one_or_more %R% DGT WRD optional
#' @importFrom imager load.image resize save.image
#' @importFrom glue glue
#' @import dplyr
#' @import stringr
#' @import tidyr
#' @import fs
#' @export
#'

procPhotos <- function(dir, data, max_dim = 500, overwrite = FALSE, CeNDR = FALSE, pub_url = "https://storage.googleapis.com/elegansvariation.org/photos/isolation/fulcrum/") {
  # edit pub_url to take into account project name and subfolder
  project_url <- glue::glue("{pub_url}",tail(strsplit(dir,"/")[[1]],1),"/sampling_thumbs/")
  # edit dir to be appropriate for path to photos
  dir_photos <- glue::glue("{dir}","/data/raw/fulcrum/photos")
  # make processed dir path
  processed_dir <- stringr::str_replace(dir_photos, pattern = "raw/fulcrum/photos", replacement = "processed/fulcrum")

  # find file names for photos where CeNDR criteria is satisfied, create folder of these images and thumbnails
  if(CeNDR){
    to_change <- data %>%
      dplyr::filter(!is.na(strain_name)) %>%
      dplyr::filter(species_id %in% c("C. elegans","C. briggsae","C. tropicalis","Caenorhabditis elegans","Caenorhabditis briggsae","Caenorhabditis tropicalis")) %>%
      dplyr::mutate(orig_file_name = glue::glue("{dir_photos}/{sample_photo1}.jpg"),
                    new_file_name = glue::glue("{processed_dir}/CeNDR/{strain_name}.jpg"),
                    thumb_file_name = glue::glue("{processed_dir}/CeNDR/thumbnails/{strain_name}.jpg")) %>%
      dplyr::mutate(orig_file_name2 = glue::glue("{dir_photos}/{sample_photo2}.jpg"),
                    new_file_name2 = glue::glue("{processed_dir}/CeNDR/{strain_name}_2.jpg"),
                    thumb_file_name2 = glue::glue("{processed_dir}/CeNDR/thumbnails/{strain_name}_2.jpg")) %>%
      dplyr::mutate(orig_file_name3 = glue::glue("{dir_photos}/{sample_photo3}.jpg"),
                    new_file_name3 = glue::glue("{processed_dir}/CeNDR/{strain_name}_3.jpg"),
                    thumb_file_name3 = glue::glue("{processed_dir}/CeNDR/thumbnails/{strain_name}_3.jpg")) %>%
      dplyr::select(strain_name, species_id, c_label, sample_photo1, sample_photo2, sample_photo3, orig_file_name:thumb_file_name3)

    to_change1 <- to_change %>%
      dplyr::filter(!is.na(sample_photo1))

    to_change2 <- to_change %>%
      dplyr::filter(!is.na(sample_photo2))

    to_change3 <- to_change %>%
      dplyr::filter(!is.na(sample_photo3))


    # make processed subdirectory in dir and a thumbnails directory below that
    fs::dir_create(glue::glue("{processed_dir}/CeNDR"))
    fs::dir_create(glue::glue("{processed_dir}/CeNDR/thumbnails"))

    # copy files to new directory and rename
    fs::file_copy(to_change1$orig_file_name, to_change1$new_file_name, overwrite = overwrite)
    fs::file_copy(to_change2$orig_file_name2, to_change2$new_file_name2, overwrite = overwrite)
    fs::file_copy(to_change3$orig_file_name3, to_change3$new_file_name3, overwrite = overwrite)

    # loop through renamed images to make thumbnails
    for(i in unique(to_change1$new_file_name)) {
      # Make message
      message(glue::glue("Processing collection photo:{to_change1 %>% dplyr::filter(new_file_name == i) %>% dplyr::pull(orig_file_name)}"))
      # setup image in R
      img <- imager::load.image(i)
      # get raw img dimesions
      raw_max_dim <- max(dim(img))
      percentage <- 100*(max_dim/raw_max_dim)

      # resize to make thumbnail
      thumb <- imager::resize(img, -percentage, -percentage) # need negative for resize function

      # write the file
      imager::save.image(thumb, file = glue::glue("{to_change1 %>% dplyr::filter(new_file_name == i) %>% dplyr::pull(thumb_file_name)}"))
    }

    for(i in unique(to_change2$new_file_name2)) {
      # Make message
      message(glue::glue("Processing collection photo:{to_change2 %>% dplyr::filter(new_file_name2 == i) %>% dplyr::pull(orig_file_name2)}"))

      # setup image in R
      img <- imager::load.image(i)
      # get raw img dimesions
      raw_max_dim <- max(dim(img))
      percentage <- 100*(max_dim/raw_max_dim)

      # resize to make thumbnail
      thumb <- imager::resize(img, -percentage, -percentage) # need negative for resize function

      # write the file
      imager::save.image(thumb, file = glue::glue("{to_change %>% dplyr::filter(new_file_name2 == i) %>% dplyr::pull(thumb_file_name2)}"))
    }

    for(i in unique(to_change3$new_file_name3)) {
      # Make message
      message(glue::glue("Processing collection photo:{to_change3 %>% dplyr::filter(new_file_name3 == i) %>% dplyr::pull(orig_file_name3)}"))

      # setup image in R
      img <- imager::load.image(i)
      # get raw img dimesions
      raw_max_dim <- max(dim(img))
      percentage <- 100*(max_dim/raw_max_dim)

      # resize to make thumbnail
      thumb <- imager::resize(img, -percentage, -percentage) # need negative for resize function

      # write the file
      imager::save.image(thumb, file = glue::glue("{to_change %>% dplyr::filter(new_file_name3 == i) %>% dplyr::pull(thumb_file_name3)}"))
    }
  }

  # copy all images create folder of these images and thumbnails
  to_change <- data %>%
    dplyr::filter(!is.na(c_label)) %>%
    dplyr::distinct(c_label, .keep_all = TRUE) %>%
    dplyr::mutate(orig_file_name = glue::glue("{dir_photos}/{sample_photo1}.jpg"),
                  new_file_name = glue::glue("{processed_dir}/{c_label}.jpg"),
                  thumb_file_name = glue::glue("{processed_dir}/thumbnails/{c_label}.jpg")) %>%
    dplyr::mutate(orig_file_name2 = glue::glue("{dir_photos}/{sample_photo2}.jpg"),
                  new_file_name2 = glue::glue("{processed_dir}/{c_label}_2.jpg"),
                  thumb_file_name2 = glue::glue("{processed_dir}/thumbnails/{c_label}_2.jpg")) %>%
    dplyr::mutate(orig_file_name3 = glue::glue("{dir_photos}/{sample_photo3}.jpg"),
                  new_file_name3 = glue::glue("{processed_dir}/{c_label}_3.jpg"),
                  thumb_file_name3 = glue::glue("{processed_dir}/thumbnails/{c_label}_3.jpg")) %>%
    dplyr::select(strain_name, species_id, c_label, sample_photo1, sample_photo2, sample_photo3, orig_file_name:thumb_file_name3)

  to_change1 <- to_change %>%
    dplyr::filter(!is.na(sample_photo1))

  to_change2 <- to_change %>%
    dplyr::filter(!is.na(sample_photo2))

  to_change3 <- to_change %>%
    dplyr::filter(!is.na(sample_photo3))


  # make a thumbnails directory below main folder
  fs::dir_create(glue::glue("{processed_dir}/thumbnails"))

  # copy files to new directory and rename
  fs::file_copy(to_change1$orig_file_name, to_change1$new_file_name, overwrite = overwrite)
  fs::file_copy(to_change2$orig_file_name2, to_change2$new_file_name2, overwrite = overwrite)
  fs::file_copy(to_change3$orig_file_name3, to_change3$new_file_name3, overwrite = overwrite)


  # loop through renamed images to make thumbnails
  for(i in unique(to_change1$new_file_name)) {
    # Make message
    message(glue::glue("Processing collection photo:{to_change1 %>% dplyr::filter(new_file_name == i) %>% dplyr::pull(orig_file_name)}"))
    # setup image in R
    img <- imager::load.image(i)
    # get raw img dimesions
    raw_max_dim <- max(dim(img))
    percentage <- 100*(max_dim/raw_max_dim)

    # resize to make thumbnail
    thumb <- imager::resize(img, -percentage, -percentage) # need negative for resize function

    # write the file
    imager::save.image(thumb, file = glue::glue("{to_change1 %>% dplyr::filter(new_file_name == i) %>% dplyr::pull(thumb_file_name)}"))
  }

  for(i in unique(to_change2$new_file_name2)) {
    # Make message
    message(glue::glue("Processing collection photo:{to_change2 %>% dplyr::filter(new_file_name2 == i) %>% dplyr::pull(orig_file_name2)}"))

    # setup image in R
    img <- imager::load.image(i)
    # get raw img dimesions
    raw_max_dim <- max(dim(img))
    percentage <- 100*(max_dim/raw_max_dim)

    # resize to make thumbnail
    thumb <- imager::resize(img, -percentage, -percentage) # need negative for resize function

    # write the file
    imager::save.image(thumb, file = glue::glue("{to_change %>% dplyr::filter(new_file_name2 == i) %>% dplyr::pull(thumb_file_name2)}"))
  }

  for(i in unique(to_change3$new_file_name3)) {
    # Make message
    message(glue::glue("Processing collection photo:{to_change3 %>% dplyr::filter(new_file_name3 == i) %>% dplyr::pull(orig_file_name3)}"))

    # setup image in R
    img <- imager::load.image(i)
    # get raw img dimesions
    raw_max_dim <- max(dim(img))
    percentage <- 100*(max_dim/raw_max_dim)

    # resize to make thumbnail
    thumb <- imager::resize(img, -percentage, -percentage) # need negative for resize function

    # write the file
    imager::save.image(thumb, file = glue::glue("{to_change %>% dplyr::filter(new_file_name3 == i) %>% dplyr::pull(thumb_file_name3)}"))
  }

  # make a md5 hash for sample photos and thubnails integrity
  message("Writing md5 hash for raw images and adding to data frame.")
  raw_hash <- dplyr::as_tibble(as.list(tools::md5sum(fs::dir_ls(glue::glue("{dir_photos}"), type = "file")))) %>%
    tidyr::gather(key = raw_file, value = sample_photo_raw_photo_hash) %>%
    dplyr::mutate(sample_photo_raw_file_name = stringr::str_extract(raw_file, pattern = ".{36}" %R% ".jpg"),
                  sample_photo = stringr::str_replace(sample_photo_raw_file_name, pattern = ".jpg", replacement = "")) %>%
    dplyr::select(-raw_file)
  # get hash 1
  raw_hash1 <- raw_hash %>%
    dplyr::filter(sample_photo %in% data$sample_photo1)
  # get hash 2
  raw_hash2 <- raw_hash %>%
    dplyr::filter(sample_photo %in% data$sample_photo2)
  # get hash 3
  raw_hash3 <- raw_hash %>%
    dplyr::filter(sample_photo %in% data$sample_photo3)

  message("Writing md5 hash for thumbnail images and adding to data frame.")
  thumb_hash <- dplyr::as_tibble(as.list(tools::md5sum(fs::dir_ls(glue::glue("{processed_dir}/thumbnails"))))) %>%
    tidyr::gather(key = thumb_file, value = sample_photo_resized_hash) %>%
    dplyr::mutate(sample_photo_resized_file_name = stringr::str_extract(thumb_file, pattern = ALPHA %R% optional("-") %R% one_or_more(DGT) %R% optional("_") %R% one_or_more(DGT) %R% ".jpg"),
                  c_label = stringr::str_extract(sample_photo_resized_file_name, pattern = ALPHA %R% optional("-") %R% one_or_more(DGT)),
                  sample_photo_processed_file_name = sample_photo_resized_file_name)

  # get hash 1
  thumb_hash1 <- thumb_hash %>%
    dplyr::filter(c_label %in% to_change1$c_label)
  # get hash 2
  thumb_hash2 <- thumb_hash %>%
    dplyr::filter(c_label %in% to_change2$c_label)
  # get hash 3
  thumb_hash3 <- thumb_hash %>%
    dplyr::filter(c_label %in% to_change3$c_label)

  # join hash to data frame
  data_out <- data %>%
    dplyr::left_join(dplyr::select(raw_hash1, sample_photo1_hash = sample_photo_raw_photo_hash,
                                   sample_photo1_raw_file_name = sample_photo_raw_file_name, sample_photo), by = c("sample_photo1" = "sample_photo")) %>%
    dplyr::left_join(dplyr::select(raw_hash2, sample_photo2_hash = sample_photo_raw_photo_hash,
                                   sample_photo2_raw_file_name = sample_photo_raw_file_name, sample_photo), by = c("sample_photo2" = "sample_photo")) %>%
    dplyr::left_join(dplyr::select(raw_hash3, sample_photo3_hash = sample_photo_raw_photo_hash,
                                   sample_photo3_raw_file_name = sample_photo_raw_file_name, sample_photo), by = c("sample_photo3" = "sample_photo")) %>%
    dplyr::left_join(dplyr::select(thumb_hash1, sample_photo1_resized_hash = sample_photo_resized_hash,
                                   sample_photo1_processed_file_name = sample_photo_processed_file_name,
                                   sample_photo1_resized_file_name = sample_photo_resized_file_name, c_label), by = c("c_label" = "c_label")) %>%
    dplyr::left_join(dplyr::select(thumb_hash2, sample_photo2_resized_hash = sample_photo_resized_hash,
                                   sample_photo2_processed_file_name = sample_photo_processed_file_name,
                                   sample_photo2_resized_file_name = sample_photo_resized_file_name, c_label), by = c("c_label" = "c_label")) %>%
    dplyr::left_join(dplyr::select(thumb_hash3, sample_photo3_resized_hash = sample_photo_resized_hash,
                                   sample_photo3_processed_file_name = sample_photo_processed_file_name,
                                   sample_photo3_resized_file_name = sample_photo_resized_file_name, c_label), by = c("c_label" = "c_label"))  %>%
    dplyr::mutate(sample_photo1_processed_url = ifelse(!is.na(sample_photo1_processed_file_name), glue::glue("{project_url}","{sample_photo1_processed_file_name}"), NA),
                  sample_photo2_processed_url = ifelse(!is.na(sample_photo2_processed_file_name), glue::glue("{project_url}","{sample_photo2_processed_file_name}"), NA),
                  sample_photo3_processed_url = ifelse(!is.na(sample_photo3_processed_file_name), glue::glue("{project_url}","{sample_photo3_processed_file_name}"), NA)) %>%
    dplyr::select(project:sample_photo1, sample_photo1_raw_file_name, sample_photo1_processed_file_name, sample_photo1_hash, sample_photo1_resized_file_name, sample_photo1_resized_hash,
                sample_photo2, sample_photo2_raw_file_name, sample_photo2_processed_file_name, sample_photo2_hash, sample_photo2_resized_file_name, sample_photo2_resized_hash,
                sample_photo3, sample_photo3_raw_file_name, sample_photo3_processed_file_name, sample_photo3_hash, sample_photo3_resized_file_name, sample_photo3_resized_hash,
                everything())
  # return
  message("DONE")
  if(is.character(dir)){
    saveRDS(object = data_out,
         file = glue::glue("{dir}","/data/processed/fulcrum/",
                           as.character(Sys.Date()),"_",
                           tail(strsplit(dir,"/")[[1]],1),"fulcrum.rds"), version = 3)}
  return(data_out)
}
