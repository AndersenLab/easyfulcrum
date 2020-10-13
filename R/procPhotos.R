#' procPhotos
#'
#' \code{procPhotos} copies raw sample photos from the Fulcrum export and renames them to strain_name for use with CeNDR. The function also resizes images.
#'
#' @param dir a directory with sample photos.
#' @param data a data frame output from the \code{joinGenoFulc} function.
#' @param max_dim This value sets the maximum dimension of the resized images in pixels. The default value is 500.
#' @param overwrite Logical, passed to fs::file_copy. If \code{TRUE} then existing files with similar names will be written over. Default is \code{FALSE}.
#' @return A folder named processed_photos in the data/processed/fulcrum directory. The folder contains full size sample photos renamed with strain names.
#' A thumbnails subfolder is also returned within the processed_photos folder. This folder contains resized images. A dataframe identical to input \code{data}
#' with md5 hash values and file names for all photos.
#' @importFrom rebus ALPHA one_or_more %R% DGT WRD optional
#' @importFrom imager load.image resize save.image
#' @export
#'

procPhotos <- function(dir, data, max_dim = 500, overwrite = FALSE, pub_url = "https://storage.googleapis.com/elegansvariation.org/photos/isolation/fulcrum/") {
  # edit pub_url to take into accoutn project name and subfolder
  project_url <- glue::glue("{pub_url}",tail(strsplit(dir,"/")[[1]],1),"/sampling_thumbs/")
  # edit dir to be appropriate for path to photos
  dir <- glue::glue("{dir}","/data/raw/fulcrum/photos")
  # make processed dir path
  processed_dir <- stringr::str_replace(dir, pattern = "raw/fulcrum/photos", replacement = "processed/fulcrum")

  # find file names for photos where nematodes with strain names were isolated
  to_change <- data %>%
    dplyr::filter(!is.na(strain_name)) %>%
    dplyr::mutate(orig_file_name = glue::glue("{dir}/{sample_photo1}.jpg"),
                  new_file_name = glue::glue("{processed_dir}/processed_photos/{strain_name}.jpg"),
                  thumb_file_name = glue::glue("{processed_dir}/processed_photos/thumbnails/{strain_name}.jpg")) %>%
    dplyr::mutate(orig_file_name2 = glue::glue("{dir}/{sample_photo2}.jpg"),
                  new_file_name2 = glue::glue("{processed_dir}/processed_photos/{strain_name}_2.jpg"),
                  thumb_file_name2 = glue::glue("{processed_dir}/processed_photos/thumbnails/{strain_name}_2.jpg")) %>%
    dplyr::mutate(orig_file_name3 = glue::glue("{dir}/{sample_photo3}.jpg"),
                  new_file_name3 = glue::glue("{processed_dir}/processed_photos/{strain_name}_3.jpg"),
                  thumb_file_name3 = glue::glue("{processed_dir}/processed_photos/thumbnails/{strain_name}_3.jpg")) %>%
    dplyr::select(strain_name, species_id, c_label, sample_photo1, sample_photo2, sample_photo3, orig_file_name:thumb_file_name3)

  to_change1 <- to_change %>%
    dplyr::filter(!is.na(sample_photo1))

  to_change2 <- to_change %>%
    dplyr::filter(!is.na(sample_photo2))

  to_change3 <- to_change %>%
    dplyr::filter(!is.na(sample_photo3))


  # make processed subdirectory in dir and a thumbnails directory below that
  fs::dir_create(glue::glue("{processed_dir}/processed_photos"))
  fs::dir_create(glue::glue("{processed_dir}/processed_photos/thumbnails"))

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
  raw_hash <- dplyr::as_tibble(as.list(tools::md5sum(fs::dir_ls(glue::glue("{dir}"), type = "file")))) %>%
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

  message("Writing md5 hash for resized images and adding to data frame.")
  thumb_hash <- dplyr::as_tibble(as.list(tools::md5sum(fs::dir_ls(glue::glue("{processed_dir}/processed_photos/thumbnails"))))) %>%
    tidyr::gather(key = thumb_file, value = sample_photo_resized_hash) %>%
    dplyr::mutate(sample_photo_resized_file_name = stringr::str_extract(thumb_file, pattern = one_or_more(WRD) %R% optional("_") %R% optional(DGT) %R% ".jpg"),
                  strain_name = stringr::str_extract(sample_photo_resized_file_name, pattern = ALPHA %R% ALPHA %R% optional(ALPHA) %R% optional(ALPHA) %R% DGT %R% optional(DGT) %R% optional(DGT) %R% optional(DGT) %R% optional(DGT)),
                  sample_photo_processed_file_name = sample_photo_resized_file_name)

  # get hash 1
  thumb_hash1 <- thumb_hash %>%
    dplyr::filter(strain_name %in% to_change1$strain_name)
  # get hash 2
  thumb_hash2 <- thumb_hash %>%
    dplyr::filter(strain_name %in% to_change2$strain_name)
  # get hash 3
  thumb_hash3 <- thumb_hash %>%
    dplyr::filter(strain_name %in% to_change3$strain_name)

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
                                   sample_photo1_resized_file_name = sample_photo_resized_file_name, strain_name), by = c("strain_name" = "strain_name")) %>%
    dplyr::left_join(dplyr::select(thumb_hash2, sample_photo2_resized_hash = sample_photo_resized_hash,
                                   sample_photo2_processed_file_name = sample_photo_processed_file_name,
                                   sample_photo2_resized_file_name = sample_photo_resized_file_name, strain_name), by = c("strain_name" = "strain_name")) %>%
    dplyr::left_join(dplyr::select(thumb_hash3, sample_photo3_resized_hash = sample_photo_resized_hash,
                                   sample_photo3_processed_file_name = sample_photo_processed_file_name,
                                   sample_photo3_resized_file_name = sample_photo_resized_file_name, strain_name), by = c("strain_name" = "strain_name"))  %>%
    dplyr::mutate(sample_photo1_processed_url = ifelse(!is.na(sample_photo1_processed_file_name), glue::glue("{project_url}","{sample_photo1_processed_file_name}"), NA),
                  sample_photo2_processed_url = ifelse(!is.na(sample_photo2_processed_file_name), glue::glue("{project_url}","{sample_photo2_processed_file_name}"), NA),
                  sample_photo3_processed_url = ifelse(!is.na(sample_photo3_processed_file_name), glue::glue("{project_url}","{sample_photo3_processed_file_name}"), NA)) %>%
    dplyr::select(project:sample_photo1, sample_photo1_raw_file_name, sample_photo1_processed_file_name, sample_photo1_hash, sample_photo1_resized_file_name, sample_photo1_resized_hash,
                sample_photo2, sample_photo2_raw_file_name, sample_photo2_processed_file_name, sample_photo2_hash, sample_photo2_resized_file_name, sample_photo2_resized_hash,
                sample_photo3, sample_photo3_raw_file_name, sample_photo3_processed_file_name, sample_photo3_hash, sample_photo3_resized_file_name, sample_photo3_resized_hash,
                everything())
  # return
  message("DONE")
  return(data_out)
}
