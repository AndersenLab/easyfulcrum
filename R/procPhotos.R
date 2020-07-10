#' procPhotos
#'
#' \code{procPhotos} copies raw sample photos from the Fulcrum export and renames them to strain_name_C-label for use with CeNDR. The function also makes thumbnails.
#'
#' @param dir a directory with sample photos.
#' @param data a data frame output from the \code{joinGenoFulc} function.
#' @param percentage This value sets the size of the thumbnail image relative to the original image. Maintians aspect ratio. default is 20.
#'
#' @return A subfolder named processed_photos within the photos folder containing full size sample photos renamed with strain names.
#' A thumbnails subfolder within the processed_photos folder for thumbnails. A dataframe identical to input \code{data}  with md5 hash values and file names for all photos.
#' @export
#'

procPhotos <- function(dir, data, percentage = 20) {
  # Make message
  message("Processing collection photos: This can take a few minutes to process 1,000 photos.")

  # find file names for photos where nematodes with strain names were isolated
  to_change <- data %>%
    dplyr::filter(!is.na(ECA_name)) %>%
    dplyr::mutate(orig_file_name = glue::glue("{dir}/{sample_photo}.jpg"),
                  new_file_name = glue::glue("{dir}/processed_photos/{ECA_name}.jpg"),
                  thumb_file_name = glue::glue("{dir}/processed_photos/thumbnails/{ECA_name}_thumbnail.jpg")) %>%
    dplyr::select(ECA_name, species_id, c_label, orig_file_name, new_file_name, thumb_file_name)


  # make processed subdirectory in dir and a thumbnails directory below that
  fs::dir_create(glue::glue("{dir}/processed_photos"))
  fs::dir_create(glue::glue("{dir}/processed_photos/thumbnails"))

  # copy files to new directory and rename
  fs::file_copy(to_change$orig_file_name, to_change$new_file_name, overwrite = F)

  # loop through renamed images to make thumbnails
  for(i in unique(to_change$new_file_name)) {
    # setup image in R
    img <- imager::load.image(i)

    # resize to make thumbnail
    thumb <- imager::resize(img, -percentage, -percentage) # need negative for resize function

    # write the file
    imager::save.image(thumb, file = glue::glue("{to_change %>% dplyr::filter(new_file_name == i) %>% dplyr::pull(thumb_file_name)}"))
  }
  # make a md5 hash for sample photos and thubnails integrity
  message("Writing md5 hash for raw images and adding to data frame.")
  raw_hash <- dplyr::as_tibble(as.list(tools::md5sum(fs::dir_ls(glue::glue("{dir}"), type = "file")))) %>%
    tidyr::gather(key = raw_file, value = sample_photo_raw_photo_hash) %>%
    dplyr::mutate(sample_photo_raw_file_name = stringr::str_extract(raw_file, pattern = ".{36}" %R% ".jpg"),
                  sample_photo = stringr::str_replace(sample_photo_raw_file_name, pattern = ".jpg", replacement = "")) %>%
    dplyr::select(-raw_file)

  message("Writing md5 hash for thumbnails and adding to data frame.")
  thumb_hash <- dplyr::as_tibble(as.list(tools::md5sum(fs::dir_ls(glue::glue("{dir}/processed_photos/thumbnails"))))) %>%
    tidyr::gather(key = thumb_file, value = sample_photo_thumbnail_hash) %>%
    dplyr::mutate(sample_photo_thumbnail_file_name = stringr::str_extract(thumb_file, pattern = one_or_more(WRD) %R% "_thumbnail.jpg"),
                  ECA_name = stringr::str_replace(sample_photo_thumbnail_file_name, pattern = "_thumbnail.jpg", replacement = ""),
                  sample_photo_processed_file_name =stringr::str_replace(sample_photo_thumbnail_file_name, pattern = "_thumbnail.jpg", replacement = ".jpg")) %>%
    dplyr::select(-thumb_file)

  # join hash to data frame
  data_out <- data %>%
    dplyr::left_join(raw_hash) %>%
    dplyr::left_join(thumb_hash) %>%
    dplyr::select(project:sample_photo, sample_photo_raw_file_name, sample_photo_processed_file_name, sample_photo_thumbnail_file_name,
                  sample_photo_raw_photo_hash, sample_photo_thumbnail_hash, everything())

  return(data_out)
}
