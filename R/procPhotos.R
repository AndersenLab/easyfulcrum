#' procPhotos
#'
#' \code{procPhotos} copies raw sample photos, renames them with the C-label,
#' and pastes them in data/processed/fulcrum/photos folder. The function also
#' makes thumbnails for use with interactive maps.
#'
#' @param dir The full path to the fulcrum project directory, raw/fulcrum/photos
#'   must contain the raw sample photos.
#' @param data a data frame output from the \code{joinGenoFulc} function.
#' @param max_dim This value sets the maximum dimension of the resized images in
#'   pixels. The default value is 500, which makes nice thumbnails.
#' @param overwrite Logical, passed to fs::file_copy. If TRUE then existing
#'   files with similar names will be written over. Default is FALSE.
#' @param CeNDR Logical, determines whether to write CeNDR criteria for
#'   qualifying photos to a subdirectory
#' @param pub_url A public url that holds sample images organized by project and
#'   named by C-label. For example, if the full url for C-5133 is
#'   https://storage.googleapis.com/elegansvariation.org/photos/isolation/fulcrum/2020JanuaryHawaii/sampling_thumbs/C-5133.jpg,
#'   the pub_url should be set to
#'   https://storage.googleapis.com/elegansvariation.org/photos/isolation/fulcrum/.
#'   The project name, "sampling_thumbs", C-label, and file extension will be
#'   filled by the function.
#' @return A new directory data/processed/fulcrum/photos. This directory
#'   contains full size sample photos renamed with C-labels and a thumbnails
#'   subfolder that contains resized images. A dataframe identical to input with
#'   file names for all photos, which is also saved as an rds file in
#'   data/processed/fulcrum directory. If \code{CeNDR} is set to TRUE other
#'   subfolders will be made with those images.
#' @importFrom rebus ALPHA one_or_more %R% DGT WRD optional
#' @importFrom imager load.image resize save.image
#' @importFrom glue glue
#' @import dplyr
#' @import stringr
#' @import tidyr
#' @import fs
#' @export
#'

procPhotos <- function(dir, data, max_dim = 500, overwrite = FALSE, CeNDR = FALSE,
                       pub_url = "https://storage.googleapis.com/elegansvariation.org/photos/isolation/fulcrum/") {
  # edit pub_url to take into account project name and subfolder
  project_url <- glue::glue("{pub_url}{unique(data$project)}/")

  # warn that more than one
  if(length(unique(data$project)) > 1){
    warning(glue::glue("{length(unique(data$project))} distinct collection projects in data, expected 1"))
  }

  # end if project NA
  if(sum(is.na(data$project)) > 0){
    stop("At least one NA detected in data$project, expected no NAs.")
  }

  # edit dir to be appropriate for path to photos
  dir_photos <- glue::glue("{dir}/data/raw/fulcrum/photos")
  # make processed dir path
  processed_dir <- stringr::str_replace(dir_photos, pattern = "raw/fulcrum/photos", replacement = "processed/fulcrum/photos")

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

  # make a thumbnails directory below photos folder
  fs::dir_create(glue::glue("{processed_dir}/thumbnails"))

  # copy files to new directory and rename
  message(glue::glue("Copying and renaming raw photos from {dir_photos} to {processed_dir}"))
  fs::file_copy(to_change1$orig_file_name, to_change1$new_file_name, overwrite = overwrite)
  fs::file_copy(to_change2$orig_file_name2, to_change2$new_file_name2, overwrite = overwrite)
  fs::file_copy(to_change3$orig_file_name3, to_change3$new_file_name3, overwrite = overwrite)

  # loop through renamed images to make thumbnails
  for(i in unique(to_change1$new_file_name)) {
    # Make message
    message(glue::glue("Making thumbnail for:{to_change1 %>% dplyr::filter(new_file_name == i) %>% dplyr::pull(orig_file_name)}"))
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
    message(glue::glue("Making thumbnail for:{to_change2 %>% dplyr::filter(new_file_name2 == i) %>% dplyr::pull(orig_file_name2)}"))

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
    message(glue::glue("Making thumbnail for:{to_change3 %>% dplyr::filter(new_file_name3 == i) %>% dplyr::pull(orig_file_name3)}"))

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

  # make output data
  data_out <- data %>%
    dplyr::left_join(dplyr::select(to_change1, sample_photo1, orig_file_name:thumb_file_name), by = c("sample_photo1" = "sample_photo1")) %>%
    dplyr::left_join(dplyr::select(to_change2, sample_photo2, orig_file_name2:thumb_file_name2), by = c("sample_photo2" = "sample_photo2")) %>%
    dplyr::left_join(dplyr::select(to_change3, sample_photo3, orig_file_name3:thumb_file_name3), by = c("sample_photo3" = "sample_photo3")) %>%
    dplyr::mutate(sample_photo1_processed_url = case_when(!is.na(sample_photo1) ~ glue::glue("{project_url}sampling_thumbs/{c_label}.jpg"),
                                                          TRUE ~ NA_character_),
                  sample_photo2_processed_url = case_when(!is.na(sample_photo2) ~ glue::glue("{project_url}sampling_thumbs/{c_label}.jpg"),
                                                          TRUE ~ NA_character_),
                  sample_photo3_processed_url = case_when(!is.na(sample_photo3) ~ glue::glue("{project_url}sampling_thumbs/{c_label}.jpg"),
                                                          TRUE ~ NA_character_)) %>%
    dplyr::select(project:sample_photo1, sample_photo1_processed_url, orig_file_name:thumb_file_name, sample_photo2, sample_photo2_processed_url, orig_file_name2:thumb_file_name2,
                  sample_photo3, sample_photo3_processed_url, orig_file_name3:thumb_file_name3, everything())

  # return
  message(glue::glue("DONE -- saving data to .rds file {dir}/data/processed/fulcrum/",
                     as.character(Sys.Date()),"_",
                     tail(strsplit(dir,"/")[[1]],1),"fulcrum.rds"))
  if(is.character(dir)){
    save(object = data_out,
            file = glue::glue("{dir}","/data/processed/fulcrum/",
                              as.character(Sys.Date()),"_",
                              tail(strsplit(dir,"/")[[1]],1),"fulcrum.rds"), version = 3)}
  return(data_out)
}
