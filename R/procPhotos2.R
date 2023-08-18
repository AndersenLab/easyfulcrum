#' procPhotos2
#'
#' \code{procPhotos2} copies raw sample photos, renames them with the C-label
#' and strain name if \code{CaeNDR} is set to TRUE, and saves them in
#' data/processed/fulcrum/photos folder. The function also makes thumbnails for
#' use with interactive maps and calculates md5 checksums for images.
#'
#' @param dir The path to the base fulcrum directory, raw/fulcrum/photos. This
#'   directory must contain the raw sample photos exported from Fulcrum.
#' @param data a data frame output from the \code{joinGenoFulc} function.
#' @param max_dim This value sets the maximum dimension of the resized images in
#'   pixels. The default value is 500.
#' @param overwrite Logical, passed to fs::file_copy. If TRUE then existing
#'   files with similar names will be written over. Default is FALSE.
#' @param CeaNDR Logical, determines whether to also rename photos to strain
#'   names for Caenorhabditis briggsae, Caenorhabditis elegans, Caenorhabditis
#'   tropicalis positive collections and add those images to the
#'   \code{data/processed/fulcrum/photos/CeaNDR/} directory.
#' @param pub_url A public url that holds sample images organized by \code{<your
#'   project>/sampling_thumbs/<C-label>.jpg}. For example, if the full url for
#'   C-5133 is
#'   https://storage.googleapis.com/elegansvariation.org/photos/isolation/fulcrum/2020JanuaryHawaii/sampling_thumbs/C-5133.jpg,
#'   the pub_url should be set to
#'   https://storage.googleapis.com/elegansvariation.org/photos/isolation/fulcrum/.
#'   The project name, "sampling_thumbs", C-label, and file extension will be
#'   filled by the function.
#' @return A folder named \code{photos} in the \code{data/processed/fulcrum}
#'   directory. The folder contains sample photos renamed with C-labels. If
#'   \code{CaeNDR} is set to TRUE sample photos will be renamed with strain names
#'   as well organized by species. Multiple sample photos for a single C-label
#'   are given a numeric suffix after the strain name \code{strain_name_#}.
#'   The isolation photos have the highest suffix. The function uses a single isolation
#'   photo even if multiple exist. A dataframe identical to input \code{data} with old
#'   and new image file names, md5 photo hash values, and a public url to find
#'   images. The function also saves a .rds file to the
#'   \code{data/processed/fulcrum} directory.
#' @importFrom rebus ALPHA one_or_more %R% DGT WRD optional
#' @importFrom imager load.image resize save.image
#' @importFrom glue glue
#' @importFrom progress progress_bar
#' @import dplyr
#' @import stringr
#' @import tidyr
#' @import fs
#' @export
#'

procPhotos2 <- function(dir, data, max_dim = 500, overwrite = FALSE, CeaNDR = FALSE,
                        pub_url = "https://storage.googleapis.com/elegansvariation.org/photos/isolation/fulcrum/") {

  # warn that more than one project detected
  if(length(unique(data$project)) > 1){
    warning(glue::glue("{length(unique(data$project))} distinct collection projects in data, expected 1"))
  }

  # end if project NA exists
  if(sum(is.na(data$project)) > 0){
    stop("At least one NA detected in data$project, expected no NAs.")
  }

  # edit pub_url to take into account project name and subfolder
  project_url <- glue::glue("{pub_url}{unique(data$project)}/sampling_thumbs/")
  # edit dir to be appropriate for path to photos
  dir_photos <- glue::glue("{dir}","/data/raw/fulcrum/photos")
  # make processed dir path
  processed_dir <- stringr::str_replace(dir_photos, pattern = "raw/fulcrum/photos", replacement = "processed/fulcrum/photos")

  # find file names for photos where CeNDR criteria is satisfied, create folder of these images and thumbnails
  if(CeaNDR == T){
    to_change <- data %>%
      dplyr::filter(!is.na(strain_name)) %>%
      dplyr::filter(species_id %in% c("C. elegans","C. briggsae","C. tropicalis","Caenorhabditis elegans","Caenorhabditis briggsae","Caenorhabditis tropicalis")) %>%
      dplyr::mutate(orig_file_name = glue::glue("{dir_photos}/{sample_photo1}.jpg"),
                    new_file_name = case_when(species_id %in% c("C. briggsae", "Caenorhabditis briggsae") ~ glue::glue("{processed_dir}/CeaNDR/Cb/{strain_name}.jpg"),
                                              species_id %in% c("C. elegans", "Caenorhabditis elegans") ~ glue::glue("{processed_dir}/CeaNDR/Ce/{strain_name}.jpg"),
                                              species_id %in% c("C. tropicalis", "Caenorhabditis tropicalis") ~ glue::glue("{processed_dir}/CeaNDR/Ct/{strain_name}.jpg")),
                    thumb_file_name = case_when(species_id %in% c("C. briggsae", "Caenorhabditis briggsae") ~ glue::glue("{processed_dir}/CeaNDR/Cb/{strain_name}.thumb.jpg"),
                                                species_id %in% c("C. elegans", "Caenorhabditis elegans") ~ glue::glue("{processed_dir}/CeaNDR/Ce/{strain_name}.thumb.jpg"),
                                                species_id %in% c("C. tropicalis", "Caenorhabditis tropicalis") ~ glue::glue("{processed_dir}/CeaNDR/Ct/{strain_name}.thumb.jpg"))) %>%
      dplyr::mutate(orig_file_name2 = glue::glue("{dir_photos}/{sample_photo2}.jpg"),
                    new_file_name2 = case_when(species_id %in% c("C. briggsae", "Caenorhabditis briggsae") ~ glue::glue("{processed_dir}/CeaNDR/Cb/{strain_name}_2.jpg"),
                                               species_id %in% c("C. elegans", "Caenorhabditis elegans") ~ glue::glue("{processed_dir}/CeaNDR/Ce/{strain_name}_2.jpg"),
                                               species_id %in% c("C. tropicalis", "Caenorhabditis tropicalis") ~ glue::glue("{processed_dir}/CeaNDR/Ct/{strain_name}_2.jpg")),
                    thumb_file_name2 = case_when(species_id %in% c("C. briggsae", "Caenorhabditis briggsae") ~ glue::glue("{processed_dir}/CeaNDR/Cb/{strain_name}_2.thumb.jpg"),
                                                 species_id %in% c("C. elegans", "Caenorhabditis elegans") ~ glue::glue("{processed_dir}/CeaNDR/Ce/{strain_name}_2.thumb.jpg"),
                                                 species_id %in% c("C. tropicalis", "Caenorhabditis tropicalis") ~ glue::glue("{processed_dir}/CeaNDR/Ct/{strain_name}_2.thumb.jpg"))) %>%
      dplyr::mutate(orig_file_name3 = glue::glue("{dir_photos}/{sample_photo3}.jpg"),
                    new_file_name3 = case_when(species_id %in% c("C. briggsae", "Caenorhabditis briggsae") ~ glue::glue("{processed_dir}/CeaNDR/Cb/{strain_name}_3.jpg"),
                                               species_id %in% c("C. elegans", "Caenorhabditis elegans") ~ glue::glue("{processed_dir}/CeaNDR/Ce/{strain_name}_3.jpg"),
                                               species_id %in% c("C. tropicalis", "Caenorhabditis tropicalis") ~ glue::glue("{processed_dir}/CeaNDR/Ct/{strain_name}_3.jpg")),
                    thumb_file_name3 = case_when(species_id %in% c("C. briggsae", "Caenorhabditis briggsae") ~ glue::glue("{processed_dir}/CeaNDR/Cb/{strain_name}_3.thumb.jpg"),
                                                 species_id %in% c("C. elegans", "Caenorhabditis elegans") ~ glue::glue("{processed_dir}/CeaNDR/Ce/{strain_name}_3.thumb.jpg"),
                                                 species_id %in% c("C. tropicalis", "Caenorhabditis tropicalis") ~ glue::glue("{processed_dir}/CeaNDR/Ct/{strain_name}_3.thumb.jpg"))) %>%
      dplyr::select(strain_name, species_id, c_label, sample_photo1, sample_photo2, sample_photo3, orig_file_name:thumb_file_name3, isolation_photo) %>%
      tidyr::pivot_longer(cols = sample_photo1:sample_photo3, names_to = "photo", values_to = "photo_hash") %>%
      dplyr::group_by(strain_name) %>%
      dplyr::mutate(n_sample_photos = sum(!is.na(photo_hash))) %>%
      dplyr::ungroup() %>%
      tidyr::pivot_wider(names_from = "photo", values_from = "photo_hash") %>%
      dplyr::mutate(orig_file_name4 = glue::glue("{dir_photos}/{isolation_photo}.jpg"),
                    new_file_name4 = case_when(species_id %in% c("C. briggsae", "Caenorhabditis briggsae") ~ glue::glue("{processed_dir}/CeaNDR/Cb/{strain_name}_{1+n_sample_photos}.jpg"),
                                               species_id %in% c("C. elegans", "Caenorhabditis elegans") ~ glue::glue("{processed_dir}/CeaNDR/Ce/{strain_name}_{1+n_sample_photos}.jpg"),
                                               species_id %in% c("C. tropicalis", "Caenorhabditis tropicalis") ~ glue::glue("{processed_dir}/CeaNDR/Ct/{strain_name}_{1+n_sample_photos}.jpg")),
                    thumb_file_name4 = case_when(species_id %in% c("C. briggsae", "Caenorhabditis briggsae") ~ glue::glue("{processed_dir}/CeaNDR/Cb/{strain_name}_{1+n_sample_photos}.thumb.jpg"),
                                                 species_id %in% c("C. elegans", "Caenorhabditis elegans") ~ glue::glue("{processed_dir}/CeaNDR/Ce/{strain_name}_{1+n_sample_photos}.thumb.jpg"),
                                                 species_id %in% c("C. tropicalis", "Caenorhabditis tropicalis") ~ glue::glue("{processed_dir}/CeaNDR/Ct/{strain_name}_{1+n_sample_photos}.thumb.jpg"))) %>%
      dplyr::select(strain_name, species_id, c_label, sample_photo1, sample_photo2, sample_photo3, orig_file_name:thumb_file_name4)

    # check for files
    should.exist.df <- to_change %>%
      dplyr::group_by(c_label) %>%
      dplyr::mutate(strain = paste(strain_name, collapse = " | ")) %>%
      dplyr::ungroup() %>%
      dplyr::distinct(c_label, .keep_all = T) %>%
      dplyr::select(strain, c_label, sample_photo1, sample_photo2, sample_photo3, isolation_photo) %>%
      tidyr::pivot_longer(col = sample_photo1:isolation_photo, names_to = "photo", values_to = "hash") %>%
      dplyr::filter(!is.na(hash))

    exist.df <- tibble::tibble(files = list.files(dir_photos)) %>%
      tidyr::separate(files, into = "hash", extra = "drop", sep = ".jp", remove = FALSE)

    test.exist.df <- dplyr::left_join(should.exist.df, exist.df, by = "hash") %>%
      dplyr::mutate(expected_photo_n = n(),
                    missing_photo_n = sum(is.na(files)))

    flag.exist.df <- test.exist.df %>%
      dplyr::mutate(flag_photo_missing = ifelse(is.na(files), TRUE, FALSE)) %>%
      dplyr::filter(flag_photo_missing == TRUE) %>%
      dplyr::mutate(missing_file = paste0(hash, ".jpg"))

    # Warning for missing photos CaeNDR
    if(unique(test.exist.df$missing_photo_n) > 0){
      # send a warning
      message(glue::glue("Warning:
                         CaeNDR processing is missing {unique(test.exist.df$missing_photo_n)} of {unique(test.exist.df$expected_photo_n)} .jpg photos expected to be in:
                         {dir_photos}
                         A missing photos report can be found in {dir}/reports/{as.character(Sys.Date())}_CaeNDR_missing_photos_report.csv"))

      # make report
      write.csv(flag.exist.df %>% select(1:4,missing_file), file = glue::glue("{dir}/reports/{as.character(Sys.Date())}_CaeNDR_missing_photos_report.csv"))

      # allow user to quite or continue
      continue = readline(prompt = glue::glue("Do you want to continue processing exisiting CaeNDR photos, enter yes or no : "))

      # user entry logic
      if(stringr::str_detect(continue, regex('y', ignore_case = T))){

        # filter the to_change data to only the existing files and process those
      missing_hashes <- paste(flag.exist.df$hash, collapse = "|")
      to_change <- to_change %>%
        mutate(across(c(sample_photo1, sample_photo2, sample_photo3, isolation_photo),
                      ~ stringr::str_replace(., pattern = regex(missing_hashes), replacement = NA_character_)))

      } else {
        # send quit message
        message("quiting, no photos have been processed.")
        return()
      }

    } else {
      # give the good news
      message(glue::glue("CaeNDR processing found all expected .jpg photos in {dir_photos}"))
    }

    to_change1 <- to_change %>%
      dplyr::filter(!is.na(sample_photo1))

    to_change2 <- to_change %>%
      dplyr::filter(!is.na(sample_photo2))

    to_change3 <- to_change %>%
      dplyr::filter(!is.na(sample_photo3))

    to_change4 <- to_change %>%
      dplyr::filter(!is.na(isolation_photo))

    # make CaeNDR subdirectories in dir
    fs::dir_create(glue::glue("{processed_dir}/CeaNDR/Cb"))
    fs::dir_create(glue::glue("{processed_dir}/CeaNDR/Ce"))
    fs::dir_create(glue::glue("{processed_dir}/CeaNDR/Ct"))

    # copy files to new directory and rename
    fs::file_copy(to_change1$orig_file_name, to_change1$new_file_name, overwrite = overwrite)
    fs::file_copy(to_change2$orig_file_name2, to_change2$new_file_name2, overwrite = overwrite)
    fs::file_copy(to_change3$orig_file_name3, to_change3$new_file_name3, overwrite = overwrite)
    fs::file_copy(to_change4$orig_file_name4, to_change4$new_file_name4, overwrite = overwrite)

    # make a nice CaeNDR progress bar
    caendr_pb <- progress::progress_bar$new(total = sum(length(unique(to_change1$new_file_name)),
                                                        length(unique(to_change2$new_file_name)),
                                                        length(unique(to_change3$new_file_name)),
                                                        length(unique(to_change4$new_file_name))),
                                            format = "CaeNDR processing [:bar] :percent eta: :eta",
                                            clear = FALSE)
    # loop through renamed images to make thumbnails
    for(i in unique(to_change1$new_file_name)) {
      # setup image in R
      img <- imager::load.image(i)
      # get raw img dimesions
      raw_max_dim <- max(dim(img))
      percentage <- 100*(max_dim/raw_max_dim)

      # resize to make thumbnail
      thumb <- imager::resize(img, -percentage, -percentage) # need negative for resize function

      # write the file
      imager::save.image(thumb, file = glue::glue("{to_change1 %>% dplyr::filter(new_file_name == i) %>% dplyr::pull(thumb_file_name)}"))

      # Update the progress bar
      caendr_pb$tick()
    }

    for(i in unique(to_change2$new_file_name2)) {
      # setup image in R
      img <- imager::load.image(i)
      # get raw img dimesions
      raw_max_dim <- max(dim(img))
      percentage <- 100*(max_dim/raw_max_dim)

      # resize to make thumbnail
      thumb <- imager::resize(img, -percentage, -percentage) # need negative for resize function

      # write the file
      imager::save.image(thumb, file = glue::glue("{to_change %>% dplyr::filter(new_file_name2 == i) %>% dplyr::pull(thumb_file_name2)}"))

      # Update the progress bar
      caendr_pb$tick()
    }

    for(i in unique(to_change3$new_file_name3)) {
      # setup image in R
      img <- imager::load.image(i)
      # get raw img dimesions
      raw_max_dim <- max(dim(img))
      percentage <- 100*(max_dim/raw_max_dim)

      # resize to make thumbnail
      thumb <- imager::resize(img, -percentage, -percentage) # need negative for resize function

      # write the file
      imager::save.image(thumb, file = glue::glue("{to_change %>% dplyr::filter(new_file_name3 == i) %>% dplyr::pull(thumb_file_name3)}"))

      # Update the progress bar
      caendr_pb$tick()
    }

    for(i in unique(to_change4$new_file_name4)) {
      # setup image in R
      img <- imager::load.image(i)
      # get raw img dimesions
      raw_max_dim <- max(dim(img))
      percentage <- 100*(max_dim/raw_max_dim)

      # resize to make thumbnail
      thumb <- imager::resize(img, -percentage, -percentage) # need negative for resize function

      # write the file
      imager::save.image(thumb, file = glue::glue("{to_change %>% dplyr::filter(new_file_name4 == i) %>% dplyr::pull(thumb_file_name4)}"))

      # Update the progress bar
      caendr_pb$tick()
    }
  }

  #============================================================================#
  # This section will process images for the project report                    #
  #============================================================================#
  # copy all images create folder of these images and thumbnails
  to_change_clabel <- data %>%
    dplyr::filter(!is.na(c_label)) %>%
    dplyr::distinct(c_label, .keep_all = TRUE) %>%
    dplyr::mutate(orig_file_name = glue::glue("{dir_photos}/{sample_photo1}.jpg"),
                  new_file_name = glue::glue("{processed_dir}/C_labels/{c_label}.jpg"),
                  thumb_file_name = glue::glue("{processed_dir}/C_labels/thumbnails/{c_label}.jpg")) %>%
    dplyr::mutate(orig_file_name2 = glue::glue("{dir_photos}/{sample_photo2}.jpg"),
                  new_file_name2 = glue::glue("{processed_dir}/C_labels/{c_label}_2.jpg"),
                  thumb_file_name2 = glue::glue("{processed_dir}/C_labels/thumbnails/{c_label}_2.jpg")) %>%
    dplyr::mutate(orig_file_name3 = glue::glue("{dir_photos}/{sample_photo3}.jpg"),
                  new_file_name3 = glue::glue("{processed_dir}/C_labels/{c_label}_3.jpg"),
                  thumb_file_name3 = glue::glue("{processed_dir}/C_labels/thumbnails/{c_label}_3.jpg")) %>%
    dplyr::select(strain_name, species_id, c_label, sample_photo1, sample_photo2, sample_photo3, orig_file_name:thumb_file_name3)

  # check for files
  should.exist.clabel.df <- to_change_clabel %>%
    dplyr::select(c_label, sample_photo1, sample_photo2, sample_photo3) %>%
    tidyr::pivot_longer(col = sample_photo1:sample_photo3, names_to = "photo", values_to = "hash") %>%
    dplyr::filter(!is.na(hash))

  exist.df <- tibble::tibble(files = list.files(dir_photos)) %>%
    tidyr::separate(files, into = "hash", extra = "drop", sep = ".jp", remove = FALSE)

  test.exist.clabel.df <- dplyr::left_join(should.exist.clabel.df, exist.df, by = "hash") %>%
    dplyr::mutate(expected_photo_n = n(),
                  missing_photo_n = sum(is.na(files)))

  flag.exist.clabel.df <- test.exist.clabel.df %>%
    dplyr::mutate(flag_photo_missing = ifelse(is.na(files), TRUE, FALSE)) %>%
    dplyr::filter(flag_photo_missing == TRUE) %>%
    dplyr::mutate(missing_file = paste0(hash, ".jpg"))

  # Warning for missing photos CaeNDR
  if(unique(test.exist.clabel.df$missing_photo_n) > 0){
    # send a warning
    message(glue::glue("Warning:
                         C-label processing is missing {unique(test.exist.clabel.df$missing_photo_n)} of {unique(test.exist.clabel.df$expected_photo_n)} .jpg photos expected to be in:
                         {dir_photos}
                         A missing photos report can be found in {dir}/reports/{as.character(Sys.Date())}_c_lablel_missing_photos_report.csv"))

    # make report
    write.csv(flag.exist.clabel.df %>% select(1:4,missing_file), file = glue::glue("{dir}/reports/{as.character(Sys.Date())}_c_label_missing_photos_report.csv"))

    # allow user to quite or continue
    continue = readline(prompt = glue::glue("Do you want to continue processing exisiting C-label photos, enter yes or no : "))

    # user entry logic
    if(stringr::str_detect(continue, regex('y', ignore_case = T))){

      # filter the to_change data to only the existing files and process those
      missing_hashes_clabel <- paste(flag.exist.clabel.df$hash, collapse = "|")
      to_change_clabel <- to_change_clabel %>%
        mutate(across(c(sample_photo1, sample_photo2, sample_photo3),
                      ~ stringr::str_replace(., pattern = regex(missing_hashes_clabel), replacement = NA_character_)))

    } else {
      # send quit message
      message("quiting")
      return()
    }
  } else {
    # give the good news
    message(glue::glue("C-label processing found all expected .jpg photos in {dir_photos}"))
  }

  to_change1_clabel <- to_change_clabel %>%
    dplyr::filter(!is.na(sample_photo1))

  to_change2_clabel <- to_change_clabel %>%
    dplyr::filter(!is.na(sample_photo2))

  to_change3_clabel <- to_change_clabel %>%
    dplyr::filter(!is.na(sample_photo3))

  # make C_label directories
  fs::dir_create(glue::glue("{processed_dir}/C_labels/full"))
  fs::dir_create(glue::glue("{processed_dir}/C_labels/thumbnails"))

  # copy files to new directory and rename
  fs::file_copy(to_change1_clabel$orig_file_name, to_change1_clabel$new_file_name, overwrite = overwrite)
  fs::file_copy(to_change2_clabel$orig_file_name2, to_change2_clabel$new_file_name2, overwrite = overwrite)
  fs::file_copy(to_change3_clabel$orig_file_name3, to_change3_clabel$new_file_name3, overwrite = overwrite)

  # make a nice C-label progress bar
  clabel_pb <- progress::progress_bar$new(total = sum(length(unique(to_change1_clabel$new_file_name)),
                                                      length(unique(to_change2_clabel$new_file_name)),
                                                      length(unique(to_change3_clabel$new_file_name))),
                                          format = "C-label processing [:bar] :percent eta: :eta",
                                          clear = FALSE)

  # loop through renamed images to make thumbnails
  for(i in unique(to_change1_clabel$new_file_name)) {
    # setup image in R
    img <- imager::load.image(i)
    # get raw img dimesions
    raw_max_dim <- max(dim(img))
    percentage <- 100*(max_dim/raw_max_dim)

    # resize to make thumbnail
    thumb <- imager::resize(img, -percentage, -percentage) # need negative for resize function

    # write the file
    imager::save.image(thumb, file = glue::glue("{to_change1_clabel %>% dplyr::filter(new_file_name == i) %>% dplyr::pull(thumb_file_name)}"))

    clabel_pb$tick()
  }

  for(i in unique(to_change2_clabel$new_file_name2)) {
    # setup image in R
    img <- imager::load.image(i)
    # get raw img dimesions
    raw_max_dim <- max(dim(img))
    percentage <- 100*(max_dim/raw_max_dim)

    # resize to make thumbnail
    thumb <- imager::resize(img, -percentage, -percentage) # need negative for resize function

    # write the file
    imager::save.image(thumb, file = glue::glue("{to_change_clabel %>% dplyr::filter(new_file_name2 == i) %>% dplyr::pull(thumb_file_name2)}"))

    clabel_pb$tick()
  }

  for(i in unique(to_change3_clabel$new_file_name3)) {
    # setup image in R
    img <- imager::load.image(i)
    # get raw img dimesions
    raw_max_dim <- max(dim(img))
    percentage <- 100*(max_dim/raw_max_dim)

    # resize to make thumbnail
    thumb <- imager::resize(img, -percentage, -percentage) # need negative for resize function

    # write the file
    imager::save.image(thumb, file = glue::glue("{to_change_clabel %>% dplyr::filter(new_file_name3 == i) %>% dplyr::pull(thumb_file_name3)}"))

    clabel_pb$tick()
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
  thumb_hash <- dplyr::as_tibble(as.list(tools::md5sum(fs::dir_ls(glue::glue("{processed_dir}/C_labels/thumbnails"))))) %>%
    tidyr::gather(key = thumb_file, value = sample_photo_resized_hash) %>%
    dplyr::mutate(sample_photo_resized_file_name = stringr::str_extract(thumb_file, pattern = ALPHA %R% optional("-") %R% one_or_more(DGT) %R% optional("_") %R% one_or_more(DGT) %R% ".jpg"),
                  c_label = stringr::str_extract(sample_photo_resized_file_name, pattern = ALPHA %R% optional("-") %R% one_or_more(DGT)),
                  sample_photo_processed_file_name = sample_photo_resized_file_name)

  # get hash 1
  thumb_hash1 <- thumb_hash %>%
    dplyr::filter(c_label %in% to_change1_clabel$c_label &
                    !(grepl("_2", thumb_hash$sample_photo_resized_file_name)) &
                    !(grepl("_3", thumb_hash$sample_photo_resized_file_name)))
  # get hash 2
  thumb_hash2 <- thumb_hash %>%
    dplyr::filter(c_label %in% to_change2_clabel$c_label &
                    grepl("_2", thumb_hash$sample_photo_resized_file_name))
  # get hash 3
  thumb_hash3 <- thumb_hash %>%
    dplyr::filter(c_label %in% to_change3_clabel$c_label &
                    grepl("_3", thumb_hash$sample_photo_resized_file_name))

  # join hash to data frame
  data_out <- data %>%
    dplyr::left_join(dplyr::select(to_change1_clabel, sample_photo1, orig_file_name:thumb_file_name), by = c("sample_photo1" = "sample_photo1")) %>%
    dplyr::left_join(dplyr::select(to_change2_clabel, sample_photo2, orig_file_name2:thumb_file_name2), by = c("sample_photo2" = "sample_photo2")) %>%
    dplyr::left_join(dplyr::select(to_change3_clabel, sample_photo3, orig_file_name3:thumb_file_name3), by = c("sample_photo3" = "sample_photo3")) %>%
    dplyr::mutate(sample_photo1_processed_url = case_when(!is.na(sample_photo1) ~ glue::glue("{project_url}{c_label}.jpg"),
                                                          TRUE ~ NA_character_),
                  sample_photo2_processed_url = case_when(!is.na(sample_photo2) ~ glue::glue("{project_url}{c_label}_2.jpg"),
                                                          TRUE ~ NA_character_),
                  sample_photo3_processed_url = case_when(!is.na(sample_photo3) ~ glue::glue("{project_url}{c_label}_3.jpg"),
                                                          TRUE ~ NA_character_)) %>%
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
    dplyr::select(project:sample_photo1, sample_photo1_raw_file_name, sample_photo1_processed_file_name, sample_photo1_processed_url, sample_photo1_hash, sample_photo1_resized_file_name, sample_photo1_resized_hash,
                sample_photo2, sample_photo2_raw_file_name, sample_photo2_processed_file_name, sample_photo2_processed_url, sample_photo2_hash, sample_photo2_resized_file_name, sample_photo2_resized_hash,
               sample_photo3, sample_photo3_raw_file_name, sample_photo3_processed_file_name, sample_photo3_processed_url, sample_photo3_hash, sample_photo3_resized_file_name, sample_photo3_resized_hash,
              everything())

  # return
  message(glue::glue("DONE -- saving data to .rds file {dir}/data/processed/fulcrum/",
                     as.character(Sys.Date()),"_",
                     tail(strsplit(dir,"/")[[1]],1),"fulcrum.rds"))
  if(is.character(dir)){
    saveRDS(object = data_out,
            file = glue::glue("{dir}","/data/processed/fulcrum/",
                              as.character(Sys.Date()),"_",
                              tail(strsplit(dir,"/")[[1]],1),"_fulcrum.rds"), version = 3)}
  return(data_out)
}
