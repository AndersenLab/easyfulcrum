#' filter_box
#'
#' \code{filter_box} finds lat and long inside bounding box
#'
#' @param longitude longitude of collection
#' @param latitude latitude of collection
#' @param coords coordinates of bounding box
#'

filter_box <- function(longitude, latitude, coords) {
  between(longitude, coords[1], coords[3]) &
    between(latitude, coords[2], coords[4]) &
    !is.na(longitude)
}

#' FtoC
#'
#' \code{FtoC} Converts fahrenheit measurement to celsius
#'
#' @param F fahrenheit measurement to convert
#'

FtoC <- function(F) {
   (F - 32) * (5 / 9)
}

#' loadFulcrum
#'
#' \code{loadFulcrum} loads .csv files exported from Fulcrum into R and names variables correctly
#'
#' @param dir The path of the directory with five Fulcrum .csv files:
#' nematode_field_sampling.csv,
#' nematode_field_sampling_sample_photo.csv,
#' nematode_isolation.csv,
#' nematode_isolation_s_labeled_plates.csv,
#' nematode_isolation_photos.csv
#' @return five named data frames generated from the .csv files.
#' Each data frame has a shortened name.
#' \tabular{ll}{
#' collection \tab nematode_field_sampling.csv\cr
#' collection_photo \tab nematode_field_sampling_sample_photo.csv\cr
#' isolation \tab nematode_isolation.csv\cr
#' isolation_slab \tab nematode_isolation_s_labeled_plates.csv\cr
#' isolation_photo \tab nematode_isolation_photos.csv\cr
#' }
#' @export

loadFulcrum <- function(dir) {

  # Read collection data
  collection <<- readr::read_csv(glue::glue("{dir}/nematode_field_sampling.csv")) %>%
    dplyr::mutate(c_label = stringr::str_to_upper(c_label)) %>%
    # name created_by to specify who picked up the sample
    dplyr::rename(collection_by = created_by) %>%
    dplyr::select(-updated_at,
                  -system_created_at,
                  -system_updated_at,
                  -date) %>%
    # choose one sample photo only. This takes the first sample photo and warns if additional photos are discarded
    tidyr::separate(col = sample_photo, into = "sample_photo", sep = ",", extra = "warn") %>%
    # this is UTC time (very important if you want to convert to local time)
    dplyr::mutate(collection_datetime_UTC = lubridate::ymd_hms(created_at, tz = "UTC")) %>%
    # again this is UTC date (very important if you want to convert to local date)
    dplyr::mutate(collection_date_UTC = lubridate::date(created_at)) %>%
    dplyr::select(-created_at) %>%
    # Fix Fahrenheit observations
    dplyr::mutate(substrate_temperature = ifelse(substrate_temperature > 40,
                                                 FtoC(substrate_temperature),
                                                 substrate_temperature)) %>%
    # Fix ambient temp F to C
    dplyr::mutate(ambient_temperature = ifelse(ambient_temperature_c > 40,
                                               FtoC(ambient_temperature_c),
                                               ambient_temperature_c)) %>%
    # force ambient temp to numeric
    dplyr::mutate(ambient_temperature = as.numeric(ambient_temperature)) %>%
    # drop ambient temp c
    dplyr::select(-ambient_temperature_c) %>%
    # add flags for runs of temperature data
    dplyr::arrange(collection_datetime_UTC) %>%
    dplyr::mutate(flag_ambient_temperature_run = (ambient_humidity == dplyr::lag(ambient_humidity)) &
                    (ambient_temperature == dplyr::lag(ambient_temperature))
                  & (gridsect == "no"))

  # Read collection photo position data (exif)
  collection_photo <<- readr::read_csv(glue::glue("{dir}/nematode_field_sampling_sample_photo.csv")) %>%
    dplyr::select(fulcrum_id, exif_gps_latitude, exif_gps_longitude, exif_gps_altitude)

  # Read isolation data
  isolation <<- readr::read_csv(glue::glue("{dir}/nematode_isolation.csv")) %>%
    dplyr::select(c_label_id = c_label,
                  isolation_id = fulcrum_id,
                  isolation_datetime_UTC = system_created_at,
                  isolation_by = created_by,
                  worms_on_sample,
                  approximate_number_of_worms,
                  isolation_date_UTC = date,
                  isolation_local_time = time, # Is this actually local time? or is it UTC?
                  isolation_latitude = latitude,
                  isolation_longitude = longitude)

  # Read S-plate data
  isolation_slab <<- readr::read_csv(glue::glue("{dir}/nematode_isolation_s_labeled_plates.csv")) %>%
    dplyr::select(fulcrum_parent_id, s_label)

  isolation_photo <<- readr::read_csv(glue::glue("{dir}/nematode_isolation_photos.csv"))
}

#' procFulcrum
#'
#' \code{procFulcrum} loads Fulcrum .csv files, joins collection and isolation data, and adds collection variables
#'
#' @param dir The path of the directory with five Fulcrum .csv files:
#' nematode_field_sampling.csv,
#' nematode_field_sampling_sample_photo.csv,
#' nematode_isolation.csv,
#' nematode_isolation_s_labeled_plates.csv,
#' nematode_isolation_photos.csv
#' @return A dataframe generated from the .csv files.
#' This data frame contains all necessary variables from Fulcrum. It also contains data quality flags. The variable names match the data dictionary.
#' @export

procFulcrum <- function(dir) {

  # Read collection data
  collection <- readr::read_csv(glue::glue("{dir}/nematode_field_sampling.csv")) %>%
    dplyr::mutate(c_label = stringr::str_to_upper(c_label)) %>%
    # name created_by to specify who picked up the sample
    dplyr::rename(collection_by = created_by) %>%
    dplyr::select(-updated_at,
                  -system_created_at,
                  -system_updated_at,
                  -date) %>%
    # choose one sample photo only. This takes the first sample photo and warns if additional photos are discarded
    tidyr::separate(col = sample_photo, into = "sample_photo", sep = ",", extra = "warn") %>%
    # this is UTC time (very important if you want to convert to local time)
    dplyr::mutate(collection_datetime_UTC = lubridate::ymd_hms(created_at, tz = "UTC")) %>%
    # again this is UTC date (very important if you want to convert to local date)
    dplyr::mutate(collection_date_UTC = lubridate::date(created_at)) %>%
    dplyr::select(-created_at) %>%
    # Fix Fahrenheit observations
    dplyr::mutate(substrate_temperature = ifelse(substrate_temperature > 40,
                                                 FtoC(substrate_temperature),
                                                 substrate_temperature)) %>%
    # Fix ambient temp F to C
    dplyr::mutate(ambient_temperature = ifelse(ambient_temperature_c > 40,
                                               FtoC(ambient_temperature_c),
                                               ambient_temperature_c)) %>%
    # force ambient temp to numeric
    dplyr::mutate(ambient_temperature = as.numeric(ambient_temperature)) %>%
    # drop ambient temp c
    dplyr::select(-ambient_temperature_c) %>%
    # add flags for runs of temperature data
    dplyr::arrange(collection_datetime_UTC) %>%
    dplyr::mutate(flag_ambient_temperature_run = (ambient_humidity == dplyr::lag(ambient_humidity)) &
                    (ambient_temperature == dplyr::lag(ambient_temperature))
                  & (gridsect == "no"))

  # Read collection photo position data (exif)
  collection_photo <- readr::read_csv(glue::glue("{dir}/nematode_field_sampling_sample_photo.csv")) %>%
    dplyr::select(fulcrum_id, exif_gps_latitude, exif_gps_longitude, exif_gps_altitude)

  # Read isolation data
  isolation <- readr::read_csv(glue::glue("{dir}/nematode_isolation.csv")) %>%
    dplyr::select(c_label_id = c_label,
                  isolation_id = fulcrum_id,
                  isolation_datetime_UTC = system_created_at,
                  isolation_by = created_by,
                  worms_on_sample,
                  approximate_number_of_worms,
                  isolation_date_UTC = date,
                  isolation_local_time = time, # Is this actually local time? or is it UTC?
                  isolation_latitude = latitude,
                  isolation_longitude = longitude)

  # Read S-plate data
  isolation_slab <- readr::read_csv(glue::glue("{dir}/nematode_isolation_s_labeled_plates.csv")) %>%
    dplyr::select(fulcrum_parent_id, s_label)

  #prevent scientific notation
  options(scipen = 999)

  # join collection, isolation, and location data
  joined_data <- dplyr::full_join(isolation, collection, by = c("c_label_id" = "fulcrum_id")) %>%
    #rename the lat and long from fulcrum to collection_fulcrum_latitude and collection_fulcrum_longitude so that we can specify lat and long from exif tool
    dplyr::rename(collection_fulcrum_latitude = latitude, collection_fulcrum_longitude = longitude) %>%
    dplyr::select(c_label,
                  everything(),
                  -c_label_id) %>%
    # Join position data from exif by sample_photo. in some cases there is not position data from the photos
    dplyr::left_join(collection_photo, by = c("sample_photo" = "fulcrum_id")) %>%
    # Create flag to track if lat and long come from record or photo
    dplyr::mutate(collection_lat_long_method = ifelse(is.na(exif_gps_latitude), "fulcrum", "photo")) %>%
    # In cases where lat/lon are not available from photo set to collection_fulcrum_latitude and collection_fulcrum_longitude
    dplyr::mutate(latitude = ifelse(is.na(exif_gps_latitude), collection_fulcrum_latitude, exif_gps_latitude)) %>%
    dplyr::mutate(longitude = ifelse(is.na(exif_gps_longitude), collection_fulcrum_longitude, exif_gps_longitude)) %>%
    dplyr::rename(fulcrum_altitude = gps_altitude) %>%
    dplyr::mutate(worms_on_sample = ifelse(is.na(worms_on_sample), "?", worms_on_sample)) %>%
    dplyr::filter(!is.na(c_label)) %>%
    # Calculate the Haversine distance between fulcrum record_latitude and record_longitue and photo latitude and longitude
    dplyr::rowwise() %>%
    dplyr::mutate(collection_lat_long_method_diff = geosphere::distHaversine(c(longitude, latitude),
                                                                             c(collection_fulcrum_longitude, collection_fulcrum_latitude)),
                  # adjust collection_lat_long_method_diff to NA if there is only a fulcrum GPS postion
                  collection_lat_long_method_diff = ifelse(collection_lat_long_method == "fulcrum", NA, collection_lat_long_method_diff)) %>%
    # fix altitude method and altitude
    dplyr::mutate(altitude = ifelse(collection_lat_long_method == "photo" & !(is.na(exif_gps_altitude)), exif_gps_altitude,
                                    ifelse(is.na(exif_gps_altitude) & !(is.na(fulcrum_altitude)), fulcrum_altitude,
                                           ifelse(is.na(exif_gps_altitude) & is.na(fulcrum_altitude), NA))),
                  altitude_method = ifelse(collection_lat_long_method == "photo" & !(is.na(exif_gps_altitude)), "photo",
                                           ifelse(is.na(exif_gps_altitude) & !(is.na(fulcrum_altitude)), "fulcrum",
                                                  ifelse(is.na(exif_gps_altitude) & is.na(fulcrum_altitude), NA, NA)))) %>%
    # rename the latitude and longitude to include 'collection_' prefix
    dplyr::ungroup() %>%
    dplyr::rename(collection_latitude = latitude,
                  collection_longitude = longitude,
                  collection_local_time = time) %>%
    # join c-plates to s-plates
    dplyr::full_join(isolation_slab, .,  by = c("fulcrum_parent_id" = "isolation_id")) %>%
    dplyr::select(-fulcrum_parent_id, -updated_by, -version, -geometry)

  # Create Island Column
  joined_data$collection_island <- NA_character_
  joined_data[filter_box(joined_data$collection_longitude, joined_data$collection_latitude, c(-158.3617, 21.1968, -157.5117, 21.7931)), "collection_island"] <- "Oahu"
  joined_data[filter_box(joined_data$collection_longitude, joined_data$collection_latitude, c(-159.9362, 21.6523, -159.1782, 22.472)), "collection_island"] <- "Kauai"
  joined_data[filter_box(joined_data$collection_longitude, joined_data$collection_latitude, c(-157.327, 21.0328, -156.685, 21.2574)), "collection_island"] <- "Molokai"
  joined_data[filter_box(joined_data$collection_longitude, joined_data$collection_latitude, c(-156.7061, 20.4712, -155.9289, 21.0743)), "collection_island"] <- "Maui"
  joined_data[filter_box(joined_data$collection_longitude, joined_data$collection_latitude, c(-156.1346, 18.6619, -154.6985, 20.4492)), "collection_island"] <- "Big Island"

  # Create location Column
  joined_data$collection_location <- NA_character_
  joined_data[filter_box(joined_data$collection_longitude, joined_data$collection_latitude, c(-155.255595, 19.410322, -155.206282, 19.461089)), "collection_location"] <- "Volcano, HI"
  joined_data[filter_box(joined_data$collection_longitude, joined_data$collection_latitude, c(-157.72537, 21.303309, -157.71919, 21.32122)), "collection_location"] <- "Kuliouou Ridge Trail"
  joined_data[filter_box(joined_data$collection_longitude, joined_data$collection_latitude, c(-159.6528864026, 22.1270612021, -159.6375305307, 22.1387999576)), "collection_location"] <- "Pu'u Ka Ohelo Berry Flat Trail"
  joined_data[filter_box(joined_data$collection_longitude, joined_data$collection_latitude, c(-158.0192352613, 21.5014265529, -158.0145925283, 21.5041245046)), "collection_location"] <- "Wahiawa Botanical Garden"
  joined_data[filter_box(joined_data$collection_longitude, joined_data$collection_latitude, c(-157.8598800302, 21.3149311581, -157.855797708, 21.3182194587)), "collection_location"] <- "Foster Community Garden"
  joined_data[filter_box(joined_data$collection_longitude, joined_data$collection_latitude, c(-157.7829487403, 21.3569863645, -157.7752268314, 21.3655295525)), "collection_location"] <- "Maunawili Demonstration Trail"
  joined_data[filter_box(joined_data$collection_longitude, joined_data$collection_latitude, c(-157.8014534712, 21.3322593, -157.798127532, 21.3427719396)), "collection_location"] <- "Manoa Falls Trail"
  joined_data[filter_box(joined_data$collection_longitude, joined_data$collection_latitude, c(-157.8135502338, 21.3779082884, -157.7915561199, 21.3970691079)), "collection_location"] <- "Ho'omaluhia Botanical Garden"
  joined_data[filter_box(joined_data$collection_longitude, joined_data$collection_latitude, c(-159.613624, 22.167098, -159.575601, 22.226422)), "collection_location"] <- "Na Pali Coast State Wilderness Park"

  # Generate list of trails and geojson polygon points from geojson output of https://boundingbox.klokantech.com/.
  # These polygons are manually curated by using the polygon tool.
  trails <- list("Awa'awapuhi Trail" = "[-159.6488926467,22.1402223841],[-159.6532089915,22.1412257815],[-159.656343488,22.1420138052],[-159.6650299896,22.1463072536],[-159.6725416835,22.1492227365],[-159.6773686539,22.1507348759],[-159.6798696462,22.1518023226],[-159.6818598453,22.1533145895],[-159.6820020024,22.1540193252],[-159.6809810866,22.1541002166],[-159.6723866183,22.1519254472],[-159.6573152859,22.1456304282],[-159.648068035,22.1412422407],[-159.6488926467,22.1402223841]",
                 "Pu'u Ka Ohelo Berry Flat Trail" = "[-159.6464149561,22.1279373294],[-159.6464691032,22.1306906065],[-159.6478631813,22.1337821946],[-159.652014235,22.1315567984],[-159.6519369539,22.1323409165],[-159.6519060247,22.1332212292],[-159.648132408,22.136480155],[-159.6419800911,22.1382322604],[-159.639415564,22.1374220881],[-159.6382828336,22.1306384299],[-159.6423470508,22.1293568345],[-159.6464149561,22.1279373294]",
                 "Aiea Loop Trail" = "[-157.9036597069,21.3911540403],[-157.9032877181,21.3961755202],[-157.9006033298,21.3986954823],[-157.896204507,21.4022200787],[-157.8944614064,21.4032046156],[-157.8827647958,21.4059746435],[-157.8808381315,21.4035239487],[-157.8839969356,21.4007159471],[-157.8972629737,21.3924647207],[-157.9036597069,21.3911540403]",
                 "Ualaka`a Trail" = "[-157.819537418,21.315974292],[-157.8210766707,21.3162514153],[-157.821027888,21.3180327534],[-157.8204963915,21.3199065201],[-157.8204103094,21.3214570035],[-157.8195493203,21.3216686057],[-157.8190909978,21.3211096942],[-157.8183983173,21.3227361381],[-157.8169985395,21.3239863716],[-157.8155018669,21.3245350437],[-157.8139766958,21.3248923392],[-157.8137935512,21.3266497583],[-157.8121093754,21.3282114919],[-157.81050466,21.3288251092],[-157.8102066834,21.3262350786],[-157.8169378545,21.3223209008],[-157.8174900543,21.3200495678],[-157.8174123541,21.317614926],[-157.8189980425,21.3167357746],[-157.8190004732,21.3163345756],[-157.819537418,21.315974292]",
                 "Wa'ahila Ridge Trail" = "[-157.7976712119,21.3056033836],[-157.7987690736,21.3057559723],[-157.7980324719,21.3084880175],[-157.7944368031,21.3144206272],[-157.7901313547,21.3208797418],[-157.7878303546,21.3269979915],[-157.7816081326,21.3348734629],[-157.7798913512,21.3357678864],[-157.7770589385,21.3357519592],[-157.7820592467,21.3306206609],[-157.7863646951,21.3232067337],[-157.7895887103,21.3147499927],[-157.7944440115,21.3088634687],[-157.7976712119,21.3056033836]",
                 "Wiliwilinui Ridge Trail" = "[-157.7610354219,21.2980014303],[-157.7636727039,21.2980178301],[-157.7656396013,21.3032706853],[-157.7655542735,21.3067857077],[-157.7571375016,21.324271483],[-157.7542390395,21.3277997446],[-157.7481931727,21.327357194],[-157.7475859877,21.326016573],[-157.7508203965,21.317912309],[-157.7549794968,21.3108024385],[-157.7585446555,21.30330364],[-157.7603906859,21.3009099537],[-157.7610354219,21.2980014303]",
                 "Kalopa State Recreation Area" = "[-155.4445436339,20.0258025884],[-155.453435995,20.0282838619],[-155.4542096446,20.032764671],[-155.4505063522,20.0361599209],[-155.4452068101,20.041061784],[-155.4429866116,20.0424226244],[-155.4317761513,20.0372755906],[-155.4366200532,20.0294838493],[-155.4445436339,20.0258025884]",
                 "Kipuka Puaulu Trail" = "[-155.3014187816,19.4369739997],[-155.3037875074,19.4375076905],[-155.3063687983,19.439252133],[-155.3066034916,19.442442035],[-155.3056325319,19.4446149707],[-155.3023801858,19.4453876473],[-155.2996805425,19.4445240766],[-155.2984629872,19.4429099473],[-155.2977664511,19.4410944082],[-155.2980630028,19.4391639233],[-155.2992270815,19.4379103288],[-155.3014187816,19.4369739997]",
                 "Manuka Nature Trail" = "[-155.8248741667,19.1080109154],[-155.8269825506,19.1095439203],[-155.8271397951,19.1100389204],[-155.827794757,19.1150164005],[-155.8279053981,19.1171844974],[-155.8258975971,19.1192328223],[-155.8215480599,19.122471411],[-155.8204518746,19.122596379],[-155.8179482001,19.1211388884],[-155.8166575546,19.1174307983],[-155.8166456523,19.1151944365],[-155.8232769112,19.1102486416],[-155.8248741667,19.1080109154]",
                 "Kaloko Loop Trial" = "[-155.9489068876,19.7179298978],[-155.9554239011,19.7181607717],[-155.9569940831,19.7189532045],[-155.956992239,19.7236042001],[-155.9479893207,19.7263051335],[-155.9465989306,19.7213376595],[-155.946744608,19.7198175933],[-155.9471745158,19.7191565394],[-155.9489068876,19.7179298978]",
                 "Kahakapao Loop Trail" = "[-156.2674384865,20.8117053473],[-156.2753739695,20.8119787889],[-156.2762706655,20.8172152958],[-156.2790301557,20.8205754146],[-156.2802150215,20.8267645811],[-156.2787543911,20.831007382],[-156.2759664024,20.832625409],[-156.2715335491,20.8303936647],[-156.2711030546,20.8245575065],[-156.2684174928,20.8211622211],[-156.2669984366,20.8141752356],[-156.2674384865,20.8117053473]",
                 "Waiakoa Loop Trail" = "[-156.3012763972,20.7179902338],[-156.3004486005,20.7195553775],[-156.3016750405,20.7222070847],[-156.287940454,20.7386961116],[-156.2792755778,20.7336055103],[-156.2970405186,20.7188187491],[-156.3000952194,20.7187074235],[-156.3012763972,20.7179902338]",
                 "Haleakala Ridge Trail" = "[-156.355557686,20.6479903804],[-156.3674029916,20.6545517867],[-156.3645618617,20.6586474239],[-156.3580511345,20.6752390367],[-156.3508014589,20.6832377485],[-156.3347209453,20.6839732952],[-156.326998195,20.6800546254],[-156.3205391004,20.6805991612],[-156.3176278978,20.6786775934],[-156.3230308725,20.6635868257],[-156.3315844371,20.648626329],[-156.355557686,20.6479903804]",
                 "Waihe'e Ridge Trail" = "[-156.5541882254,20.9408518775],[-156.556302812,20.944586389],[-156.5573579259,20.948246912],[-156.5538670309,20.9503162789],[-156.5443867631,20.9519885971],[-156.5359083004,20.9533659519],[-156.5318087116,20.953163686],[-156.5300669521,20.9531630598],[-156.5307745524,20.950061093],[-156.5356600285,20.9451949429],[-156.5444627032,20.9410087561],[-156.5541882254,20.9408518775]",
                 "Phallic Rock" = "[-157.0077103749,21.1748246808],[-157.0092100231,21.1752497522],[-157.0092106936,21.175967138],[-157.0085473917,21.1763918935],[-157.0070383977,21.1763859925],[-157.0057461597,21.1758722527],[-157.0051755197,21.1755982262],[-157.0055008214,21.1752116103],[-157.0062635746,21.1749420767],[-157.0077103749,21.1748246808]",
                 "Molokai Forest Reserve Road" = "[-156.9191520475,21.1151045779],[-156.9255467691,21.1288975083],[-156.9492474385,21.1339765688],[-156.9545018859,21.1364871072],[-156.9804562815,21.1367310287],[-156.996150557,21.1308029973],[-157.0065642335,21.1309524832],[-157.0143915899,21.1325117557],[-157.0243982412,21.1359442241],[-157.0366881229,21.1287989964],[-157.0533184893,21.1223852608],[-157.0536266081,21.1251086616],[-157.0231476612,21.1395223376],[-157.0147000439,21.1369099042],[-156.9970423914,21.1359310898],[-156.9818228669,21.1419814979],[-156.9514428265,21.1402665944],[-156.938153822,21.1368961445],[-156.9247146137,21.1335287453],[-156.9217293151,21.1327247237],[-156.9156353362,21.1231355458],[-156.917111557,21.1154433023],[-156.9191520475,21.1151045779]",
                 "Pepe'opae Bog Trail" = "[-156.9161409326,21.1143839651],[-156.9168440066,21.1161757949],[-156.9160198979,21.1177427291],[-156.8974848278,21.1232737805],[-156.8935822137,21.1213979062],[-156.8911642022,21.1170399565],[-156.9012184627,21.1149347463],[-156.9134674408,21.1149384995],[-156.9161409326,21.1143839651]",
                 "Manoa Falls Trail" = "[-157.800434656,21.3320734516], 	[-157.8010582696,21.3322249197], 	[-157.8014771133,21.3330526825], 	[-157.8012557473,21.3341031116], 	[-157.8008915536,21.3350359526], 	[-157.8004235081,21.3361521841], 	[-157.8002073388,21.3369723516], 	[-157.8000125434,21.337800478], 	[-157.8003042336,21.3384155385], 	[-157.8006628952,21.3390400433], 	[-157.800430968,21.3399587999], 	[-157.8000117052,21.341347932], 	[-157.7996431529,21.3425639681], 	[-157.798469435,21.342660932], 	[-157.7993015065,21.3410165964], 	[-157.7994287438,21.3390842325], 	[-157.7991028554,21.3381653924], 	[-157.7992583397,21.3368521179], 	[-157.7999583963,21.3351283931], 	[-157.8004301298,21.3340607948], 	[-157.8002174809,21.3330285571], 	[-157.800434656,21.3320734516]"
  )

  # Make dataframe with trail specific polygon points
  trail_coordinates <- NULL

  for(i in 1:length(trails)){
    longs <- as_tibble(str_match_all(trails,  "(?<=\\[).+?(?=,)")[[i]]) %>%
      dplyr::rename(longitudes = V1) %>%
      dplyr::mutate(longitudes = as.numeric(longitudes))

    lats <- as_tibble(str_match_all(trails,  "(?<=[0-9],).+?(?=\\])")[[i]]) %>%
      dplyr::rename(latitudes = V1) %>%
      dplyr::mutate(latitudes = as.numeric(latitudes))

    long_lats <- bind_cols(longs, lats) %>%
      dplyr::mutate(trail = names(trails)[i])

    trail_coordinates <- rbind(trail_coordinates, long_lats)
  }

  # Create trail polygon object from trail_coordinates dataframe. Can be looped if gets too long
  trail_polygons <-  sp::SpatialPolygons(list(
    sp::Polygons(list(sp::Polygon(cbind(trail_coordinates %>% dplyr::filter(trail == glue::glue("{list(unique(trail_coordinates$trail))[[1]][1]}")) %>% .$longitudes,
                                trail_coordinates %>% dplyr::filter(trail == glue::glue("{list(unique(trail_coordinates$trail))[[1]][1]}")) %>% .$latitudes))),
             ID = glue::glue("{list(unique(trail_coordinates$trail))[[1]][1]}")),
    sp::Polygons(list(sp::Polygon(cbind(trail_coordinates %>% dplyr::filter(trail == glue::glue("{list(unique(trail_coordinates$trail))[[1]][2]}")) %>% .$longitudes,
                                trail_coordinates %>% dplyr::filter(trail == glue::glue("{list(unique(trail_coordinates$trail))[[1]][2]}")) %>% .$latitudes))),
             ID = glue::glue("{list(unique(trail_coordinates$trail))[[1]][2]}")),
    sp::Polygons(list(sp::Polygon(cbind(trail_coordinates %>% dplyr::filter(trail == glue::glue("{list(unique(trail_coordinates$trail))[[1]][3]}")) %>% .$longitudes,
                                trail_coordinates %>% dplyr::filter(trail == glue::glue("{list(unique(trail_coordinates$trail))[[1]][3]}")) %>% .$latitudes))),
             ID = glue::glue("{list(unique(trail_coordinates$trail))[[1]][3]}")),
    sp::Polygons(list(sp::Polygon(cbind(trail_coordinates %>% dplyr::filter(trail == glue::glue("{list(unique(trail_coordinates$trail))[[1]][4]}")) %>% .$longitudes,
                                trail_coordinates %>% dplyr::filter(trail == glue::glue("{list(unique(trail_coordinates$trail))[[1]][4]}")) %>% .$latitudes))),
             ID = glue::glue("{list(unique(trail_coordinates$trail))[[1]][4]}")),
    sp::Polygons(list(sp::Polygon(cbind(trail_coordinates %>% dplyr::filter(trail == glue::glue("{list(unique(trail_coordinates$trail))[[1]][5]}")) %>% .$longitudes,
                                trail_coordinates %>% dplyr::filter(trail == glue::glue("{list(unique(trail_coordinates$trail))[[1]][5]}")) %>% .$latitudes))),
             ID = glue::glue("{list(unique(trail_coordinates$trail))[[1]][5]}")),
    sp::Polygons(list(sp::Polygon(cbind(trail_coordinates %>% dplyr::filter(trail == glue::glue("{list(unique(trail_coordinates$trail))[[1]][6]}")) %>% .$longitudes,
                                trail_coordinates %>% dplyr::filter(trail == glue::glue("{list(unique(trail_coordinates$trail))[[1]][6]}")) %>% .$latitudes))),
             ID = glue::glue("{list(unique(trail_coordinates$trail))[[1]][6]}")),
    sp::Polygons(list(sp::Polygon(cbind(trail_coordinates %>% dplyr::filter(trail == glue::glue("{list(unique(trail_coordinates$trail))[[1]][7]}")) %>% .$longitudes,
                                trail_coordinates %>% dplyr::filter(trail == glue::glue("{list(unique(trail_coordinates$trail))[[1]][7]}")) %>% .$latitudes))),
             ID = glue::glue("{list(unique(trail_coordinates$trail))[[1]][7]}")),
    sp::Polygons(list(sp::Polygon(cbind(trail_coordinates %>% dplyr::filter(trail == glue::glue("{list(unique(trail_coordinates$trail))[[1]][8]}")) %>% .$longitudes,
                                trail_coordinates %>% dplyr::filter(trail == glue::glue("{list(unique(trail_coordinates$trail))[[1]][8]}")) %>% .$latitudes))),
             ID = glue::glue("{list(unique(trail_coordinates$trail))[[1]][8]}")),
    sp::Polygons(list(sp::Polygon(cbind(trail_coordinates %>% dplyr::filter(trail == glue::glue("{list(unique(trail_coordinates$trail))[[1]][9]}")) %>% .$longitudes,
                                trail_coordinates %>% dplyr::filter(trail == glue::glue("{list(unique(trail_coordinates$trail))[[1]][9]}")) %>% .$latitudes))),
             ID = glue::glue("{list(unique(trail_coordinates$trail))[[1]][9]}")),
    sp::Polygons(list(sp::Polygon(cbind(trail_coordinates %>% dplyr::filter(trail == glue::glue("{list(unique(trail_coordinates$trail))[[1]][10]}")) %>% .$longitudes,
                                trail_coordinates %>% dplyr::filter(trail == glue::glue("{list(unique(trail_coordinates$trail))[[1]][10]}")) %>% .$latitudes))),
             ID = glue::glue("{list(unique(trail_coordinates$trail))[[1]][10]}")),
    sp::Polygons(list(sp::Polygon(cbind(trail_coordinates %>% dplyr::filter(trail == glue::glue("{list(unique(trail_coordinates$trail))[[1]][11]}")) %>% .$longitudes,
                                trail_coordinates %>% dplyr::filter(trail == glue::glue("{list(unique(trail_coordinates$trail))[[1]][11]}")) %>% .$latitudes))),
             ID = glue::glue("{list(unique(trail_coordinates$trail))[[1]][11]}")),
    sp::Polygons(list(sp::Polygon(cbind(trail_coordinates %>% dplyr::filter(trail == glue::glue("{list(unique(trail_coordinates$trail))[[1]][12]}")) %>% .$longitudes,
                                trail_coordinates %>% dplyr::filter(trail == glue::glue("{list(unique(trail_coordinates$trail))[[1]][12]}")) %>% .$latitudes))),
             ID = glue::glue("{list(unique(trail_coordinates$trail))[[1]][12]}")),
    sp::Polygons(list(sp::Polygon(cbind(trail_coordinates %>% dplyr::filter(trail == glue::glue("{list(unique(trail_coordinates$trail))[[1]][13]}")) %>% .$longitudes,
                                trail_coordinates %>% dplyr::filter(trail == glue::glue("{list(unique(trail_coordinates$trail))[[1]][13]}")) %>% .$latitudes))),
             ID = glue::glue("{list(unique(trail_coordinates$trail))[[1]][13]}")),
    sp::Polygons(list(sp::Polygon(cbind(trail_coordinates %>% dplyr::filter(trail == glue::glue("{list(unique(trail_coordinates$trail))[[1]][14]}")) %>% .$longitudes,
                                trail_coordinates %>% dplyr::filter(trail == glue::glue("{list(unique(trail_coordinates$trail))[[1]][14]}")) %>% .$latitudes))),
             ID = glue::glue("{list(unique(trail_coordinates$trail))[[1]][14]}")),
    sp::Polygons(list(sp::Polygon(cbind(trail_coordinates %>% dplyr::filter(trail == glue::glue("{list(unique(trail_coordinates$trail))[[1]][15]}")) %>% .$longitudes,
                                trail_coordinates %>% dplyr::filter(trail == glue::glue("{list(unique(trail_coordinates$trail))[[1]][15]}")) %>% .$latitudes))),
             ID = glue::glue("{list(unique(trail_coordinates$trail))[[1]][15]}")),
    sp::Polygons(list(sp::Polygon(cbind(trail_coordinates %>% dplyr::filter(trail == glue::glue("{list(unique(trail_coordinates$trail))[[1]][16]}")) %>% .$longitudes,
                                trail_coordinates %>% dplyr::filter(trail == glue::glue("{list(unique(trail_coordinates$trail))[[1]][16]}")) %>% .$latitudes))),
             ID = glue::glue("{list(unique(trail_coordinates$trail))[[1]][16]}")),
    sp::Polygons(list(sp::Polygon(cbind(trail_coordinates %>% dplyr::filter(trail == glue::glue("{list(unique(trail_coordinates$trail))[[1]][17]}")) %>% .$longitudes,
                                trail_coordinates %>% dplyr::filter(trail == glue::glue("{list(unique(trail_coordinates$trail))[[1]][17]}")) %>% .$latitudes))),
             ID = glue::glue("{list(unique(trail_coordinates$trail))[[1]][17]}")),
    sp::Polygons(list(sp::Polygon(cbind(trail_coordinates %>% dplyr::filter(trail == glue::glue("{list(unique(trail_coordinates$trail))[[1]][18]}")) %>% .$longitudes,
                                trail_coordinates %>% dplyr::filter(trail == glue::glue("{list(unique(trail_coordinates$trail))[[1]][18]}")) %>% .$latitudes))),
             ID = glue::glue("{list(unique(trail_coordinates$trail))[[1]][18]}"))
  ))
  # find if collection locations fall within trail polygons
  pts <- sp::SpatialPoints(cbind(joined_data %>% dplyr::filter(!is.na(collection_longitude)) %>% .$collection_longitude,
                                 joined_data %>% dplyr::filter(!is.na(collection_latitude)) %>% .$collection_latitude))

  # bind trails
  fulc_data <- cbind(unname(sp::over(pts, trail_polygons)), joined_data %>%
                        dplyr::filter(!is.na(collection_longitude) & !is.na(collection_latitude))) %>%
    dplyr::rename(collection_trail = 1) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(collection_trail = glue::glue("{list(unique(trail_coordinates$trail))[[1]][{collection_trail}]}")) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(collection_trail = ifelse(collection_trail == "NA", NA_character_, collection_trail)) %>% # fix NAs
    dplyr::full_join(joined_data) # add back joined_data with NAs for complete collection

  return(fulc_data)
}

#' loadGenotypes
#'
#' \code{loadGenotypes} loads genotyping data from project specific genotyping Google sheets
#'
#' @param gsKey a vector containing the google sheet keys for the genotype data to load.
#' Your Google Sheet keys are found in your Google sheets URL. Select the string
#' found between the slashes after spreadsheets/d in your Google Sheet URL.
#'
#' @return A dataframe generated from the Google sheets \code{gsKey} argument. If multiple Google sheets
#' are provided the data are appended using rbind. A \code{project_id} variable is assigned from the Google sheets name.
#' The genotyping sheet must be named with the convention \code{YOUR_PROJECT_NAME}_wild_isolate_genotyping.
#' @export
#'

loadGenotypes <- function(gsKey) {
  # read genotyping sheet(s)
  genotyping_sheet <- NULL

    for(i in unique(gsKey)){
      # get project from sheet name
      project_geno <- stringr::str_replace(googlesheets4::gs4_get(i)$name,
                                      pattern = "_wild_isolate_genotyping", replacement = "")

      # get data from sheet
      geno <- googlesheets4::read_sheet(i, range = "genotyping template") %>%
        dplyr::filter(!is.na(s_label)) %>%
        dplyr::mutate(project_geno = project_geno)

    genotyping_sheet <- rbind(genotyping_sheet, geno)
    }

    # find s_labels in genotyping sheet
    slabs <- str_subset(genotyping_sheet$s_label, pattern = "S-")

    # filter genotyping sheet by s_labels matching "S-" pattern
    unusual_slabs <- genotyping_sheet %>%
      dplyr::filter(!(s_label %in% slabs)) %>%
      dplyr::pull(s_label)

    duplicated_slabs <- genotyping_sheet %>%
      dplyr::group_by(s_label) %>%
      dplyr::mutate(slab_n = n(),
                    slab_duplicate = ifelse(slab_n > 1, 1, 0)) %>%
      dplyr::ungroup() %>%
      dplyr::distinct(s_label, .keep_all = T) %>%
      dplyr::filter(slab_duplicate == 1) %>%
      dplyr::pull(s_label)

    # print warning if duplicates or unusual names found for S-labels
    print(paste0("There are ", length(unusual_slabs), " unsual S-label names in genotyping sheet(s) ", unusual_slabs))

    # print warning if duplicates or unusual names found for strain names
    print(paste0("There are ", length(duplicated_slabs), " duplicated S-label names in genotyping sheet(s) ", duplicated_slabs))

  return(genotyping_sheet)
}


#' readGenotypes
#'
#' \code{readGenotypes} reads genotyping data from project specific genotyping Google sheets
#'
#' @param gsKey a vector containing the google sheet keys for the genotype data to load.
#' Your Google Sheet keys are found in your Google sheets URL. Select the string
#' found between the slashes after spreadsheets/d in your Google Sheet URL.
#'
#' @return A dataframe generated from the Google sheets \code{gsKey} argument. If multiple Google sheets
#' are provided the data are appended using rbind. Note, the genotyping sheet must contain variable names
#' specified by the \code{wild_isolate_genotyping_template} and the data must be found on the sheet named
#' \code{genotyping template}.
#' @export
#'

readGenotypes <- function(gsKey) {
  # read genotyping sheet(s)
  genotyping_sheet <- NULL

  for(i in unique(gsKey)){
    # get data from sheet
    geno <- googlesheets4::read_sheet(i, range = "genotyping template") %>%
      dplyr::filter(!is.na(s_label))

    genotyping_sheet <- rbind(genotyping_sheet, geno)
  }

  # find s_labels in genotyping sheet
  slabs <- str_subset(genotyping_sheet$s_label, pattern = "S-")

  # filter genotyping sheet by s_labels matching "S-" pattern
  unusual_slabs <- genotyping_sheet %>%
    dplyr::filter(!(s_label %in% slabs)) %>%
    dplyr::pull(s_label)

  duplicated_slabs <- genotyping_sheet %>%
    dplyr::group_by(s_label) %>%
    dplyr::mutate(slab_n = n(),
                  slab_duplicate = ifelse(slab_n > 1, 1, 0)) %>%
    dplyr::ungroup() %>%
    dplyr::distinct(s_label, .keep_all = T) %>%
    dplyr::filter(slab_duplicate == 1) %>%
    dplyr::pull(s_label)

  # print warning if duplicates or unusual names found for S-labels
  print(paste0("There are ", length(unusual_slabs), " unsual S-label names in genotyping sheet(s) ", unusual_slabs))

  # print warning if duplicates or unusual names found for strain names
  print(paste0("There are ", length(duplicated_slabs), " duplicated S-label names in genotyping sheet(s) ", duplicated_slabs))

  return(genotyping_sheet)
}


#' joinFulcGeno
#'
#' \code{joinFulcGeno} joins the collection data output from the \code{procFulcrum} function
#' with the genotyping data output from the \code{readGenotypes} function.
#' Blast data output from the \code{procSanger} function can also be joined if desired.
#'
#' @param fulc a collection data frame output from the \code{procFulcrum} function.
#' @param geno a genotyping data frame output from the \code{loadGenotypes} function.
#' @param blast OPTIONAL, a blast results data frame output from the \code{procSanger} function.
#'
#' @return A single collection dataframe with variables descriped in the data dictionary.
#' @export
#'

joinFulcGeno <- function(fulc, geno, blast = NULL) {
  if(is.null(blast)){
    # Join genotyping sheet with collection and isolation data
    out_dat <- fulc %>%
      dplyr::full_join(geno) %>%
      # Rename variables
      dplyr::rename(collection_id = c_label,
                    isolation_id = s_label) %>%
      # Reorder variables
      dplyr::select(project_id,
                    collection_id,
                    isolation_id,
                    species_id,
                    ECA_name,
                    collection_by,
                    collection_datetime_UTC,
                    collection_date_UTC,
                    collection_local_time,
                    collection_island,
                    collection_location,
                    collection_trail,
                    collection_latitude,
                    collection_longitude,
                    collection_fulcrum_latitude,
                    collection_fulcrum_longitude,
                    collection_lat_long_method,
                    collection_lat_long_method_diff,
                    ambient_temperature,
                    flag_ambient_temperature_run,
                    ambient_humidity,
                    substrate_temperature,
                    altitude,
                    altitude_method,
                    #altitude_methods_range,
                    landscape,
                    sky_view,
                    substrate,
                    substrate_other,
                    substrate_notes,
                    sample_photo_url,
                    gridsect,
                    gridsect_index,
                    grid_sect_direction,
                    gridsect_radius,
                    isolation_by,
                    isolation_datetime_UTC,
                    isolation_date_UTC,
                    isolation_local_time,
                    isolation_latitude,
                    isolation_longitude,
                    worms_on_sample,
                    approximate_number_of_worms,
                    shipment_sent_date,
                    shipment_received_date,
                    proliferation_48,
                    proliferation_168,
                    proliferating,
                    lysis_date,
                    pcr_product_its2,
                    pcr_product_ssu,
                    general_notes,
                    manual_blast_notes,
                    possible_new_caeno_sp,
                    make_strain_name,
                    reason_strain_not_named,)
  }
  else{
    # load blast results
    blast_results <- read_tsv(blast)
    print(paste0("loading blast results from", blast))

    # Join genotyping sheet with collection and isolation data
    out_dat <- fulc %>%
      dplyr::full_join(geno) %>%
      # Rename variables
      dplyr::rename(project_fulc = project,
                    collection_id = c_label,
                    isolation_id = s_label) %>%
      # Reorder variables
      dplyr::select(project_id,
                    project_fulc,
                    collection_id,
                    isolation_id,
                    species_id,
                    ECA_dirty,
                    ECA_clean,
                    collection_by,
                    collection_datetime_UTC,
                    collection_date_UTC,
                    collection_local_time,
                    collection_island,
                    collection_location,
                    collection_trail,
                    collection_latitude,
                    collection_longitude,
                    collection_fulcrum_latitude,
                    collection_fulcrum_longitude,
                    collection_lat_long_method,
                    collection_lat_long_method_diff,
                    ambient_temperature,
                    flag_ambient_temperature_run,
                    ambient_humidity,
                    substrate_temperature,
                    altitude,
                    altitude_method,
                    #altitude_methods_range,
                    landscape,
                    sky_view,
                    substrate,
                    substrate_other,
                    substrate_notes,
                    sample_photo_url,
                    gridsect,
                    gridsect_index,
                    grid_sect_direction,
                    gridsect_radius,
                    isolation_by,
                    isolation_datetime_UTC,
                    isolation_date_UTC,
                    isolation_local_time,
                    isolation_latitude,
                    isolation_longitude,
                    worms_on_sample,
                    proliferation_48,
                    proliferation_168,
                    proliferating,
                    approximate_number_of_worms,
                    shipment_number,
                    pcr_product_its2,
                    pcr_product_ssu,
                    general_notes,
                    manual_blast_notes,
                    possible_new_caeno_sp,
                    make_strain_name,
                    reason_strain_not_named)
  }
  return(out_dat)
}

#' readFulcrum
#'
#' \code{readFulcrum} reads .csv files exported from Fulcrum into R
#'
#' @param dir The path of the directory with five Fulcrum .csv files:
#' nematode_field_sampling.csv,
#' nematode_field_sampling_sample_photo.csv,
#' nematode_isolation.csv,
#' nematode_isolation_s_labeled_plates.csv,
#' nematode_isolation_photos.csv
#' @return A list of five named data frames generated from the .csv files.
#' \tabular{ll}{
#' nematode_field_sampling \tab nematode_field_sampling.csv\cr
#' nematode_field_sampling_sample_photo \tab nematode_field_sampling_sample_photo.csv\cr
#' nematode_isolation \tab nematode_isolation.csv\cr
#' nematode_isolation_s_labeled_plates \tab nematode_isolation_s_labeled_plates.csv\cr
#' nematode_isolation_photos \tab nematode_isolation_photos.csv\cr
#' }
#' @export

readFulcrum <- function(dir) {
  # make file list
  files <- list(glue::glue("{dir}/nematode_field_sampling_sample_photo.csv"),
                glue::glue("{dir}/nematode_field_sampling.csv"),
                glue::glue("{dir}/nematode_isolation_photos.csv"),
                glue::glue("{dir}/nematode_isolation_s_labeled_plates.csv"),
                glue::glue("{dir}/nematode_isolation.csv"))

  #read files
  fulc_data <- lapply(files, readr::read_csv)

  # set names
  names(fulc_data) <- c("nematode_field_sampling_sample_photo",
                        "nematode_field_sampling",
                        "nematode_isolation_photos",
                        "nematode_isolation_s_labeled_plates.csv",
                        "nematode_isolation.csv")

  # return data
  return(fulc_data)
}


readFulcrum <- function(dir) {
  fulcrum <- NULL
  # Read collection data
  nematode_field_sampling <- readr::read_csv(glue::glue("{dir}/nematode_field_sampling.csv")) %>%
    dplyr::mutate(c_label = stringr::str_to_upper(c_label)) %>%
    # name created_by to specify who picked up the sample
    dplyr::rename(collection_by = created_by) %>%
    dplyr::select(-updated_at,
                  -system_created_at,
                  -system_updated_at,
                  -date) %>%
    # choose one sample photo only. This takes the first sample photo and warns if additional photos are discarded
    tidyr::separate(col = sample_photo, into = "sample_photo", sep = ",", extra = "warn") %>%
    # this is UTC time (very important if you want to convert to local time)
    dplyr::mutate(collection_datetime_UTC = lubridate::ymd_hms(created_at, tz = "UTC")) %>%
    # again this is UTC date (very important if you want to convert to local date)
    dplyr::mutate(collection_date_UTC = lubridate::date(created_at)) %>%
    dplyr::select(-created_at) %>%
    # Fix Fahrenheit observations to Celcius
    dplyr::mutate(substrate_temperature = ifelse(substrate_temperature > 40,
                                                 FtoC(substrate_temperature),
                                                 substrate_temperature)) %>%
    # Fix ambient temp Fahrenheit to Celcius
    dplyr::mutate(ambient_temperature = ifelse(ambient_temperature_c > 40,
                                               FtoC(ambient_temperature_c),
                                               ambient_temperature_c)) %>%
    # force ambient temp to numeric
    dplyr::mutate(ambient_temperature = as.numeric(ambient_temperature)) %>%
    # drop ambient temp c
    dplyr::select(-ambient_temperature_c) %>%
    # add flags for runs of temperature data
    dplyr::arrange(collection_datetime_UTC) %>%
    dplyr::mutate(flag_ambient_temperature_run = (ambient_humidity == dplyr::lag(ambient_humidity)) &
                    (ambient_temperature == dplyr::lag(ambient_temperature))
                  & (gridsect == "no"))

  # Read collection photo position data (exif)
  nematode_field_sampling_sample_photo <<- readr::read_csv(glue::glue("{dir}/nematode_field_sampling_sample_photo.csv")) %>%
    dplyr::select(fulcrum_id, exif_gps_latitude, exif_gps_longitude, exif_gps_altitude)

  # Read isolation data
  nematode_isolation <- readr::read_csv(glue::glue("{dir}/nematode_isolation.csv")) %>%
    dplyr::select(c_label_id = c_label,
                  isolation_id = fulcrum_id,
                  isolation_datetime_UTC = system_created_at,
                  isolation_by = created_by,
                  worms_on_sample,
                  approximate_number_of_worms,
                  isolation_date_UTC = date,
                  isolation_local_time = time, # Is this actually local time? or is it UTC?
                  isolation_latitude = latitude,
                  isolation_longitude = longitude)

  # Read S-plate data
  nematode_isolation_s_labeled_plates <- readr::read_csv(glue::glue("{dir}/nematode_isolation_s_labeled_plates.csv")) %>%
    dplyr::select(fulcrum_parent_id, s_label)

  nematode_isolation_photos <- readr::read_csv(glue::glue("{dir}/nematode_isolation_photos.csv"))

  fulcrum <- list(nematode_field_sampling, nematode_field_sampling_sample_photo, nematode_isolation, nematode_isolation_s_labeled_plates, nematode_isolation_photos)
  return(fulcrum)
}

##################
testing

dir = "/Users/tim/repos/easyfulcrum/test_data/2020FebruaryAustralia/data/fulcrum"
