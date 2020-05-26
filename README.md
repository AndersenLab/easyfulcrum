# easyfulcrum

easyfulcrum is specialized for use in the Andersen Lab and, therefore, is not available from CRAN. 

To install easyfulcrum you will need the [`devtools`](https://github.com/hadley/devtools) package. You can install `devtools` and `easyfulcrum` using the commands below:

```r
install.packages("devtools")
devtools::install_github("AndersenLab/easyfulcrum")
```

The functionality of the package can be broken down into three main goals:

+ Reading raw data from Fulcrum exports, genotyping Google sheets, and blast results.

+ Joining these data, flagging anomalies, and correcting anomalies

+ Exporting corrected data and reporting trends

## Directory structure

Every collection project should be contained in its own repository. The repository name should follow the `YearMonthPlace` format used for Fulcrum collection projects, e.g. `2020FebruaryAustralia`.

The directory structure within the repository is critically important for
`easyfulcrum` functions. The `data` directory contains the  `fulcrum` and `sanger` subdirectories. 
    - `data` holds the `.csv` files exported from Fulcrum and the `photos` subdirectory that contains `.jpg` files exported from Fulcrum.
    - `sanger` holds the `raw` subdirectory that contains the `.seq` and `.ab1`files exported from the sequencing facility.
The `plots` and `reports` directories hold `easyfulcrum` function outputs.

```
2020FebruaryAustralia/
├── data
│   ├── fulcrum
│       ├── nematode_field_sampling.csv
│       ├── nematode_field_sampling_sample_photo.csv
│       ├── nematode_isolation.csv
│       ├── nematode_isolation_s_labeled_plates.csv
│       ├── nematode_isolation_photos.csv
│       ├── photos
│           ├── 0a7a5879-8453-4f20-ab3b-8eabb725d492.jpg
│           ├── 0b16d8c7-3cb3-44a8-8b61-e0789e4062c2.jpg
│           └── ... all collection photos here
│   ├── sanger
│       ├── raw
│           ├── email@northwestern.edu_01_SEQ1677048A_122319D
│               ├── S-10206_oECA306_A01.ab1
│               ├── S-10206_oECA306_A01.phd.1
│               ├── S-10206_oECA306_A01.scf
│               ├── S-10206_oECA306_A01.seq
│               └── ... more sequences here if present
│           └── ... more sequence folders here if present
├── plots
│   ├── empty
├── reports
│   ├── empty
```

This directory exhibits the minimal file content and naming for all the `easyfulcrum` functions to work. The file names in the `photos` directory should not be altered from the Fulcrum export, the names shown here are examples only. The `sanger` subdirectory is optional. In order to use `easyfulcrum` blast analysis functions this directory must contain `.seq` and `.ab1` files exported from the sequencing facility. The file names must contain the S-label and primer used. The folders containing the raw sequence data are optional, they are included in this example because this is the way the data are often exported from the sequencing facility. The `plots` and `reports` directories are used to hold `easyfulcrum` function outputs.

## Project workflow

A detailed wild nematode collection protocol can be found here **link**

![File name convention](./READMEfiles/FileNaming.png)

## Pipeline

The complete easy sorter package consists of only 6 functions: `read_data`, `remove_contamination`, `sumplate`, `bioprune`, `bamf_prune`, and `regress`.

### `read_data()`

`read_data()` can take as an argument a path to a single data file, a directory with sorter data files, or an experiment directory with both setup and score subdirectories. If the function is given either a single file or directory of files, it will output a single data frame of all of the data that were read. If the function is given an experiment directory with both setup and score subdirectories, it will output a two element list with the first element being the score data and the second element being the setup data.

For further information use the command `?read_plate` to access the documentation.

### `remove_contamination()`

`remove_contamination()` takes as an argument the raw data output from read_data. It will automatically remove any data from contaminated wells as per the contamination files stored in the data directory.

For further information use the command `?remove_contamination()` to access the documentation.

### `sumplate()`

`sumplate` summarizes the plates that have been read in to R using the `read_data` function. This function can take either a single data frame or the list of two data frames. If a list is passed, the `n.sorted` column will be calculated automatically using the setup data. Otherwise, n.sorted will be set to 0 and can be changed manually by the user.

*For a V3 assay (no sorting), use the `v3_assay = TRUE` flag to avoid calculating `norm.n`.*

For further information use the command `?sumplate` to access the documentation.

### `bioprune()`

`bioprune` will remove any biologically impossible wells from the data set (n > 1000, n < 5, norm.n > 350). It takes as input the standard output from `sumplate`.

For further information use the command `?bioprune` to access the documentation.

### `bamf_prune()`

`bamf_prune()` takes a summarized plate as input and outputs a plate data frame either with three additional columns indicating outlier calls or a trimmed data frame with all outliers removed. It is generally recommended to use `bamf_prune` when running mappings or other experiments with many strains and few replicates because it keeps outliers that are grouped together.

For further information use the command `?bamf_prune` to access the documentation.

### `prune_outliers()`

`prune_outliers()` is an alternative to `bamf_prune()` that takes a summarized plate as input and outputs a trimmed data frame with all outliers removed. Outliers are claculated either as the median +/- (2 * IQR) if `iqr = TRUE` or mean +/- (2 * standard deviation) if `iqr = FALSE` (default). It is generally recommended to use `prune_outliers` with experiments with many replicates because it calculates outliers for each strain independently.

For further information use the command `?prune_outliers` to access the documentation.

### `regress()`

`regress()` can take either a pruned or unpruned data frame and will return a data frame in long form with the phenotype column replaced with residual values (either `phenotype ~ control` if `assay = FALSE` or `phenotype ~ assay` if `assay = TRUE`).

For further information use the command `?regress` to access the documentation.

### Overview

![Overview](./READMEfiles/Overview.png)

### Example

```r
library(easyfulcrum)
library(dplyr)

# Define a vector of your experiement directories
dirs <- c("~/Dropbox/HTA/Results/20150706_McGrathRILs1a/",
          "~/Dropbox/HTA/Results/20150707_McGrathRILs1b/")

# Read in the data
raw <- read_data(dirs)

# Remove all data from the contaminated wells
raw_nocontam <- remove_contamination(raw)

# Summarize the data
summedraw <- sumplate(raw_nocontam, directories = TRUE, quantiles = TRUE)

#Prune based on biological impossibilities
biopruned <- bioprune(summedraw)

# Regress out the effect of assay first
assayregressed <- regress(biopruned, assay = TRUE)

# Prune based on bins
bamfpruned <- bamf_prune(assayregressed, drop = TRUE)

# If you have replicates in the data, summarize the data down to one observation
# per strain. Here we do that by taking the mean of the two replicates. Make
# sure to include `na.rm = TRUE` to avoid losing data missing in only one assay.
sumpruned <- bamfpruned %>%
    group_by(condition, control, strain, trait) %>%
    summarize(phenotype = mean(phenotype, na.rm = TRUE))

# Regress out the effect of the control strains (effects not due to changed
# environmental condition)
LSJ2data <- regress(sumpruned)

# Save the final data frame
saveRDS(LSJ2data, "~/LSJ2phenotype.rds")
```
=