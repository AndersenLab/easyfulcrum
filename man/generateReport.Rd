% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/generateReport.R
\name{generateReport}
\alias{generateReport}
\title{generateReport}
\usage{
generateReport(
  data,
  dir,
  target_sp = c("Caenorhabditis elegans", "Caenorhabditis briggsae",
    "Caenorhabditis tropicalis"),
  profile = "nematode"
)
}
\arguments{
\item{data}{a checked dataframe generated from the \code{procPhotos}
function.}

\item{dir}{The path to the base fulcrum directory, report html will be saved
as reports/sampleReport_general.html or reports/sampleReport.html}

\item{target_sp}{vector of target species for species id checks. Default
target species names are: Caenorhabditis elegans,Caenorhabditis briggsae,
Caenorhabditis tropicalis. \code{generateReport} currently supports up to
three target species for use with the \code{"general"} profile.}

\item{profile}{set to \code{"nematode"} by default. This will use nematode
specific reporting standard to the Andersen Lab. Set to \code{"general"} to
use the non-nematode specific reporting.}
}
\value{
a report as an html file
}
\description{
\code{generateReport} produces a report html for project
}
