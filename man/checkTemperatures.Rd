% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/check_fix_functions.R
\name{checkTemperatures}
\alias{checkTemperatures}
\title{checkTemperatures}
\usage{
checkTemperatures(data, return_flags = FALSE)
}
\arguments{
\item{data}{list format output of \code{procFulcrum}.}

\item{return_flags}{a logical, default FALSE, set to TRUE if flagged rows in \code{procFulcrum} dataframes are to be returned and saved in list format.}
}
\value{
rows with flagged substrate temperature, flagged ambient temperature,
flagged ambient run temperature (and surrounding rows that cause flag to be triggered)
 these returns will also contain relevant columns for understanding flagged values and how they arose
 these dataframes can be saved as a list of three dataframes when \code{return_flags} is set to TRUE
}
\description{
\code{checkTemperatures} checks for flags (3) regarding raw temperature parameters.
}
\details{
Will return the flagged rows (and neighboring rows if appropriate), and necessary columns for flag visualization on \code{procFulcrum} output.
The returned \code{fulcrum_id} can be used in \code{fixTemperatures} to make changes to these raw temperature parameters.
See \code{fixTemperatures} documentation for information on how to make these changes.
flagged substrate temperature: occurs when substrate temperature > 40,
\code{procFulcrum} will automatically convert to celsius (assumes temperature incorrectly in fahrenheit).
flagged ambient temperature: occurs when ambient temperature > 40,
\code{procFulcrum} will automatically convert to celsius (assumes temperature incorrectly in fahrenheit).
flagged ambient run temperature: occurs when ambient humidity and temperature are repeated five times or more in subsequent measurements,
this is meant to flag values if probe is stuck, \code{procFulcrum} does not automatically modify anything for this flag.
}
