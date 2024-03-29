% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/check_fix_functions.R
\name{checkJoin}
\alias{checkJoin}
\title{checkJoin}
\usage{
checkJoin(data, return_flags = FALSE)
}
\arguments{
\item{data}{dataframe output from \code{joinFulcrum}.}

\item{return_flags}{set to TRUE if flagged rows are to be returned as a dataframe.}
}
\value{
\emph{c_labels} and \emph{s_label}s (as appropriate) for rows with each of the six flags,
and the origin of the location of the raw data that triggered the flag
 the rows corresponding to these labels can be saved as a list of six dataframes when return is set to TRUE
}
\description{
\code{checkJoin} checks for flags (9) in \code{joinFulcrum} output.
}
\details{
This check function will return \emph{c_labels} and \emph{s_labels} for various flags relating to missing or duplicated data.
Messages regarding what checks are being done, and where they might lie in the raw dataframes are also provided.
We advise returning to the raw dataframes using the information returned in these checks to better understand
issues that might have occurred during data collection, before re-running.
}
