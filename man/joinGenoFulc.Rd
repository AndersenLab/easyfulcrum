% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/joinGenoFulc.R
\name{joinGenoFulc}
\alias{joinGenoFulc}
\title{joinGenoFulc}
\usage{
joinGenoFulc(geno, fulc, dir = NULL, select_vars = F)
}
\arguments{
\item{geno}{a genotyping data frame output from the \code{loadGenotypes}
function.}

\item{fulc}{a collection data frame output from the \code{procFulcrum}
function.}

\item{dir}{OPTIONAL, the path to the project directory, if passed will
save the genotyping data in data/processed/genotypes.}

\item{select_vars}{Logical, TRUE  will return only the default variables,
FALSE will return all variables. FALSE is recommended if using customized
Fulcrum applications other than "Nematode field sampling" and "Nematode
isolation". FALSE is default.}
}
\value{
A single collection dataframe with variables described in the data
  dictionary.
}
\description{
\code{joinGenoFulc} joins the collection data output from the
\code{procFulcrum} function with the genotyping data output from the
\code{checkGenotypes} function.
}
