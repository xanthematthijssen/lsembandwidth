% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calculate_likelihoods.R
\name{calculate_likelihoods}
\alias{calculate_likelihoods}
\title{Calculates likelihood for every row of data with list of RAM matrices for
every moderator value}
\usage{
calculate_likelihoods(data, RAM_list, moderator_name)
}
\arguments{
\item{data}{dataframe for which likelihoods have to be calculated}

\item{RAM_list}{list of RAM matrices for every moderator value}

\item{moderator_name}{name of the moderator in the dataframe}
}
\value{
vector of loglikelihoods for every row of data
}
\description{
Calculates likelihood for every row of data with list of RAM matrices for
every moderator value
}
