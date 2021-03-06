% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/misc.R
\name{expired}
\alias{expired}
\title{How many surveys were expired?}
\usage{
expired(survey, variable = "expired")
}
\arguments{
\item{survey}{which survey are you asking about?}

\item{variable}{which variable should be filled out, defaults to "expired"}
}
\description{
Just a simple to check how many times a survey (e.g. diary)
has expired (i.e. user missed it). It defaults to checking the "expired" variable for this.
}
\examples{
survey <- data.frame(expired = c(NA, "2016-05-29 10:11:00", NA))
expired(survey = survey)
}
