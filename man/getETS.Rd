% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/funset.R
\name{getETS}
\alias{getETS}
\title{plug in ts object and return with ets object.}
\usage{
getETS(tsobj, EventSet = NULL, ee_win = 30, naomit = TRUE)
}
\arguments{
\item{tsobj}{A ts object.}

\item{EventSet}{a vector to specify the events. If null, all the events in DefaultEventSet will be applied to use.}
}
\value{
A data frame with date, stockID, event and the lag of days.
}
\description{
plug in ts object and return with ets object.
}

