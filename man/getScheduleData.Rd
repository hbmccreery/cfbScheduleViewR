% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/getScheduleData.R
\name{getScheduleData}
\alias{getScheduleData}
\title{Function to get schedule data}
\usage{
getScheduleData(is_fbs, week = NA, is_bowls = F)
}
\arguments{
\item{is_fbs}{Boolean value to determine if FBS/FCS schedule.}

\item{week}{Optional integer week of season of schedule to retrieve.}

\item{is_bowls}{Optional parameter for if it's bowling szn}
}
\value{
JSON returned from ESPN.com
}
\description{
Function to get schedule data
}
