% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/perf.R
\name{cdEnergy}
\alias{cdEnergy}
\title{Extract cdEnergy}
\usage{
cdEnergy(x)
}
\arguments{
\item{x}{see \code{\link{perf}}}
}
\value{
a double column with \code{\link{colnames}} 'cdEnergy'
}
\description{
Returns the RAPL CORE + DRAM in Joule
}
\details{
\code{\link{cdEnergy}} is \code{\link{cEnergy}} + \code{\link{dEnergy}}.
This is *after* the adjustment specified in \code{\link{options}} `greensort_perf_calc` with default 'raw',
see \code{\link{perfcalibrate}}#' see \code{\link{perfcalibrate}}
}
\examples{
ret <- Insertionsort(runif(1000))
perf(ret)
cEnergy(ret)
dEnergy(ret)
cdEnergy(ret)
}
\seealso{
\code{\link{perf}}, \code{\link{aEnergy}}
}
