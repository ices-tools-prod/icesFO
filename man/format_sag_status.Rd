% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/format_sag_status.R
\name{format_sag_status}
\alias{format_sag_status}
\title{Format the stock status data as is in the ICES Stock Assessment Database.}
\usage{
format_sag_status(x, year, ecoregion)
}
\arguments{
\item{x}{a dataframe output from load_sag_status() required.}

\item{year}{the year required}

\item{ecoregion}{an identifier of the Ecoregion of interest}
}
\value{
A data frame..
}
\description{
Format the data from the ICES Stock Assessment Database for the downloaded year and the specific Ecoregion
for which you are producing the Fisheries Overviews.
}
\note{
Can add some helpful information here
}
\examples{
\dontrun{
sag_status <- format_sag_status(x,"Celtic Seas")
}

}
\references{
The ICES stock information Database web sevices: \url{http://sid.ices.dk/services/}
}
\seealso{
\code{\link{format_sid}} for formatting raw data from the ICES Stock Information database. 

\code{\link{icesFO-package}} gives an overview of the package.
}
