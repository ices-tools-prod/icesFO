% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/format_sid.R
\name{format_sid}
\alias{format_sid}
\title{Format the data from the ICES Stock Information Database.}
\usage{
format_sid(x, ecoregion)
}
\arguments{
\item{x}{a dataframe output from load_sid() required.}

\item{ecoregion}{an identifier of the Ecoregion of interest}
}
\value{
A data frame..
}
\description{
Format the data from the ICES Stock Information Database for the downloaded year and the specific Ecoregion
for which you are producing the Fisheries Overviews.
}
\note{
Can add some helpful information here
}
\examples{
\dontrun{
sid_format <- format_sid(x, "Celtic Seas")
}

}
\references{
The ICES stock information Database web sevices: \url{http://sid.ices.dk/services/}
}
\seealso{
\code{\link{format_sag}} for formatting raw data from the ICES Stock Assessment database. 

\code{\link{icesFO-package}} gives an overview of the package.
}
