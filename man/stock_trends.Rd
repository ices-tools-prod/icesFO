% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/stock_status_trends.R
\name{stock_trends}
\alias{stock_trends}
\title{Wrangling of format_sag output}
\usage{
stock_trends(x)
}
\arguments{
\item{x}{a dataframe output of format_sag function}
}
\value{
A data frame..
}
\description{
Wrangling of format_sag output to obtain a dataframe with time-series of F, Fmsy, SSB and MSY B trigger for
each stock in the Ecoregion, according to the last assessment (relative to the set year)
}
\note{
Can add some helpful information here
}
\examples{
\dontrun{
trends <- stock_trends(sag_formatted)
}

}
\references{
The ICES stock information Database web sevices: \url{http://sid.ices.dk/services/}
}
\seealso{
\code{\link{format_sag}} for formatting data from the ICES Stock Assessment database. 

\code{\link{icesFO-package}} gives an overview of the package.
}
