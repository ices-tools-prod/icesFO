% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_stock_status_trends.R
\name{plot_stock_trends}
\alias{plot_stock_trends}
\title{Plotting time series of F/Fmsy and SSB/MSY B trigger for all stocks with data available by guild.}
\usage{
plot_stock_trends(x, guild, cap_year, cap_month, return_data = FALSE)
}
\arguments{
\item{x}{a dataframe output of stock_trends function}

\item{guild}{an identifier of the Fisheries guild to plot}

\item{cap_year}{the year to be shown in the figure caption}

\item{cap_month}{the month to be shown in the figure caption, the accession date to SAG usually}

\item{return_data}{a parameter indicating if the data behind the plot should be returned as a dataframe}
}
\value{
A plot
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
plot1 <- plot_stock_trends(trends2018)
}

}
\references{
The ICES stock information Database web sevices: \url{http://sid.ices.dk/services/}
}
\seealso{
\code{\link{format_sag}} for formatting data from the ICES Stock Assessment database. 

\code{\link{icesFO-package}} gives an overview of the package.
}
