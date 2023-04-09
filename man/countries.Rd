\name{country_codes}
\alias{country_codes}

\title{Get country codes}


\description{
Get country codes for all countries in the world.
}

\usage{
country_codes(query=NULL)
}


\arguments{
  \item{query}{character. A single word that can be used to subset the returned data.frame}
}


\value{
data.frame
}

\examples{
cc <- country_codes()
head(cc)

p <- country_codes(query="Per")
p
}
