% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/BHmulti.R
\name{BHmulti}
\alias{BHmulti}
\title{Génère le Bilan Hydrique à 3 horizons différents}
\usage{
BHmulti(data, deb)
}
\arguments{
\item{data}{les données météo du site à modéliser}

\item{deb}{la date de débourrement}
}
\value{
Un data frame avec les PHFB à 80, 120 et 180mm
}
\description{
Génère le Bilan Hydrique à 3 horizons différents : sol superficiel (ru 80 mm), sol moyen (ru 120 mm) et sol profond (ru 180 mm)
}
\examples{
BHmulti(data, datedeb)

}
