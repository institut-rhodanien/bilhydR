% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dateVeraison.R
\name{dateVeraison}
\alias{dateVeraison}
\title{Récupération de la date de veraison}
\usage{
dateVeraison(data, methode, cepage)
}
\arguments{
\item{data}{le data frame des données météo}

\item{methode}{choisir entre plusieurs méthode d'estimation de la date de veraison ("GFV")}

\item{cepage}{choisir le cépage pour ajuster le modèle au cépage ("Grenache", "Syrah")}

\item{site}{le site (parcelle) choisi}
}
\value{
La date de débourrement du site choisi
}
\description{
Calcul la date de veraison de la parcelle de vigne sur la base d'un dataframe de données météo, d'un site, d'une méthode de calcul et d'un cépage
}
\examples{
dateVeraison(meteo, "GFV", "Syrah")

}
