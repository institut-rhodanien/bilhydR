% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/BH.R
\name{BH}
\alias{BH}
\title{Calcul du bilan hydrique}
\usage{
BH(
  data,
  deb,
  ru,
  dd_Kvmax = 600,
  Kvmax = 0.55,
  dd_D = 80,
  irrig = 0,
  taux_ieff = 1,
  FTSWini = 1,
  seuil_Peff = 5,
  taux_Peff = 0.5,
  regul_P_FTSW = 0.5,
  regul_Tv_FTSW = 0.4,
  coef_a = 1.0572,
  coef_b = 5.3452,
  PHFBmin = -1.5,
  j_Es_P = 5,
  j_Es_sup = 0
)
}
\arguments{
\item{data}{les données météo}

\item{deb}{la date de débourrement}

\item{ru}{la réserve utile, en mm}

\item{dd_Kvmax}{. La valeur par défaut est 600.}

\item{Kvmax}{. La valeur par défaut est 0.55}

\item{dd_D}{. La valeur par défaut est 80.}

\item{irrig}{L'irrigation apportée sur le site, en mm. Doit être un vecteur de la taille du tableau de données météo. La valeur par défaut est 0.}

\item{taux_ieff}{Taux d'efficacité de l'irrigation. La valeur par défaut est 1.}

\item{FTSWini}{. La valeur par défaut est 100.}

\item{seuil_Peff}{Le seuil (en mm) de pluie efficace. La valeur par défaut est 5.}

\item{taux_Peff}{Le taux d'efficacité des pluies. La valeur par défaut est 0.5.}

\item{regul_P_FTSW}{. La valeur par défaut est 50.}

\item{regul_Tv_FTSW}{. La valeur par défaut est 40.}

\item{coef_a}{. La valeur par défaut est 1.0572.}

\item{coef_b}{. La valeur par défaut est 5.3452.}

\item{PHFBmin}{. La valeur par défaut est -1.5.}

\item{j_Es_P}{. La valeur par défaut est 5.}

\item{j_Es_sup}{. La valeur par défaut est 0.}
}
\value{
Un data frame avec les données du bilan hydrique calculées
}
\description{
Calcul le bilan hydrique sur la base des données météo, de la parcelle choisie, de la date de débourrement et de la réserve utile
}
\examples{
BH(meteo, "2021-04-01", 90)
}
