#' Récupération de la date de veraison
#'
#' Calcul la date de veraison de la parcelle de vigne sur la base d'un dataframe de données météo, d'un site, d'une méthode de calcul et d'un cépage
#'
#' @param data le data frame des données météo
#' @param site le site (parcelle) choisi
#' @param methode choisir entre plusieurs méthode d'estimation de la date de veraison ("GFV")
#' @param cepage choisir le cépage pour ajuster le modèle au cépage ("Grenache", "Syrah")
#'
#' @return La date de débourrement du site choisi
#'
#' @examples
#' dateVeraison(meteo, "GFV", "Syrah")
#'
#' @export
dateVeraison <- function(data, methode, cepage) {

  if (methode == "GFV") {
    seuilGrenache <- 2750

    data_GDD0 <- data %>%
      dplyr::filter(doy>59)%>%
      dplyr::rowwise() %>%
      dplyr::mutate(C0 = max(0, tmoy)) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(GDD0 = cumsum(C0))

    if (cepage == "Grenache") {
      data_GDD0 %>%
        dplyr::filter(GDD0 > seuilGrenache) %>%
        dplyr::slice(1) %>%
        dplyr::pull(date)
    }
  }
}
