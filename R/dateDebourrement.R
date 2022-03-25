#' Récupération de la date de débourrement
#'
#' Calcul la date de débourrement de la parcelle de vigne sur la base d'un dataframe de données météo, d'un site, d'une méthode de calcul et d'un cépage
#'
#' @param data le data frame des données météo
#' @param site le site (parcelle) choisi
#' @param methode choisir entre plusieurs méthode d'estimation du débourrement ("GDD5", "GDD10")
#' @param cepage choisir le cépage pour ajuster le modèle au cépage ("Grenache", "Syrah")
#'
#' @return La date de débourrement du site choisi
#'
#' @examples
#' dateDebourrement(meteo, "Parcelle1", "GDD5", "Syrah")
#'
#' @export
dateDebourrement <- function(data, methode, cepage) {

  if (methode == "GDD5") {
    seuilGrenache <- 321

    data_GDD5 <- data %>%
      dplyr::rowwise() %>%
      dplyr::mutate(C5 = max(0, tmoy - 5)) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(GDD5 = cumsum(C5))

    if (cepage == "Grenache") {
      data_GDD5 %>%
        dplyr::filter(GDD5 > seuilGrenache) %>%
        dplyr::slice(1) %>%
        dplyr::pull(date)
    }
  }
}
