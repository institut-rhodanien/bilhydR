#' Récupération des données météo
#'
#' Récupère les données météo sur la base d'un login, password et nom de fichier
#'
#' @param loginEpicure votre login d'accès à EPICURE
#' @param passEpicure votre mot de passe pour EPICURE
#' @param nomfichier le nom du fichier à consulter
#'
#' @return Un data frame avec les données météo pour chaque site.
#'
#' @examples
#' recupMeteo("monlogin", "monpass", "monfichier.csv")
#'
#' @export
recupMeteo <- function(loginEpicure, passEpicure, nomfichier) {
  tmp <- utils::read.csv2(paste0("ftp://", loginEpicure, ":", passEpicure, "@ftp.vignevin-epicure.com/", nomfichier)) %>%
    dplyr::select(-X) %>%
    dplyr::mutate(DATE_MESURE = as.Date(DATE_MESURE, format = "%d/%m/%y")) %>%
    dplyr::mutate(DOY = lubridate::yday(DATE_MESURE)) %>%
    dplyr::arrange(Site, DOY) %>%
    dplyr::select(1, 2, 13, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12) %>%
    janitor::clean_names() %>%
    dplyr::rename(date = date_mesure)
  cat("\nDes donn\u00E9es sont disponibles du", as.character(tmp$date[1]), "au", as.character(tmp$date[nrow(tmp)]), "\n\n")
  cat("Il y a", length(unique(tmp[, 1])), "site(s) disponible(s) :\n\n")
  cat(unique(tmp[, 1]), sep = "\n")
  return(tmp)
}
