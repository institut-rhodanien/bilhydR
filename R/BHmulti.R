#' Génère le Bilan Hydrique à 3 horizons différents
#'
#' Génère le Bilan Hydrique à 3 horizons différents : sol superficiel (ru 80 mm), sol moyen (ru 120 mm) et sol profond (ru 180 mm)
#'
#' @param data les données météo du site à modéliser
#' @param deb la date de débourrement
#'
#' @return Un data frame avec les PHFB à 80, 120 et 180mm
#'
#' @examples
#' BHmulti(data, datedeb)
#'
#' @export
BHmulti <- function(data, deb) {
 BH80 <- BH(data, deb, ru=80)%>%
   rename(PHFB80 = PHFB)

 BH120 <- BH(data, deb, ru=120)%>%
   rename(PHFB120 = PHFB)
 BH180 <- BH(data, deb, ru=180)%>%
   rename(PHFB180 = PHFB)

dt_multi <- BH80
dt_multi$PHFB120 <- BH120$PHFB120
dt_multi$PHFB180 <- BH180$PHFB180
return(dt_multi)
}
