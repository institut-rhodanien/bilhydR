#' Calcul du bilan hydrique
#'
#' Calcul le bilan hydrique sur la base des données météo, de la parcelle choisie, de la date de débourrement et de la réserve utile
#'
#' @param data les données météo
#' @param deb la date de débourrement
#' @param ru la réserve utile, en mm
#' @param dd_Kvmax . La valeur par défaut est 600.
#' @param Kvmax . La valeur par défaut est 0.55
#' @param dd_D . La valeur par défaut est 80.
#' @param irrig L'irrigation apportée sur le site, en mm. Doit être un vecteur de la taille du tableau de données météo. La valeur par défaut est 0.
#' @param taux_ieff Taux d'efficacité de l'irrigation. La valeur par défaut est 1.
#' @param FTSWini . La valeur par défaut est 100.
#' @param seuil_Peff Le seuil (en mm) de pluie efficace. La valeur par défaut est 5.
#' @param taux_Peff Le taux d'efficacité des pluies. La valeur par défaut est 0.5.
#' @param regul_P_FTSW . La valeur par défaut est 50.
#' @param regul_Tv_FTSW . La valeur par défaut est 40.
#' @param coef_a . La valeur par défaut est 1.0572.
#' @param coef_b . La valeur par défaut est 5.3452.
#' @param PHFBmin . La valeur par défaut est -1.5.
#' @param j_Es_P . La valeur par défaut est 5.
#' @param j_Es_sup . La valeur par défaut est 0.
#' @return Un data frame avec les données du bilan hydrique calculées
#'
#' @examples
#' BH(meteo, "2021-04-01", 90)
#' @export


BH <- function(data, deb, ru, dd_Kvmax = 600, Kvmax = 0.55, dd_D = 80, irrig = 0,taux_ieff = 1,
               FTSWini = 1, seuil_Peff = 5, taux_Peff = 0.5, regul_P_FTSW = 0.5, regul_Tv_FTSW = 0.4,
               coef_a = 1.0572, coef_b = 5.3452, PHFBmin = -1.5, j_Es_P = 5, j_Es_sup = 0) {
  {
    date_regul_P <- deb
    date_FTSWini <- deb
    TTSW <- ru

    data <- data %>%
      dplyr::mutate(irrig = irrig)

  }
  # temperatures GDD 10 ---------------

  data_GDD10 <- data %>%
    dplyr::rowwise() %>%
    dplyr::mutate(C10 = max(0, tmoy - 10)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(GDD10 = cumsum(C10))

  # data kv -------------

  result_ddD <- data_GDD10 %>%
    dplyr::filter(date == deb) %>%
    dplyr::slice(1) %>%
    dplyr::pull(GDD10)


  date_Kvmax <- data_GDD10 %>%
    dplyr::filter(GDD10 >= result_ddD + dd_Kvmax - dd_D) %>%
    dplyr::slice(1) %>%
    dplyr::pull(date)

  if(length(date_Kvmax)==0){
    date_Kvmax <- as.Date(paste0(year(deb),"-06","-15"))
  }

  data_kv <- data_GDD10 %>%
    dplyr::mutate(kv = dplyr::case_when(
      date < deb ~ 0,
      date > date_Kvmax ~ Kvmax,
      TRUE ~ (GDD10 - result_ddD) / (dd_Kvmax - result_ddD) * Kvmax
    ))

  # irrigation efficace
  data_ieff <- data_kv %>%
    dplyr::mutate(ieff = irrig * taux_ieff)

  # attributs interdependants = on fait tout ensemble

  # on fixe les valeurs de d?part
  data_tmp <- data_ieff %>%
    dplyr::mutate(
      Tv = 0,
      coefregulateur = 1,
      Peff = 0,
      D = 0,
      Nb_j_Es_co_PD = 0,
      Jours_Es = 0,
      Es_th = 0,
      Cumul_P_si_jour_Es = 0,
      Cumul_Es_reelle = 0,
      Cumul_ES_max_depuis_jours_Es_sup_0 = 0,
      Es_max = 0,
      Es_reelle = 0,
      ETR = 0,
      ATSW = 0,
      FTSW = 0,
      PHFB = 0
    )

  data_tmp$ATSW[1] <- TTSW
  data_tmp$FTSW[1] <- 1


  for (j in 2:nrow(data_tmp)) {
    # Peff
    data_tmp$Peff[j] <- ifelse((isTRUE((data_tmp$date[j] > date_regul_P) && (data_tmp$FTSW[j - 1] < regul_P_FTSW))) == TRUE,
      max(0, (data_tmp$pluie[j] - seuil_Peff) * taux_Peff, na.rm = T), data_tmp$Peff[j] <- data_tmp$pluie[j]
    )
    # Tv

    data_tmp$Tv[j] <- ifelse(data_tmp$FTSW[j - 1] > regul_Tv_FTSW, data_tmp$kv[j] * data_tmp$etp[j],
                            data_tmp$kv[j] * data_tmp$etp[j] * data_tmp$ATSW[j - 1] / regul_Tv_FTSW/TTSW)


    data_tmp$coefregulateur[j] <- min(1,data_tmp$ATSW[j - 1] / regul_Tv_FTSW/TTSW)

    # D

    data_tmp$D[j] <- max(0, data_tmp$ATSW[j - 1] + data_tmp$Peff[j] - TTSW, na.rm = T)

    # Nb_j_Es_co_PD

    data_tmp$Nb_j_Es_co_PD[j] <- max(0, ceiling((data_tmp$Peff[j] - data_tmp$D[j]) / j_Es_P + j_Es_sup), na.rm = T)

    # Jours_ES

    data_tmp$Jours_Es[j] <- ifelse(data_tmp$Jours_Es[j - 1] == 0, data_tmp$Nb_j_Es_co_PD[j], data_tmp$Jours_Es[j - 1] + data_tmp$Nb_j_Es_co_PD[j] - 1)

    # Es_th

    data_tmp$Es_th[j] <- ifelse(data_tmp$Jours_Es[j] == 0, 0, data_tmp$etp[j] * (1 - data_tmp$kv[j]))

    # Cumul_P_si_jour_Es

    data_tmp$Cumul_P_si_jour_Es[j] <- ifelse(data_tmp$Jours_Es[j] == 0, 0, data_tmp$Cumul_P_si_jour_Es[j - 1] + data_tmp$Peff[j] - data_tmp$D[j])

    # Es_max

    data_tmp$Es_max[j] <- ifelse(data_tmp$Nb_j_Es_co_PD[j] == 0, data_tmp$Es_th[j],
      ifelse(((data_tmp$Cumul_P_si_jour_Es[j] - data_tmp$Cumul_Es_reelle[j - 1]) >= (data_tmp$Peff[j] - data_tmp$D[j])),
        min(data_tmp$Cumul_P_si_jour_Es[j - 1] - data_tmp$Cumul_Es_reelle[j - 1] + data_tmp$Peff[j] - data_tmp$D[j], data_tmp$Es_th[j], na.rm = T),
        min(data_tmp$Es_th[j], data_tmp$Peff[j] - data_tmp$D[j], na.rm = T)
      )
    )
    # Cumul_ES_max_depuis_jours_Es_sup_0

    data_tmp$Cumul_ES_max_depuis_jours_Es_sup_0[j] <- ifelse(data_tmp$Jours_Es[j] == 0, 0, data_tmp$Cumul_ES_max_depuis_jours_Es_sup_0[j - 1] + data_tmp$Es_max[j])

    # Es_reelle

    data_tmp$Es_reelle[j] <- ifelse((data_tmp$Jours_Es[j] == 0 | data_tmp$Cumul_Es_reelle[j - 1] == data_tmp$Cumul_P_si_jour_Es[j]), 0,
      ifelse(data_tmp$Cumul_ES_max_depuis_jours_Es_sup_0[j] <= data_tmp$Cumul_P_si_jour_Es[j - 1],
        data_tmp$Es_max[j],
        min(data_tmp$Es_max[j], data_tmp$Cumul_P_si_jour_Es[j - 1] - data_tmp$Cumul_Es_reelle[j - 1] + data_tmp$Peff[j] - data_tmp$D[j], na.rm = T)
      )
    )

    # ETR

    data_tmp$ETR[j] <- data_tmp$Es_reelle[j] + data_tmp$Tv[j]

    # ATSW

    data_tmp$ATSW[j] <- max(0, ifelse(data_tmp$date[j] == date_FTSWini, FTSWini * TTSW, min(TTSW, data_tmp$ATSW[j - 1] + data_tmp$Peff[j] - data_tmp$D[j] + data_tmp$ieff[j] - data_tmp$ETR[j], na.rm = T)))

    # FTSW

    data_tmp$FTSW[j] <- ((data_tmp$ATSW[j - 1] + data_tmp$Peff[j] - data_tmp$D[j]) / TTSW)

    # PHFB

    data_tmp$PHFB[j] <- max(PHFBmin, ifelse(data_tmp$FTSW[j] == 0, PHFBmin, (log(data_tmp$FTSW[j] ) - log(coef_a)) / coef_b))
  }

  return(data_tmp)
}
