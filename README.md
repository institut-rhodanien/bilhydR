# Bilan hydrique viticole avec R

Ce package fournit quelques fonctions simples pour l'utilisation du bilan hydrique sous R.

Le package permet :

-   de récupérer les données météorologiques sous EPICURE
-   de modéliser le bilan hydrique
-   de réaliser des sorties graphiques

## Installation

Pour installer la dernière version de bilhydR :

    library(remotes)
    install_github("institut-rhodanien/bilhydR")

## Exemple d'utilisation
### Générer des bilans hydriques + graphiques
```
    # LIBRAIRIES ----
    # remotes::install_github("institut-rhodanien/bilhydR")

    library(bilhydR)
    library(lubridate)
    library(tidyverse)

    # données d'exemple fournies avec le package
    data <- meteoStHilaire2021
    datedeb <- "2021-04-01"


    BHdata <- BHmulti(meteoStHilaire2021, deb=datedeb)

    seuils_classes <- bilhydR::BH_graphics(annee = 2021)$seuils_classes
    doy_seq <- bilhydR::BH_graphics(annee = 2021)$doy_seq
    jour_seq <- bilhydR::BH_graphics(annee = 2021)$jour_seq

    ylim.prim <- c(0, 80)   # seuils precip
    ylim.sec <- c(-1.5, 0)    # seuils phfb

    b <- diff(ylim.prim)/diff(ylim.sec)
    a <- ylim.prim[1] - b*ylim.sec[1]

    ggplot(data = BHdata%>%filter(date > datedeb)) +
      geom_area(data=seuils_classes%>%filter(Classe == "A"),
                aes(x= doy, y= a + PHFB*b,fill="A"))+
      geom_area(data=seuils_classes%>%filter(Classe == "B"),
                aes(x= doy, y= a + PHFB*b,fill="B"))+
      geom_area(data=seuils_classes%>%filter(Classe == "C"),
                aes(x= doy, y= a + PHFB*b,fill="C"))+
      geom_area(data=seuils_classes%>%filter(Classe == "D"),
                aes(x= doy, y= a + PHFB*b,fill="D"))+
      geom_area(data=seuils_classes%>%filter(Classe == "E"),
                aes(x= doy, y= a + PHFB*b, fill="E"))+
      scale_fill_manual(breaks = c('A','B','C','D','E'), values = c(
        '#99CCFF','#CCFFCC','#FFFF99','#FFCC00','#FF9900'
      ))+
      geom_col(data = BHdata%>%filter(date > datedeb), aes(x = doy, pluie), fill = "#3E9DFD", color = NA)  +
      geom_line(aes(x=doy, y = a + PHFB80*b,color = "80 mm"), size= 1) +
      geom_line(aes(x=doy,y = a + PHFB120*b, color = "120 mm"), size= 1) +
      geom_line(aes(x=doy,y = a + PHFB180*b, color = "180 mm"), size= 1) +

      scale_color_manual(breaks = c('80 mm','120 mm', '180 mm'), values = c('#2C0707', '#DC2323', '#f8D3D3'))+
      scale_x_continuous(limits = c(91,273), breaks = doy_seq,
                         labels = jour_seq, expand = c(0,0))+
      scale_y_continuous("Precipitations (mm)", breaks = seq(0,100,10), expand = c(0, 0), 
                         sec.axis = sec_axis(~ (. - a)/b, name = "PHFB (MPa)\n", breaks = seq(-1.5, 0, 0.1))) +
      labs(x="", title = "Bilan Hydrique - St-Hilaire", color = "RU")+
      guides(fill="none")+
      theme_light()+
      theme(
        axis.line = element_line(color = "black"),
        axis.text = element_text(color = "black"),
        axis.text.x = element_text(angle=90, hjust = 1, vjust=0.5),
        axis.ticks = element_line(color="black"),
        panel.border = element_rect(colour = "black", fill=NA),
        legend.position = c(0.1, 0.65)
      )


    ggplot(BHdata)+
      geom_line(aes(x=doy, y = FTSW80))+
      scale_x_continuous(limits = c(91,273), breaks = doy_seq,
                         labels = jour_seq, expand = c(0,0))+
      labs(x="", title = "FTSW - St-Hilaire")+
      theme_light()+
      theme(
        axis.line = element_line(color = "black"),
        axis.text = element_text(color = "black"),
        axis.text.x = element_text(angle=90, hjust = 1, vjust=0.5),
        axis.ticks = element_line(color="black"),
        panel.border = element_rect(colour = "black", fill=NA),
        legend.position = c(0.1, 0.65)
      )
```
![](https://image.noelshack.com/fichiers/2023/11/5/1679063296-screenshot-3.png)

### Récupérer des données météo sous EPICURE

```
# LOGIN/PASS EPICURE----

login <- "monlogin"
pass <- "monmdp"

# liste des fichiers disponibles sur le FTP avec login/mdp
EPICUREfiles("monlogin", "monmdp")

# recupere les données meteo d'un fichier

meteo2021 <- recupMeteo(login, pass, "mon_super_fichier_2023.csv")

# recupere les données meteo d'un POM

id_pom <- "XXXXXXXXX"
nom_station <- "Perpète-lès-oies"

result <- fromJSON(getURL(paste0("ftp://",login,":",pass,"@ftp.vignevin-epicure.com/",id_pom,"/meteo/2023.json" )))
  
meteo <- t(as.data.frame(do.call(cbind, result)))%>%
    as.data.frame()%>%
    rownames_to_column(var="date")%>%
    dplyr::select(1:11)%>%
    as_tibble()%>%
    mutate(station = nom_station)%>%
    mutate_at(.vars = c(3:11), .funs = as.character)%>%
    mutate_at(.vars = c(3:11), .funs = as.numeric)%>%
    mutate_at(.vars = c(3:11), .funs = ~round(.x, digits=2))%>%
    mutate(date = as.Date(date, "%Y/%m/%d"))%>%
    mutate(doy = yday(date))%>%
    dplyr::filter(prev == FALSE)%>%
    dplyr::select(12,1,13,6,7,8,3,5,4,9,10,11)

```

## Contribuer

Les contributions de toutes sortes sont les bienvenues, issues et pull requests sont la manière préférentielle de les partager.


## Références

Payan J.-C. et Salançon E. 2003. DEFINIR LE REGIME HYDRIQUE DES PARCELLES ET DES MILLESIMES. Wine internet technical journal, (15), p. 5.
Payan J.-C. 2007. Contrôle du stress hydrique pour la gestion de l’irrigation en viticulture. Dans : Euroviti. p. 7. Disponible sur : https://www.vignevin-occitanie.com/wp-content/uploads/2018/10/20-stressHydrique_JCP_2007.pdf
Payan J.-C. 2012. Outils de gestion de l’irrigation au vignoble. Présenté à : 2012- Soirées Rhodaniennes, Institut Rhodanien. Disponible sur : https://www.institut-rhodanien.com/upload/article/file/15irrigationjeanchristophepayan-616432c044e51.pdf 
Payan J.-C. et Dufourcq T. 2019. L’irrigation de la vigne. Disponible sur : https://www.vignevin-occitanie.com/fiches-pratiques/irrigation-de-la-vigne/ 
