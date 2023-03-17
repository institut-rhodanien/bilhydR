#' Génère les éléments de sortie graphique
#'
#'
#' @param deb la date de débourrement, en doy. Defaut = 91
#' @param flo la date de floraison, en doy. Defaut =  152
#' @param ver la date de véraison, en doy. Defaut =  213
#' @param rec la date de récolte, en doy. Defaut =  252
#' @param annee l'année
#'
#' @return Un data frame avec les seuils de classes de contrainte
#' @return Un vecteur de dates
#'
#' @examples
#' BH_graphics(deb = 95, flo = 154, 2023)
#'
#' @export




BH_graphics <- function(deb = 91, flo = 152, ver = 213, rec = 252, annee){
{
doy_seq <- seq(91,273, 7)
jour_seq <- format(seq.Date(from = as.Date(paste0(annee,"-04-01")), to = as.Date(paste0(annee,"-09-30")), by = "week"),"%d %b")

dt_seuils <- data.frame(
  stringsAsFactors = FALSE,
  A = c( # A
    -0.000928035707206787, 0.008437986641672,
    -0.244036965364011, -0.302444997155871
  ),
  B = c( # B
    0.002786976777978, -0.034254170557438,
    -0.417485050007359, -0.568647405386464
  ),
  C = c(# C
    -0.200431504744377, -0.213716880511673,
    -0.597569056550461, -0.783261595442008
  ),
  D = c(# D
    -0.296534222386228, -0.335304148375041,
    -0.758667677372952, -0.997112701636749
  ),
  E = c(-1.5, -1.5, -1.5, -1.5)
)



eqA1 <- -ver*rec^2*dt_seuils$A[1]+ver*rec^2*dt_seuils$A[2]-deb*rec^2*dt_seuils$A[2]-flo*rec^2*dt_seuils$A[3]+flo*rec^2*dt_seuils$A[1]+deb*rec^2*dt_seuils$A[3]+deb^2*ver*dt_seuils$A[1]-deb^2*ver*dt_seuils$A[2]+deb^3*dt_seuils$A[2]+deb^2*flo*dt_seuils$A[3]-deb^2*flo*dt_seuils$A[1]-deb^3*dt_seuils$A[3]
eqA2 <- deb^2*flo-flo*ver^2+deb*ver^2-deb^2*ver+flo^2*ver-deb*flo^2
eqA3 <- deb^3*ver-flo^3*ver+flo^3*deb-deb^3*flo+ver^3*flo-ver^3*deb

eqB1 <- -ver*rec^2*dt_seuils$B[1]+ver*rec^2*dt_seuils$B[2]-deb*rec^2*dt_seuils$B[2]-flo*rec^2*dt_seuils$B[3]+flo*rec^2*dt_seuils$B[1]+deb*rec^2*dt_seuils$B[3]+deb^2*ver*dt_seuils$B[1]-deb^2*ver*dt_seuils$B[2]+deb^3*dt_seuils$B[2]+deb^2*flo*dt_seuils$B[3]-deb^2*flo*dt_seuils$B[1]-deb^3*dt_seuils$B[3]
eqB2 <- deb^2*flo-flo*ver^2+deb*ver^2-deb^2*ver+flo^2*ver-deb*flo^2
eqB3 <- deb^3*ver-flo^3*ver+flo^3*deb-deb^3*flo+ver^3*flo-ver^3*deb

eqC1 <- -ver*rec^2*dt_seuils$C[1]+ver*rec^2*dt_seuils$C[2]-deb*rec^2*dt_seuils$C[2]-flo*rec^2*dt_seuils$C[3]+flo*rec^2*dt_seuils$C[1]+deb*rec^2*dt_seuils$C[3]+deb^2*ver*dt_seuils$C[1]-deb^2*ver*dt_seuils$C[2]+deb^3*dt_seuils$C[2]+deb^2*flo*dt_seuils$C[3]-deb^2*flo*dt_seuils$C[1]-deb^3*dt_seuils$C[3]
eqC2 <- deb^2*flo-flo*ver^2+deb*ver^2-deb^2*ver+flo^2*ver-deb*flo^2
eqC3 <- deb^3*ver-flo^3*ver+flo^3*deb-deb^3*flo+ver^3*flo-ver^3*deb

eqD1 <- -ver*rec^2*dt_seuils$D[1]+ver*rec^2*dt_seuils$D[2]-deb*rec^2*dt_seuils$D[2]-flo*rec^2*dt_seuils$D[3]+flo*rec^2*dt_seuils$D[1]+deb*rec^2*dt_seuils$D[3]+deb^2*ver*dt_seuils$D[1]-deb^2*ver*dt_seuils$D[2]+deb^3*dt_seuils$D[2]+deb^2*flo*dt_seuils$D[3]-deb^2*flo*dt_seuils$D[1]-deb^3*dt_seuils$D[3]
eqD2 <- deb^2*flo-flo*ver^2+deb*ver^2-deb^2*ver+flo^2*ver-deb*flo^2
eqD3 <- deb^3*ver-flo^3*ver+flo^3*deb-deb^3*flo+ver^3*flo-ver^3*deb

eqE1 <- -ver*rec^2*dt_seuils$E[1]+ver*rec^2*dt_seuils$E[2]-deb*rec^2*dt_seuils$E[2]-flo*rec^2*dt_seuils$E[3]+flo*rec^2*dt_seuils$E[1]+deb*rec^2*dt_seuils$E[3]+deb^2*ver*dt_seuils$E[1]-deb^2*ver*dt_seuils$E[2]+deb^3*dt_seuils$E[2]+deb^2*flo*dt_seuils$E[3]-deb^2*flo*dt_seuils$E[1]-deb^3*dt_seuils$E[3]
eqE2 <- deb^2*flo-flo*ver^2+deb*ver^2-deb^2*ver+flo^2*ver-deb*flo^2
eqE3 <- deb^3*ver-flo^3*ver+flo^3*deb-deb^3*flo+ver^3*flo-ver^3*deb

coefAA <- (eqA2*((flo-deb)*dt_seuils$A[4]-(-rec*dt_seuils$A[1]+rec*dt_seuils$A[2])-(flo-deb)*dt_seuils$A[1]-deb*dt_seuils$A[1]+deb*dt_seuils$A[2])-eqA1*(flo-deb)-(-ver*dt_seuils$A[1]+ver*dt_seuils$A[2]-deb*dt_seuils$A[2]-flo*dt_seuils$A[3]+flo*dt_seuils$A[1]+deb*dt_seuils$A[3])*(rec*(deb^2-flo^2)-deb*(deb^2-flo^2)))/((eqA2*rec^3*(flo-deb)+eqA3*rec^2*(flo-deb)+eqA2*rec*(deb^3-flo^3)-eqA2*deb^3*(flo-deb)-eqA3*deb^2*(flo-deb)-eqA2*deb*(deb^3-flo^3))+eqA3*(rec*(deb^2-flo^2)-deb*(deb^2-flo^2)))
coefBA <- (coefAA*eqA3-ver*dt_seuils$A[1]+ver*dt_seuils$A[2]-deb*dt_seuils$A[2]-flo*dt_seuils$A[3]+flo*dt_seuils$A[1]+deb*dt_seuils$A[3])/eqA2
coefCA <- (coefAA*(deb^3-flo^3)+coefBA*(deb^2-flo^2)-dt_seuils$A[1]+dt_seuils$A[2])/(flo-deb)
coefDA <- dt_seuils$A[1]-coefAA*deb^3-coefBA*deb^2-coefCA*deb

coefAB <- (eqB2*((flo-deb)*dt_seuils$B[4]-(-rec*dt_seuils$B[1]+rec*dt_seuils$B[2])-(flo-deb)*dt_seuils$B[1]-deb*dt_seuils$B[1]+deb*dt_seuils$B[2])-eqB1*(flo-deb)-(-ver*dt_seuils$B[1]+ver*dt_seuils$B[2]-deb*dt_seuils$B[2]-flo*dt_seuils$B[3]+flo*dt_seuils$B[1]+deb*dt_seuils$B[3])*(rec*(deb^2-flo^2)-deb*(deb^2-flo^2)))/((eqB2*rec^3*(flo-deb)+eqB3*rec^2*(flo-deb)+eqB2*rec*(deb^3-flo^3)-eqB2*deb^3*(flo-deb)-eqB3*deb^2*(flo-deb)-eqB2*deb*(deb^3-flo^3))+eqB3*(rec*(deb^2-flo^2)-deb*(deb^2-flo^2)))
coefBB <- (coefAB*eqB3-ver*dt_seuils$B[1]+ver*dt_seuils$B[2]-deb*dt_seuils$B[2]-flo*dt_seuils$B[3]+flo*dt_seuils$B[1]+deb*dt_seuils$B[3])/eqB2
coefCB <- (coefAB*(deb^3-flo^3)+coefBB*(deb^2-flo^2)-dt_seuils$B[1]+dt_seuils$B[2])/(flo-deb)
coefDB <- dt_seuils$B[1]-coefAB*deb^3-coefBB*deb^2-coefCB*deb

coefAC <- (eqC2*((flo-deb)*dt_seuils$C[4]-(-rec*dt_seuils$C[1]+rec*dt_seuils$C[2])-(flo-deb)*dt_seuils$C[1]-deb*dt_seuils$C[1]+deb*dt_seuils$C[2])-eqC1*(flo-deb)-(-ver*dt_seuils$C[1]+ver*dt_seuils$C[2]-deb*dt_seuils$C[2]-flo*dt_seuils$C[3]+flo*dt_seuils$C[1]+deb*dt_seuils$C[3])*(rec*(deb^2-flo^2)-deb*(deb^2-flo^2)))/((eqC2*rec^3*(flo-deb)+eqC3*rec^2*(flo-deb)+eqC2*rec*(deb^3-flo^3)-eqC2*deb^3*(flo-deb)-eqC3*deb^2*(flo-deb)-eqC2*deb*(deb^3-flo^3))+eqC3*(rec*(deb^2-flo^2)-deb*(deb^2-flo^2)))
coefBC <- (coefAC*eqC3-ver*dt_seuils$C[1]+ver*dt_seuils$C[2]-deb*dt_seuils$C[2]-flo*dt_seuils$C[3]+flo*dt_seuils$C[1]+deb*dt_seuils$C[3])/eqC2
coefCC <- (coefAC*(deb^3-flo^3)+coefBC*(deb^2-flo^2)-dt_seuils$C[1]+dt_seuils$C[2])/(flo-deb)
coefDC <- dt_seuils$C[1]-coefAC*deb^3-coefBC*deb^2-coefCC*deb

coefAD <- (eqD2*((flo-deb)*dt_seuils$D[4]-(-rec*dt_seuils$D[1]+rec*dt_seuils$D[2])-(flo-deb)*dt_seuils$D[1]-deb*dt_seuils$D[1]+deb*dt_seuils$D[2])-eqD1*(flo-deb)-(-ver*dt_seuils$D[1]+ver*dt_seuils$D[2]-deb*dt_seuils$D[2]-flo*dt_seuils$D[3]+flo*dt_seuils$D[1]+deb*dt_seuils$D[3])*(rec*(deb^2-flo^2)-deb*(deb^2-flo^2)))/((eqD2*rec^3*(flo-deb)+eqD3*rec^2*(flo-deb)+eqD2*rec*(deb^3-flo^3)-eqD2*deb^3*(flo-deb)-eqD3*deb^2*(flo-deb)-eqD2*deb*(deb^3-flo^3))+eqD3*(rec*(deb^2-flo^2)-deb*(deb^2-flo^2)))
coefBD <- (coefAD*eqD3-ver*dt_seuils$D[1]+ver*dt_seuils$D[2]-deb*dt_seuils$D[2]-flo*dt_seuils$D[3]+flo*dt_seuils$D[1]+deb*dt_seuils$D[3])/eqD2
coefCD <- (coefAD*(deb^3-flo^3)+coefBD*(deb^2-flo^2)-dt_seuils$D[1]+dt_seuils$D[2])/(flo-deb)
coefDD <- dt_seuils$D[1]-coefAD*deb^3-coefBD*deb^2-coefCD*deb

coefAE <- (eqE2*((flo-deb)*dt_seuils$E[4]-(-rec*dt_seuils$E[1]+rec*dt_seuils$E[2])-(flo-deb)*dt_seuils$E[1]-deb*dt_seuils$E[1]+deb*dt_seuils$E[2])-eqE1*(flo-deb)-(-ver*dt_seuils$E[1]+ver*dt_seuils$E[2]-deb*dt_seuils$E[2]-flo*dt_seuils$E[3]+flo*dt_seuils$E[1]+deb*dt_seuils$E[3])*(rec*(deb^2-flo^2)-deb*(deb^2-flo^2)))/((eqE2*rec^3*(flo-deb)+eqE3*rec^2*(flo-deb)+eqE2*rec*(deb^3-flo^3)-eqE2*deb^3*(flo-deb)-eqE3*deb^2*(flo-deb)-eqE2*deb*(deb^3-flo^3))+eqE3*(rec*(deb^2-flo^2)-deb*(deb^2-flo^2)))
coefBE <- (coefAE*eqE3-ver*dt_seuils$E[1]+ver*dt_seuils$E[2]-deb*dt_seuils$E[2]-flo*dt_seuils$E[3]+flo*dt_seuils$E[1]+deb*dt_seuils$E[3])/eqE2
coefCE <- (coefAE*(deb^3-flo^3)+coefBE*(deb^2-flo^2)-dt_seuils$E[1]+dt_seuils$E[2])/(flo-deb)
coefDE <- dt_seuils$E[1]-coefAE*deb^3-coefBE*deb^2-coefCE*deb


seuils_dad <- data.frame()

for (i in 1:rec) {
  seuils_dad[i,"doy"] <- i
  seuils_dad[i,"A"] <- 0
  seuils_dad[i,"B"] <- ifelse(ifelse(i<deb,NA,ifelse(i>rec,NA,coefAA*i^3+coefBA*i^2+coefCA*i+coefDA))>0, 0,ifelse(i<deb,NA,ifelse(i>rec,NA,coefAA*i^3+coefBA*i^2+coefCA*i+coefDA)))
  seuils_dad[i,"C"] <- ifelse(ifelse(i<deb,NA,ifelse(i>rec,NA,coefAB*i^3+coefBB*i^2+coefCB*i+coefDB))>0, 0,ifelse(i<deb,NA,ifelse(i>rec,NA,coefAB*i^3+coefBB*i^2+coefCB*i+coefDB)))
  seuils_dad[i,"D"] <- ifelse(i<deb,NA,ifelse(i>rec,NA,coefAC*i^3+coefBC*i^2+coefCC*i+coefDC))
  seuils_dad[i,"E"] <- ifelse(i<deb,NA,ifelse(i>rec,NA,coefAD*i^3+coefBD*i^2+coefCD*i+coefDD))
}

maxdoy <- max(seuils_dad$doy)

for (j in maxdoy+1:273) {
  seuils_dad[j,"doy"] <- j
  seuils_dad[j,"A"] <- seuils_dad[maxdoy,"A"]
  seuils_dad[j,"B"] <- seuils_dad[maxdoy,"B"]
  seuils_dad[j,"C"] <- seuils_dad[maxdoy,"C"]
  seuils_dad[j,"D"] <- seuils_dad[maxdoy,"D"]
  seuils_dad[j,"E"] <- seuils_dad[maxdoy,"E"]

}

}

seuils_classes <- seuils_dad%>%pivot_longer(cols=c(2:6),values_to = "PHFB", names_to = "Classe")
return(seuils_classes)
return(doy_seq, jour_seq)
}
