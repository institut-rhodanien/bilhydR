#' Liste les fichiers disponibles sur EPICURE avec un compte
#'
#' Récupère les noms des fichiers disponibles sur EPICURE sur la base d'un login et d'un password
#'
#' @param loginEpicure votre login d'accès à EPICURE
#' @param passEpicure votre mot de passe pour EPICURE
#'
#' @return Un vecteur avec les noms des fichiers accessibles sur le FTP EPICURE avec le login et mot de passe.
#'
#' @examples
#' EPICUREfiles("monlogin", "monpass")
#'
#' @export
EPICUREfiles <- function(loginEpicure, passEpicure) {
  result <- RCurl::getURL(paste0("ftp://",loginEpicure,":",passEpicure,"@ftp.vignevin-epicure.com/"))
  result2 <- paste(strsplit(result, "\r*\n")[[1]], sep = "")
  result3 <- gsub("^.* ", "", result2)

  .GlobalEnv$EPICUREfiles <-   length(result3)

  cat("Il y a", length(result3), "fichiers(s) disponible(s) :\n\n")
  cat(result3,sep = "\n")
}

