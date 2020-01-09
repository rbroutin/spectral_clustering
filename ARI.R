# FONCTION my_ari
# Permettant de calculer le 'Adjusted Rand Index'
# Prends en input P0 et P1, vecteurs comportant les données sur la répartition des classes
# théoriques et observées respectivement (même si inversé les deux ne changera pas le résultat).


my_ari <- function (P0, P1) {
  # Matrice de confusion entre donnée observé et prédite
  MC <- table(theo_class, pred_class)
  MC <- as.matrix(MC)
  
  # Calcul des constantes
  ni <- 0
  nj <- 0
  nij <- 0
  for (i in 1:nrow(MC)) {
    ni <- ni + sum(choose(sum(MC[i,]),2))
    nj <- nj + sum(choose(sum(MC[,i]),2))
    for (j in 1:ncol(MC)) {
      nij <- nij + sum(choose(MC[i,j],2))
    }
  }
  
  # Calcul ARI
  n <- sum(MC)
  numerateur <- nij - ((ni * nj) / choose(n,2))
  denominateur <- (0.5 * (ni + nj)) - ((ni * nj) / choose(n,2))
  ari <- numerateur/denominateur
  
  # retour du résultat
  return(ari)
}


