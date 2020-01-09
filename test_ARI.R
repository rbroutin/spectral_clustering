# Chargement des donnees et tests
load("smiley.RData")
plot(D$x, col=D$classes)
df <- as.data.frame(D$x)

# Classes théoriques
theo_class <- D$classes
theo_class <- as.numeric(theo_class)
# On crée de fausses valeurs de classes prédites
pred_class <- sample(1:4, length(D$classes), replace=TRUE)
pred_class <- as.numeric(pred_class)

# Matrice de confusion entre donnée observé et prédite
MC <- table(theo_class, pred_class)
MC <- as.matrix(MC)



#### Calcul ARI
# Les constantes
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

# ARI
n <- sum(MC)
numerateur <- nij - ((ni * nj) / choose(n,2))
denominateur <- (0.5 * (ni + nj)) - ((ni * nj) / choose(n,2))
ari2 <- numerateur/denominateur
print(ari2)


# Comparaison des résultats avec librairie(fossil)
library(fossil)
print(adj.rand.index(pred_class, theo_class))



# FONCTION ARI
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


my_ari(theo_class, pred_class)

