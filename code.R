## Chargement des donnees

load("smiley.RData")

## Fonctions

dist_euclidienne <- function(x, y) {
  diff <- sum((x-y)**2)
  d <- sqrt(diff)
  return(d)
}

my_kmeans <- function(X, k, niter = 20) {
  X <- as.matrix(X)
  
  plot(X)
  
  # Tirage au hasard des k premiers centres de classe
  i_c <- sample(nrow(X), k)
  
  # premiere matrice des centres de classe
  M_c <- as.matrix(X[i_c,])
  
  # choisir les points de depart
  #coord <- locator(k)
  #M_c <- matrix(c(coord$x, coord$y), ncol = 2)
  
  # initialisation de la matrice des distances
  M_d <- matrix(nrow = nrow(X), ncol = k)
  
  # boucle avec un nombre d'iteration max
  for(j in 1:niter){
    # stockage des anciens centre de classe
    M_c_a <- M_c
    
    # boucle sur les centres de classe
    for(i in 1:k) {
      M_d[,i] <- apply(X, 1, dist_euclidienne, y = M_c[i,]) # calcul la distance de chaque point au centre de classe
    }
    
    # vecteur des classes
    V_c <- apply(M_d, 1, which.min)
    
    # recalculer les centres des classes
    for(i in 1:ncol(X)){
      M_c[,i] = tapply(X[,i], V_c, mean)
    }
    
    # si les centres sont identiques, alors on stop
    if(all(M_c_a==M_c)){return(V_c)}
  }
}

my_nngraph <- function(X, similarity = "linear", neighbor = "seuil", sigma = 1/8, deg = 3, theta = 0, knn = 3) {
  # transformer en matrix pour appliquer le produit matriciel
  M_X <- as.matrix(X)
  
  # choisi la fonction de similarite
  if(similarity=="linear"){
    similarity_function <- function(Xi, Xj){
      return(Xi%*%Xj)
    }
  }
  else if(similarity=="gaussian"){
    similarity_function <- function(Xi, Xj){
      return(exp(-((Xi-Xj)%*%(Xi-Xj))/(2*sigma**2)))
    }
  }
  else if(similarity=="polynomial"){
    similarity_function <- function(Xi, Xj){
      return((Xi%*%Xj+1)**deg)
    }
  }
  else{stop("Wrong value for <similarity> parameter")}
  
  # conditions sur le voisinnage
  neighbor_theta <- ifelse(neighbor == "seuil" | neighbor == "connexe", TRUE, FALSE)
  neighbor_k <- ifelse(neighbor == "knn", TRUE, FALSE)
  if(!neighbor_theta & !neighbor_k){stop("Wrong value for <neighbor> parameter")}
  
  # boucle pour creer la matrice de similarite
  S <- matrix(ncol=nrow(M_X), nrow=nrow(M_X))
  
  for(i in 1:nrow(M_X)){
    for(j in i:nrow(M_X)){
      Sij <- similarity_function(M_X[i,], M_X[j,])
      S[i, j] <- ifelse(neighbor_theta, ifelse(Sij>=theta, Sij, 0), Sij) # si seuil ou connexe, on transforme la similarite en voisinnage a la vole
      S[j, i] <- S[i, j]
    }
  }
  diag(S) <- 0
  
  # matrice de voisinnage
  if(neighbor_theta){W <- S}
  else{
    W <- matrix(ncol=nrow(M_X), nrow=nrow(M_X))
    
    for(i in 1:nrow(M_X)){
      for(j in i:nrow(M_X)){
        condition_knn <- (j%in%which(S[i,] %in% sort(S[i,], decreasing = TRUE)[1:knn]) | i%in%which(S[j,] %in% sort(S[j,], decreasing = TRUE)[1:knn]))
        W[i, j] <- ifelse(condition_knn, S[i, j], 0)
        W[j, i] <- W[i, j]
      }
    }
    
    diag(W) <- 0
  }

  return(W)
  
}

my_spclust <- function(X, similarity = "linear", neighbor = "seuil", sigma = 1/8, deg = 3, theta = 0, knn = 3, k = 4, normalized = TRUE){
  # construction du graphe de voisinnage W_X
  W_X <- my_nngraph(X, similarity = similarity, neighbor = neighbor, sigma = sigma, deg = deg, theta = theta, knn = knn)
  print(W_X[1:6,1:6])
  # construction de la matrice des degres
  D_X <- matrix(0, nrow=nrow(W_X), ncol=nrow(W_X))
  diag(D_X) <- colSums(W_X)
  print(D_X[1:6,1:6])
  # construction de la matrice laplacienne
  L_X <- D_X - W_X
  print(L_X[1:6,1:6])
  # construction de la matrice laplacienne normalisee
  if(normalized){
    diag(D_X) <- diag(D_X)**(-1/2)
    L_X <- D_X%*%L_X%*%D_X
  }
  print(L_X[1:6,1:6])
  # extraction des vecteurs propres
  F_X <- eigen(L_X, TRUE)$vectors[, (nrow(X)-k+1):nrow(X)]
  print(F_X[1:6,])
  # normalisation des vecteurs propres
  if(normalized){
    for(i in (1:nrow(F_X))){
      sum_row_i <- max(as.matrix(F_X[i,]))
      for(j in (1:ncol(F_X))){
        F_X[i, j] <- F_X[i, j]/sum_row_i
      }
    }
  }
  
  # application des kmeans
  partition <- my_kmeans(F_X, k = k)

  # finish
  return(partition)
}

my_ari <- function (P0, P1) {
  # Matrice de confusion entre donnees observees et predites
  MC <- table(P0, P1)
  
  # calcul des elements presents dans la formule
  N_ij <- sum(sapply(MC, choose, 2))
  N_i <- sum(sapply(apply(MC, 1, sum), choose, 2))
  N_j <- sum(sapply(apply(MC, 2, sum), choose, 2))
  N <- sum(MC)
  
  # formule Adjusted Rand Index
  ARI <- (N_ij - ((N_i * N_j) / choose(N, 2)))/(0.5 * (N_i + N_j) - (N_i * N_j) / choose(N, 2))
  
  # retour du resultat
  return(ARI)
}

## Tests

# k-means
plot(D$x)
test_kmeans <- my_kmeans(D$x, k = 4)
plot(D$x, col=test_kmeans)

# spectral clustering
test_sp <- my_spclust(D$x, k = 4, similarity = "gaussian", sigma = 1/8, neighbor = "connexe", normalized = TRUE)
plot(D$x, col=test_sp)

# adjusted rand index
my_ari(D$classes, test_sp)