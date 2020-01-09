# Chargement des donnees et visualisation
load("smiley.RData")
plot(D$x, col=D$classes)
df <- as.data.frame(D$x)

# Fonctions
dist_euclidienne <- function(x, y) {
  diff <- sum((x-y)**2)
  d <- sqrt(diff)
  return(d)
}

pause <- function(){readline("Press <ENTER> to Continue.")} 

my_kmeans <- function(X, k, niter = 20) {
  # Tirage au hasard des k premiers centres de classe
  i_c <- sample(nrow(df), k)
  
  # premiere matrice des centres de classe
  M_c <- as.matrix(df[i_c,])
  
  # initialisation de la matrice des distances
  M_d <- matrix(nrow = nrow(X), ncol = k)
  
  # boucle avec un nombre d'iteration max
  for(j in 1:niter){
    # stockage des anciens centre de classe
    M_c_a <- M_c
    
    # boucle sur les centres de classe
    for(i in 1:k) {
      M_d[,i] <- apply(df, 1, dist_euclidienne, y = M_c[i,]) # calcul la distance de chaque point au centre de classe
    }
    
    # vecteur des classes
    V_c <- apply(M_d, 1, which.min)
    
    # recalculer les centres des classes
    for(i in 1:2){
      M_c[,i] = tapply(df[[i]], V_c, mean)
    }
    
    # si les centres sont identiques, alors on stop (et on plot)
    if(all(M_c_a==M_c)){
      plot(df, col=V_c)
      return(V_c)
    }

  }
}

my_kmeans(df, 4)

my_nngraph <- function(X, similarity = "linear", neighbor = "seuil", sigma = 1/8, deg = 3, theta = 0, k = 3) {
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
        condition_knn <- (j%in%which(S[i,] %in% sort(S[i,], decreasing = TRUE)[1:k]) | i%in%which(S[j,] %in% sort(S[j,], decreasing = TRUE)[1:k]))
        W[i, j] <- ifelse(condition_knn, S[i, j], 0)
        W[j, i] <- W[i, j]
      }
    }
    
    diag(W) <- 0
  }
  
  return(W)
  
}

my_nngraph(df[1:6,], similarity = "polynomial", neighbor = "knn", deg = 2, k = 4)

