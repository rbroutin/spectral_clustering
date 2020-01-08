# Chargement des donnees et tests
load("smiley.RData")
plot(D$x, col=D$classes)
df <- as.data.frame(D$x)

# Fonctions
dist_euclidienne <- function(x, y) {
  diff <- sum((x-y)**2)
  d <- sqrt(diff)
  return(d)
}

norme <- function(x){
  return(sqrt(x%*%x))
}

gaussian <- function(x, y, sigma){
  return(exp(-(norme(x-y)**2)/2*sigma**2))
}

pause <- function(){readline("Press <ENTER> to Continue.")} 

my_kmeans <- function(X, k, niter = 20) {
  #if(missing(k)){stop("argument 'centers' must be a number")}
  
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

my_nngraph <- function(X, similarity = "linear", neighbor = "seuil") {
  # transformer en matrix pour appliquer le produit matriciel
  M_X <- as.matrix(X)
  
}

## brouillon
M_X <- as.matrix(df)
M_X <- M_X[1:6,]
M_X
S <- matrix(ncol = nrow(M_X), nrow = nrow(M_X))
sigma = 1/8
deg = 2
knn <- 3

for(i in 1:nrow(M_X)) {
  for(j in 1:nrow(M_X)) {
    
    # linear
    #if(i>j){S[i, j] <- ifelse(M_X[i,]%*%M_X[j,]>=0,M_X[i,]%*%M_X[j,],0)}
    # gaussian
    #if(i!=j){S[i, j] <- exp(-norme(M_X[i,]-M_X[j,])**2/(2*sigma**2))}
    # polynomial
    #if(i>j){S[i, j] <- (M_X[i,]%*%M_X[j,]+1)**deg
    #S[j, i] <- S[i, j]
    #}
    # gaussian via function
    if(i!=j){S[i, j] <- gaussian(M_X[i,], M_X[j,], sigma)}
    #print(which(S[j,] %in% sort(S[j,], decreasing = TRUE)[1:knn]))
  }
}

diag(S) <- 0
S

W <- matrix(nrow=nrow(M_X), ncol = nrow(M_X))

for(i in 1:nrow(M_X)) {
  for(j in 1:nrow(M_X)) {
    if(i==j){W[i, j]<-0}
    else if(
      !j%in%which(S[i,] %in% sort(S[i,], decreasing = TRUE)[1:knn]) &
      !i%in%which(S[j,] %in% sort(S[j,], decreasing = TRUE)[1:knn])
    ) {W[i, j]<-0}
    else {W[i, j] <- S[i, j]}
    #print(W)
    #pause()
  }
}

W_1 <- W


