pairwise_mass<- function(clus){

  # Computes the relational representation of an evidential partition. Function called by mass_from_P_new
  n<-nrow(clus$mass)
  M<-clus$mass
  mat<-build_matrices(clus$F)
  
  Me<-as.dist(M %*% mat$E %*% t(M))
  M1<-as.dist(M %*% mat$S %*% t(M))
  M0<-as.dist(M %*% (mat$C-mat$E) %*% t(M))
  return(list(Me=Me,M1=M1,M0=M0))
}

