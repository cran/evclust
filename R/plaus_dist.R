plaus_dist<-function(m1,m2){
  d<-0.5*(abs(m1[2]+m1[4]-(m2[2]+m2[4])) + abs(m1[3]+m1[4]-(m2[3]+m2[4])))
  return(d)
}
