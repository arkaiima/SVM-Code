#setwd("C:\\Users\\Krishna\\Desktop\\arm")
#install.packages("pracma")
require(pracma)
#dot(c(1,0.5,1), c(1,0.5,0.5))
x<-matrix(c(8,9,6.5,6.8,6,4,7.1,7.5,1.5,1,2,3,1,2),nrow =2,byrow = F )
#mat2<-matrix(c(1,array(0,5),0,1,array(0,6),1,array(0,6),1,array(0,6),1,array(0,6),1),nrow =6,byrow = F )
#dot(x[,1],x[,1])
X<-x
plot(x[1,],x[2,])
y<-c(1,1,1,1,-1,-1,-1)
n<-length(y)
vec1<-c()
for(i in 1 : n){
  for (j in 1:n){
    vec1<-c(vec1,dot(x[,i],x[,j]))
    
  }
}
vec2<-c()
for(i in 1 : n){
  for (j in 1:n){
    vec2<-c(vec2,dot(y[i],y[j]))
    
  }
}
vec3<-c()
for(i in 1:(n*n)){
  vec3<-c(vec3,vec1[i]*vec2[i])
}


mat1<-matrix(vec3,nrow = n,byrow = TRUE)

require(quadprog)
d<-c(array(data=1,dim = n))
#solve.QP(Dmat = mat1, dvec=d)
j=1
con<-c()
for(i in 1:(n*n)){
  if(i==j){
    con<-c(con,1);
    j=j+n+1;}    else{
      con<-c(con,0)
    }
  
}
eps <- 5e-4
#conmat<-matrix(con,nrow = 7,byrow = T)
A <- t(rbind(matrix(y, nrow=1, ncol=n), diag(nrow=n)))
b<-c(array(0,n+1))
sol<-solve.QP(Dmat = mat1+eps*diag(n), dvec=d,Amat = A,bvec = b,meq=1,factorized=FALSE)
qpsol <- matrix(sol$solution, nrow=n)

w<-c(0,0)
for(i in 1:n){
  w=w+qpsol[i]*y[i]*x[,i]
}
b_val<-c()
temp<-0
for(i in 1:n){
  if(qpsol[i] !=0){
    temp=(dot(w,x[,i])-y[i]);
    b_val<-c(b_val,temp);
  }
}
b<-mean(b_val)
slope1 <- -w[1]/w[2]
intercept1 <- b/w[2]
train = data.frame(v1=t(x)[,1], v2=t(x)[,2], v3=y)

library(ggplot2)
plt <- ggplot(train, aes(x=v1, y=v2)) + 
  ggtitle("Solving the SVM QP") +
  geom_point(aes(fill=factor(y)), size=3, pch=21) +
  geom_abline(intercept=intercept1, slope=slope1, size=1, aes(color="quadprog"), show_guide=TRUE)+
  scale_fill_manual(values=c("red","blue"), guide='none')+
  scale_color_manual(values=c("green", "yellow", "black"))+
  theme(legend.position="bottom", legend.title=element_blank())
print(plt)

