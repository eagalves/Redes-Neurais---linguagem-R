library("corpcor")
rm(list = ls())
N<-10 # Para 10 amostras
#N<-100 # para 100 amostras
X <- runif(n=N,min=-15,max=10)
Y<- ((1/2)*X^2+3*X+10) + 10*rnorm(length(X))
xgrid <- seq(-15,10,0.1)
ygrid <- ((1/2)*xgrid^2+3*xgrid+10) 
plot(xgrid,ygrid,type='l',xlim=c(-15,10),ylim=c(-20,100))
par(new=T)
plot(X,Y,xlim=c(-15,10),ylim=c(-20,100)) #amostras com RUIDO
H=cbind(X^2,X,1) #primeira coluna x^2, segunda x, terceira 1

#H=cbind(X^4,X^3,X^2,X,1) #Aproximação com um polinomio de quarto grau

#H=cbind(X^6,X^5,X^4X^3,X^2,X,1) #primeira coluna x^2, segunda x, terceira 1

#H=cbind(X^8,X^7X^6,X^5,X^4X^3,X^2,X,1 #primeira coluna x^2, segunda x, terceira 1

# calcular a inversa de H para achar w, pois y=H*w
w <- pseudoinverse(H) %*% Y
#Achando o w, fazer o y aproximado que é y=H*w
yaprox = H %*% w
Hgrid=cbind(xgrid^2,xgrid,1)
#Hgrid=cbind(xgrid^4,xgrid^3,xgrid^2,xgrid,1) #Aproximação com um polinomio de quarto grau

#Hgrid=cbind(xgrid^6,xgrid^5,xgrid^4xgrid^3,xgrid^2,xgrid,1) #primeira coluna x^2, segunda x, terceira 1

#Hgrid=cbind(xgrid^8,xgrid^7xgrid^6,xgrid^5,xgrid^4xgrid^3,xgrid^2,xgrid,1 #primeira coluna x^2, segunda x, terceira 1
Yaprox=Hgrid%*%w

par(new=T)
plot(X,yaprox,col='red',xlim=c(-15,10),ylim=c(-20,100))
par(new=T)
plot(xgrid,Yaprox,col='red',type='l',xlim=c(-15,10),ylim=c(-20,100))
