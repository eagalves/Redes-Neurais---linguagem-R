Trainadaline <- function(X,y,eta,tol,maxepocas,par)
{
  dimX <- dim(X) # dim(X) = a um vetor[numero de linhas, numero de colunas]
  N <- dimX[1] 
  n <- dimX[2]
  if(par==1){
    w <- as.matrix(runif(n+1)-0.5)
    X <- cbind(1,X)
  }
  else{
    w <- as.matrix(runif(n)-0.5)
  }
  
  nepocas <- 0
  eepoca <- tol+1
  
  evec <- matrix(nrow=1,ncol=maxepocas)
  
  while ((nepocas < maxepocas) && (eepoca > tol))
  {
    ei2 <- 0
    xseq <-sample(N)
    for (i in 1:N)
    {
      irand <- xseq[i]
      yhati <- 1.0*((X[irand,]%*%w))
      ei <- y[irand]-yhati
      dw <- eta*ei*X[irand,]
      w <- w +dw
      ei2 <- ei2+ei*ei
    }
    nepocas <- nepocas+1
    evec[nepocas] <- ei2/N
    eepoca <- evec[nepocas]
  }
  retlist<-list(w,evec[1:nepocas])
  return(retlist)
}

# ADQUIRINDO DADOS DO ARQUIVO
t <- read.table("t")
x <- read.table("x")
y <- read.table("y")
tin <- as.matrix(t)
xin <- as.matrix(x)
yin <- as.matrix(y)

#index <- sample(NROW(xin)*0.7)
index <- sample(NROW(xin)) # indices aleatorios do vetor
index70 <- index[1:round(0.7*length(index)-1)] # primeiros 70% numeros do index
index30 <- index[round(0.7*length(index)):length(index)] # ultimos 30% numeros do index

# X INDEX
x <- xin[index70,]
xteste <- xin[index30,]
#x[index[round(0.7*length(index))]]

# Y INDEX
y <- yin[index70,]
yteste <- yin[index30,]

# t INDEX
t <- tin[index70,]
t_teste <- tin[index30,]

###################

Resul <- Trainadaline(x,y,0.001,0.001,1000,1)

# Achando a funcao aproximada
H <- cbind(1,x)
w <- Resul[[1]]
w <- as.matrix(w)
print(w)
yaprox <- H%*%w
###############

#Achando o erro
N <- length(y) 
Erro = y-yaprox
Erro = Erro^2
Erro2 = 0
for (i in 1:N)
{
  Erro2 <- Erro2+ Erro[i]
}
Erro2 = Erro2/N

print(paste('O erro quadratico é:',Erro2))
######################

# Plotando o grafico de "y" e "y aproximado"


plot(t_teste,yteste,col='purple',xlim=c(0,5),ylim=c(-2,3))

par(new=T)

plot(t,yaprox,col='red',xlim=c(0,5),ylim=c(-2,3))



rm(list=ls())
