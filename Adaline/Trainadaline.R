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

# Lendo os dados dos arquivos
t<-read.table("Ex1_t")
x<-read.table("Ex1_x")
y<-read.table("Ex1_y")
# transformando os dados em tipo matrix
tin <- as.matrix(t)
xin <- as.matrix(x)
yin <- as.matrix(y)

# pegando indices aleatorios do vetor 
#index <- sample(NROW(xin)*0.7)
index <- sample(NROW(xin)) # indices aleatorios do vetor
index70 <- index[1:round(0.7*length(index)-1)] # primeiros 70% numeros do index
index30 <- index[round(0.7*length(index)):length(index)] # ultimos 30% numeros do index

# ultilizando os indices aleatorios para embaralhar as posicoes dos dados
x <- xin[index70,]
x <- as.matrix(x)
xteste <- xin[index30,]
xteste <- as.matrix(xteste)
#x[index[round(0.7*length(index))]]

# Y INDEX
y <- yin[index70,]
y <- as.matrix(y)
yteste <- yin[index30,]
yteste <- as.matrix(yteste)

# t INDEX
t <- tin[index70,]
t <- as.matrix(t)
t_teste <- tin[index30,]
t_teste <- as.matrix(t_teste)
Resul <- Trainadaline(x,y,0.001,0.001,1000,1)

H <- cbind(1,x) # a + bx

w <- Resul[[1]]
w <- as.matrix(w) # pesos aproximados

yaprox = H %*% w # funcao aproximada

plot(t,yaprox,col='red',xlim=c(0,7),ylim=c(0,1))

par(new=T)
plot(t_teste,yteste,col='blue',xlim=c(0,7),ylim=c(0,1))

#Erro quadratico médio 
N <- length(yteste)
Erro = y-yaprox
Erro = Erro^2
Erro2 = 0
for (i in 1:N)
{
  Erro2 <- Erro2+ Erro[i]
}
Erro2 = Erro2/N
print(paste('O erro quadratico é:',Erro2))
##########################


#ret[[1]][1]

# t<- matrix(seq(0,2*pi,0.1*pi),ncol=1)
# x<-sin(t)
# y<-matrix(4*x+2,ncol=1)
