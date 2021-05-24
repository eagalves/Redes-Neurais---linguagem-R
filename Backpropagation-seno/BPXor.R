rm(list=ls())

sech2 <- function(u){
  return (((2/(exp(u) + exp(-u)))*(2/(exp(u) +exp(-u)))))
  
}

x <- matrix(c(0,0,0,1,1,0,1,1), ncol=2, byrow = T)
y <- matrix(c(-1,1,1,-1,1,-1,-1,+1), ncol=2, byrow = T)

w61 <- runif(1)-0.5
w62 <- runif(1)-0.5
w63 <- runif(1)-0.5


w72 <- runif(1)-0.5
w73 <- runif(1)-0.5
w74 <- runif(1)-0.5


w95 <- runif(1)-0.5
w96 <- runif(1)-0.5
w97 <- runif(1)-0.5


w106 <- runif(1)-0.5
w107 <- runif(1)-0.5
w108 <- runif(1)-0.5

i1 <- 1
i4 <- 1
i5 <- 1
i8 <- 1

maxEpoch <- 4000
currentEpoch <- 1
tol <- 0.1
eta <- 0.01
epochError <- tol + 1

errorVector <- matrix(nrow = 1, ncol = maxEpoch)
numberOfLines <- 4

while((currentEpoch < maxEpoch) && (epochError > tol)){
  
  xSequence <- sample(numberOfLines)
  ei2 <- 0
  for(i in 1:numberOfLines){
    irand <- xSequence[i]
    
    i2 <- x[irand,1]
    i3 <- x[irand,2]
    
    
    
    
    u6 <- i1*w61 + i2*w62 + i3*w63
    
    i6 <- tanh(u6)
    
    u7 <- i2 * w72 + i3*w73 + i4 * w74
    i7 <- tanh(u7)
    
    u9 <- i5*w95 + i6*w96 + i7*w97
    
    i9 <- tanh(u9)
    
    u10 <- i6*w106 + i7*w107 + i8*w108
    i10 <- tanh(u10)
    
    y9 <- y[irand, 1]
    
    
    e9 <- y9 - i9
    
    y10 <- y[irand, 2]
    e10 <- y10 - i10
    
    d9 <- e9*sech2(u9)
    d10 <- e10*sech2(u10)
    
    dw95 <- eta*d9*i5
    dw96 <- eta*d9*i6
    dw97 <- eta*d9*i7
    
    dw106 <- eta * d10 * i6
    dw107 <- eta * d10 * i7
    dw108 <- eta * d10 * i8
    
    d6 <- (d9*w96 + d10*w106)*sech2(u6)
    dw61 <- eta* d6*i1
    dw62 <- eta*d6*i2
    dw63 <- eta*d6*i3
    
    d7 <- (d9*w97 + d10*w107)*sech2(u7)
    dw72 <- eta*d7*i2
    dw73 <- eta*d7*i3
    dw74 <- eta*d7*i4
    
    w95 <- w95+dw95
    w96 <- w96 +dw96
    w97 <- w97+dw97
    
    w106 <- w106+dw106
    w107 <- w107+dw107
    w108 <- w108+dw108
    
    w61 <- w61 +dw61
    w62 <- w62 +dw62
    w63 <- w63 +dw63
    
    w72 <- w72 +dw72
    w72 <- w72 +dw72
    w73 <- w73 +dw73
    w74 <- w74 +dw74
    
    ei2 <- ei2+(e9*e9 + e10*e10)/2
    
    
  }
  
  currentEpoch <- currentEpoch + 1
  errorVector[currentEpoch] <- ei2/numberOfLines
  epochError <- errorVector[currentEpoch]
  
  
}


plot(seq(1,maxEpoch, 1), errorVector[1:maxEpoch], type = 'l')

# U6 <- i1*w61 + i2*w62 + i3*w63
# H6 <- tanh(U6)
H <- cbind(1,x)
w1 <- rbind(w61,w62,w63)
H6 <- tanh(H%*%w1)


# U7 <- i2 * w72 + i3*w73 + i4 * w74
# i7 <- tanh(u7)
w2 <- rbind(w74,w72,w73)
H7 <- tanh(H%*%w2)
# U9 <- i5*w95 + i6*w96 + i7*w97
# i9 <- tanh(u9)
H1 <- cbind(1,H6,H7)

w3 <- rbind(w95,w96,w97)

U9 <- H1%*%w3

Yhat9 <- tanh(U9)

# u10 <- i6*w106 + i7*w107 + i8*w108
# i10 <- tanh(u10)

