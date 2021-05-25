x_train <- seq(from=0, to=2*pi, by =0.15)
x_train<-x_train + (runif(length(x_train))-0.5)/5
i <- sample(length(x_train))
x_train <- x_train[i]
y_train <- sin(x_train)
y_train<-y_train + (runif(length(y_train))-0.5)/5
plot(x_train,y_train,col='blue',xlim = c(0,2*pi),
ylim = c(-1,1),xlab = 'x',ylab = 'y')
x_test <-seq(from=0, to=2*pi, by =0.01)
y_test <-sin(x_test)
par(new=T)
plot(x_test,y_test,col='red',type='l',xlim = c(0,2*pi),
ylim = c(-1,1),xlab = 'x',ylab = 'y')
legend(x=4, y=1, legend = c('train','test'),
col = c('blue','red'),pch=c('o','_'))
