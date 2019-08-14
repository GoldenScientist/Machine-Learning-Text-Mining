set.seed(12938)
x = matrix(rnorm(20*2),ncol=2)
y = c(rep(-1,10),rep(+1,10))
x [y==1,]=x[y==1,]+1
data = data.frame(x=x, y=as.factor(y))

plot(data$x.1,data$x.2, col=data$y,
	pch=15)

library(e1071)
?svm

svm.fit = svm(y ~ ., data=data,
	kernel="linear",cost=100)

plot(svm.fit, data)
summary(svm.fit)
data[svm.fit$index,]

tn.svm = tune.svm(y ~ ., data=data,
	kernel="linear",
	cost = 10^(-3:5))
plot(tn.svm,log="x")

plot(10^(-3:5),
	tn.svm$performance$error,log="x")
lines(10^(-3:5),
	tn.svm$performance$error)

N=100
x_test = matrix(rnorm(2*N*2),ncol=2)
y_test = c(rep(-1,N),rep(+1,N))
x_test [y_test==1,]=
	x_test[y_test==1,]+1
data_test = data.frame(x=x_test, 
	y=as.factor(y_test))

svm.fit = svm(y ~ ., data=data,
	kernel="linear",cost=10^3)
plot(svm.fit,data)

sum(data_test$y == 
	predict(svm.fit, data_test))/
	nrow(data_test)

xlm = range(c(data$x.1,data_test$x.1))
ylm = range(c(data$x.2,data_test$x.2))


plot(data$x.2,data$x.1,
	xlim=xlm,ylim=ylm,
	pch=ifelse(data$y==1,1,2))

points(data_test$x.2,data_test$x.1,
	col=as.numeric(data_test$y==
	predict(svm.fit, data_test))+2,
		pch=ifelse(data_test$y==1,16,17)
)



svm.fit = svm(y ~ ., data=data,
	kernel="radial",cost=100,
	probability=T)
plot(svm.fit,data)

svm.pr = svm()
predInitial= predict(svm.fit,data_test,
	probability =T)
predInitial.proba = attr(predInitial,
	"probabilities")

library(ROCR)
pred <- prediction(predInitial.proba[,2],
 data_test$y)
perf <- performance(pred,"tpr","fpr")

plot(perf,add=T,col="red")
title('linear')
