print(names(mtcars))

print(summary(lm(mpg ~ ., data=mtcars)))

newmtcars <- cbind(mtcars, sqrt(mtcars$cyl)  )

print(head(newmtcars))

#print(summary(lm(mpg ~ ., data=newmtcars)))

## Full formula for mtcars linear regression
print(summary(lm(mpg ~ disp*cyl + hp + drat + wt + qsec + vs + am + gear + carb ,data=mtcars)))

# EQUIV 
newmtcars <- cbind(mtcars, mtcars$disp*mtcars$cyl  )
print(summary(lm(mpg ~ ., newmtcars)))

#### more features
print(summary(lm(mpg ~ (disp+ cyl + hp + drat + wt + qsec + vs + am + gear + carb)^2 ,data=mtcars)))

print(dim(mtcars))

###############
print(summary(lm(mpg ~ disp*cyl * hp * wt ,data=mtcars)))

############
boxplot(c(mtcars$mpg , c(56,59))) 

print(lm(mpg ~ hp, mtcars))
n_mpg =c(mtcars$mpg,320)
n_hp = c(mtcars$hp,2800)
plot(n_hp, n_mpg)
print(lm(n_mpg ~ n_hp))
abline(lm(mpg ~ hp, mtcars))
abline(lm(n_mpg ~ n_hp),col="red")

#############################

print(summary(lm(Volume ~ Girth + Height,
    data = trees)))
############
N = 1000
n=20
res <- c()
for (i in 1:N){
  K <- sample(1:nrow(trees),n)
  ll <- lm(Volume ~ Girth + Height,
    data = trees[K,])
  res <- cbind(res, ll$coefficients)
}

#print(t(res))

hist(t(res)[,3])

#############################
### RMSE

############
print(summary(lm(Volume ~ Girth + Height,
    data = trees)))
N = 1000
n=20
train_error <- c()
test_error <- c()
for (i in 1:N){
  K <- sample(1:nrow(trees),n)
  ll <- lm(Volume ~ Girth + Height,
    data = trees[K,])
  train_error <- c(train_error, 
    sqrt(mean((ll$residuals^2))))
  
  test_error <- c(test_error, 
   sqrt(mean((
     predict(ll,
        newdata=trees[-K,])-trees[-K,]$Volume)^2)))
}

hist(train_error)
hist(test_error)
