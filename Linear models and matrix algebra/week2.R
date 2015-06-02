v                                 #week 2 linear algebra course
#standard error homework
g = 9.8 ## meters per second
h0 = 56.67
v0 = 0
n = 25
tt = seq(0,3.4,len=n) ##time in secs, t is a base function
y = h0 + v0 *tt - 0.5* g*tt^2 + rnorm(n,sd=1)
#to solve for least square error (lse)
X = cbind(1,tt,tt^2)
A = solve(crossprod(X))%*%t(X)
#y = b0 + b1 t + b2 t^2 + e 
#obtain the LSE we have used in this class
#Note that g = -2 b2
-2*(A %*% y)[3] #estimate of g

#repeat for 100000 monte carlo simulated datasets
  tt = seq(0,3.4,len=n) ##time in secs, t is a base function
solveg<-function(h0=56.67,v0=0,g=9.8){  
  y = h0 + v0 *tt - 0.5* g*tt^2 + rnorm(n,sd=1)
  #to solve for least square error (lse)
  X = cbind(1,tt,tt^2)
  A = solve(crossprod(X))%*%t(X)
  #y = b0 + b1 t + b2 t^2 + e 
  #Note that g = -2 b2
  -2*(A %*% y)[3] #estimate of g
}

result<-replicate(100000,solveg())
sd(result)/(sqrt(length(result)))

##
library(UsingR)
x = father.son$fheight
y = father.son$sheight
n = length(y)

#monte carlo simulation n=50
height<-function(N = 50){
  index = sample(n,N)
  sampledat = father.son[index,]
  x = sampledat$fheight
  y = sampledat$sheight
  betahat = lm(y~x)$coef
  betahat[2]
}
#standard error of mean of 10000 replicates
result2<-replicate(10000,height(),simplify=TRUE)
sd(result2)/(sqrt(length(result2)))

#covariance
#mean( (Y - mean(Y))*(X-mean(X) ) )

library(UsingR)
x = father.son$fheight
y = father.son$sheight
n = length(y)
N = 50
set.seed(1)
index = sample(n,N)
sampledat = father.son[index,]
x = sampledat$fheight
y = sampledat$sheight
betahat = lm(y~x)$coef
#formulas for standard error and variance
#SE(betahat) = sqrt(var(betahat))
#var(betahat) = sigma^2 (X^T X)^-1

#get Yhat (fitted values)
fit = lm(y ~ x)
fit$fitted.values
#residuals are given by r_i = Y_i - Y-hat_i
#find sum of squares of residuals
sum((y-fit$fitted.values)^2)

#design matrix
X = cbind(rep(1,N), x)
#calculate (X^T X)^-1, the inverse of X transpose times X
Z=solve((t(X) %*% X))

#take diagonals
d=diag(Z)
#sigma2 = SSR / 48
sigma2 = (sum((y-fit$fitted.values)^2))/48
#multiply diagonals by sigma2 to get variance
v = d*sigma2
#sqrt of above, is standard error of intercept and standard error of slope
sqrt(v)

#expressing experimental designs using R formula
x<-c(1,1,2,2)
f<-formula(~x) #tilda says that the model represents the variables affecting the system
f
model.matrix(f)
#if you change x to a factor, the second column of the matrix in 'model matrix' becomes
#an indicator variable. i.e., if the element of the column = 2, the variable shows up as 1,
#if not it shows up as 0
x<-as.factor(c(1,1,2,2,3,3))
model.matrix(~x)
#can change layout by using the following:
model.matrix(~x,contrasts=list(x="contr.sum"))
#two variables example
x<-factor(c(1,1,1,1,2,2,2,2))
y<-factor(c("a","a","b","b","a","a","b","b"))
model.matrix(~x+y)
#add an interaction term in case the effect of both '2' and 'b' together is not explained
#by the sum effect of 2 and b
model.matrix(~x+y+x:y)
#another way to write this (in shorthand) is:
model.matrix(~x*y)
#you can 'relevel' the variables - i.e. switch their indicators
x<-factor(c(1,1,2,2))
model.matrix(~x)
x<-relevel(x,"2")
model.matrix(~x)
x<-factor(x,levels=c("1","2"))

#####

z<-1:4
model.matrix(~z)
#you can specify that the intercept should be set to 0
model.matrix(~0+z)
#if you want to include a numeric transformation of z, you need to wrap it in the I function
#this allows you to use arithmetic operators outside the context of the model matrix
model.matrix(~z+I(z^2))

#####
#linear models
url<-"https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/femaleMiceWeights.csv"
filename<-"femaleMiceWeights.csv"
library(downloader)

#--unfinished--#
#running a linear model
#similar to model.matrix, but lm for linear model, add y value (i.e. want to model
#bodyweight over the diet, and the data frame is 'dat')
fit<-lm(Bodyweight~Diet,data=dat)
#coef function gives you coefficients
(coefs<-coef(fit)) #aside - putting brackets around a variable you are defining also prints it

#as before, solving a linear model:
#beta hat = (X^t X)^(-1) X^t y
y<-dat$Bodyweight
X<-model.matrix(~Diet,data=dat)
solve(t(X)%*%X) %*% t(X)%*%y

#getting the coefficients without using lm
s<-split(dat$Bodyweight, dat$Diet)
mean(s[["chow"]]) #intercept is the mean of the control group
mean(s[["hf"]])-mean(s[["chow"]]) #second coefficient is difference between means

#result from linear model is equivalent to result from ttest
ttest<-t.test(s[["chow"]],s[["hf"]],var.equal=TRUE)
#if the t statistic is negative here and positive in the linear model, it's just because
#of the order of the variables chow and hf.
