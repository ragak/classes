install.packages("UsingR")
library(UsingR)
?father.son
father.son
dat<-father.son
head(dat)
mean(dat$sheight)

#Using the father.son dataset described above, we want to know the expected height of sons if we condition 
#on the father being 71 inches. Create a list of son height's for sons that have fathers with height of 71 inches 
sons<-dat[round(dat$fheight,0)==71,]
head(sons)
mean(sons$sheight)

#making matrices
#dropping object example - put together two vectors of time and time squared
X = matrix(1:1000,100,10)
X[25,3]

x=1:10
M=cbind(x,2*x,3*x,4*x,5*x)
sum(M[7,])
#t(M) transposes matritx - turns rows into columns
#symbol for matrix multiplication is %*%
#diag(# of dimenstions) gives you identity matrix
#inverse matrices - multiplying a matrix by its inverse gives the identity matrix
#X^-1 (inverse) X = I (identity matrix)
#the solve function in R computes the inverse

X<-matrix(c(1,3,2,1,-2,1,1,1,-1),3,3)
y<-matrix(c(6,2,1),3,1)
solve(X)%*%y

#assignment
#solve
#3a + 4b - 5c + d = 10
#2a + 2b + 2c - d = 5
#a -b + 5c - 5d = 7
#5a + d = 4

X<-matrix(c(3,4,-5,1,2,2,2,-1,1,-1,5,-5,5,0,0,1),4,4,byrow=TRUE)
Y<-matrix(c(10,5,7,4),4,1)
solve(X)%*%Y

#assignment
a <- matrix(1:12, nrow=4)
b <- matrix(1:15, nrow=3)
dim(a)
dim(b)
c=a%*%b
c[3,2]
d<-a[3,]
e<-b[,2]
f=d*e
sum(f)

#matrix models in R
#using object falling from tower of pisa as example
g<-9.8
n<-25
tt<-seq(0,3.4,len=n)
f<-56.67+0*tt-0.5*g*tt^2
#add an observational error term
y<-f+rnorm(n,sd=1)
plot(tt,y,xlab="Time in seconds",ylab="Distance in meters")
#show actual path without error
lines(tt,f,col=2)
#let's say we want to come up with the betas in this equation (56.67,0,-0.5) how do we do that from the data?
#rss - residual sum of squares
rss<-function(Beta0,Beta1,Beta2){
    r<-y-(Beta0+Beta1*tt+Beta2*tt^2)
    sum(r^2)
}

#can we try it out to see if it works?
#let's fix two Betas and check if the third one fits
Beta2s<-seq(-10,0,len=100)
RSS<-sapply(Beta2s,rss,Beta0=55,Beta1=0)
plot(Beta2s,RSS,type="l")

#using matrix algebra to get rss
X<-cbind(rep(1,length(tt)),tt,tt^2)
head(X)
#compute rss for any given beta
#first define an arbitrary beta
Beta<-matrix(c(55,0,5),3,1)
#r for residuals
r<-y-X%*%Beta
RSS<-t(r)%*%r
#can also be done using the 'crossprod' function
RSS<-crossprod(r)
#should be the same answer as the function we defined earlier

#now solve for least squares using matrix algebra. Formula: X prime X inverse, X prime y
#inverse of a matrix in R - 'solve'
betahat<-solve(t(X)%*%X) %*% (t(X)) %*% y
#write using crossprod
betahat<-solve(crossprod(X)) %*% crossprod(X,y)

#assignment
X <- matrix(c(1,1,1,1,0,0,1,1),nrow=4)
rownames(X) <- c("a","a","b","b")
beta <- c(5, 2)

X <- matrix(c(1,1,1,1,1,1,0,0,1,1,0,0,0,0,0,0,1,1),nrow=6)
rownames(X) <- c("a","a","b","b","c","c")
beta <- c(10,3,-3)
X %*% beta
