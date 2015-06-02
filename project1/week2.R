library(devtools)
install_github("jennybc/gapminder")
library(gapminder)
data(gapminder)
head(gapminder)
#Create a vector 'x' of the life expectancies of each country for the year 1952. 
#Plot a histogram of these life expectancies to see the spread of the different countries.

x<-gapminder$lifeExp[gapminder$year=="1952"]
x
hist(x)
mean(x <= 60)-mean(x <= 40)

#Create custom function for proportion under a certain number

prop = function(q) {
    mean(x <= q)
}
prop(40)
#Now let's build a range of q's that we can apply the function to:
qs = seq(from=min(x), to=max(x), length=20)
#function prop at each interval
props = sapply(qs, prop)

#Note that we could also have written this in one line, by defining the 'prop' function but without naming it:
props = sapply(qs, function(q) mean(x <= q))

#this already exists in R!
plot(ecdf(x))

#population size
y<-gapminder$pop[gapminder$year=="1952"]
y
hist(y)
hist(log10(y))
sd(log10(y))
x<-log10(y)
qqnorm(x)

#subtract mean and divide by ds (standardize)
z<-(x-mean(x))/sd(x)
qqnorm(z)
abline(0,1)
tail(sort(z),1)

F = function(q) pnorm(q, mean=mean(x), sd=sd(x))
F(6) #proportion of countries with population less than 1 million (10^6, therefore log10(10^6)=6)
n = length(x) #number of countries
#Finally, using the Normal approximation, estimate the number of countries that should have a log10 1952 
#population between 6 and 7 

prop67=F(7)-F(6)
prop67*n

head(pnorm(x, mean=mean(x), sd=sd(x)))
n = length(x)
ps = ((1:n) - 0.5)/n
#we want to find the quantiles of the standard normal distribution which are associated 
#with the following probabilities
pnorm(ps)
head(sort(x),1)
index<-match(head(sort(x),1),x)

plot(qnorm(ps), sort(x))

