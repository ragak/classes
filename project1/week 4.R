library(UsingR)
x=father.son$fheight
#in hist, 
#breaks - where to have intervals and number of individuals in each interval

hist(x,breaks=seq(floor(min(x)),ceiling(max(x))),
        main="Height histogram",xlab="Height in inches")

#empirical cumulative distribution function
xs<-seq(floor(min(x)),ceiling(max(x)),0.1) #xs is just a sequence of increasing numbers for heights
plot(xs,ecdf(x)(xs),type="l",
     xlab="Height in inches",ylab="F(x)") #ecdf - cumulative distribution funciton for x using xs as points to calc

#if data normal, then if we want to compute proportion of people with height above 70:
mean(x>70)
#verify with normal approximation
1-pnorm(70,mean(x),sd(x))

#qq plot checks this over many values
ps <-seq(0.01,0.99,0.01)
qs <-quantile(x,ps)
normalqs <-qnorm(ps,mean(x),sd(x))
#compare manually calculated quantiles with normal distribution quantiles to see how well data fits normal
plot(normalqs, qs, xlab="Normal percentiles",ylab="Height percentiles")
abline(0,1)

#shortcut
qqnorm(x)
qqline(x)

#assignment
load("skew.RData")
dim(dat)
#set up 3X3 plots
par(mfrow = c(3,3))
#compare each column to normal dist

for (i in 1:9) {
    qqnorm(dat[,i])
    qqline(dat[,i])
}

#assignment 2
head(InsectSprays)
boxplot(split(InsectSprays$count, InsectSprays$spray))
boxplot(InsectSprays$count ~ InsectSprays$spray)

#multivariate data
x=father.son$fheight
y=father.son$sheight
plot(x,y,xlab="Father's height in inches",ylab="Son's height in inches",
    main=paste("correlation =",signif(cor(x,y),2)))
#stratifying data
boxplot(split(y,round(x)))
print(mean(y[ round(x) == 72]))
#standardize father and son heights
x=(x-mean(x))/sd(x)
y=(y-mean(y))/sd(y)
means=tapply(y,round(x*4)/4,mean)
#because it's standardized, the names of the vectors actually represent the father heights
fatherheights=as.numeric(names(means))
plot(fatherheights,means,ylab="average of strata of son heights",ylim=range(fatherheights))
#find slope of the line (which is the correlation)
abline(0,cor(x,y))

#assignment
plot(father.son$fheight, father.son$sheight)
cor(father.son$fheight, father.son$sheight)
identify(father.son$fheight, father.son$sheight)
x = father.son$fheight
y = father.son$sheight
n = nrow(father.son)
plot(scale(x), scale(y))
abline(h=0, v=0)
mean(scale(x)*scale(y))

#assignment
library(UsingR)
data(nym.2002)
head(nym.2002)
#histogram of times
hist(nym.2002$time)
#plot of runner's age vs their time
plot(nym.2002$age,nym.2002$time)
#plot of runner's time vs their place in the race
plot(nym.2002$time,nym.2002$place)
#qqnorm() of the runner's times. Are they normal? Positive or negative skew?
qqnorm(nym.2002$time)
abline(1,1)
#a barplot of the most common location of origin. hint: tail(sort(table(nym.2002$home)),10)
barplot(tail(sort(table(nym.2002$home)),10))
#a boxplot of the runner's time over their gender
boxplot(nym.2002$time ~ nym.2002$gender)

plot(time/median(time), ylim=c(1/4,4))
abline(h=c(1/2,1,2))
plot(log2(time/median(time)),ylim=c(-2,2))
abline(h=-1:1)

###dplyr
install.packages("dplyr")
library(dplyr)
msleep<-read.csv("msleep_ggplot2.csv")

#dplyr verbs to remember
#select()    select columns
#filter()	filter rows
#arrange()	re-order or arrange rows
#mutate()	create new columns
#summarise()	summarise values
#group_by()	allows for group operations in the "split-apply-combine" concept

sleepData<-select(msleep,name,sleep_total)
#if you want all columns EXCEPT sleep column, use "-"
head(select(msleep,-name))
#range of columns - use ":"
#columns starting with string, use 'starts_with()
head(select(msleep,starts_with("sl")))
#also, 'ends_with()', 'contains()', 'matches()', 'one_of()'

#selecting rows using 'filter()'
filter(msleep,sleep_total >= 16)
filter(msleep,sleep_total >= 16, bodywt >= 1)

#pipe operator: %>%
#another way of saying this is piping functions one after another
#select(msleep,name,sleep_total)
msleep %>%
        select(name,sleep_total)%>%
        head

#'arrange()' will order a data set by a specific column
msleep %>% arrange(order) %>% head
msleep %>%
        select(name,order,sleep_total) %>%
        arrange(order, sleep_total) %>%
        head
#add a filter
msleep %>%
    select(name,order,sleep_total) %>%
    arrange(order, sleep_total) %>%
    filter(sleep_total >= 16)
#if you want sleep total descending instead
msleep %>%
    select(name,order,sleep_total) %>%
    arrange(order, desc(sleep_total)) %>%
    filter(sleep_total >= 16)

#create new columns using 'mutate()'
msleep %>%
    mutate(rem_proportion = sleep_rem/sleep_total) %>%
    head

msleep %>%
    mutate(rem_proportion = sleep_rem/sleep_total,
           bodywt_grams=bodywt*1000) %>%
    head

#summarise() to create summary stats for a given column
msleep %>%
        summarise(avg_sleep=mean(sleep_total))

#different summary stats for each taxonomic order
msleep %>%
    group_by(order)%>%
    summarise(avg_sleep=mean(sleep_total),
              min_sleep=min(sleep_total),
              max_sleep=max(sleep_total),
              total=n())

#assignment
#Add a column of the proportion of REM sleep to total sleep
#Group the animals by their taxonomic order
#Summarise by the median REM proportion
#Arrange by the median REM proportion
#Take the head() of this to see just the orders with smallest median REM proportion

msleep %>%
    mutate(rem_proportion = sleep_rem/sleep_total) %>%
    group_by(order) %>%
    summarise(median_REMprop=median(rem_proportion)) %>%
    arrange(median_REMprop) %>%
    head

#assignment
data(ChickWeight)
plot(ChickWeight$Time, ChickWeight$weight, col=ChickWeight$Diet)
head(ChickWeight)
#reshape the data from long to wide, where the columns Chick and Diet are the 
#ID's and the column Time indicates different observations for each ID
chick = reshape(ChickWeight,idvar=c("Chick","Diet"),timevar="Time",direction="wide")
head(chick)
chick = na.omit(chick)
#day 4 chick weights
day4<-chick$weight.4
day4out<-c(chick$weight.4,3000)
day21<-chick$weight.21
day21out<-c(chick$weight.21,3000)
sd(day4out)/sd(day4)
plot(chick$weight.4,chick$weight.21)
cor(day4out,day21out)/cor(day4,day21)

###

data(ChickWeight)
chick <- reshape(ChickWeight,idvar=c("Chick","Diet"),timevar="Time",direction="wide")
chick <- na.omit(chick)
stripchart(chick$weight.4 ~ chick$Diet, method="jitter", vertical=TRUE)
x<-chick$weight.4[chick$Diet==1]
y<-chick$weight.4[chick$Diet==4]
t.test(x,y)
wilcox.test(x,y)
z<-c(x,200)
t.test(z,y)$p.value

par(mfrow=c(1,3))
boxplot(x,y)
boxplot(x,y+10)
boxplot(x,y+100)

a<-y+10
b<-y+100

t.test(x,a)$statistic - t.test(x,b)$statistic
