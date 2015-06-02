pops<-read.csv("mice_pheno.csv")
head(pops)
hf<-pops[pops$Diet=="hf" & pops$Sex=="F",3]
hist(hf)
chow<-pops[pops$Diet=="chow" & pops$Sex=="F",3]
mean(hf)-mean(chow)

x<-sample(hf,12)
y<-sample(chow,12)
mean(x)-mean(y)

#try to see if central limit theorem holds for a few sample sizes:
Ns<-c(3,5,10,25)
B<-10000
#use sapply to study distribution of mean differences for the various samples sizes, with 10000 samplings for each size:
res<-sapply(Ns,function(n){
    sapply(1:B,function(j){
        mean(sample(hf,n))-mean(sample(chow,n))})
})

library(rafalib)
mypar(2,2)
#look at distribution for all four sample sizes (graphs partitioned into 2 by 2)
for(i in seq(along=Ns)){
    title <- paste("Avg=",signif(mean(res[,i]),3))
    title <- paste(title,"SD=",signif(sd(res[,i]),3))
    qqnorm(res[,i],main=title)
    qqline(res[,i])
}

#Next assignment - ttests
dat<-read.csv("femaleMiceWeights.csv")
control<-dat[1:12,2]
treatment<-dat[13:24,2]
diff<-mean(treatment)-mean(control)

#use central limit theorem to get distribution of random variable diff
#therefore, assume means are normally distributed and under the null hypothesis they are the same
#what is standard error of these means? standard deviation of populations (don't have) divided by sqrt sample size
#since we don't know stdev of whole population, we use stdev of just the sample we're given here (12 mice)

sd(control)
#now standard error
sd(control)/sqrt(length(control))

#the variance of the difference of two random variables that don't affect each other is the sum of their variances
#therefore standard error of difference of means:
se <- sqrt( var(treatment)/length(treatment) + var(control)/length(control))
se
#t-statistic is the random variable divided by it's standard error, in this case:
tstat<-diff/se
#central limit theorem tells us that under the null, the difference of means should be 0, and the sd should be 1 (normal
#distribution). so the probability that the tstat we observe is different from that is as follows:
1-pnorm(tstat)
#for a two-sided ttest (i.e. you would be equally interested if the deviation was positive or negative):
1-pnorm(tstat)+pnorm(-tstat)
#but if you do
t.test(treatment,control)
#you get a different p-value. This is because 12 samples is not enough to use central limit theorem. So it is using the 
#t distribution approximation instead
#t dist is only used when the DATA (not just the means of the data) is approx normally distributed. do a quick check:
qqnorm(control)
qqline(control)
qqnorm(treatment)
qqline(treatment)
#so now assume that tstat (from above) follows a t distribution, not a normal distribution
#the t distribution assumes that the standard error (se) that you use to estimate tstat is also a random variable
#(since you only have a sample of the population)

#Quiz question
babies = read.table("babies.txt", header=TRUE)
bwt.nonsmoke = babies$bwt[babies$smoke==0]
bwt.smoke = babies$bwt[babies$smoke==1]
mean(bwt.nonsmoke)-mean(bwt.smoke)
sd(bwt.nonsmoke)
sd(bwt.smoke)

#first 30 from each
dat.ns<-bwt.nonsmoke[1:30]
dat.ns
dat.s<-bwt.smoke[1:30]
dat.s

#manually calculate t statistic

X.ns = mean(dat.ns)
sd.ns = sd(dat.ns)
X.s = mean(dat.s)
sd.s = sd(dat.s)
sd.diff = sqrt(sd.ns^2/length(dat.ns)+sd.s^2/length(dat.s))
tval = (X.ns - X.s)/sd.diff

#using R fucntion
t.test(dat.ns, dat.s)$statistic

pval = 1-(pnorm(abs(tval))-pnorm(-abs(tval)))
pval

2*pnorm(-abs(tval))

#Part 2
dat<-read.csv("mice_pheno.csv")
pop<-dat[dat$Sex=="F" & dat$Diet=="chow",3]
mu<-mean(pop)
#if we just look at a sample
N<-30
y<-sample(pop,N)
mean(y)
#use confidence interval to report variability of mean, first use central limit theorem for standard error
se<-sd(y)/sqrt(N)

#dividing difference of means by se -> normally distributed with mean 0 and sd 1
#want to select Q such that 95% of points are within one SD (normal dist 0,1)
Q<-qnorm(1-0.05/2)
#-Q < (mean(pop)-mean(y))/se < Q
#solve for mean(pop) since that is unknown
#mean(y)-Q*se < mean(pop) < mean(y)+Q*se
interval<-c(mean(y)-Q*se, mean(y)+Q*se)

#now we plot confidence intervals (use 7 around mean since it's much bigger than the interval, which is about 3)
plot( mu + c(-7,7), c(1,1), type="n", xlab="weights", ylab="intervals", ylim=c(1,100))
#vertical line at mean of population
abline(v=mean(pop))
lines(interval,c(1,1))
#now run a for loop to plot a number of intervals, say 100 for samples of 30
#ask to color lines differently depending on whether interval crosses mean or not using a logical argument and
#ifelse
for(i in 2:100){
    y<-sample(pop,N)
    mean(y)
    interval<-c(mean(y)-Q*se, mean(y)+Q*se)
    color<-ifelse(interval[1]<=mean(pop) & interval[2]>=mean(pop),1,2)
    lines(interval,c(i,i),col=color)
}

#repeating for 5 samples instead of 30 - central limit theorem no longer works. need t-distribution. instead of qnorm
#use qt to find intervals.

dat<-read.csv("mice_pheno.csv")
pop<-dat[dat$Sex=="F" & dat$Diet=="chow",3]
mu<-mean(pop)
N<-5
y<-sample(pop,N)
mean(y)
se<-sd(y)/sqrt(N)
#instead of qnorm use qt to find 95% interval
#in t dist, need to tell it degrees of freedom (sample size - 1), since the tails of the distribution change with
#sample size, unlike the normal distribution
Q<-qt(1-0.05/2,4)
interval<-c(mean(y)-Q*se, mean(y)+Q*se)
plot( mu + c(-7,7), c(1,1), type="n", xlab="weights", ylab="intervals", ylim=c(1,100))
abline(v=mean(pop))
for(i in 1:100){
    y<-sample(pop,N)
    mean(y)
    interval<-c(mean(y)-Q*se, mean(y)+Q*se)
    color<-ifelse(interval[1]<=mean(pop) & interval[2]>=mean(pop),1,2)
    lines(interval,c(i,i),col=color)
}

#compare with t.test in R
dat<-read.csv("femaleMiceWeights.csv")
t.test(dat[13:24,2],dat[1:12,2])
#null hypothesis - difference of population means is 0 (aka population parameter is 0)
#if null hypothesis is true, the confidence intervals should fall on 0 95% of the time. The fact that it does in this
#case tells us that the p-value has to be bigger than 0.05

#assignment

babies = read.table("babies.txt", header=TRUE)
N<-30
intervals<-function(x,y){
    a<-sample(x,N)
    b<-sample(y,N)
    mytest <- t.test(a, b)
    conf<-mytest$conf.int
    return(conf[2]-conf[1])
}


x = babies$bwt[babies$smoke==0]
y = babies$bwt[babies$smoke==1]
data<-replicate(1000, intervals(x,y), simplify = "array")
mean(data)

bwt.nonsmoke = babies$bwt[babies$smoke==0]
bwt.smoke = babies$bwt[babies$smoke==1]
popdiff = mean(bwt.nonsmoke) - mean(bwt.smoke)

intervals2<-function(x,y){
    a<-sample(x,N)
    b<-sample(y,N)
    mytest <- t.test(a, b)
    conf<-mytest$conf.int
    return(conf[1]<=mean(popdiff) & conf[2]>=mean(popdiff))
}

end<-replicate(1000,intervals2(x,y))
sum(end)/length(end)

#######

dat.ns = sample(bwt.nonsmoke, 30)
dat.s = sample(bwt.smoke, 30)
X.ns = mean(dat.ns)
sd.ns = sd(dat.ns)
X.s = mean(dat.s)
sd.s = sd(dat.s)
sd.diff = sqrt(sd.ns^2/30 + sd.s^2/30)
tval = (X.ns - X.s)/sd.diff

tval
#Because our sample sizes are rather large, we can then use the qnorm() function to tell whether 
#tval corresponds to a p-value that is less than 0.05.

qnorm(1-0.05/2)

#We can use the same numbers to construct a confidence interval for the difference 
#in means between the smoking and nonsmoking populations. To do this, we follow Rafa's 
#instruction in the CI 2 video

ci.upper = (X.ns-X.s) + sd.diff*1.96
ci.lower = (X.ns-X.s) - sd.diff*1.96

####next lecture####
dat<-read.csv("mice_pheno.csv")
hfpop<-dat[dat$Diet=="hf" & dat$Sex=="F",3]
chowpop<-dat[dat$Diet=="chow" & dat$Sex=="F",3]

N<-5
hf<-sample(hfpop,N)
chow<-sample(chowpop,N)
t.test(hf,chow)
#cannot reject null - not enough power
#try with N=12, with alpha=0.05 (value at which we reject null), run simulation 10000 times

N<-12
alpha<-0.05
B<-10000

#in function if you just want to repeat over and over, use a variable i that ends up being useless
rejections<-sapply(1:B,function(i){
    hf<-sample(hfpop,N)
    chow<-sample(chowpop,N)
    t.test(hf,chow)$p.value < alpha
})
head(rejections)
#how often did we reject the null (which is the right thing to do here)?
mean(rejections)
#0.2265 so our power is 22%
#do this for several Ns

Ns<-seq(5,50,5)
power<-sapply(Ns,function(N){
    rejections<-sapply(1:B,function(i){
        hf<-sample(hfpop,N)
        chow<-sample(chowpop,N)
        t.test(hf,chow)$p.value < alpha
    })
return(mean(rejections))
})

plot(Ns,power)

###assignment

babies = read.table("babies.txt", header=TRUE)
bwt.nonsmoke = babies$bwt[babies$smoke==0]
bwt.smoke = babies$bwt[babies$smoke==1]

N<-15
B<-1000
alphas<-c(0.1,0.05,0.01)
reject<-sapply(alphas,function(alpha){
    rejections<-sapply(1:B,function(i){
        hf<-sample(hfpop,N)
        chow<-sample(chowpop,N)
        t.test(hf,chow)$p.value < alpha
    })
})

mean(reject[,1])
mean(reject[,2])
mean(reject[,3])

### chi squared tests etc
tab<-matrix(c(180,40,20,10),2,2)
tab
rownames(tab)<-c("AA or Aa","aa")
colnames(tab)<-c("Controls","Cases")
prop.table(tab,1) #Show as proportions of each row
ctest<-chisq.test(tab)
ctest

#assignment
d = read.csv("assoctest.csv")
d
domcase<-sum(d[,1]==1 & d[,2]==1)
domctrl<-sum(d[,1]==1 & d[,2]==0)
reccase<-sum(d[,1]==0 & d[,2]==1)
recctrl<-sum(d[,1]==0 & d[,2]==0)
tab<-matrix(c(domctrl,recctrl,domcase,reccase),2,2)
prop.table(tab,1)
ctest<-chisq.test(tab)
ctest
fisher.test(tab)

#monte carlo simulation
dat=read.table("http://www.biostat.jhsph.edu/bstcourse/bio751/data/babies.data",header=TRUE)
dim(dat)
set.seed(1) #gives you the same random numbers every time you run this code, so you get the same answer
#take 10 cases from smokers and non smokers
smokers=sample(dat$bwt[dat$smoke==1],10)
nonsmokers=sample(dat$bwt[dat$smoke==0],10)
cat("observed difference = ",mean(smokers)-mean(nonsmokers)," ounces")
#do this ten times
for(i in 1:10){
    smokers=sample(dat$bwt[dat$smoke==1],10)
    nonsmokers=sample(dat$bwt[dat$smoke==0],10)
    cat("observed difference = ",mean(smokers)-mean(nonsmokers)," ounces\n")
}
#repeat this 1000 times for samples of 10 and create a t statistic, and see what distribution it has
ttestgenerator<-function(b=1000,n=10){
    sapply(1:b,function(i){
        smokers=sample(dat$bwt[dat$smoke==0],n) #create t stat for null hypothesis so sample for both from non smokers
        nonsmokers=sample(dat$bwt[dat$smoke==0],n)
        return((mean(smokers)-mean(nonsmokers))/sqrt((var(smokers))/n+var(nonsmokers)/n)) #formula for t stat
    })
}
ttests<-ttestgenerator(1000,10)
hist(ttests)
qqnorm(ttests)
abline(0,1)

#normal dist works for samples of 10, but not so well for samples of 3

ttests<-ttestgenerator(1000,3)
hist(ttests)
qqnorm(ttests)
abline(0,1)
#so can we use the t distribution instead?
#start by creating 1000 quantiles, in increments of 0.5
qs<-(seq(0,999)+0.5)/1000
#compare these quantiles with the statistics generated in the above monte carlo simulation (i.e. ttests)
qqplot(qt(qs,df=2*3-2),ttests,xlim=c(-6,6),ylim=c(-6,6))
abline(0,1)
#it's not perfect because for t distribution, you assume the ORIGINAL data is normally distributed, which it
#may not perfectly be
qqnorm(dat$bwt[dat$smoke==0])
qqline(dat$bwt[dat$smoke==0])

##monte carlo assignment
babies = read.table("babies.txt", header=TRUE)
bwt.nonsmoke = babies$bwt[babies$smoke==0]
pop.var = var(bwt.nonsmoke)

#Using Monte Carlo simulation, take 1000 samples of size 10 from bwt.nonsmoke and calculate the variance. 
#Look at a histogram of vars, and add the population variance as a vertical line.
b=1000
n=50
nonsmokers=function(n){
    data<-sample(bwt.nonsmoke,n)
    var(data)
}
vars<-replicate(b, nonsmokers(n))
hist(vars)
abline(v=pop.var)
(length(vars[vars>1.5*pop.var]))/1000

sample.size = 2:400
var.estimate = sapply(sample.size, function(n) var(sample(bwt.nonsmoke, n)))
plot(sample.size, var.estimate)
abline(h=pop.var, col="blue")

##permutations
set.seed(0)
N=10
smokers=sample(dat$bwt[dat$smoke==1],N)
nonsmokers=sample(dat$bwt[dat$smoke==0],N)
obs=mean(smokers)-mean(nonsmokers)
#use permutations to see if this difference is significant
#sample from total smokers and non smokers to get two random groups of 50
avgdiff<-sapply(1:1000,function(i){
    all=sample(c(smokers,nonsmokers))
    smokersstar=all[1:N] #randomly sample
    nonsmokersstar=all[(N+1):(2*N)] #randomly sample
    return(mean(smokersstar)-mean(nonsmokersstar))
})
hist(avgdiff)
abline(v=obs)
print(mean(abs(avgdiff) > abs(obs)))

#assignment - use difference in medians instead
dat=read.table("http://www.biostat.jhsph.edu/bstcourse/bio751/data/babies.data",header=TRUE)
N=50
smokers=sample(dat$bwt[dat$smoke==1],N)
nonsmokers=sample(dat$bwt[dat$smoke==0],N)
obs=median(smokers)-median(nonsmokers)
#use permutations to see if this difference is significant
#sample from total smokers and non smokers to get two random groups of 50
avgdiff<-sapply(1:1000,function(i){
    all=sample(c(smokers,nonsmokers))
    smokersstar=all[1:N] #randomly sample
    nonsmokersstar=all[(N+1):(2*N)] #randomly sample
    return(median(smokersstar)-median(nonsmokersstar))
})
hist(avgdiff)
abline(v=obs)
print(median(abs(avgdiff) > abs(obs)))

