dat <- read.csv("femaleMiceWeights.csv")
mean(dat[13:24,2]) - mean(dat[1:12,2])
s = split(dat[,2], dat[,1])
outlier = s$chow[s$chow>mean(dat[13:24,2])]
outlier
highfat = s[["hf"]] #can also be highfat=s$hf
highfat
sample(highfat, 6)
population = read.csv("femaleControlsPopulation.csv")
population = population[,1]
mean(population)
sampleMean = replicate(10000, mean(sample(population, 12)))
plot(sampleMean)
null = replicate(10000, mean(sample(population, 12)) - mean(sample(population, 12)))
hist(null)
abline(v=diff, col="red")
abline(v=-diff, col="red")
mean(null>diff)
negdiff=-diff
mean(null<negdiff)
mean(null>diff|null<negdiff)

