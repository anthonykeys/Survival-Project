# setwd("~/Documents/Git/Survival-Project")

n = 1000
norm1.mean = -10
norm1.var = 3
weight1 = 0.3
norm2.mean = -5
norm2.var = 2
weight2 = 0.5
norm3.mean = -1
norm3.var = 1
weight3 = 0.2

sample.ind = sample(c(1,2,3), n, replace = TRUE, prob = c(weight1, weight2, weight3))
summary(as.factor(sample.ind))

sample1 = rnorm(as.numeric(summary(as.factor(sample.ind))["1"]), 
                mean = norm1.mean, sd = sqrt(norm1.var))
sample2 = rnorm(as.numeric(summary(as.factor(sample.ind))["2"]), 
                mean = norm2.mean, sd = sqrt(norm2.var))
sample3 = rnorm(as.numeric(summary(as.factor(sample.ind))["3"]), 
                mean = norm3.mean, sd = sqrt(norm3.var))
sam.tot = c(sample1, sample2, sample3)
plot(density(sam.tot))