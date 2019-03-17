# setwd("~/Documents/Git/Survival-Project")
source('stick.break.norm.R')

n = 1000
alpha = 20
num.of.breaks = 100
G.std =1
std.dev = 1

y = numeric(n)
for (i in 1:n) {
  holder = stick.break.norm(alpha, num.of.breaks, std = G.std)
  y[i] = rnorm(1, mean = sample(holder[,1], 1, prob = holder[,2]), sd = std.dev)
}
var(y)
