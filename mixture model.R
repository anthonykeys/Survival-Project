source('DP Examples/4. Stick Breaking method.R')

#keeping num.breaks

fine = 0.01
support = seq(from=0, to=1-fine, by=fine)
grand.support = matrix(nrow = length(support), ncol = num.breaks)
for (i in 1:length(gimme)) {
  grand.support[,i]=pnorm(support, mean=gimme[i], sd=1)
}

holder = matrix(w,nrow=length(w), ncol=1)
f.of.stuff= grand.support %*% holder

plot(support,f.of.stuff, type = "l")
