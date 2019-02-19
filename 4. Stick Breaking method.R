# Stick Breaking

# make grid?
num.breaks = 1000
alpha = 20
z = rbeta(num.breaks,1,alpha)
w = numeric(num.breaks)
for (i in 1:num.breaks) {
  w[i] = ifelse(i==1, z[i], z[i]*prod((1-z)[1:(i-1)]))
}

# Using N(0,1) from slides
delta = rnorm(num.breaks, 0,1)

# need to work on this plot
plot(delta[order(delta)],w[order(delta)])

###########################
# this is exactly from b4 #
###########################
fixer = function(x){
  result = numeric(length(x))
  result[1] = x[1]
  for (i in 2:length(x)) {
    result[i] = sum(x[1:i])
  }
  return(result)
}
############################

gimme = fixer(w[order(delta)])
plot(delta[order(delta)],gimme, type = "l", main = "Sample Path From DP", 
     xlab = "x", ylab = "P(X<x)")
norm.val = pnorm(delta[order(delta)])
lines(delta[order(delta)], norm.val, type = "l", col = "red")



