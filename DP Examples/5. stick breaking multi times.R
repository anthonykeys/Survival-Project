# Stick Breaking

how.many.times = 100

many.runs = function(alpha = 20, 
                     num.breaks = 1000){

  z = rbeta(num.breaks,1,alpha)
  w = numeric(num.breaks)
  for (i in 1:num.breaks) {
    w[i] = ifelse(i==1, z[i], z[i]*prod((1-z)[1:(i-1)]))
  }
  
  # Using N(0,1) from slides
  delta = rnorm(num.breaks, 0,1)
  
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
  result = cbind(delta[order(delta)], gimme)
  return(result)
}

lot = many.runs()
plot(lot[,1], lot[,2], type = "l", main = "CDF Sample Paths From DP", xlab = "x", ylab = "P(X<x)")

for (i in 2:how.many.times) {
  lot = many.runs()
  lines(lot[,1], lot[,2], type = "l", col = rainbow(how.many.times)[i])
}
