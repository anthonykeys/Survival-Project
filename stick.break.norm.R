# Normal(0,1) Stick Breaking

stick.break.norm = function(alpha = 20, num.breaks = 100, std = 1){
  # make grid?
  num.breaks = 100
  alpha = 20
  z = rbeta(num.breaks,1,alpha)
  w = numeric(num.breaks)
  for (i in 1:num.breaks) {
    w[i] = ifelse(i==1, z[i], z[i]*prod((1-z)[1:(i-1)]))
  }
  
  # Using N(0,1) from slides
  delta = rnorm(num.breaks, 0, std)
  
  return(cbind(z=delta,weights=w))
}

