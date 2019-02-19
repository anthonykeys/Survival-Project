# Stick Breaking

how.many.times = 40
num.breaks.str = 100
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

del.holder = matrix(nrow = num.breaks.str,ncol = how.many.times)
w.holder = matrix(nrow = num.breaks.str,ncol = how.many.times)
for (i in 1:how.many.times) {
  lot = many.runs(num.breaks = num.breaks.str)
  del.holder[,i] = lot[,1]
  w.holder[,i] = lot[,2]
}

todo.list = unique(as.numeric(as.list(del.holder)))[order(unique(as.numeric(as.list(del.holder))))]
mean.w = numeric(length(todo.list))

for (i in 1:length(todo.list)) {
  holder.mean = numeric(how.many.times)
  for (j in 1:how.many.times) {
    if(sum(del.holder[,j]<=todo.list[i])>=1){
      index = which(del.holder[,j] == max(del.holder[del.holder[,j]<=todo.list[i],j]))
      holder.mean[j] = w.holder[index,j]
    } else {
      holder.mean[j] = 0
    }

  }
  mean.w[i] = mean(holder.mean)
  
}

plot(todo.list, mean.w, type = "l", main = "Mean of DP Sample Paths", 
     xlab = "x", ylab = "P(X<x)")
norm.val = pnorm(todo.list)
lines(todo.list, norm.val, type = "l", col = "red")
