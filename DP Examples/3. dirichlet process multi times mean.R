how.many.times = 100
alpha.str = .1
grid.size.str = .005

many.runs = function(alpha = 2,
                     grid.size = .01){
  grid = seq(from = 0, to = 1, by = grid.size)
  
  
  # Cumulative Densities
  G = punif(grid,min=0, max=1)
  
  #initialization
  dir.param = numeric(length(G))
  
  
  # Dirichlet Parameters
  dir.param[1] = G[1]
  for (i in 2:length(G)) {
    dir.param[i] = G[i] - G[i-1]
  }
  
  dir.param.alpha = dir.param * alpha
  
  rand.samp = numeric(length(G))
  for (i in 1:length(G)) {
    rand.samp[i] = rgamma(1,shape = dir.param.alpha[i],rate = 1)
  }
  
  norm.rand.samp = rand.samp/sum(rand.samp)
  
  fixer = function(x){
    result = numeric(length(x))
    result[1] = x[1]
    for (i in 2:length(x)) {
      result[i] = sum(x[1:i])
    }
    return(result)
  }
  gimme = fixer(norm.rand.samp)
  return(gimme)
}

# +1 for the grid
holder = matrix(nrow = length(seq(from = 0, to = 1, by = grid.size.str)), ncol = how.many.times)
for (i in 1:how.many.times) {
  holder[,i] = many.runs(alpha = alpha.str,
                  grid.size = grid.size.str)
  
}
grid = seq(from = 0, to = 1, by = grid.size.str)

#plotting
plot(grid, holder[,1], type = "l")
for (i in 2:how.many.times) {
  lines(grid, holder[,i], type = "l")
}


abline(0,1, col = "red")

plot(grid, apply(holder,1, mean), type = "l", main = "Mean of DP Sample Paths", 
     xlab = "x", ylab = "P(X<x)")
abline(0,1, col = "red")
