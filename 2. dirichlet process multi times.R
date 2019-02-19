how.many.times = 100

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
  return(cbind(grid, gimme))
}

lot = many.runs()
plot(lot, type = "l", main = "CDF Sample Paths From DP", xlab = "x", ylab = "P(X<x)")

for (i in 2:how.many.times) {
  lot = many.runs()
  lines(lot, type = "l", col = rainbow(how.many.times)[i])
}

abline(0,1, col = "red")
