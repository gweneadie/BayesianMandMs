

# ------------------------- plot the prior distribution

# parameters for the prior distribution
a = 4
b = 15

# set the margins to make room for labels (don't have to do this, there are defaults)
# numbers here are bottom, left, top, right
par(mar=c(5,7,4,2))

# with curve(), but with fancier labels
curve( dbeta(x, shape1 = a, shape2 = b),xlab = expression(theta), ylab = expression(p(theta)) , cex.lab=2, cex.axis=2, lwd = 2.5, main="Prior Distribution for proportion of blue m&m's")

# ... and a grid:
grid()


# -------------------------- Write a function for plotting the posterior distribution

# function to plot posterior distribution
posteriorfunc = function(x, a, b, n, y){
  
  dbeta(x, shape1 = (a + y), shape2 = (b + n - y))
  
}


# --------------------------- Collect data

# number of blue m&m's 
nblue = 28
  
# total number of m&m's
ntotal = 120
  
  
# plot posterior
par(mar=c(5,5,2,2))
curve( posteriorfunc(x, a=a, b=b, n=ntotal, y=nblue),  xlab = expression(theta), ylab = expression(p(theta|y)) , cex.lab=2, cex.axis=2, n = 2e3, lwd = 2.5, main="Posterior Distribution for proportion of blue m&m's")

# add a curve showing the prior distribution
curve( dbeta(x, shape1 = a, shape2 = b), add=TRUE, col="blue", lty=2)

# add a legend
legend("topright", legend=c("prior", "posterior"), col = c("blue", "black"), lwd = 2.5, lty = c(2,1))

# ---------------------------- Calculate the sample mean and sample variance

  
# function to calculate the expected value of theta (the mean) of the posterior
mean.beta = function( a, b ){ 
    a/(a+b)
  }

# function ot calculate the posterior variance
var.beta = function( a, b, n, y){
  (a + y)*(b + n - y)/( (a + b + n)^2 * (a + b + n + 1) )
}


mean.beta(a=(a+nblue), b=(b+ntotal-nblue))  


