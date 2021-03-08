# script to make plots for publication
# function to calculate the expected value of theta (the mean) of the posterior
mean.beta = function( a, b ){ 
  a/(a+b)
}

# function ot calculate the posterior variance
var.beta = function( a, b, n, y){
  (a + y)*(b + n - y)/( (a + b + n)^2 * (a + b + n + 1) )
}

# hyperparameters for beta prior, chosen as a class
aclass = 2
bclass = 9

# Author's data from class
# data from author's bag of m&m's
mmdata = read.table(file = "MandM_data/MandM_data_author1.txt", header=TRUE, skip=1)

# total number of m&ms in my bag
ntotal = nrow(mmdata)
# total number of blues in my bag
nblues = sum(mmdata$colour=="blue")

# total number of m&ms in class
nblueclass = 25
ntotalclass = 100

# mean and variance of posterior distribution from all data
totalmean = mean.beta(a = (aclass + nblueclass) , b = (bclass + ntotalclass - nblueclass) )
totalvar = var.beta(a = aclass, b = bclass, n = ntotalclass, y = nblueclass)

# plot the posterior distribution for my bag of m&m's, with the prior, and the posterior for the entire classes' m&m's

pdf("posterior_class.pdf", useDingbats = FALSE)
par(mar=c(5,5,5,5))

curve(dbeta(x, shape1 = (aclass + nblues), shape2 = (bclass + ntotal - nblues)), xlab = expression(theta), ylab = expression(p(theta)) , main = "Posterior distribution for the probability of drawing a blue m&m", cex.lab=2, lwd=2, ylim=c(0,10), lty=2)

grid()

# add the prior to show how it changed
curve(dbeta(x, shape1 = aclass, shape2 = bclass), lwd=2, lty=3, add=TRUE)

# add the posterior using the entire class' data
curve(dbeta(x, shape1 = (aclass + nblueclass), shape2 = (bclass + ntotalclass - nblueclass)), lwd=2, lty=1, add=TRUE)

# add a vertical line to show the mean of the posterior for all the data
abline(v = totalmean, lty=4, lwd=2, col="red")

legend("topright", legend = c( as.expression(bquote("prior, "~alpha == .(aclass)~"and"~beta == .(bclass))), "posterior (first author's data)", "posterior (class' data)", "posterior mean (class' data)"), lty=c(3,2,1,4), lwd=2, col=c("black", "black", "black", "red"))

dev.off()

######
# hyperparameters for beta prior, chosen by Seminar members
aSeminar = 3
bSeminar = 7

  
# load data from seminar 
seminar = read.csv(file = "MandM_data/SEMINAR_mmsTallyCLVTennessee.csv", stringsAsFactors = FALSE, row.names = 1)

# author's m&m's
nbluesAuthor = sum(seminar["d", "Blue" ])
ntotalAuthor = sum(seminar["d", ])

# m&ms data in Seminar seminar
nblueSeminar = sum(seminar$Blue)
ntotalSeminar = sum(seminar)

# mean and variance of posterior distribution from all data in seminar
totalmeanSeminar = mean.beta(a = (aSeminar + nblueSeminar) , b = (bSeminar + ntotalSeminar - nblueSeminar) )
totalvarSeminar = var.beta(a = aSeminar, b = bSeminar, n = ntotalSeminar, y = nblueSeminar)


# make a pdf file of 
pdf("posterior_seminar.pdf", useDingbats = FALSE)
par(mar=c(5,5,5,5))

curve(dbeta(x, shape1 = (aSeminar + nbluesAuthor), shape2 = (bSeminar + ntotalAuthor - nbluesAuthor)), xlab = expression(theta), ylab = expression(p(theta)) , main = "Posterior distribution for the probability of drawing a blue m&m", cex.lab=2, lwd=2, ylim=c(0,18), lty=2)

grid()

# add the prior to show how it changed
curve(dbeta(x, shape1 = aSeminar, shape2 = bSeminar), lwd=2, lty=3, add=TRUE)

# add the posterior using the entire class' data
curve(dbeta(x, shape1 = (aSeminar + nblueSeminar), shape2 = (bSeminar + ntotalSeminar - nblueSeminar)), lwd=2, lty=1, add=TRUE)

# add a vertical line to show the mean of the posterior for all the data
abline(v = totalmeanSeminar, lty=4, lwd=2, col="red")

legend("topright", legend = c( as.expression(bquote("prior, "~alpha == .(aSeminar)~"and"~beta == .(bSeminar))), "posterior (second author's data)", "posterior (seminar data)", 
                               "posterior mean (seminar data)",
                               as.expression(bquote(~~~~~mu==.(totalmeanSeminar)~", "~~sigma== .(sqrt(totalvarSeminar))) )), lty=c(3,2,1,4, NA), lwd=2, col=c("black", "black", "black", "red"))

dev.off()

