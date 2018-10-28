
# class set the hyperparameters alpha and beta for the prior distribution to be
aclass=2
bclass=9

# Seminar used
aseminar = 3
bseminar = 7

pdf(file = "prior_class_vs_seminar.pdf", useDingbats = FALSE, width=7, height=8)
par(mar=c(5,5,5,5))

# plot the prior distribution from students
curve(dbeta(x, shape1 = aclass, shape2 = bclass),
      xlab = expression(theta), ylab = expression(p(theta)) , cex.lab=2, cex.axis=2, lwd = 2.5, main="Prior Distributions for class and seminar", ylim=c(0,5))
# add a grid to the plot if you like
grid()


# add curve for the prior distribution from faculty, postdocs, gradstudents
curve(dbeta(x, shape1 = aseminar, shape2 = bseminar), add=TRUE, lty=2, lwd=2.5)

# add a legend showing the alpha and beta values
legend("topright", legend = c(
  as.expression( bquote("class values:"~alpha == .(aclass)~","~beta == .(bclass)) ), 
  as.expression( bquote("seminar values:"~alpha == .(aseminar)~","~beta == .(bseminar)) )
  ), cex=1.5, lty=c(1,2))

dev.off()


