setwd('~/Box Sync/abarciausksas/second_term/Machine Learning/test_prep/')
source('utils.R')
if (!require('mvtnorm')) install.packages('mvtnorm')

n <- 4
means <- c(2,3)
sd = 1
x1 <- rnorm(n/2, mean = means[1], sd = sd)
x2 <- rnorm(n/2, mean = means[2], sd = sd)
q1 <- runif(1)
q0 <- 1-q1
y <- rbinom(n, 1, prob = q1)

ylimits = c(0.9,1.1)
# arbitrary
xmid = min(x1[2], x2[1]) + abs(x1[2] - x2[1])/2

dev.new(width=10, height=4)
plot(c(x1,x2), rep(1,n), pch = '|', ylab = '', col = ifelse(y == 1, 'red', 'blue'), ylim = ylimits)

# arbitrary classifier
segments(x0 = xmid, y0 = ylimits[1], x1 = xmid, y1 = ylimits[2], rgb(red = 0, green = 0, blue = 0, alpha = 0.4))

# shade left
polygon(
  x = c(-10,xmid,xmid,-10),
  y = c(ylimits[1],ylimits[1],ylimits[2],ylimits[2]),
  col = rgb(red = 0.99, green = 0, blue = 0, alpha = 0.1),
  border = NA)

# shade right
polygon(
  x = c(xmid,20,20,xmid),
  y = c(ylimits[1],ylimits[1],ylimits[2],ylimits[2]),
  col = rgb(red = 0, green = 0, blue = 0.99, alpha = 0.1),
  border = NA)

# calculate empirical risk of our classifier
# right now this is for 1-d
empirical.risk <- function(w, x, y) {
  errors = 0
  for (i in 1:length(y)) {
    clssfr.func = t(w)%*%x[,i]
    if ((clssfr.func <= 0) && (y[i] == 1)) {
      errors = errors + 1
    } else if ((clssfr.func > 0) && (y[i] == 0)) {
      errors = errors + 1
    }
  }
  return(errors/length(y))
}
# if we want to classify left as +1
w = c(xmid, -1)
# if we want to classify right as +1
# w = c(-xmid, 1)
x1.plus = sapply(x1, function(i) { return(c(1,i)) })
x2.plus = sapply(x2, function(i) { return(c(1,i)) })

empirical.risk(w, cbind(x1.plus, x2.plus), y)
