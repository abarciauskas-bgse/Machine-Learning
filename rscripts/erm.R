setwd('~/Box Sync/abarciausksas/second_term/Machine Learning/test_prep/')

if (!require('mvtnorm')) install.packages('mvtnorm')
source('utils.R')
# vc dimension leq d+1
d = 2
print(paste('vc dimension of linear classifier centered at origin is:',d+1))
n <- d+1

# 2 points can be shattered
x <- rmvnorm(d, mean = c(2,3), sigma = diag(1,2))
plot(x, pch = 19, ylim = c(0,6), xlim = c(0,6))
# x should be 2x2 matrix
mid <- midpoint.2d(x[1,], x[2,])
arrows(0,0,mid[1]*2,mid[2]*2, col = 'green')
# line from origin to midpoint
(newline.mat <- matrix(c(0,mid[1],0,mid[2]), nrow = 2, ncol = 2))
slope(newline.mat)
x[1,]%*%c(1,-slope(newline.mat))

# w, our orthogonal vector will be w = c(a,b)
# arbitrary assignment of the first element in w,
# which can take on infinitely many values
a <- 1
b <- -mid[1]/mid[2]
(w <- c(a,b))
t(x[2,])%*%w
pt1 <- x[1,]
(proj <- ((pt1%*%w)/(w%*%w))%*%w)
(projpt1 <- pt1-proj)
points(projpt1, col = 'red', pch = 19)
arrows(x0=projpt1[1], y0 = projpt1[2], x1=pt1[1], y1=pt1[2])

pt2 <- x[2,]
(proj <- ((pt2%*%w)/(w%*%w))%*%w)
(projpt2 <- pt2-proj)
points(projpt2, col = 'red', pch = 19)
arrows(x0=projpt2[1], y0 = projpt2[2], x1=pt2[1], y1=pt2[2])
