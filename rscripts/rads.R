halfline.classify <- function(xs, threshold) {
  return(ifelse(xs >= threshold, 1, -1))
}

# Returns all possible ways to classify n points using the half-line classifier
halfline.sets <- function(n) {
  xs <- runif(n)
  xs <- xs[order(xs)]
  # for each n, classify based on 
  all.sets <- matrix(NA, nrow = n+1, ncol = n)
  # first set is all are 1
  # last set is all are -1
  all.sets[1,] <- rep(1, n)
  all.sets[nrow(all.sets),] <- rep(-1, n)
  for (set.idx in 2:n) {
    current.x = xs[set.idx]
    all.sets[set.idx,] <- halfline.classify(xs, current.x)
  }
  all.sets <- rbind(xs,all.sets)
  return(all.sets)
}

n<-3
print('Sets possible using the class of intervals:')
halfline.sets <- halfline.sets(n)
print(halfline.sets)
halfline.sets <- halfline.sets[2:nrow(halfline.sets),]

interval.classify <- function(xs, a, b) {
  # Not sure if this should be inclusive
  return(ifelse(xs >= a & xs <= b, 1, -1))
}

interval.sets <- function(n) {
  # Returns all possible ways to classify n points using the interval classifier
  xs <- runif(n)
  xs <- xs[order(xs)]

  # for each n, classify based on 
  num.sets <- n*(n+1)/2 + 1
  all.sets <- matrix(NA, nrow = num.sets, ncol = n)
  # first set is all are 1
  # last set is all are -1
  all.sets[1,] <- rep(1, n)
  all.sets[nrow(all.sets),] <- rep(-1, n)

  # for each x, create an n intervals that includes x plus n_{i}
  for (i in 1:length(xs)) {
    a <- xs[i]
    for (j in 1:length(xs)) {
      if (j >= i) {
        b <- xs[j]
        all.sets[i+j,] <- interval.classify(xs, a, b)
      }
    }
  }
  all.sets <- rbind(xs,all.sets)
  return(all.sets)
}

print('Sets possible using the class of intervals:')
interval.sets <- interval.sets(n)
# remove xs
interval.sets <- interval.sets[2:nrow(interval.sets),]

# TODO: Make some plots

# generate mistakes sets matrices
# check for mistakes in each row
# returns a matrix setlength x n in {0,1}
# corresponding to whether a mistake was made in that set
#
mistake.sets <- function(ys, sets.matrix) {
  mistakes <- matrix(NA, nrow = nrow(sets.matrix), ncol = ncol(sets.matrix))
  for (set.idx in 1:nrow(sets.matrix)) {
    current.set <- sets.matrix[set.idx,]
    mistakes[set.idx,] <- as.numeric(ys != current.set)
  }
  return(mistakes)
}
mistake.sets(irl.ys, interval.sets)
# do the assignment of "actual" y's
irl.ys <- ifelse(rbinom(3, 1, 0.5) == 1, 1, -1)

# Generate rademacher averages
nsims <- 1000
# for every trial
# generate n rademacher variables
# call these rads.trial.i
# 
# take the max of 1/n*rowsums(rads.trial.i*num.mistakes)
rad.trials <- matrix(NA, nrow = nsims, ncol = 2)
colnames(rad.trials) <- c('halfline','interval')

for (s in 1:nsims) {
  rads.trial.i <- ifelse(rbinom(3, 1, 0.5) == 1, 1, -1)
  halfline.mistakes <- mistake.sets(irl.ys, halfline.sets)
  interval.mistakes <- mistake.sets(irl.ys, interval.sets)
  sup.halfline <- max(1/n*apply(halfline.mistakes, 1, function(row) { return(rads.trial.i%*%row) }))
  sup.interval <- max(1/n*apply(interval.mistakes, 1, function(row) { return(rads.trial.i%*%row) }))
  rad.trials[s,'halfline'] <- sup.halfline
  rad.trials[s,'interval'] <- sup.interval
}
rad.average.halfline <- mean(rad.trials[,'halfline'])
rad.average.interval <- mean(rad.trials[,'interval'])
print(paste('Rademacher average for halflines:',rad.average.halfline))
print(paste('Rademacher average for intervals:',rad.average.interval))

# Expected deviation of true risk of data-based classifier from the minimal risk of each class is (i.e. Exp[ R(g_n) - R(gbar) ])


