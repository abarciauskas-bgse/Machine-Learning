# create an odd number of equally distributed 1's and 0's
n <- 10
ys <- sample(rep(c(0,1), n/2))[2:n]
errors <- 0
# estimate risk using leave-one-out
# expected risk is 1/2?
for (i in 1:length(ys)) {
  decision = 0
  actual <- ys[i]
  # if the sum of y's divided by n - 1 is greater than or equal to 1/2 -> decision is 1
  withouts <- ys[-1]
  decision.prob <- sum(withouts)/(n-1)
  if (decision.prob >= (1/2)) {
    decision = 1
  }
  
  if (!(decision == actual)) {
    errors <- errors + 1
  }
}

# risk estimate is number of errors divided by number of trials (n)
nrisk.estimate <- errors/n
# upper bound when true expected risk is 1/2?
# expected risk is min(1, 1-p), which is sup at 1/2
# we are trying to lower bound the variance with c/sqrt(n)
# so this will indeed be lower bounded when the expected risk is maximized at 1/2
(var <- (nrisk.estimate - 1/2)**2)
# Show that for some distributions Var(R(D)(g)) >= c/sqrt(n) for some constant c
# if we take c to be 1/3, is the variance always greater than 1/3?
c <- 1e-20
# as n->inf, var -> 0
# but if n is 3, var -> 0.25
bound <- c/sqrt(n)
var >= bound
