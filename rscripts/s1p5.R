gamma.function <- function(n) {
  factorial(n-1)
}

dball.volume <- function(d, radius) {
  (pi^(d/2))/(gamma.function(d/2+1)) * radius^(d)
}

# Probability that X' is greater than eps away from X is always greater than...
lower.probability.threshold <- function(eps, d) {
  (eps^(d)*dball.volume(d, eps))/(2^(d))
}

eps <- 0.1
d <- seq(1,6,1)

# as d increases, probability X' is "not far" away decreases
(res <- lower.probability.threshold(eps, d))

# as eps increases, probability X' is "not far" away (according to eps) increases
eps <- seq(0.1,0.9, 0.1)
d <- 3
(res <- lower.probability.threshold(eps, d))
