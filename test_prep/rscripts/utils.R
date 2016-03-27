slope <- function(x) {
  diff(x[,2])/diff(x[,1])
}
# find the midpoint and make a line from origin to that point
midpoint.2d <- function(pt1,pt2) {
  # mid point on first dimension
  first.coord <- (pt1[1] + pt2[1])/2
  second.coord <- (pt1[2] + pt2[2])/2
  return(c(first.coord, second.coord))
}

# a set of classifiers may be: the set of half-planes, the set of half-planes centered at 0 or a class of closed balls.

# For each type of classifier, output the VC dimension and give an example of the VC dimension + 1 being shattered

# Linear half-spaces {x: t(x)%*% w leq 0}

linear.classifier.origin <- function(x, w) {
  return(ifelse(t(x)%*%w > 0), 1, 0)
}
