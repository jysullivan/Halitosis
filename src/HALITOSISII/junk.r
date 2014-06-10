

# simulating draws from an inhomogeneous Poisson process

# if our intensity is lambda(x,y) = 1.35-.25x+.4y+.75xy
# and if 0 < x < 3 and  1 < y < 7
# then the maximum of lambda on this region
#  is when x = 0 and y=0, and lambda.max = 1.35

library(splancs)

#plot( 0,0, xlim=c(0,2), ylim=c(0,1) )
#tmp <- getpoly()
#tmp

# if we integrate lambda(x,y) across this region,
# we get 200.  This is the expected number of points.
expected.num <- 128
num.points <- rpois( 1, expected.num )

lambda.max <- 19.15
# An easy way to calculate lambda.max for this
# intensity function was to evaluate lambda(x,y)
# at the four corners of the rectangle...
# Will this always tell us where the max is?

n.attempts <- 0
n.events <- 0
xx <- rep(NA,num.points)
yy <- rep(NA,num.points)
while( n.events < num.points ) {
  x1 <- runif(1, min=0,max=3 )
  y1 <- runif(1, min=1,max=7 )
  # with probability lambda(here) / lambda.max,
  # retain this point.
  prob <- (1.35-.25*x1+.4*+.75*x1*y1)/lambda.max
  if( rbinom(1, 1,prob) == 1 ) {
    n.events <- n.events + 1
    xx[n.events] <- x1
    yy[n.events] <- y1
  }
  n.attempts <- 1 + n.attempts
}
print(paste("n.attempts=",n.attempts))
print(paste("n.events=",n.events))

library(spatstat)

my.ppp <- ppp( xx,yy, window=owin(c(0,3),c(1,7)) )
plot(my.ppp)

