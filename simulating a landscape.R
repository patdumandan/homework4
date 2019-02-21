#do this first:
#####CHOSE AN ORGANISM
####CHOSE A COVARIATE THAT IS IMPORTANT FOR THAT
#Organism's distribution
library("raster")
library("rgeos")
library("rgdal")



# Define function to draw random samples from a multivariate normal
# distribution


rmvn <- function(n, mu = 0, V = matrix(1)) {
  p <- length(mu)
  if (any(is.na(match(dim(V), p)))) 
    stop("Dimension problem!")
  D <- chol(V)
    t(matrix(rnorm(n * p), ncol = p) %*% D + rep(mu, rep(n, p)))
}

# Set up a square lattice region
simgrid <- expand.grid(1:50, 1:50)
n <- nrow(simgrid)

# Set up distance matrix
distance <- as.matrix(dist(simgrid))
# Generate random variable

phi = 0.07 #phi determines scale of distance variation
#how does changing phi change the spatial aggregation in the plotted raster?
plot(1:100, exp(-phi * 1:100), type = "l", xlab = "Distance", ylab = "Correlation")



X <-rmvn(1, rep(0, n),exp(-phi * distance))

#X is predictor variable. Does it make more sense to have a discrete or
#continuous predictor?
X <- rpois(n, lambda=exp(-1+rmvn(1, rep(0, n), exp(-phi * distance))))


# Visualize results
Xraster <- rasterFromXYZ(cbind(simgrid[, 1:2] - 0.5, X))

plot(Xraster)

#Converting raster to a dataframe
spat_dat=rasterToPoints(Xraster)


#how many points can you sample?
GO=sample(x=c(1:nrow(spat_dat)),size=100)

points(spat_dat[GO,c(1:2)])

#how would you deal with spatial autocorrelation here?

presence_intercept=0.5
presence_slope=1.3

#assumptions?
PA=rbinom(100,plogis(presence_intercept+spat_dat[GO,3]*presence_slope),
          size=1)


count_intercept=2
count_slope=0.8
over_dispersion=0.5
  
abundance=PA*rnbinom(100,mu=
exp(count_intercept+count_slope*spat_dat[GO,3]),
size=over_dispersion)

plot(abundance~spat_dat[GO,3])


