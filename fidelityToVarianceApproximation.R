library(CircStats)
library(plyr)
library(ggplot2)

# radian/degree conversions
rad2deg <- function(rad) {(rad * 180) / (pi)}
deg2rad <- function(deg) {(deg * pi) / (180)}

# von mises parameters 
n = 10000
mu = pi  # mean
k = seq(.5, 20, .5) # kappa/fidelity/concentration/precision
repis = 10

# generate pseudo-random data for increasing 'k's
kVsSD = ldply(
  .data = k
  , .fun = function(x) {
    x_dists = NULL
    x_sds = NULL
    for (repi in 1:repis) {
      x_dist = rvm(n, mu, x) 
      x_dists = cbind(x_dists, x_dist)
      x_sd = sd(x_dist)
      x_sds = c(x_sds, x_sd) 
    }
    
    # # plot all repititions together for given k
    # hist(x_dists, breaks = seq(0, 2*pi, pi/64), main = sprintf("kappa = %f", x), ylim = c(0, 10000) )  
    
    # df
    x_df = data.frame(
      k = x 
      , sd_est = sqrt(1/x)  
      , sd_real_mean = mean(x_sds)  
      , error = mean(x_sds) - sqrt(1/x)  
    )
    return(x_df)
  }
)

# plot error (in radians)
plot(
  x = k
  , y = kVsSD$error
  , type = "l"
  , ylab = "error"
  , xlab = "kappa"
  , ylim = c(0,max(kVsSD$error))
  , main = "radian scale"
  )
abline(h=0, lty = "dashed", lwd = 2)  # zero line
abline(h=deg2rad(1), col = "red")  # one 'degree' line

# plot 
plot(
  x = k
  , y = rad2deg(kVsSD$error)
  , type = "l"
  , ylab = "error"
  , xlab = "kappa"
  , ylim = c(0, max(rad2deg(kVsSD$error)) )
  , main = "degree scale"
)
abline(h=0, lty = "dashed", lwd = 2)  # zero line
abline(h=1, col = "red")  # one degree line
