# library(shinystan)
library(coda)
library(ggplot2)
library(ggmcmc)
library(CircStats)


#### Diagnostics ####
# convert stanfit sample to dataframe table 
gg_toj_color_post = ggs(toj_color_post)

# look
gg_toj_color_post

# look structure
str(gg_toj_color_post)

### Look at posteriors
ggs_histogram(gg_toj_color_post, family = "logitRhoEffectMean")
ggs_histogram(gg_toj_color_post, family = "logitRhoMean")
ggs_histogram(gg_toj_color_post, family = "logKappaEffectMean")
ggs_histogram(gg_toj_color_post, family =  "logKappaMean" )
ggs_histogram(gg_toj_color_post, family =  "population_logjnd_effect_mean" )
ggs_histogram(gg_toj_color_post, family =  "population_logjnd_convention_effect_mean" )
ggs_histogram(gg_toj_color_post, family =  "population_logjnd_intercept_mean" )
ggs_histogram(gg_toj_color_post, family =  "population_pss_effect_mean" )
ggs_histogram(gg_toj_color_post, family =  "population_pss_convention_effect_mean" )
ggs_histogram(gg_toj_color_post, family =   "population_pss_intercept_mean"  )
ggs_histogram(gg_toj_color_post, family =   "zlogitRhoEffectSD"  )
ggs_histogram(gg_toj_color_post, family =    "zlogitRhoSD"   )
ggs_histogram(gg_toj_color_post, family =    "zlogKappaEffectSD"   )
ggs_histogram(gg_toj_color_post, family =    "zlogKappaSD"    )
ggs_histogram(gg_toj_color_post, family =    "zpopulation_logjnd_effect_sd"    )
ggs_histogram(gg_toj_color_post, family =    "zpopulation_logjnd_intercept_sd"    )
ggs_histogram(gg_toj_color_post, family =    "zpopulation_pss_effect_sd"   )
ggs_histogram(gg_toj_color_post, family =    "zpopulation_pss_intercept_sd"   )

### Posterior by chain
ggs_density(gg_toj_color_post, family = "logitRhoEffectMean")
ggs_density(gg_toj_color_post, family = "logitRhoMean")
ggs_density(gg_toj_color_post, family = "logKappaEffectMean")
ggs_density(gg_toj_color_post, family =  "logKappaMean" )
ggs_density(gg_toj_color_post, family =  "population_logjnd_effect_mean" )
ggs_density(gg_toj_color_post, family =  "population_logjnd_convention_effect_mean" )
ggs_density(gg_toj_color_post, family =  "population_logjnd_intercept_mean" )
ggs_density(gg_toj_color_post, family =  "population_pss_effect_mean" )
ggs_density(gg_toj_color_post, family =  "population_pss_convention_effect_mean" )
ggs_density(gg_toj_color_post, family =   "population_pss_intercept_mean"  )
ggs_density(gg_toj_color_post, family =   "zlogitRhoEffectSD"  )
ggs_density(gg_toj_color_post, family =    "zlogitRhoSD"   )
ggs_density(gg_toj_color_post, family =    "zlogKappaEffectSD"   )
ggs_density(gg_toj_color_post, family =    "zlogKappaSD"    )
ggs_density(gg_toj_color_post, family =    "zpopulation_logjnd_effect_sd"    )
ggs_density(gg_toj_color_post, family =    "zpopulation_logjnd_intercept_sd"    )
ggs_density(gg_toj_color_post, family =    "zpopulation_pss_effect_sd"   )
ggs_density(gg_toj_color_post, family =    "zpopulation_pss_intercept_sd"   )

### Traceplots
ggs_traceplot(gg_toj_color_post, family = "logitRhoEffectMean")
ggs_traceplot(gg_toj_color_post, family = "logitRhoMean")
ggs_traceplot(gg_toj_color_post, family = "logKappaEffectMean")
ggs_traceplot(gg_toj_color_post, family =  "logKappaMean" )
ggs_traceplot(gg_toj_color_post, family =  "population_logjnd_effect_mean" )
ggs_traceplot(gg_toj_color_post, family =  "population_logjnd_convention_effect_mean" )
ggs_traceplot(gg_toj_color_post, family =  "population_logjnd_intercept_mean" )
ggs_traceplot(gg_toj_color_post, family =  "population_pss_effect_mean" )
ggs_traceplot(gg_toj_color_post, family =  "population_pss_convention_effect_mean" )
ggs_traceplot(gg_toj_color_post, family =   "population_pss_intercept_mean"  )
ggs_traceplot(gg_toj_color_post, family =   "zlogitRhoEffectSD"  )
ggs_traceplot(gg_toj_color_post, family =    "zlogitRhoSD"   )
ggs_traceplot(gg_toj_color_post, family =    "zlogKappaEffectSD"   )
ggs_traceplot(gg_toj_color_post, family =    "zlogKappaSD"    )
ggs_traceplot(gg_toj_color_post, family =    "zpopulation_logjnd_effect_sd"    )
ggs_traceplot(gg_toj_color_post, family =    "zpopulation_logjnd_intercept_sd"    )
ggs_traceplot(gg_toj_color_post, family =    "zpopulation_pss_effect_sd"   )
ggs_traceplot(gg_toj_color_post, family =    "zpopulation_pss_intercept_sd"   )

### Running Means
ggs_running(gg_toj_color_post, family = "logitRhoEffectMean")
ggs_running(gg_toj_color_post, family = "logitRhoMean")
ggs_running(gg_toj_color_post, family = "logKappaEffectMean")
ggs_running(gg_toj_color_post, family =  "logKappaMean" )
ggs_running(gg_toj_color_post, family =  "population_logjnd_effect_mean" )
ggs_running(gg_toj_color_post, family =  "population_logjnd_convention_effect_mean" )
ggs_running(gg_toj_color_post, family =  "population_logjnd_intercept_mean" )
ggs_running(gg_toj_color_post, family =  "population_pss_effect_mean" )
ggs_running(gg_toj_color_post, family =  "population_pss_convention_effect_mean" )
ggs_running(gg_toj_color_post, family =   "population_pss_intercept_mean"  )
ggs_running(gg_toj_color_post, family =   "zlogitRhoEffectSD"  )
ggs_running(gg_toj_color_post, family =    "zlogitRhoSD"   )
ggs_running(gg_toj_color_post, family =    "zlogKappaEffectSD"   )
ggs_running(gg_toj_color_post, family =    "zlogKappaSD"    )
ggs_running(gg_toj_color_post, family =    "zpopulation_logjnd_effect_sd"    )
ggs_running(gg_toj_color_post, family =    "zpopulation_logjnd_intercept_sd"    )
ggs_running(gg_toj_color_post, family =    "zpopulation_pss_effect_sd"   )
ggs_running(gg_toj_color_post, family =    "zpopulation_pss_intercept_sd"   )

### Compare complete and last part of chain
ggs_compare_partial(gg_toj_color_post, family = "logitRhoEffectMean")
ggs_compare_partial(gg_toj_color_post, family = "logitRhoMean")
ggs_compare_partial(gg_toj_color_post, family = "logKappaEffectMean")
ggs_compare_partial(gg_toj_color_post, family =  "logKappaMean" )
ggs_compare_partial(gg_toj_color_post, family =  "population_logjnd_effect_mean" )
ggs_compare_partial(gg_toj_color_post, family =  "population_logjnd_convention_effect_mean" )
ggs_compare_partial(gg_toj_color_post, family =  "population_logjnd_intercept_mean" )
ggs_compare_partial(gg_toj_color_post, family =  "population_pss_effect_mean" )
ggs_compare_partial(gg_toj_color_post, family =  "population_pss_convention_effect_mean" )
ggs_compare_partial(gg_toj_color_post, family =   "population_pss_intercept_mean"  )
ggs_compare_partial(gg_toj_color_post, family =   "zlogitRhoEffectSD"  )
ggs_compare_partial(gg_toj_color_post, family =    "zlogitRhoSD"   )
ggs_compare_partial(gg_toj_color_post, family =    "zlogKappaEffectSD"   )
ggs_compare_partial(gg_toj_color_post, family =    "zlogKappaSD"    )
ggs_compare_partial(gg_toj_color_post, family =    "zpopulation_logjnd_effect_sd"    )
ggs_compare_partial(gg_toj_color_post, family =    "zpopulation_logjnd_intercept_sd"    )
ggs_compare_partial(gg_toj_color_post, family =    "zpopulation_pss_effect_sd"   )
ggs_compare_partial(gg_toj_color_post, family =    "zpopulation_pss_intercept_sd"   )

### Autocorrelation
# autocorrelation is not indicative of lack of convergence per se, but is indicative of misbehavior perhaps.
# solution to autocorrelation is thinining.
ggs_autocorrelation(gg_toj_color_post, family = "logitRhoEffectMean")
ggs_autocorrelation(gg_toj_color_post, family = "logitRhoMean")
ggs_autocorrelation(gg_toj_color_post, family = "logKappaEffectMean")
ggs_autocorrelation(gg_toj_color_post, family =  "logKappaMean" )
ggs_autocorrelation(gg_toj_color_post, family =  "population_logjnd_effect_mean" )
ggs_autocorrelation(gg_toj_color_post, family =  "population_logjnd_convention_effect_mean" )
ggs_autocorrelation(gg_toj_color_post, family =  "population_logjnd_intercept_mean" )
ggs_autocorrelation(gg_toj_color_post, family =  "population_pss_effect_mean" )
ggs_autocorrelation(gg_toj_color_post, family =  "population_pss_convention_effect_mean" )
ggs_autocorrelation(gg_toj_color_post, family =   "population_pss_intercept_mean"  )
ggs_autocorrelation(gg_toj_color_post, family =   "zlogitRhoEffectSD"  )
ggs_autocorrelation(gg_toj_color_post, family =    "zlogitRhoSD"   )
ggs_autocorrelation(gg_toj_color_post, family =    "zlogKappaEffectSD"   )
ggs_autocorrelation(gg_toj_color_post, family =    "zlogKappaSD"    )
ggs_autocorrelation(gg_toj_color_post, family =    "zpopulation_logjnd_effect_sd"    )
ggs_autocorrelation(gg_toj_color_post, family =    "zpopulation_logjnd_intercept_sd"    )
ggs_autocorrelation(gg_toj_color_post, family =    "zpopulation_pss_effect_sd"   )
ggs_autocorrelation(gg_toj_color_post, family =    "zpopulation_pss_intercept_sd"   )

### Catepillar
# good for examining correlations
## ORDER:
# (1) population_pss_intercept_mean      
# (2) population_pss_effect_mean          
# (3) population_logjnd_intercept_mean    
# (4) population_logjnd_effect_mean     
# (5) logitRhoMean                         
# (6) logKappaMean                        
# (7) logitRhoEffectMean                 
# (8) logKappaEffectMean                   
# NOTE: not HDIs per se
ggs_caterpillar(gg_toj_color_post, family = "cor") + geom_vline(xintercept = 0, col = "red")



#### Posterior Predictive Checks ####
### TOJ actual data
load("toj_trials.Rdata")
real_toj = aggregate(safe ~ soa2 + base_probe_dist, data = toj_trials, FUN = mean)
real_toj_convention = aggregate(safe ~ soa2 + know_tie_goes_runner, data = toj_trials, FUN = mean)

### TOJ simulated data
# pss intercept mean
pss_intercept_mean = gg_toj_color_post[gg_toj_color_post$Parameter == "population_pss_intercept_mean",]$value
# pss effect mean
pss_effect_mean = gg_toj_color_post[gg_toj_color_post$Parameter == "population_pss_effect_mean",]$value
# pss convention effect mean
pss_convention_effect_mean = gg_toj_color_post[gg_toj_color_post$Parameter == "population_pss_convention_effect_mean",]$value

# pss glove 
pss_glove_mean = (pss_intercept_mean - pss_effect_mean/2) * 250
hist(pss_glove_mean, breaks = 10, col = rgb(.1,.1,.1,.5), freq = F, xlim = c(min(pss_glove_mean), max(pss_glove_mean)))
# sample from posterior  
pss_glove_mean_reps = sample(pss_glove_mean, 50, replace = T)
hist(pss_glove_mean_reps, breaks = 10, col = rgb(.9,.9,.9,.5), freq = F, xlim = c(min(pss_glove_mean), max(pss_glove_mean)), add = T)

# pss base
pss_base_mean = (pss_intercept_mean + pss_effect_mean/2) * 250
hist(pss_base_mean, breaks = 10, col = rgb(.1,.1,.1,.5), freq = F, xlim = c(min(pss_base_mean), max(pss_base_mean)))
# sample from posterior  
pss_base_mean_reps = sample(pss_base_mean, 50, replace = T)
hist(pss_base_mean_reps, breaks = 10, col = rgb(.9,.9,.9,.5), freq = F, xlim = c(min(pss_base_mean), max(pss_base_mean)), add = T)

# pss TRUE convention
pss_know_mean = (pss_intercept_mean - pss_convention_effect_mean/2) * 250
hist(pss_know_mean, breaks = 10, col = rgb(.1,.1,.1,.5), freq = F, xlim = c(min(pss_know_mean), max(pss_know_mean)))
# sample from posterior  
pss_know_mean_reps = sample(pss_know_mean, 50, replace = T)
hist(pss_know_mean_reps, breaks = 10, col = rgb(.9,.9,.9,.5), freq = F, xlim = c(min(pss_know_mean), max(pss_know_mean)), add = T)

# pss FALSE convention
pss_dontknow_mean = (pss_intercept_mean + pss_convention_effect_mean/2) * 250
hist(pss_dontknow_mean, breaks = 10, col = rgb(.1,.1,.1,.5), freq = F, xlim = c(min(pss_dontknow_mean), max(pss_dontknow_mean)))
# sample from posterior  
pss_dontknow_mean_reps = sample(pss_dontknow_mean, 50, replace = T)
hist(pss_dontknow_mean_reps, breaks = 10, col = rgb(.9,.9,.9,.5), freq = F, xlim = c(min(pss_dontknow_mean), max(pss_dontknow_mean)), add = T)


# jnd intercept mean
jnd_intercept_mean = gg_toj_color_post[gg_toj_color_post$Parameter == "population_logjnd_intercept_mean",]$value
# jnd effect mean
jnd_effect_mean = gg_toj_color_post[gg_toj_color_post$Parameter == "population_logjnd_effect_mean",]$value
# jnd convention effect mean
jnd_convention_effect_mean = gg_toj_color_post[gg_toj_color_post$Parameter == "population_logjnd_convention_effect_mean",]$value

# jnd glove
jnd_glove_mean = exp( jnd_intercept_mean - jnd_effect_mean/2 ) * 250
hist(jnd_glove_mean, breaks = 10, col = rgb(.1,.1,.1,.5), freq = F, xlim = c(min(jnd_glove_mean), max(jnd_glove_mean)))
# sample from posterior  
jnd_glove_mean_reps = sample(jnd_glove_mean, 50, replace = T)
hist(jnd_glove_mean_reps, breaks = 10, col = rgb(.9,.9,.9,.5), freq = F, xlim = c(min(jnd_glove_mean), max(jnd_glove_mean)), add=T)

# jnd base
jnd_base_mean = exp( jnd_intercept_mean + jnd_effect_mean/2 ) * 250
hist(jnd_base_mean, breaks = 10, col = rgb(.1,.1,.1,.5), freq = F, xlim = c(min(jnd_base_mean), max(jnd_base_mean)))
# sample from posterior  
jnd_base_mean_reps = sample(jnd_base_mean, 50, replace = T)
hist(jnd_base_mean_reps, breaks = 10, col = rgb(.9,.9,.9,.5), freq = F, xlim = c(min(jnd_base_mean), max(jnd_base_mean)), add=T)

# jnd TRUE convention
jnd_know_mean = exp( jnd_intercept_mean - jnd_convention_effect_mean/2 ) * 250
hist(jnd_know_mean, breaks = 10, col = rgb(.1,.1,.1,.5), freq = F, xlim = c(min(jnd_know_mean), max(jnd_know_mean)))
# sample from posterior  
jnd_know_mean_reps = sample(jnd_know_mean, 50, replace = T)
hist(jnd_know_mean_reps, breaks = 10, col = rgb(.9,.9,.9,.5), freq = F, xlim = c(min(jnd_know_mean), max(jnd_know_mean)), add=T)

# jnd FALSE convention
jnd_dontknow_mean = exp( jnd_intercept_mean + jnd_convention_effect_mean/2 ) * 250
hist(jnd_dontknow_mean, breaks = 10, col = rgb(.1,.1,.1,.5), freq = F, xlim = c(min(jnd_dontknow_mean), max(jnd_dontknow_mean)))
# sample from posterior  
jnd_dontknow_mean_reps = sample(jnd_dontknow_mean, 50, replace = T)
hist(jnd_dontknow_mean_reps, breaks = 10, col = rgb(.9,.9,.9,.5), freq = F, xlim = c(min(jnd_dontknow_mean), max(jnd_dontknow_mean)), add=T)


### draw psychometric functions
SOAs = c(-250, -150, -100, -50, -17, 17, 50, 100, 150, 250)
# base
plot(SOAs, pnorm( SOAs, mean = pss_base_mean_reps[1], sd = jnd_base_mean_reps[1]), main = "attend base", ylab = "safe proportion", xlab = "SOA" , col = alpha("turquoise", 0.5) )
for (i in 2:length(pss_base_mean_reps)) {
  points(SOAs, pnorm( SOAs, mean = pss_base_mean_reps[i], sd = jnd_base_mean_reps[i]), col = alpha("turquoise", 0.5) )
}
# real base
real_base = real_toj[real_toj$base_probe_dist == 0.8,]$safe
points(SOAs,real_base, col = "blue", pch = 15 )

# glove
plot(SOAs, pnorm( SOAs, mean = pss_glove_mean_reps[1], sd = jnd_glove_mean_reps[1]), main = "attend glove", ylab = "safe proportion", xlab = "SOA", col = alpha("pink", 0.5)  )
for (i in 2:length(pss_glove_mean_reps)) {
  points(SOAs, pnorm( SOAs, mean = pss_glove_mean_reps[i], sd = jnd_glove_mean_reps[i]), col = alpha("pink", 0.5)   )
}
# real glove
real_glove = real_toj[real_toj$base_probe_dist == 0.2,]$safe
points(SOAs, real_glove, col = "red", pch = 15)

# TRUE convention
plot(SOAs, pnorm( SOAs, mean = pss_know_mean_reps[1], sd = jnd_know_mean_reps[1]), main = "know convention", ylab = "safe proportion", xlab = "SOA", col = alpha("turquoise", 0.5) )
for (i in 2:length(pss_know_mean_reps)) {
  points(SOAs, pnorm(SOAs, mean = pss_know_mean_reps[i], sd = jnd_know_mean_reps[i]), ylab = "safe proportion", xlab = "SOA", col = alpha("turquoise", 0.5) )
}
# real base
real_know = real_toj_convention[real_toj_convention$know_tie_goes_runner == TRUE,]$safe
points(SOAs,real_know, col = "blue", pch = 15 )

# FALSE convention
plot( SOAs, pnorm( SOAs, mean = pss_dontknow_mean_reps[1], sd = jnd_dontknow_mean_reps[1]), main = "don't know convention", ylab = "safe proportion", xlab = "SOA", col = alpha("pink", 0.5) )
for (i in 2:length(pss_dontknow_mean_reps)) {
  points( SOAs, pnorm( SOAs, mean = pss_dontknow_mean_reps[i], sd = jnd_dontknow_mean_reps[i]), col = alpha("pink", 0.5) )
}
# real base
real_dontknow = real_toj_convention[real_toj_convention$know_tie_goes_runner == FALSE,]$safe
points(SOAs,real_dontknow, col = "red", pch = 15 )


### Color actual data
load("color_trials.Rdata")
hist(color_trials[color_trials$attended == TRUE,]$color_diff_radians, breaks = 30, freq = F, col = rgb(.1,.1,.1,.5))
hist(color_trials[color_trials$attended == FALSE,]$color_diff_radians, breaks = 30, freq = F, col = rgb(.9,.9,.9,.5), add = T)
real_color = aggregate(color_diff_radians ~ attended, data = color_trials, FUN  = mean)

### Color simulated data 
# rho intercept mean
rho_intercept_mean = gg_toj_color_post[gg_toj_color_post$Parameter == "logitRhoMean",]$value
# rho effect mean
rho_effect_mean = gg_toj_color_post[gg_toj_color_post$Parameter == "logitRhoEffectMean",]$value

# rho attend 
rho_attend = plogis(rho_intercept_mean + rho_effect_mean/2)
hist(rho_attend, breaks = 10, freq = F, col = rgb(.1,.1,.1,.5))
# sample from posterior 
rho_attend_reps = sample(rho_attend, 50, replace = T)
hist(rho_attend_reps, breaks = 10, freq = F, col = rgb(.9,.9,.9,.5), add = T)

# rho unattend
rho_unattend = plogis(rho_intercept_mean - rho_effect_mean/2)
hist(rho_unattend, breaks = 10, freq = F, col = rgb(.1,.1,.1,.5))
# sample from posterior 
rho_unattend_reps = sample(rho_unattend, 50, replace = T)
hist(rho_unattend_reps, breaks = 10, freq = F, col = rgb(.9,.9,.9,.5), add = T)

# kappa intercept mean 
kappa_intercept_mean = gg_toj_color_post[gg_toj_color_post$Parameter == "logKappaMean",]$value
# kappa effect mean
kappa_effect_mean = gg_toj_color_post[gg_toj_color_post$Parameter == "logKappaEffectMean",]$value

# kappa attend 
kappa_attend = exp(kappa_intercept_mean + kappa_effect_mean/2)
hist(kappa_attend, breaks = 10, freq = F, col = rgb(.1,.1,.1,.5))
# sample from posterior 
kappa_attend_reps = sample(kappa_attend, 50, replace = T)
hist(kappa_attend_reps, breaks = 10, freq = F, col = rgb(.9,.9,.9,.5), add = T)

# kappa unattend
kappa_unattend = exp(kappa_intercept_mean - kappa_effect_mean/2)
hist(kappa_unattend, breaks = 10, freq = F, col = rgb(.1,.1,.1,.5))
# sample from posterior 
kappa_unattend_reps = sample(kappa_unattend, 50, replace = T)
hist(kappa_unattend_reps, breaks = 10, freq = F, col = rgb(.9,.9,.9,.5), add = T)

### draw from distributions
# look at mixture model
plot( seq(-pi, pi, pi/200), (rho_unattend_reps[1])*dvm(seq(-pi, pi, pi/200), 0, kappa_unattend_reps[1]) 
      + (1-rho_unattend_reps[1]) * dunif(seq(-pi, pi, pi/200), -pi, pi)
      , xlab = "radian deviations", ylab = "density")

# unattend
# sample from distributions
hist( 
  c( rvm(1000*(rho_unattend_reps[1]), pi, kappa_unattend_reps[1]) - pi , runif(1000*(1- rho_unattend_reps[1]), -pi, pi) )
     , breaks = 50, col = rgb(.1,.1,.1,.1), xlim = c(-pi, pi), freq = F, ylim = c(0, 2)
     , xlab = "radian deviations", main = "unattended")
# cycle
for (i in 2:length(kappa_unattend_reps)) {
  hist( 
    c( rvm(1000*(rho_unattend_reps[i]), pi, kappa_unattend_reps[i]) - pi , runif(1000*(1- rho_unattend_reps[i]), -pi, pi) )
    , breaks = 50, col = rgb(.1,.1,.1,.1), xlim = c(-pi, pi), freq = F, add = T)
}
hist(color_trials[color_trials$attended == FALSE,]$color_diff_radians, breaks = 50, freq = F, col = rgb(.9,.9,.9,.5), add = T)

# attend
# sample from distributions
hist( 
  c( rvm(1000*(rho_attend_reps[1]), pi, kappa_attend_reps[1]) - pi , runif(1000*(1- rho_attend_reps[1]), -pi, pi) )
  , breaks = 50, col = rgb(.1,.1,.1,.1), xlim = c(-pi, pi), freq = F, ylim = c(0, 2)
  , xlab = "radian deviations", main = "attended")
# cycle
for (i in 2:length(kappa_attend_reps)) {
  hist( 
    c( rvm(1000*(rho_attend_reps[i]), pi, kappa_attend_reps[i]) - pi , runif(1000*(1- rho_attend_reps[i]), -pi, pi) )
    , breaks = 50, col = rgb(.1,.1,.1,.1), xlim = c(-pi, pi), freq = F, add = T)
}
hist(color_trials[color_trials$attended == TRUE,]$color_diff_radians, breaks = 50, freq = F, col = rgb(.9,.9,.9,.5), add = T)

### plot scatterplot of correlations
# extract samples
detach('package:rstan', unload = T)  # to ensure 
library(rstan)
ex_toj_color_post = extract(toj_color_post)


# ~~~~ posterior check ~~~~
# (1) population_pss_intercept_mean      
# (2) population_pss_effect_mean          
# (3) population_logjnd_intercept_mean    
# (4) population_logjnd_effect_mean     
# (5) logitRhoMean                         
# (6) logKappaMean                        
# (7) logitRhoEffectMean                 
# (8) logKappaEffectMean 
# JND and PSS intercepts
betas2 = data.frame(value = ex_toj_color_post$beta)
betas2$iteration = rownames(betas2)
betas = melt( betas2 )
betas$parameter = rep( c(
  "population_pss_intercept_mean"      
  , "population_pss_effect_mean"          
  , "population_logjnd_intercept_mean"    
  , "population_logjnd_effect_mean"     
  , "logitRhoMean"                       
  , "logKappaMean"                       
  , "logitRhoEffectMean"                 
  , "logKappaEffectMean"
)
, times = 1
, each = nrow(betas2)*length(unique(betas$variable))/8  # 8 is number of parameters 
)  
betas$participant = rep(c(1:44), times = 8, each = nrow(betas2))

pssmean2 = data.frame(value = ex_toj_color_post$population_pss_intercept_mean)
pssmean2$iteration = rownames(pssmean2)
pssmean = melt( pssmean2 )$value
pssmean_reps = sample(pssmean, 50, replace = T)

psssd2 = data.frame(value = tan(ex_toj_color_post$zpopulation_pss_intercept_sd))
psssd2$iteration = rownames(psssd2)
psssd = melt( psssd2 )$value
psssd_reps = sample(psssd, 50, replace = T)

pssconventioneffect2 = data.frame(value = ex_toj_color_post$population_pss_convention_effect_mean)
pssconventioneffect2$iteration = rownames(pssconventioneffect2)
pssconventioneffect = melt( pssconventioneffect2 )$value
pssconventioneffect_reps = sample(pssconventioneffect, 50, replace = T)
conventionfactor = ifelse(aggregate(know_tie_goes_runner~id,data = toj_trials, FUN =unique)$know_tie_goes_runner, -1, 1)

# cycle through each participant 
pss_per_id = ddply(
  .data = betas
  , .variables = .(participant)
  , .fun = function(x){
    i = unique(x$participant)
    x_reps = sample(x[x$parameter == "population_pss_intercept_mean",]$value, 50, replace = TRUE)
    pssintercept = pssmean_reps + pssconventioneffect_reps*conventionfactor[i]/2 + psssd_reps*x_reps
    df = data.frame(pssintercept)*250
    return(df)
  }
)

logjndmean2 = data.frame(value = ex_toj_color_post$population_logjnd_intercept_mean)
logjndmean2$iteration = rownames(logjndmean2)
logjndmean = melt( logjndmean2 )$value
logjndmean_reps = sample(logjndmean, 50, replace = T)

logjndsd2 = data.frame(value = tan(ex_toj_color_post$zpopulation_logjnd_intercept_sd))
logjndsd2$iteration = rownames(logjndsd2)
logjndsd = melt( logjndsd2 )$value
logjndsd_reps = sample(logjndsd, 50, replace = T)

logjndconventioneffect2 = data.frame(value = ex_toj_color_post$population_logjnd_convention_effect_mean)
logjndconventioneffect2$iteration = rownames(logjndconventioneffect2)
logjndconventioneffect = melt( logjndconventioneffect2 )$value
logjndconventioneffect_reps = sample(logjndconventioneffect, 50, replace = T)

conventionfactor = ifelse(aggregate(know_tie_goes_runner~id,data = toj_trials, FUN =unique)$know_tie_goes_runner, -1, 1)

jnd_per_id = ddply(
  .data = betas
  , .variables = .(participant)
  , .fun = function(x){
    i = unique(x$participant)
    x_reps = sample(x[x$parameter == "population_logjnd_intercept_mean",]$value, 50, replace = TRUE)
    logjndintercept = logjndmean_reps + logjndconventioneffect_reps*conventionfactor[i]/2 + logjndsd_reps*x_reps
    df = exp(data.frame(logjndintercept))*250
    names(df) = "jndintercept"
    return(df)
  }
)

# get line of best fit through median values 
jnd_med_per_id = aggregate(jndintercept ~ participant, data = jnd_per_id, FUN = median)$jndintercept
pss_med_per_id = aggregate(pssintercept ~ participant, data = pss_per_id, FUN = median)$pssintercept


### Get REAL PSSs and JNDs
toj_by_condition = ddply(
  .data = toj_trials
  , .variables = .(id)  
  , .fun = function(x){
    fit = glm(
      formula = safe~soa2
      , data = x
      , family = binomial(link = "probit")
    )
    to_return = data.frame(
      id = x$id[1]
      , pss = -coef(fit)[1]/coef(fit)[2]
      , jnd = (  (1-coef(fit)[1])/coef(fit)[2] -  (-1-coef(fit)[1])/coef(fit)[2] ) / 2  # unsure about this
    )
    return(to_return)
  }
)

# plot posterior samples
plot(pss_per_id$pssintercept, jnd_per_id$jndintercept, ylab = "jnd intercepts", xlab = "pss intercepts", col = alpha(pss_per_id$participant, 0.2) , ylim = c(0, 500) )
abline(lm(jnd_med_per_id~pss_med_per_id), lty = 2)
# get correllation
cor(jnd_med_per_id, pss_med_per_id)

# look at JND and PSS intercept scatter plot 
# for attend glove
pss_by_id = toj_by_condition$pss
jnd_by_id = toj_by_condition$jnd
points(
  pss_by_id
  , jnd_by_id
  , col = toj_by_condition$id
  , pch = 20
)
abline(lm(jnd_by_id~pss_by_id))
# get correlation
cor(jnd_by_id, pss_by_id)





#### Analysis ####
# look at parameter distribution estimates  
toj_color_post

# visualize
plot(toj_color_post)


# # quantile functions 
# quantile_95_summary = function(y) {
#   quan = quantile(y, prob = c(0.025, 0.5, 0.975) )
#   min = quan[[1]]
#   med = quan[[2]]
#   max = quan[[3]]
#   return( c( ymin = min, y = med, ymax = max) )
# }
# 
# quantile_50_summary = function(y) {
#   quan = quantile(y, prob = c(0.25, 0.5, 0.75) )
#   min = quan[[1]]
#   med = quan[[2]]
#   max = quan[[3]]
#   return( c( ymin = min, y = med, ymax = max) )
# }

# HDI and mode functions
get_95_HDI = function(y) {
  HDI = HPDinterval( as.mcmc( as.vector(y) ), prob = .95 )
  Den = density( as.vector(y) )
  min = HDI[1]
  # mod = Den$x[which(Den$y == max(Den$y))] # mode as indicator of central tendency
  med = median(y)
  max = HDI[2]
  return( c( ymin = min, y = med, ymax = max) )
}

get_50_HDI = function(y) {
  HDI = HPDinterval( as.mcmc( as.vector(y) ), prob = .50 )
  Den = density( as.vector(y) )
  min = HDI[1]
  # mod = Den$x[which(Den$y == max(Den$y))]  # mode as indicator of central tendency
  med = median(y)
  max = HDI[2]
  return( c( ymin = min, y = med, ymax = max) )
}


#### violin plot for correlations ####
# (1) population_pss_intercept_mean      
# (2) population_pss_effect_mean          
# (3) population_logjnd_intercept_mean    
# (4) population_logjnd_effect_mean     
# (5) logitRhoMean                         
# (6) logKappaMean                        
# (7) logitRhoEffectMean                 
# (8) logKappaEffectMean 
library(reshape2)
pos_corr2 = data.frame(value = ex_toj_color_post$cor)
pos_corr2$id = rownames(pos_corr2)
pos_corr = melt( pos_corr2 )
names(pos_corr)[2] = c("parameter")

# plot
ggplot(data = pos_corr, aes(x = parameter, y = value))+
  geom_violin()+
  labs(x = "", y = "Correlation Coefficient (r)")+
#   stat_summary(fun.data = get_95_HDI, size = 0.5)+
#   stat_summary(fun.data = get_50_HDI, size = 1.5)+  # error in computing stat_summary here...
  geom_hline(yintercept = 0)+
  theme_gray(base_size = 18) 

# get PSS vs. JND intercepts (for RJ)
ggplot(
  data = pos_corr[pos_corr$parameter == "value.1.3",]
  , aes(x = parameter, y = value)
  )+
  geom_violin()+
  labs(x = "Log JND vs. PSS Intercept Means", y = "Correlation Coefficient (r)")+
  stat_summary(fun.data = get_95_HDI, size = 0.7)+
  stat_summary(fun.data = get_50_HDI, size = 2.5)+  
  geom_hline(yintercept = 0, linetype = 2, size = 1)+
    theme_gray(base_size = 24)+
    theme(panel.grid.major = element_line(size = 1.5)
          ,panel.grid.minor = element_line(size = 1) 
          , axis.text.x = element_blank()
          , axis.ticks.x = element_blank()) 

# get PSS effect vs. Rho effect 
ggplot(
  data = pos_corr[pos_corr$parameter == "value.2.7",]
  , aes(x = parameter, y = value)
)+
  geom_violin()+
  labs(x = "Logit \u03C1 vs. PSS Effect Means", y = "Correlation Coefficient (r)")+
  stat_summary(fun.data = get_95_HDI, size = 0.7)+
  stat_summary(fun.data = get_50_HDI, size = 2.5)+  
  geom_hline(yintercept = 0, linetype = 2, size = 1)+
  theme_gray(base_size = 24)+
  theme(panel.grid.major = element_line(size = 1.5)
        ,panel.grid.minor = element_line(size = 1)
        , axis.text.x = element_blank()
        , axis.ticks.x = element_blank()) 

# get PSS intercept vs. JND effect 
ggplot(
  data = pos_corr[pos_corr$parameter == "value.1.4",]
  , aes(x = parameter, y = value)
)+
  geom_violin()+
  labs(x = "Log JND Effect Means vs. PSS Intercept Means", y = "Correlation Coefficient (r)")+
  stat_summary(fun.data = get_95_HDI, size = 0.7)+
  stat_summary(fun.data = get_50_HDI, size = 2.5)+  
  geom_hline(yintercept = 0, linetype = 2, size = 1)+
  theme_gray(base_size = 24)+
  theme(panel.grid.major = element_line(size = 1.5)
        ,panel.grid.minor = element_line(size = 1)
        , axis.text.x = element_blank()
        , axis.ticks.x = element_blank()) 


### scatterplot of correlation
### See robustness of bayes cor parameter to outliers 
# PSS vs. JND intercepts (for RJ explanation)
pss_ids = ddply(
  .data = betas
  , .variables = .(participant)
  , .fun = function(x){
    i = unique(x$participant)
    x_use = x[x$parameter ==  "population_pss_intercept_mean",]$value
    pssintercept = median(pssmean) + median(pssconventioneffect)*conventionfactor[i]/2 + median(psssd)*median(x_use)
    df = data.frame(pssintercept)
    names(df) = "pssintercept"
    return(df)
  }
)

jnd_ids = ddply(
  .data = betas
  , .variables = .(participant)
  , .fun = function(x){
    i = unique(x$participant)
    x_use = x[x$parameter == "population_logjnd_intercept_mean",]$value
    logjndintercept = median(logjndmean) + median(logjndconventioneffect)*conventionfactor[i]/2 + median(logjndsd)*median(x_use)
    df = data.frame(logjndintercept)
    names(df) = "jndintercept"
    return(df)
  }
)

pss_v_jnd = merge(jnd_ids, pss_ids)
# get rid of outliers
pss_v_jnd = pss_v_jnd[pss_v_jnd$jndintercept != max(pss_v_jnd$jndintercept)
                      & pss_v_jnd$pssintercept != max(pss_v_jnd$pssintercept)
                      & pss_v_jnd$jndintercept != min(pss_v_jnd$jndintercept)
                      & pss_v_jnd$pssintercept != min(pss_v_jnd$pssintercept)
                      ,]

ggplot(data = pss_v_jnd, aes(x =pssintercept, y = jndintercept))+
  scale_x_continuous(name = "PSS Intercept Mean (Normalized)")+
  scale_y_continuous(name = "Log JND Intercept Mean (Normalized)")+
  geom_point(size = 2)+
  geom_smooth(method = "lm", se = FALSE, size = 1)+
  theme_gray(base_size = 24)+
  theme(panel.grid.major = element_line(size = 1.5)
        ,panel.grid.minor = element_line(size = 1))

# PSS Intercepts vs. JND Effects (for RJ explanation)
logjndeffect2 = data.frame(value = ex_toj_color_post$population_logjnd_effect_mean) # must devide by two for SD to make sense 
logjndeffect2$iteration = rownames(logjndeffect2)
logjndeffect = melt( logjndeffect2 )$value

logjndeffectsd2 = data.frame(value = tan(ex_toj_color_post$zpopulation_logjnd_effect_sd))
logjndeffectsd2$iteration = rownames(logjndeffectsd2)
logjndeffectsd = melt( logjndeffectsd2 )$value

jndeffect_ids = ddply(
  .data = betas
  , .variables = .(participant)
  , .fun = function(x){
    i = unique(x$participant)
    x_use = x[x$parameter == "population_logjnd_effect_mean",]$value
    logjndeffect = median(logjndeffect)/2 + median(logjndeffectsd)*median(x_use)
    df = data.frame(logjndeffect)
    names(df) = "jndeffect"
    return(df)
  }
)

pss_v_jndeffect = merge(jndeffect_ids, pss_ids)
# get rid of outliers
pss_v_jndeffect = pss_v_jndeffect[pss_v_jndeffect$jndeffect != max(pss_v_jndeffect$jndeffect)
                      & pss_v_jndeffect$pssintercept != max(pss_v_jndeffect$pssintercept)
                      & pss_v_jndeffect$jndeffect != min(pss_v_jndeffect$jndeffect)
                      & pss_v_jndeffect$pssintercept != min(pss_v_jndeffect$pssintercept)
                      ,]

ggplot(data = pss_v_jndeffect, aes(x =pssintercept, y = jndeffect))+
  scale_x_continuous(name = "PSS Intercept Mean (Normalized)")+
  scale_y_continuous(name = "Log JND Effect Mean (Normalized)")+
  geom_point(size = 2)+
  geom_smooth(method = "lm", se = FALSE, size = 1)+
  theme_gray(base_size = 24)+
  theme(panel.grid.major = element_line(size = 1.5)
        ,panel.grid.minor = element_line(size = 1))


# Rho effect ~ PSS effect
psseffect2 = data.frame(value = ex_toj_color_post$population_pss_effect_mean)
psseffect2$iteration = rownames(psseffect2)
psseffect = melt( psseffect2 )$value

psseffectsd2 = data.frame(value = tan(ex_toj_color_post$zpopulation_pss_effect_sd))
psseffectsd2$iteration = rownames(psseffectsd2)
psseffectsd = melt( psseffectsd2 )$value

psseffect_ids = ddply(
  .data = betas
  , .variables = .(participant)
  , .fun = function(x){
    i = unique(x$participant)
    x_use = x[x$parameter ==  "population_pss_effect_mean",]$value
    psseffect = median(psseffect/2)  + median(psseffectsd)*median(x_use)
    df = data.frame(psseffect)
    names(df) = "psseffect"
    return(df)
  }
)

rhoeffect2 = data.frame(value = ex_toj_color_post$logitRhoEffectMean)
rhoeffect2$iteration = rownames(rhoeffect2)
rhoeffect = melt( rhoeffect2 )$value

rhoeffectsd2 = data.frame(value = tan(ex_toj_color_post$zlogitRhoEffectSD))
rhoeffectsd2$iteration = rownames(rhoeffectsd2)
rhoeffectsd = melt( rhoeffectsd2 )$value

rhoeffect_ids = ddply(
  .data = betas
  , .variables = .(participant)
  , .fun = function(x){
    i = unique(x$participant)
    x_use = x[x$parameter == "logitRhoEffectMean",]$value
    logitrhoeffect = median(rhoeffect) + median(rhoeffectsd)*median(x_use)
    df = data.frame(logitrhoeffect)
    names(df) = "logitrhoeffect"
    return(df)
  }
)

psseffect_v_rhoeffect = merge(rhoeffect_ids, psseffect_ids)
# get rid of outliers
psseffect_v_rhoeffect = psseffect_v_rhoeffect[psseffect_v_rhoeffect$logitrhoeffect != max(psseffect_v_rhoeffect$logitrhoeffect)
                      & psseffect_v_rhoeffect$psseffect != max(psseffect_v_rhoeffect$psseffect)
                      & psseffect_v_rhoeffect$logitrhoeffect != min(psseffect_v_rhoeffect$logitrhoeffect)
                      & psseffect_v_rhoeffect$psseffect != min(psseffect_v_rhoeffect$psseffect)
                      ,]

ggplot(data = psseffect_v_rhoeffect, aes(x =psseffect, y = logitrhoeffect))+
  scale_x_continuous(name = "Half PSS Effect Mean (Normalized)")+
  scale_y_continuous(name = "Logit \u03C1 Effect Mean (Normalized, Magnitude)")+
  geom_point(size = 2)+
  geom_smooth(method = "lm", se = FALSE, size = 1)+
  theme_gray(base_size = 24)+
  theme(panel.grid.major = element_line(size = 1.5)
        ,panel.grid.minor = element_line(size = 1))


#### violin plot for key parameters (plot things on same scale) ####
### SOA scale 
pos_SOA_scale = data.frame(  
  value = c(
  ex_toj_color_post$population_pss_intercept_mean * 250
  , exp( ex_toj_color_post$population_logjnd_intercept_mean ) * 250
  )
  , parameter = c(
    rep("PSS Intercept Mean", 80000)  
    , rep("JND Intercept Mean", 80000)  
    )
  # , dummy = rep("dummy", 80000)
)

# plot
ggplot(data = pos_SOA_scale)+
  geom_violin(aes(x = parameter, y = value))+
  # coord_flip()+
  labs(x = "", y = "SOA (ms)")+
  # stat_summary(aes(x = dummy, y = value), fun.data = get_95_HDI, size = 0.5)+
  # stat_summary(aes(x = dummy, y = value), fun.data = get_50_HDI, size = 1.5)+
  stat_summary(aes(x = parameter, y = value), fun.data = get_95_HDI, size = 0.7)+  # a bit thicker
  stat_summary(aes(x = parameter, y = value), fun.data = get_50_HDI, size = 2.5)+
  facet_wrap(~parameter, scales = "free")+
  # scale_x_discrete(labels = "", breaks = c())+
  # geom_hline(yintercept = 0, linetype = 2)+
  theme_gray(base_size = 30)+
  theme(
    panel.grid.major = element_line(size = 1.5)
    , panel.grid.minor = element_line(size = 1)
    , strip.background = element_blank()
    , strip.text.x = element_blank() 
    , axis.ticks.x = element_blank()
    )

# 95% HDIs 
# pss intercept
get_95_HDI(ex_toj_color_post$population_pss_intercept_mean*250)
# get_95_HDI(tan(ex_toj_color_post$zpopulation_pss_intercept_sd)*250)
# jnd intercept 
get_95_HDI(exp( ex_toj_color_post$population_logjnd_intercept_mean ) * 250)
# get_95_HDI(tan(ex_toj_color_post$zpopulation_logjnd_intercept_sd))  # (log scale, normalized)




### SOA scale - effects 
pos_SOA_scale_effects = data.frame(  
  effect = c(
    ( (ex_toj_color_post$population_pss_intercept_mean + ex_toj_color_post$population_pss_effect_mean/2) 
    - (ex_toj_color_post$population_pss_intercept_mean - ex_toj_color_post$population_pss_effect_mean/2) ) * 250
    , ( exp( ex_toj_color_post$population_logjnd_intercept_mean + ex_toj_color_post$population_logjnd_effect_mean/2 )
        - exp( ex_toj_color_post$population_logjnd_intercept_mean - ex_toj_color_post$population_logjnd_effect_mean/2  ) ) * 250 
  )
  , parameter = c(
    rep("PSS Effect Mean", 80000)
    , rep("JND Effect Mean", 80000)
  )
)

ggplot(data = pos_SOA_scale_effects, aes(x = parameter, y = effect))+
  geom_violin()+
  stat_summary(fun.data = get_95_HDI, size = 0.7)+
  stat_summary(fun.data = get_50_HDI, size = 2.5)+
  labs(x = "", y = "SOA (Base - Glove; ms)")+
#  scale_x_discrete(labels = c("PSS Effect Mean", "JND Effect Mean"))+
  geom_hline(yintercept = 0, linetype = 2, size = 1)+
  theme_gray(base_size = 30)+
  theme(panel.grid.major = element_line(size = 1.5)
        ,panel.grid.minor = element_line(size = 1)
        , axis.ticks.x = element_blank()) 

# 95% HDIs
# pss effect 
get_95_HDI(ex_toj_color_post$population_pss_effect_mean*250 )  
# get_95_HDI(tan(ex_toj_color_post$zpopulation_pss_effect_sd)*2*250)  
# print("ERROR: Does it make sense to multiply the SD by 2?")
# jnd effect 
get_95_HDI(
  ( exp( ex_toj_color_post$population_logjnd_intercept_mean + ex_toj_color_post$population_logjnd_effect_mean/2 )
    - exp( ex_toj_color_post$population_logjnd_intercept_mean - ex_toj_color_post$population_logjnd_effect_mean/2  ) ) * 250 
)
# get_95_HDI( tan(ex_toj_color_post$zpopulation_logjnd_effect_sd)*2) #  (log scale, normalized)
# print("ERROR: Does it make sense to multiply the SD by 2?")



### SOA scale - convention effect 
pos_SOA_scale_convention = data.frame(  
  effect = c(
    ( (ex_toj_color_post$population_pss_intercept_mean + ex_toj_color_post$population_pss_convention_effect_mean/2) 
      - (ex_toj_color_post$population_pss_intercept_mean - ex_toj_color_post$population_pss_convention_effect_mean/2) ) * 250
    , ( exp( ex_toj_color_post$population_logjnd_intercept_mean + ex_toj_color_post$population_logjnd_convention_effect_mean/2 )
        - exp( ex_toj_color_post$population_logjnd_intercept_mean - ex_toj_color_post$population_logjnd_convention_effect_mean/2  ) ) * 250 
  )
  , parameter = c(
    rep("PSS Convention Effect Mean", 80000)
    , rep("JND Convention Effect Mean", 80000)
  )
)

ggplot(data = pos_SOA_scale_convention, aes(x = parameter, y = effect))+
  geom_violin()+
  stat_summary(fun.data = get_95_HDI, size = 0.7)+
  stat_summary(fun.data = get_50_HDI, size = 2.5)+
  labs(x = "", y = "SOA (Don't Know - Know; ms)")+
  scale_x_discrete(labels = c("PSS Convention\nEffect Mean", "JND Convention\nEffect Mean"))+
  geom_hline(yintercept = 0, linetype = 2, size = 1)+
  theme_gray(base_size = 30)+
  theme(panel.grid.major = element_line(size = 1.5)
        ,panel.grid.minor = element_line(size = 1)
        , axis.ticks.x = element_blank()) 

# 95% HDIs
# pss effect 
get_95_HDI(ex_toj_color_post$population_pss_convention_effect_mean*250 )  # multiply by two for full difference
# get_95_HDI(tan(ex_toj_color_post$zpopulation_pss_effect_sd)*2*250)  
# print("ERROR: Does it make sense to multiply the SD by 2?")
# jnd effect 
get_95_HDI(
  ( exp( ex_toj_color_post$population_logjnd_intercept_mean + ex_toj_color_post$population_logjnd_convention_effect_mean/2 )
    - exp( ex_toj_color_post$population_logjnd_intercept_mean - ex_toj_color_post$population_logjnd_convention_effect_mean/2  ) ) * 250 
)




### Rho scale
pos_rho_scale = data.frame(  
  value = c(
    plogis(ex_toj_color_post$logitRhoMean)
  )
  , parameter = c(
    rep("rhoInterceptMean", 80000)
  )
)

ggplot(data = pos_rho_scale, aes(x = parameter, y = value))+
  geom_violin()+
  stat_summary(fun.data = get_95_HDI, size = 0.7)+
  stat_summary(fun.data = get_50_HDI, size = 2.5)+
  labs(x = "", y = "\u03C1")+
  scale_x_discrete(labels = c("Probability of Memory Intercept Mean"))+
  theme_gray(base_size = 30)+
  theme(panel.grid.major = element_line(size = 1.5)
        ,panel.grid.minor = element_line(size = 1)
        , axis.ticks.x = element_blank()) 

# 95% HDIs
# rho intercept 
get_95_HDI( plogis(ex_toj_color_post$logitRhoMean) )
# get_95_HDI( tan(ex_toj_color_post$zlogitRhoSD) ) # (logit scale)



### Rho scale - effects
pos_rho_scale_effects = data.frame(  
  effect = c(
    ( plogis(ex_toj_color_post$logitRhoMean + ex_toj_color_post$logitRhoEffectMean/2 )
    - plogis(ex_toj_color_post$logitRhoMean - ex_toj_color_post$logitRhoEffectMean/2 ) )
  )
  , parameter = c(
    rep("rhoEffectMean", 80000)
  )
)

ggplot(data = pos_rho_scale_effects, aes(x = parameter, y = effect))+
  geom_violin()+
  labs(x = "", y = "\u03C1 (Attended - Unattended)")+
  geom_hline(yintercept = 0, linetype = 2, size = 1)+
  stat_summary(fun.data = get_95_HDI, size = 0.7)+
  stat_summary(fun.data = get_50_HDI, size = 2.5)+
  scale_x_discrete(labels = c("Probability of Memory Effect Mean"))+
  theme_gray(base_size = 30)+
  theme(panel.grid.major = element_line(size = 1.5)
        ,panel.grid.minor = element_line(size = 1)
        , axis.ticks.x = element_blank()) 

# 95% HDIs
# rho effect 
get_95_HDI( 
  ( plogis(ex_toj_color_post$logitRhoMean + ex_toj_color_post$logitRhoEffectMean/2 )
              - plogis(ex_toj_color_post$logitRhoMean - ex_toj_color_post$logitRhoEffectMean/2 ) ) 
  ) 
# get_95_HDI( tan(ex_toj_color_post$zlogitRhoEffectSD) )  # (logit scale)



### Kappa scale
# rad2deg <- function(rad) {(rad * 180) / (pi)}

pos_kappa_scale = data.frame(  
  value = c(
    exp( ex_toj_color_post$logKappaMean ) # regular scale ~ kappa; log scale ~ kappa prime;
  )
  , parameter = c(
    rep("kappaInterceptMean", 80000)
  )
)

ggplot(data = pos_kappa_scale, aes(x = parameter, y = value))+
  geom_violin()+
  labs(x = "", y = "\u03BA")+  # are there any units here?
  stat_summary(fun.data = get_95_HDI, size = 0.7)+
  stat_summary(fun.data = get_50_HDI, size = 2.5)+
  scale_x_discrete(labels = c("Fidelity of Memory Intercept Mean"))+
#  scale_y_continuous(limits = c(0,20) ) +
  theme_gray(base_size = 30)+
  theme(panel.grid.major = element_line(size = 1.5)
        ,panel.grid.minor = element_line(size = 1)
        , axis.ticks.x = element_blank())

# 95% HDIs
# kappa intercept (*radians*)
get_95_HDI( exp( ex_toj_color_post$logKappaMean ) )
# get_95_HDI(tan(ex_toj_color_post$zlogKappaSD))  # (log scale)



### Kappa scale - effects
pos_kappa_scale_effects = data.frame(  
  value = c(
    ( exp(ex_toj_color_post$logKappaMean + ex_toj_color_post$logKappaEffectMean/2) 
      - exp(ex_toj_color_post$logKappaMean - ex_toj_color_post$logKappaEffectMean/2) )
  )
  , parameter = c(
    rep("kappaEffectMean", 80000)
  )
)

ggplot(data = pos_kappa_scale_effects, aes(x = parameter, y = value))+
  geom_violin()+
  labs(x = "", y = "\u03BA (Attended - Unattended)")+  # are there any units here?
  geom_hline(yintercept = 0, linetype = 2, size = 1 ) +
  stat_summary(fun.data = get_95_HDI, size = 0.7)+
  stat_summary(fun.data = get_50_HDI, size = 2.5)+
  scale_x_discrete(labels = c("Fidelity of Memory Effect Mean"))+
  theme_gray(base_size = 30)+
  theme(panel.grid.major = element_line(size = 1.5)
        ,panel.grid.minor = element_line(size = 1)
        , axis.ticks.x = element_blank())

# 95% HDIs
# kappa effects (*radians*)
get_95_HDI( 
  exp(ex_toj_color_post$logKappaMean + ex_toj_color_post$logKappaEffectMean/2) 
                     - exp(ex_toj_color_post$logKappaMean - ex_toj_color_post$logKappaEffectMean/2) 
)
# get_95_HDI(tan(ex_toj_color_post$zlogKappaEffectSD)) # (log scale)




# #### TOJ: plot posterior pss means with effects ####
# pos_pssMean_WithEffect = data.frame(
#   c( 
#     ex_toj_color_post$population_pss_intercept_mean + ex_toj_color_post$population_pss_effect_mean/2
#     , ex_toj_color_post$population_pss_intercept_mean - ex_toj_color_post$population_pss_effect_mean/2
#   ) * 250
#   , c(rep("Base",80000), rep("Glove", 80000))
# )
# names(pos_pssMean_WithEffect) = c("pssMean", "Effect")
# 
# # overlapping
# ggplot(pos_pssMean_WithEffect, aes(x = pssMean, ..density.., fill = Effect))+
#   geom_density(data = pos_pssMean_WithEffect[pos_pssMean_WithEffect$Effect == "Base",], alpha = 0.5) + 
#   geom_density(data = pos_pssMean_WithEffect[pos_pssMean_WithEffect$Effect == "Glove",], alpha = 0.5) + 
#   geom_vline(xintercept = 0, linetype = 2)+
#   labs(x = "PSS Population Mean", y = "Density")+
#   theme_gray(base_size = 18)  # recall that negative SOAs are glove first




#### TOJ: recreate NCF using pss intercept mean and jnd intercept mean ####
# including effects 
yGlove = pnorm(
  -250:250
  , mean = ( median(ex_toj_color_post$population_pss_intercept_mean) - median(ex_toj_color_post$population_pss_effect_mean)/2 ) * 250
  , sd = ( exp( median(ex_toj_color_post$population_logjnd_intercept_mean) - median(ex_toj_color_post$population_logjnd_effect_mean)/2 )   ) * 250
)
yBase = pnorm(
  -250:250
  , mean = ( median(ex_toj_color_post$population_pss_intercept_mean) + median(ex_toj_color_post$population_pss_effect_mean)/2 ) * 250
  , sd = ( exp( median(ex_toj_color_post$population_logjnd_intercept_mean) + median(ex_toj_color_post$population_logjnd_effect_mean)/2 )   ) * 250
  
)
df = data.frame(SOA = -250:250, Prop = c(yGlove, yBase), Attend = c(rep("Glove",501), rep("Base", 501)))


gg = ggplot(data = df, aes(y = Prop, x = SOA, colour = Attend))+
  geom_line(size = 1.25)+
  # scale_color_manual("Attend", values = c("red", "blue"))+
  scale_color_hue("Attend", l = c(60, 15), c = c(100, 50), h = c(240, 360) ) +
  labs(x = "SOA (ms)", y = "Proportion of 'Safe' Responses")+
  theme_gray(base_size = 24)+
  theme(panel.grid.major = element_line(size = 1.5)
        ,panel.grid.minor = element_line(size = 1))
  # define text to add
  Text1 = textGrob(label = paste("Out"), gp = gpar(fontsize= 24))
  Text2 = textGrob(label = paste("Safe"), gp = gpar(fontsize= 24)) 
  gg = gg+
  annotation_custom(grob = Text1,  xmin = -200, xmax = -200, ymin = -0.115, ymax = -0.115)+
  annotation_custom(grob = Text2,  xmin = 200, xmax = 200, ymin = -0.115, ymax = -0.115)
  # Code to override clipping
  gg2 <- ggplot_gtable(ggplot_build(gg))
  gg2$layout$clip[gg2$layout$name=="panel"] <- "off"
  grid.draw(gg2)





#### Color: plot posterior means with effects ####
pos_rhoMean_WithEffect = data.frame(
  c( 
    plogis(ex_toj_color_post$logitRhoMean + ex_toj_color_post$logitRhoEffectMean/2)
    , plogis(ex_toj_color_post$logitRhoMean - ex_toj_color_post$logitRhoEffectMean/2)
  ) 
  , c(rep("Attended",80000), rep("Unattended", 80000))
)
names(pos_rhoMean_WithEffect) = c("rhoMean", "Effect")


# overlapping
ggplot(pos_rhoMean_WithEffect, aes(x = rhoMean, ..density.., fill = Effect))+
  geom_density(data = pos_rhoMean_WithEffect[pos_rhoMean_WithEffect$Effect == "Attended",],alpha = 0.5)+
  geom_density(data = pos_rhoMean_WithEffect[pos_rhoMean_WithEffect$Effect == "Unattended",],alpha = 0.5)+
  scale_fill_hue("Effect", l = c(90, 45), c = c(100, 50) ) +
  labs(x = "Probability of Memory Population Mean", y = "Density", colour = "")+
  theme_gray(base_size = 24)+
  theme(panel.grid.major = element_line(size = 1.5)
        ,panel.grid.minor = element_line(size = 1))
