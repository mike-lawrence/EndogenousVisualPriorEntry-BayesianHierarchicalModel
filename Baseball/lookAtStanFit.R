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
ggs_histogram(gg_toj_color_post, family =  "population_logjnd_intercept_mean" )
ggs_histogram(gg_toj_color_post, family =  "population_pss_effect_mean" )
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
ggs_density(gg_toj_color_post, family =  "population_logjnd_intercept_mean" )
ggs_density(gg_toj_color_post, family =  "population_pss_effect_mean" )
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
ggs_traceplot(gg_toj_color_post, family =  "population_logjnd_intercept_mean" )
ggs_traceplot(gg_toj_color_post, family =  "population_pss_effect_mean" )
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
ggs_running(gg_toj_color_post, family =  "population_logjnd_intercept_mean" )
ggs_running(gg_toj_color_post, family =  "population_pss_effect_mean" )
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
ggs_compare_partial(gg_toj_color_post, family =  "population_logjnd_intercept_mean" )
ggs_compare_partial(gg_toj_color_post, family =  "population_pss_effect_mean" )
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
ggs_autocorrelation(gg_toj_color_post, family =  "population_logjnd_intercept_mean" )
ggs_autocorrelation(gg_toj_color_post, family =  "population_pss_effect_mean" )
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
# NOTE: not HDIs per se
ggs_caterpillar(gg_toj_color_post, family = "cor") + geom_vline(xintercept = 0, col = "red")



#### Posterior Predictive Checks ####
### TOJ actual data
load("toj_trials.Rdata")
real_toj = aggregate(safe ~ soa2 + base_probe_dist, data = toj_trials, FUN = mean)

### TOJ simulated data
# pss intercept mean
pss_intercept_mean = gg_toj_color_post[gg_toj_color_post$Parameter == "population_pss_intercept_mean",]$value
# pss effect mean
pss_effect_mean = gg_toj_color_post[gg_toj_color_post$Parameter == "population_pss_effect_mean",]$value

# pss glove 
pss_glove_mean = (pss_intercept_mean - pss_effect_mean) * 250
hist(pss_glove_mean, breaks = 10, col = rgb(.1,.1,.1,.5), freq = F, xlim = c(min(pss_glove_mean), max(pss_glove_mean)))
# sample from posterior  
pss_glove_mean_reps = sample(pss_glove_mean, 50)
hist(pss_glove_mean_reps, breaks = 10, col = rgb(.9,.9,.9,.5), freq = F, xlim = c(min(pss_glove_mean), max(pss_glove_mean)), add = T)

# pss base
pss_base_mean = (pss_intercept_mean + pss_effect_mean) * 250
hist(pss_base_mean, breaks = 10, col = rgb(.1,.1,.1,.5), freq = F, xlim = c(min(pss_base_mean), max(pss_base_mean)))
# sample from posterior  
pss_base_mean_reps = sample(pss_base_mean, 50)
hist(pss_base_mean_reps, breaks = 10, col = rgb(.9,.9,.9,.5), freq = F, xlim = c(min(pss_base_mean), max(pss_base_mean)), add = T)

# jnd intercept mean
jnd_intercept_mean = gg_toj_color_post[gg_toj_color_post$Parameter == "population_logjnd_intercept_mean",]$value
# jnd effect mean
jnd_effect_mean = gg_toj_color_post[gg_toj_color_post$Parameter == "population_logjnd_effect_mean",]$value

# jnd glove
jnd_glove_mean = exp( jnd_intercept_mean - jnd_effect_mean ) * 250
hist(jnd_glove_mean, breaks = 10, col = rgb(.1,.1,.1,.5), freq = F, xlim = c(min(jnd_glove_mean), max(jnd_glove_mean)))
# sample from posterior  
jnd_glove_mean_reps = sample(jnd_glove_mean, 50)
hist(jnd_glove_mean_reps, breaks = 10, col = rgb(.9,.9,.9,.5), freq = F, xlim = c(min(jnd_glove_mean), max(jnd_glove_mean)), add=T)

# jnd base
jnd_base_mean = exp( jnd_intercept_mean + jnd_effect_mean ) * 250
hist(jnd_base_mean, breaks = 10, col = rgb(.1,.1,.1,.5), freq = F, xlim = c(min(jnd_base_mean), max(jnd_base_mean)))
# sample from posterior  
jnd_base_mean_reps = sample(jnd_base_mean, 50)
hist(jnd_base_mean_reps, breaks = 10, col = rgb(.9,.9,.9,.5), freq = F, xlim = c(min(jnd_base_mean), max(jnd_base_mean)), add=T)

### draw psychometric functions
# base
plot( pnorm( c(-250, -150, -100, -50, -17, 17, 50, 100, 150, 250), mean = pss_base_mean_reps[1], sd = jnd_base_mean_reps[1]), main = "safe proportion", col = alpha("turquoise", 0.5) )
for (i in 2:length(pss_base_mean_reps)) {
  points( pnorm( c(-250, -150, -100, -50, -17, 17, 50, 100, 150, 250), mean = pss_base_mean_reps[i], sd = jnd_base_mean_reps[i]), col = alpha("turquoise", 0.5) )
}
# real base
real_base = real_toj[real_toj$base_probe_dist == 0.8,]$safe
points(real_base, col = "blue", pch = 15 )

# glove
plot( pnorm( c(-250, -150, -100, -50, -17, 17, 50, 100, 150, 250), mean = pss_base_mean_reps[1], sd = jnd_base_mean_reps[1]), main = "safe proportion", col = alpha("pink", 0.5)  )
for (i in 2:length(pss_glove_mean_reps)) {
  points( pnorm( c(-250, -150, -100, -50, -17, 17, 50, 100, 150, 250), mean = pss_glove_mean_reps[i], sd = jnd_glove_mean_reps[i]), col = alpha("pink", 0.5)   )
}
# real glove
real_glove = real_toj[real_toj$base_probe_dist == 0.2,]$safe
points(real_base, col = "red", pch = 15)

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
rho_attend_reps = sample(rho_attend, 50)
hist(rho_attend_reps, breaks = 10, freq = F, col = rgb(.9,.9,.9,.5), add = T)

# rho unattend
rho_unattend = plogis(rho_intercept_mean - rho_effect_mean/2)
hist(rho_unattend, breaks = 10, freq = F, col = rgb(.1,.1,.1,.5))
# sample from posterior 
rho_unattend_reps = sample(rho_unattend, 50)
hist(rho_unattend_reps, breaks = 10, freq = F, col = rgb(.9,.9,.9,.5), add = T)

# kappa intercept mean 
kappa_intercept_mean = gg_toj_color_post[gg_toj_color_post$Parameter == "logKappaMean",]$value
# kappa effect mean
kappa_effect_mean = gg_toj_color_post[gg_toj_color_post$Parameter == "logKappaEffectMean",]$value

# kappa attend 
kappa_attend = exp(kappa_intercept_mean + kappa_effect_mean/2)
hist(kappa_attend, breaks = 10, freq = F, col = rgb(.1,.1,.1,.5))
# sample from posterior 
kappa_attend_reps = sample(kappa_attend, 50)
hist(kappa_attend_reps, breaks = 10, freq = F, col = rgb(.9,.9,.9,.5), add = T)

# kappa unattend
kappa_unattend = exp(kappa_intercept_mean - kappa_effect_mean/2)
hist(kappa_unattend, breaks = 10, freq = F, col = rgb(.1,.1,.1,.5))
# sample from posterior 
kappa_unattend_reps = sample(kappa_unattend, 50)
hist(kappa_unattend_reps, breaks = 10, freq = F, col = rgb(.9,.9,.9,.5), add = T)

### draw from distributions
# look at mixture model
plot( (rho_unattend_reps[1])*dvm(seq(-pi, pi, pi/200), 0, kappa_unattend_reps[1]) 
      + (1-rho_unattend_reps[1]) * dunif(seq(-pi, pi, pi/200), -pi, pi) )

# unattend
# sample from distributions
hist( 
  c( rvm(1000*(rho_unattend_reps[1]), pi, kappa_unattend_reps[1]) - pi , runif(1000*(1- rho_unattend_reps[1]), -pi, pi) )
     , breaks = 50, col = rgb(.1,.1,.1,.1), xlim = c(-pi, pi), freq = F, ylim = c(0, 2))
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
  , breaks = 50, col = rgb(.1,.1,.1,.1), xlim = c(-pi, pi), freq = F, ylim = c(0, 2))
# cycle
for (i in 2:length(kappa_attend_reps)) {
  hist( 
    c( rvm(1000*(rho_attend_reps[i]), pi, kappa_attend_reps[i]) - pi , runif(1000*(1- rho_attend_reps[i]), -pi, pi) )
    , breaks = 50, col = rgb(.1,.1,.1,.1), xlim = c(-pi, pi), freq = F, add = T)
}
hist(color_trials[color_trials$attended == TRUE,]$color_diff_radians, breaks = 50, freq = F, col = rgb(.9,.9,.9,.5), add = T)




#### Analysis ####
# look at parameter distribution estimates  
toj_color_post

# visualize
plot(toj_color_post)

# extract samples
library(rstan)
ex_toj_color_post = extract(toj_color_post)

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



#### violin plot for key parameters (plot things on same scale) ####
### SOA scale 
pos_SOA_scale = data.frame(  
  value = c(
  ex_toj_color_post$population_pss_intercept_mean * 250
  , exp( ex_toj_color_post$population_logjnd_intercept_mean ) * 250
  )
  , parameter = c(
    rep("PSS Intercept Mean", 40000)  
    , rep("JND Intercept Mean", 40000)  
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
  theme_gray(base_size = 24)+
  theme(
    panel.grid.major = element_line(size = 1.5)
    , panel.grid.minor = element_line(size = 1)
    , strip.background = element_blank()
    , strip.text.x = element_blank() 
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
    ( (ex_toj_color_post$population_pss_intercept_mean + ex_toj_color_post$population_pss_effect_mean) 
    - (ex_toj_color_post$population_pss_intercept_mean - ex_toj_color_post$population_pss_effect_mean) ) * 250
    , ( exp( ex_toj_color_post$population_logjnd_intercept_mean + ex_toj_color_post$population_logjnd_effect_mean )
        - exp( ex_toj_color_post$population_logjnd_intercept_mean - ex_toj_color_post$population_logjnd_effect_mean  ) ) * 250 
  )
  , parameter = c(
    rep("PSS Effect Mean", 40000)
    , rep("JND Effect Mean", 40000)
  )
)

ggplot(data = pos_SOA_scale_effects, aes(x = parameter, y = effect))+
  geom_violin()+
  stat_summary(fun.data = get_95_HDI, size = 0.5)+
  stat_summary(fun.data = get_50_HDI, size = 2.5)+
  labs(x = "", y = "SOA (Base - Glove; ms)")+
#  scale_x_discrete(labels = c("PSS Effect Mean", "JND Effect Mean"))+
  geom_hline(yintercept = 0, linetype = 2, size = 1)+
  theme_gray(base_size = 24)+
  theme(panel.grid.major = element_line(size = 1.5)
        ,panel.grid.minor = element_line(size = 1)) 

# 95% HDIs
# pss effect 
get_95_HDI(ex_toj_color_post$population_pss_effect_mean*2*250 )  # multiply by two for full difference
# get_95_HDI(tan(ex_toj_color_post$zpopulation_pss_effect_sd)*2*250)  
# print("ERROR: Does it make sense to multiply the SD by 2?")
# jnd effect 
get_95_HDI(
  ( exp( ex_toj_color_post$population_logjnd_intercept_mean + ex_toj_color_post$population_logjnd_effect_mean )
    - exp( ex_toj_color_post$population_logjnd_intercept_mean - ex_toj_color_post$population_logjnd_effect_mean  ) ) * 250 
)
# get_95_HDI( tan(ex_toj_color_post$zpopulation_logjnd_effect_sd)*2) #  (log scale, normalized)
# print("ERROR: Does it make sense to multiply the SD by 2?")


### Rho scale
pos_rho_scale = data.frame(  
  value = c(
    plogis(ex_toj_color_post$logitRhoMean)
  )
  , parameter = c(
    rep("rhoInterceptMean", 40000)
  )
)

ggplot(data = pos_rho_scale, aes(x = parameter, y = value))+
  geom_violin()+
  stat_summary(fun.data = get_95_HDI, size = 0.5)+
  stat_summary(fun.data = get_50_HDI, size = 2.5)+
  labs(x = "", y = "\u03C1")+
  scale_x_discrete(labels = c("Probability of Memory Intercept Mean"))+
  theme_gray(base_size = 24)+
  theme(panel.grid.major = element_line(size = 1.5)
        ,panel.grid.minor = element_line(size = 1)) 

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
    rep("rhoEffectMean", 40000)
  )
)

ggplot(data = pos_rho_scale_effects, aes(x = parameter, y = effect))+
  geom_violin()+
  labs(x = "", y = "\u03C1 (Attended - Unattended)")+
  geom_hline(yintercept = 0, linetype = 2, size = 1)+
  stat_summary(fun.data = get_95_HDI, size = 0.5)+
  stat_summary(fun.data = get_50_HDI, size = 2.5)+
  scale_x_discrete(labels = c("Probability of Memory Effect Mean"))+
  theme_gray(base_size = 24)+
  theme(panel.grid.major = element_line(size = 1.5)
        ,panel.grid.minor = element_line(size = 1)) 

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
    rep("kappaInterceptMean", 40000)
  )
)

ggplot(data = pos_kappa_scale, aes(x = parameter, y = value))+
  geom_violin()+
  labs(x = "", y = "\u03BA")+  # are there any units here?
  stat_summary(fun.data = get_95_HDI, size = 0.5)+
  stat_summary(fun.data = get_50_HDI, size = 2.5)+
  scale_x_discrete(labels = c("Fidelity of Memory Intercept Mean"))+
#  scale_y_continuous(limits = c(0,20) ) +
  theme_gray(base_size = 24)+
  theme(panel.grid.major = element_line(size = 1.5)
        ,panel.grid.minor = element_line(size = 1))

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
    rep("kappaEffectMean", 40000)
  )
)

ggplot(data = pos_kappa_scale_effects, aes(x = parameter, y = value))+
  geom_violin()+
  labs(x = "", y = "\u03BA (Attended - Unattended)")+  # are there any units here?
  geom_hline(yintercept = 0, linetype = 2, size = 1 ) +
  stat_summary(fun.data = get_95_HDI, size = 0.5)+
  stat_summary(fun.data = get_50_HDI, size = 2.5)+
  scale_x_discrete(labels = c("Fidelity of Memory Effect Mean"))+
  theme_gray(base_size = 24)+
  theme(panel.grid.major = element_line(size = 1.5)
        ,panel.grid.minor = element_line(size = 1))

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
#     ex_toj_color_post$population_pss_intercept_mean + ex_toj_color_post$population_pss_effect_mean
#     , ex_toj_color_post$population_pss_intercept_mean - ex_toj_color_post$population_pss_effect_mean
#   ) * 250
#   , c(rep("Base",40000), rep("Glove", 40000))
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
  , mean = ( median(ex_toj_color_post$population_pss_intercept_mean) - median(ex_toj_color_post$population_pss_effect_mean) ) * 250
  , sd = ( exp( median(ex_toj_color_post$population_logjnd_intercept_mean) - median(ex_toj_color_post$population_logjnd_effect_mean) )   ) * 250
)
yBase = pnorm(
  -250:250
  , mean = ( median(ex_toj_color_post$population_pss_intercept_mean) + median(ex_toj_color_post$population_pss_effect_mean) ) * 250
  , sd = ( exp( median(ex_toj_color_post$population_logjnd_intercept_mean) + median(ex_toj_color_post$population_logjnd_effect_mean) )   ) * 250
  
)
df = data.frame(SOA = -250:250, Prop = c(yGlove, yBase), Attend = c(rep("Glove",501), rep("Base", 501)))

# stretch out picture
ggplot(data = df, aes(y = Prop, x = SOA, colour = Attend))+
  geom_line(size = 1.25)+
  # scale_color_manual("Attend", values = c("red", "blue"))+
  scale_color_hue("Attend", l = c(60, 15), c = c(100, 50), h = c(240, 360) ) +
  labs(x = "SOA (ms)", y = "Proportion of 'Safe' Responses")+
  theme_gray(base_size = 24)+
  theme(panel.grid.major = element_line(size = 1.5)
        ,panel.grid.minor = element_line(size = 1))




#### Color: plot posterior means with effects ####
pos_rhoMean_WithEffect = data.frame(
  c( 
    ex_toj_color_post$logitRhoMean + ex_toj_color_post$logitRhoEffectMean/2
    , ex_toj_color_post$logitRhoMean - ex_toj_color_post$logitRhoEffectMean/2
  ) 
  , c(rep("Attended",40000), rep("Unattended", 40000))
)
names(pos_rhoMean_WithEffect) = c("rhoMean", "Effect")


# overlapping
ggplot(pos_rhoMean_WithEffect, aes(x = rhoMean, ..density.., fill = Effect))+
  geom_density(data = pos_rhoMean_WithEffect[pos_rhoMean_WithEffect$Effect == "Attended",],alpha = 0.5)+
  geom_density(data = pos_rhoMean_WithEffect[pos_rhoMean_WithEffect$Effect == "Unattended",],alpha = 0.5)+
  scale_fill_hue("Effect", l = c(90, 45), c = c(100, 50) ) +
  labs(x = "Probability of Memory Population Mean (Logit)", y = "Density", colour = "")+
  theme_gray(base_size = 24)+
  theme(panel.grid.major = element_line(size = 1.5)
        ,panel.grid.minor = element_line(size = 1))
