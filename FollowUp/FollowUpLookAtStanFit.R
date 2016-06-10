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
# ggs_histogram(gg_toj_color_post, family =  "population_logjnd_effect_mean" )
ggs_histogram(gg_toj_color_post, family =  "population_logjnd_intercept_mean" )
# ggs_histogram(gg_toj_color_post, family =  "population_pss_effect_mean" )
ggs_histogram(gg_toj_color_post, family =   "population_pss_intercept_mean"  )
ggs_histogram(gg_toj_color_post, family =   "zlogitRhoEffectSD"  )
ggs_histogram(gg_toj_color_post, family =    "zlogitRhoSD"   )
ggs_histogram(gg_toj_color_post, family =    "zlogKappaEffectSD"   )
ggs_histogram(gg_toj_color_post, family =    "zlogKappaSD"    )
# ggs_histogram(gg_toj_color_post, family =    "zpopulation_logjnd_effect_sd"    )
ggs_histogram(gg_toj_color_post, family =    "zpopulation_logjnd_intercept_sd"    )
# ggs_histogram(gg_toj_color_post, family =    "zpopulation_pss_effect_sd"   )
ggs_histogram(gg_toj_color_post, family =    "zpopulation_pss_intercept_sd"   )

ggs_histogram(gg_toj_color_post, family =  "population_logjnd_initial_bias_effect_mean" )
ggs_histogram(gg_toj_color_post, family =  "population_logjnd_judgement_type_effect_mean" )
ggs_histogram(gg_toj_color_post, family =  "population_logjnd_probe_effect_mean" )
ggs_histogram(gg_toj_color_post, family =  "population_pss_initial_bias_effect_mean" )
ggs_histogram(gg_toj_color_post, family =  "population_pss_judgement_type_effect_mean" )
ggs_histogram(gg_toj_color_post, family =  "population_pss_probe_effect_mean" )
ggs_histogram(gg_toj_color_post, family = "logitRhoProbeEffectMean")
ggs_histogram(gg_toj_color_post, family = "logKappaProbeEffectMean")
ggs_histogram(gg_toj_color_post, family = "logitRhoProbeInteractionEffectMean")
ggs_histogram(gg_toj_color_post, family = "logKappaProbeInteractionEffectMean")

### Posterior by chain
ggs_density(gg_toj_color_post, family = "logitRhoEffectMean")
ggs_density(gg_toj_color_post, family = "logitRhoMean")
ggs_density(gg_toj_color_post, family = "logKappaEffectMean")
ggs_density(gg_toj_color_post, family =  "logKappaMean" )
# ggs_density(gg_toj_color_post, family =  "population_logjnd_effect_mean" )
ggs_density(gg_toj_color_post, family =  "population_logjnd_intercept_mean" )
# ggs_density(gg_toj_color_post, family =  "population_pss_effect_mean" )
ggs_density(gg_toj_color_post, family =   "population_pss_intercept_mean"  )
ggs_density(gg_toj_color_post, family =   "zlogitRhoEffectSD"  )
ggs_density(gg_toj_color_post, family =    "zlogitRhoSD"   )
ggs_density(gg_toj_color_post, family =    "zlogKappaEffectSD"   )
ggs_density(gg_toj_color_post, family =    "zlogKappaSD"    )
# ggs_density(gg_toj_color_post, family =    "zpopulation_logjnd_effect_sd"    )
ggs_density(gg_toj_color_post, family =    "zpopulation_logjnd_intercept_sd"    )
# ggs_density(gg_toj_color_post, family =    "zpopulation_pss_effect_sd"   )
ggs_density(gg_toj_color_post, family =    "zpopulation_pss_intercept_sd"   )

ggs_density(gg_toj_color_post, family =  "population_logjnd_initial_bias_effect_mean" )
ggs_density(gg_toj_color_post, family =  "population_logjnd_judgement_type_effect_mean" )
ggs_density(gg_toj_color_post, family =  "population_logjnd_probe_effect_mean" )
ggs_density(gg_toj_color_post, family =  "population_pss_initial_bias_effect_mean" )
ggs_density(gg_toj_color_post, family =  "population_pss_judgement_type_effect_mean" )
ggs_density(gg_toj_color_post, family =  "population_pss_probe_effect_mean" )
ggs_density(gg_toj_color_post, family = "logitRhoProbeEffectMean")
ggs_density(gg_toj_color_post, family = "logKappaProbeEffectMean")
ggs_density(gg_toj_color_post, family = "logitRhoProbeInteractionEffectMean")
ggs_density(gg_toj_color_post, family = "logKappaProbeInteractionEffectMean")

### Traceplots
ggs_traceplot(gg_toj_color_post, family = "logitRhoEffectMean")
ggs_traceplot(gg_toj_color_post, family = "logitRhoMean")
ggs_traceplot(gg_toj_color_post, family = "logKappaEffectMean")
ggs_traceplot(gg_toj_color_post, family =  "logKappaMean" )
# ggs_traceplot(gg_toj_color_post, family =  "population_logjnd_effect_mean" )
ggs_traceplot(gg_toj_color_post, family =  "population_logjnd_intercept_mean" )
# ggs_traceplot(gg_toj_color_post, family =  "population_pss_effect_mean" )
ggs_traceplot(gg_toj_color_post, family =   "population_pss_intercept_mean"  )
ggs_traceplot(gg_toj_color_post, family =   "zlogitRhoEffectSD"  )
ggs_traceplot(gg_toj_color_post, family =    "zlogitRhoSD"   )
ggs_traceplot(gg_toj_color_post, family =    "zlogKappaEffectSD"   )
ggs_traceplot(gg_toj_color_post, family =    "zlogKappaSD"    )
# ggs_traceplot(gg_toj_color_post, family =    "zpopulation_logjnd_effect_sd"    )
ggs_traceplot(gg_toj_color_post, family =    "zpopulation_logjnd_intercept_sd"    )
# ggs_traceplot(gg_toj_color_post, family =    "zpopulation_pss_effect_sd"   )
ggs_traceplot(gg_toj_color_post, family =    "zpopulation_pss_intercept_sd"   )

ggs_traceplot(gg_toj_color_post, family =  "population_logjnd_initial_bias_effect_mean" )
ggs_traceplot(gg_toj_color_post, family =  "population_logjnd_judgement_type_effect_mean" )
ggs_traceplot(gg_toj_color_post, family =  "population_logjnd_probe_effect_mean" )
ggs_traceplot(gg_toj_color_post, family =  "population_pss_initial_bias_effect_mean" )
ggs_traceplot(gg_toj_color_post, family =  "population_pss_judgement_type_effect_mean" )
ggs_traceplot(gg_toj_color_post, family =  "population_pss_probe_effect_mean" )
ggs_traceplot(gg_toj_color_post, family = "logitRhoProbeEffectMean")
ggs_traceplot(gg_toj_color_post, family = "logKappaProbeEffectMean")
ggs_traceplot(gg_toj_color_post, family = "logitRhoProbeInteractionEffectMean")
ggs_traceplot(gg_toj_color_post, family = "logKappaProbeInteractionEffectMean")

### Running Means
ggs_running(gg_toj_color_post, family = "logitRhoEffectMean")
ggs_running(gg_toj_color_post, family = "logitRhoMean")
ggs_running(gg_toj_color_post, family = "logKappaEffectMean")
ggs_running(gg_toj_color_post, family =  "logKappaMean" )
# ggs_running(gg_toj_color_post, family =  "population_logjnd_effect_mean" )
ggs_running(gg_toj_color_post, family =  "population_logjnd_intercept_mean" )
# ggs_running(gg_toj_color_post, family =  "population_pss_effect_mean" )
ggs_running(gg_toj_color_post, family =   "population_pss_intercept_mean"  )
ggs_running(gg_toj_color_post, family =   "zlogitRhoEffectSD"  )
ggs_running(gg_toj_color_post, family =    "zlogitRhoSD"   )
ggs_running(gg_toj_color_post, family =    "zlogKappaEffectSD"   )
ggs_running(gg_toj_color_post, family =    "zlogKappaSD"    )
# ggs_running(gg_toj_color_post, family =    "zpopulation_logjnd_effect_sd"    )
ggs_running(gg_toj_color_post, family =    "zpopulation_logjnd_intercept_sd"    )
# ggs_running(gg_toj_color_post, family =    "zpopulation_pss_effect_sd"   )
ggs_running(gg_toj_color_post, family =    "zpopulation_pss_intercept_sd"   )

ggs_running(gg_toj_color_post, family =  "population_logjnd_initial_bias_effect_mean" )
ggs_running(gg_toj_color_post, family =  "population_logjnd_judgement_type_effect_mean" )
ggs_running(gg_toj_color_post, family =  "population_logjnd_probe_effect_mean" )
ggs_running(gg_toj_color_post, family =  "population_pss_initial_bias_effect_mean" )
ggs_running(gg_toj_color_post, family =  "population_pss_judgement_type_effect_mean" )
ggs_running(gg_toj_color_post, family =  "population_pss_probe_effect_mean" )
ggs_running(gg_toj_color_post, family = "logitRhoProbeEffectMean")
ggs_running(gg_toj_color_post, family = "logKappaProbeEffectMean")
ggs_running(gg_toj_color_post, family = "logitRhoProbeInteractionEffectMean")
ggs_running(gg_toj_color_post, family = "logKappaProbeInteractionEffectMean")


### Compare complete and last part of chain
ggs_compare_partial(gg_toj_color_post, family = "logitRhoEffectMean")
ggs_compare_partial(gg_toj_color_post, family = "logitRhoMean")
ggs_compare_partial(gg_toj_color_post, family = "logKappaEffectMean")
ggs_compare_partial(gg_toj_color_post, family =  "logKappaMean" )
# ggs_compare_partial(gg_toj_color_post, family =  "population_logjnd_effect_mean" )
ggs_compare_partial(gg_toj_color_post, family =  "population_logjnd_intercept_mean" )
# ggs_compare_partial(gg_toj_color_post, family =  "population_pss_effect_mean" )
ggs_compare_partial(gg_toj_color_post, family =   "population_pss_intercept_mean"  )
ggs_compare_partial(gg_toj_color_post, family =   "zlogitRhoEffectSD"  )
ggs_compare_partial(gg_toj_color_post, family =    "zlogitRhoSD"   )
ggs_compare_partial(gg_toj_color_post, family =    "zlogKappaEffectSD"   )
ggs_compare_partial(gg_toj_color_post, family =    "zlogKappaSD"    )
# ggs_compare_partial(gg_toj_color_post, family =    "zpopulation_logjnd_effect_sd"    )
ggs_compare_partial(gg_toj_color_post, family =    "zpopulation_logjnd_intercept_sd"    )
# ggs_compare_partial(gg_toj_color_post, family =    "zpopulation_pss_effect_sd"   )
ggs_compare_partial(gg_toj_color_post, family =    "zpopulation_pss_intercept_sd"   )

ggs_compare_partial(gg_toj_color_post, family =  "population_logjnd_initial_bias_effect_mean" )
ggs_compare_partial(gg_toj_color_post, family =  "population_logjnd_judgement_type_effect_mean" )
ggs_compare_partial(gg_toj_color_post, family =  "population_logjnd_probe_effect_mean" )
ggs_compare_partial(gg_toj_color_post, family =  "population_pss_initial_bias_effect_mean" )
ggs_compare_partial(gg_toj_color_post, family =  "population_pss_judgement_type_effect_mean" )
ggs_compare_partial(gg_toj_color_post, family =  "population_pss_probe_effect_mean" )
ggs_compare_partial(gg_toj_color_post, family = "logitRhoProbeEffectMean")
ggs_compare_partial(gg_toj_color_post, family = "logKappaProbeEffectMean")
ggs_compare_partial(gg_toj_color_post, family = "logitRhoProbeInteractionEffectMean")
ggs_compare_partial(gg_toj_color_post, family = "logKappaProbeInteractionEffectMean")

### Autocorrelation
# autocorrelation is not indicative of lack of convergence per se, but is indicative of misbehavior perhaps.
# solution to autocorrelation is thinining.
ggs_autocorrelation(gg_toj_color_post, family = "logitRhoEffectMean")
ggs_autocorrelation(gg_toj_color_post, family = "logitRhoMean")
ggs_autocorrelation(gg_toj_color_post, family = "logKappaEffectMean")
ggs_autocorrelation(gg_toj_color_post, family =  "logKappaMean" )
# ggs_autocorrelation(gg_toj_color_post, family =  "population_logjnd_effect_mean" )
ggs_autocorrelation(gg_toj_color_post, family =  "population_logjnd_intercept_mean" )
# ggs_autocorrelation(gg_toj_color_post, family =  "population_pss_effect_mean" )
ggs_autocorrelation(gg_toj_color_post, family =   "population_pss_intercept_mean"  )
ggs_autocorrelation(gg_toj_color_post, family =   "zlogitRhoEffectSD"  )
ggs_autocorrelation(gg_toj_color_post, family =    "zlogitRhoSD"   )
ggs_autocorrelation(gg_toj_color_post, family =    "zlogKappaEffectSD"   )
ggs_autocorrelation(gg_toj_color_post, family =    "zlogKappaSD"    )
# ggs_autocorrelation(gg_toj_color_post, family =    "zpopulation_logjnd_effect_sd"    )
ggs_autocorrelation(gg_toj_color_post, family =    "zpopulation_logjnd_intercept_sd"    )
# ggs_autocorrelation(gg_toj_color_post, family =    "zpopulation_pss_effect_sd"   )
ggs_autocorrelation(gg_toj_color_post, family =    "zpopulation_pss_intercept_sd"   )

ggs_autocorrelation(gg_toj_color_post, family =  "population_logjnd_initial_bias_effect_mean" )
ggs_autocorrelation(gg_toj_color_post, family =  "population_logjnd_judgement_type_effect_mean" )
ggs_autocorrelation(gg_toj_color_post, family =  "population_logjnd_probe_effect_mean" )
ggs_autocorrelation(gg_toj_color_post, family =  "population_pss_initial_bias_effect_mean" )
ggs_autocorrelation(gg_toj_color_post, family =  "population_pss_judgement_type_effect_mean" )
ggs_autocorrelation(gg_toj_color_post, family =  "population_pss_probe_effect_mean" )
ggs_autocorrelation(gg_toj_color_post, family = "logitRhoProbeEffectMean")
ggs_autocorrelation(gg_toj_color_post, family = "logKappaProbeEffectMean")
ggs_autocorrelation(gg_toj_color_post, family = "logitRhoProbeInteractionEffectMean")
ggs_autocorrelation(gg_toj_color_post, family = "logKappaProbeInteractionEffectMean")

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
load("FollowUp_toj_trials.Rdata")
real_toj = aggregate(left_first_TF ~ soa2 + block_bias, data = toj_trials, FUN = mean)
real_toj_judgement_type = aggregate(left_first_TF ~ soa2 + toj_judgement_type, data = toj_trials, FUN = mean)
real_toj_initial_bias = aggregate(left_first_TF ~ soa2 +  probe_initial_bias, data = toj_trials, FUN = mean)
real_toj_probe_duration = aggregate(left_first_TF ~ soa2 +  onehundredms, data = toj_trials, FUN = mean)

### TOJ simulated data
# pss intercept mean
pss_intercept_mean = gg_toj_color_post[gg_toj_color_post$Parameter == "population_pss_intercept_mean",]$value
# # pss effect mean
# pss_effect_mean = gg_toj_color_post[gg_toj_color_post$Parameter == "population_pss_effect_mean",]$value
# pss judgement type effect mean
pss_judgement_type_effect_mean = gg_toj_color_post[gg_toj_color_post$Parameter == "population_pss_judgement_type_effect_mean",]$value
# pss probe initial bias effect mean
pss_initial_bias_effect_mean = gg_toj_color_post[gg_toj_color_post$Parameter == "population_pss_initial_bias_effect_mean",]$value
# pss probe duration effect mean
pss_probe_duration_effect_mean = gg_toj_color_post[gg_toj_color_post$Parameter == "population_pss_probe_effect_mean",]$value


# pss attend RIGHT
pss_right_mean = (pss_intercept_mean - pss_effect_mean/2) * 250
hist(pss_right_mean, breaks = 10, col = rgb(.1,.1,.1,.5), freq = F, xlim = c(min(pss_right_mean), max(pss_right_mean)))
# sample from posterior  
pss_right_mean_reps = sample(pss_right_mean, 50, replace = T)
hist(pss_right_mean_reps, breaks = 10, col = rgb(.9,.9,.9,.5), freq = F, xlim = c(min(pss_right_mean), max(pss_right_mean)), add = T)

# pss attend LEFT
pss_left_mean = (pss_intercept_mean + pss_effect_mean/2) * 250
hist(pss_left_mean, breaks = 10, col = rgb(.1,.1,.1,.5), freq = F, xlim = c(min(pss_left_mean), max(pss_left_mean)))
# sample from posterior  
pss_left_mean_reps = sample(pss_left_mean, 50, replace = T)
hist(pss_left_mean_reps, breaks = 10, col = rgb(.9,.9,.9,.5), freq = F, xlim = c(min(pss_left_mean), max(pss_left_mean)), add = T)

# pss judgement type FIRST 
pss_first_mean = (pss_intercept_mean - pss_judgement_type_effect_mean/2) * 250
hist(pss_first_mean, breaks = 10, col = rgb(.1,.1,.1,.5), freq = F, xlim = c(min(pss_first_mean), max(pss_first_mean)))
# sample from posterior  
pss_first_mean_reps = sample(pss_first_mean, 50, replace = T)
hist(pss_first_mean_reps, breaks = 10, col = rgb(.9,.9,.9,.5), freq = F, xlim = c(min(pss_first_mean), max(pss_first_mean)), add = T)

# pss judgement type SECOND
pss_second_mean = (pss_intercept_mean + pss_judgement_type_effect_mean/2) * 250
hist(pss_second_mean, breaks = 10, col = rgb(.1,.1,.1,.5), freq = F, xlim = c(min(pss_second_mean), max(pss_second_mean)))
# sample from posterior  
pss_second_mean_reps = sample(pss_second_mean, 50, replace = T)
hist(pss_second_mean_reps, breaks = 10, col = rgb(.9,.9,.9,.5), freq = F, xlim = c(min(pss_second_mean), max(pss_second_mean)), add = T)

# pss initial bias RIGHT
pss_initial_right_mean = (pss_intercept_mean - pss_initial_bias_effect_mean/2) * 250
hist(pss_initial_right_mean, breaks = 10, col = rgb(.1,.1,.1,.5), freq = F, xlim = c(min(pss_initial_right_mean), max(pss_initial_right_mean)))
# sample from posterior  
pss_initial_right_mean_reps = sample(pss_initial_right_mean, 50, replace = T)
hist(pss_initial_right_mean_reps, breaks = 10, col = rgb(.9,.9,.9,.5), freq = F, xlim = c(min(pss_initial_right_mean), max(pss_initial_right_mean)), add = T)

# pss initial bias SECOND
pss_initial_left_mean = (pss_intercept_mean + pss_initial_bias_effect_mean/2) * 250
hist(pss_initial_left_mean, breaks = 10, col = rgb(.1,.1,.1,.5), freq = F, xlim = c(min(pss_initial_left_mean), max(pss_initial_left_mean)))
# sample from posterior  
pss_initial_left_mean_reps = sample(pss_initial_left_mean, 50, replace = T)
hist(pss_initial_left_mean_reps, breaks = 10, col = rgb(.9,.9,.9,.5), freq = F, xlim = c(min(pss_initial_left_mean), max(pss_initial_left_mean)), add = T)

# pss probe duration short (100 ms)
pss_short_mean = (pss_intercept_mean - pss_probe_duration_effect_mean/2) * 250
hist(pss_short_mean, breaks = 10, col = rgb(.1,.1,.1,.5), freq = F, xlim = c(min(pss_short_mean), max(pss_short_mean)))
# sample from posterior  
pss_short_mean_reps = sample(pss_short_mean, 50, replace = T)
hist(pss_short_mean_reps, breaks = 10, col = rgb(.9,.9,.9,.5), freq = F, xlim = c(min(pss_short_mean), max(pss_short_mean)), add = T)

# pss probe duration long (200 ms)
pss_long_mean = (pss_intercept_mean + pss_probe_duration_effect_mean/2) * 250
hist(pss_long_mean, breaks = 10, col = rgb(.1,.1,.1,.5), freq = F, xlim = c(min(pss_long_mean), max(pss_long_mean)))
# sample from posterior  
pss_long_mean_reps = sample(pss_long_mean, 50, replace = T)
hist(pss_long_mean_reps, breaks = 10, col = rgb(.9,.9,.9,.5), freq = F, xlim = c(min(pss_long_mean), max(pss_long_mean)), add = T)


# jnd intercept mean
jnd_intercept_mean = gg_toj_color_post[gg_toj_color_post$Parameter == "population_logjnd_intercept_mean",]$value
# # jnd effect mean
# jnd_effect_mean = gg_toj_color_post[gg_toj_color_post$Parameter == "population_logjnd_effect_mean",]$value
# pss judgement type effect mean
jnd_judgement_type_effect_mean = gg_toj_color_post[gg_toj_color_post$Parameter == "population_logjnd_judgement_type_effect_mean",]$value
# jnd probe initial bias effect mean
jnd_initial_bias_effect_mean = gg_toj_color_post[gg_toj_color_post$Parameter == "population_logjnd_initial_bias_effect_mean",]$value
# jnd probe duration effect mean
jnd_probe_duration_effect_mean = gg_toj_color_post[gg_toj_color_post$Parameter == "population_logjnd_probe_effect_mean",]$value


# jnd attend RIGHT
jnd_right_mean = exp( jnd_intercept_mean - jnd_effect_mean/2 ) * 250
hist(jnd_right_mean, breaks = 10, col = rgb(.1,.1,.1,.5), freq = F, xlim = c(min(jnd_right_mean), max(jnd_right_mean)))
# sample from posterior  
jnd_right_mean_reps = sample(jnd_right_mean, 50, replace = T)
hist(jnd_right_mean_reps, breaks = 10, col = rgb(.9,.9,.9,.5), freq = F, xlim = c(min(jnd_right_mean), max(jnd_right_mean)), add=T)

# jnd attend LEFT
jnd_left_mean = exp( jnd_intercept_mean + jnd_effect_mean/2 ) * 250
hist(jnd_left_mean, breaks = 10, col = rgb(.1,.1,.1,.5), freq = F, xlim = c(min(jnd_left_mean), max(jnd_left_mean)))
# sample from posterior  
jnd_left_mean_reps = sample(jnd_left_mean, 50, replace = T)
hist(jnd_left_mean_reps, breaks = 10, col = rgb(.9,.9,.9,.5), freq = F, xlim = c(min(jnd_left_mean), max(jnd_left_mean)), add=T)

# jnd judgement type FIRST
jnd_first_mean = exp( jnd_intercept_mean - jnd_judgement_type_effect_mean/2 ) * 250
hist(jnd_first_mean, breaks = 10, col = rgb(.1,.1,.1,.5), freq = F, xlim = c(min(jnd_first_mean), max(jnd_first_mean)))
# sample from posterior  
jnd_first_mean_reps = sample(jnd_first_mean, 50, replace = T)
hist(jnd_first_mean_reps, breaks = 10, col = rgb(.9,.9,.9,.5), freq = F, xlim = c(min(jnd_first_mean), max(jnd_first_mean)), add=T)

# jnd judgement type SECOND
jnd_second_mean = exp( jnd_intercept_mean + jnd_judgement_type_effect_mean/2 ) * 250
hist(jnd_second_mean, breaks = 10, col = rgb(.1,.1,.1,.5), freq = F, xlim = c(min(jnd_second_mean), max(jnd_second_mean)))
# sample from posterior  
jnd_second_mean_reps = sample(jnd_second_mean, 50, replace = T)
hist(jnd_second_mean_reps, breaks = 10, col = rgb(.9,.9,.9,.5), freq = F, xlim = c(min(jnd_second_mean), max(jnd_second_mean)), add=T)

# jnd initial bias RIGHT
jnd_initial_right_mean = exp( jnd_intercept_mean - jnd_initial_bias_effect_mean/2 ) * 250
hist(jnd_initial_right_mean, breaks = 10, col = rgb(.1,.1,.1,.5), freq = F, xlim = c(min(jnd_initial_right_mean), max(jnd_initial_right_mean)))
# sample from posterior  
jnd_initial_right_mean_reps = sample(jnd_initial_right_mean, 50, replace = T)
hist(jnd_initial_right_mean_reps, breaks = 10, col = rgb(.9,.9,.9,.5), freq = F, xlim = c(min(jnd_initial_right_mean), max(jnd_initial_right_mean)), add=T)

# jnd initial bias LEFT
jnd_initial_left_mean = exp( jnd_intercept_mean + jnd_initial_bias_effect_mean/2 ) * 250
hist(jnd_initial_left_mean, breaks = 10, col = rgb(.1,.1,.1,.5), freq = F, xlim = c(min(jnd_initial_left_mean), max(jnd_initial_left_mean)))
# sample from posterior  
jnd_initial_left_mean_reps = sample(jnd_initial_left_mean, 50, replace = T)
hist(jnd_initial_left_mean_reps, breaks = 10, col = rgb(.9,.9,.9,.5), freq = F, xlim = c(min(jnd_initial_left_mean), max(jnd_initial_left_mean)), add=T)

# jnd probe duration short (100 ms)
jnd_short_mean = exp( jnd_intercept_mean - jnd_probe_duration_effect_mean/2 ) * 250
hist(jnd_short_mean, breaks = 10, col = rgb(.1,.1,.1,.5), freq = F, xlim = c(min(jnd_short_mean), max(jnd_short_mean)))
# sample from posterior  
jnd_short_mean_reps = sample(jnd_short_mean, 50, replace = T)
hist(jnd_short_mean_reps, breaks = 10, col = rgb(.9,.9,.9,.5), freq = F, xlim = c(min(jnd_short_mean), max(jnd_short_mean)), add=T)

# jnd probe duration long (200 ms)
jnd_long_mean = exp( jnd_intercept_mean + jnd_probe_duration_effect_mean/2 ) * 250
hist(jnd_long_mean, breaks = 10, col = rgb(.1,.1,.1,.5), freq = F, xlim = c(min(jnd_long_mean), max(jnd_long_mean)))
# sample from posterior  
jnd_long_mean_reps = sample(jnd_long_mean, 50, replace = T)
hist(jnd_long_mean_reps, breaks = 10, col = rgb(.9,.9,.9,.5), freq = F, xlim = c(min(jnd_long_mean), max(jnd_long_mean)), add=T)



### draw psychometric functions
SOAs = c(-250, -150, -100, -50, -17, 17, 50, 100, 150, 250)
# right
plot(SOAs, pnorm( SOAs, mean = pss_right_mean_reps[1], sd = jnd_right_mean_reps[1]), main = "attend right", ylab = "left proportion", xlab = "SOA" , col = alpha("turquoise", 0.5) )
for (i in 2:length(pss_right_mean_reps)) {
  points(SOAs, pnorm( SOAs, mean = pss_right_mean_reps[i], sd = jnd_right_mean_reps[i]), col = alpha("turquoise", 0.5) )
}
# real right
real_right = real_toj[real_toj$block_bias == "RIGHT",]$left_first_TF
points(SOAs,real_right, col = "blue", pch = 15 )

# left
plot(SOAs, pnorm( SOAs, mean = pss_left_mean_reps[1], sd = jnd_left_mean_reps[1]), main = "attend left", ylab = "left proportion", xlab = "SOA", col = alpha("pink", 0.5)  )
for (i in 2:length(pss_left_mean_reps)) {
  points(SOAs, pnorm( SOAs, mean = pss_left_mean_reps[i], sd = jnd_left_mean_reps[i]), col = alpha("pink", 0.5)   )
}
# real left
real_left = real_toj[real_toj$block_bias == "LEFT",]$left_first_TF
points(SOAs, real_left, col = "red", pch = 15)

# toj judgement type first
plot(SOAs, pnorm( SOAs, mean = pss_first_mean_reps[1], sd = jnd_first_mean_reps[1]), main = "which came first?", ylab = "left proportion", xlab = "SOA", col = alpha("turquoise", 0.5) )
for (i in 2:length(pss_first_mean_reps)) {
  points(SOAs, pnorm(SOAs, mean = pss_first_mean_reps[i], sd = jnd_first_mean_reps[i]), ylab = "left proportion", xlab = "SOA", col = alpha("turquoise", 0.5) )
}
# real first
real_first = real_toj_judgement_type[real_toj_judgement_type$toj_judgement_type == "first",]$left_first_TF
points(SOAs,real_first, col = "blue", pch = 15 )

# toj judgement type second
plot( SOAs, pnorm( SOAs, mean = pss_second_mean_reps[1], sd = jnd_second_mean_reps[1]), main = "which came second?", ylab = "left proportion", xlab = "SOA", col = alpha("pink", 0.5) )
for (i in 2:length(pss_second_mean_reps)) {
  points( SOAs, pnorm( SOAs, mean = pss_second_mean_reps[i], sd = jnd_second_mean_reps[i]), col = alpha("pink", 0.5) )
}
# real second
real_second = real_toj_judgement_type[real_toj_judgement_type$toj_judgement_type=="second",]$left_first_TF
points(SOAs,real_second, col = "red", pch = 15 )

# toj initial bias right
plot(SOAs, pnorm( SOAs, mean = pss_initial_right_mean_reps[1], sd = jnd_initial_right_mean_reps[1]), main = "initial right", ylab = "left proportion", xlab = "SOA", col = alpha("turquoise", 0.5) )
for (i in 2:length(pss_initial_right_mean_reps)) {
  points(SOAs, pnorm(SOAs, mean = pss_initial_right_mean_reps[i], sd = jnd_initial_right_mean_reps[i]), ylab = "left proportion", xlab = "SOA", col = alpha("turquoise", 0.5) )
}
# real initial right
real_initial_right = real_toj_initial_bias[real_toj_initial_bias$probe_initial_bias== "RIGHT",]$left_first_TF
points(SOAs,real_initial_right, col = "blue", pch = 15 )

# toj initial bias left
plot(SOAs, pnorm( SOAs, mean = pss_initial_left_mean_reps[1], sd = jnd_initial_left_mean_reps[1]), main = "initial left", ylab = "left proportion", xlab = "SOA", col = alpha("pink", 0.5) )
for (i in 2:length(pss_initial_left_mean_reps)) {
  points(SOAs, pnorm(SOAs, mean = pss_initial_left_mean_reps[i], sd = jnd_initial_left_mean_reps[i]), ylab = "left proportion", xlab = "SOA", col = alpha("pink", 0.5) )
}
# real initial left
real_initial_left = real_toj_initial_bias[real_toj_initial_bias$probe_initial_bias== "LEFT",]$left_first_TF
points(SOAs,real_initial_left, col = "red", pch = 15 )

# toj probe duration short 
plot(SOAs, pnorm( SOAs, mean = pss_short_mean_reps[1], sd = jnd_short_mean_reps[1]), main = "short probe duration", ylab = "left proportion", xlab = "SOA", col = alpha("turquoise", 0.5) )
for (i in 2:length(pss_short_mean_reps)) {
  points(SOAs, pnorm(SOAs, mean = pss_short_mean_reps[i], sd = jnd_short_mean_reps[i]), ylab = "left proportion", xlab = "SOA", col = alpha("turquoise", 0.5) )
}
# real short
real_short = real_toj_probe_duration[real_toj_probe_duration$onehundredms== TRUE,]$left_first_TF
points(SOAs,real_short, col = "blue", pch = 15 )

# toj probe duration long
plot(SOAs, pnorm( SOAs, mean = pss_long_mean_reps[1], sd = jnd_long_mean_reps[1]), main = "long probe duration", ylab = "left proportion", xlab = "SOA", col = alpha("pink", 0.5) )
for (i in 2:length(pss_long_mean_reps)) {
  points(SOAs, pnorm(SOAs, mean = pss_long_mean_reps[i], sd = jnd_long_mean_reps[i]), ylab = "left proportion", xlab = "SOA", col = alpha("pink", 0.5) )
}
# real initial left
real_long = real_toj_probe_duration[real_toj_probe_duration$onehundredms == FALSE,]$left_first_TF
points(SOAs,real_long, col = "red", pch = 15 )

### Color actual data
load("FollowUp_color_trials.Rdata")
hist(color_trials[color_trials$attended == TRUE,]$color_diff_radians, breaks = 30, freq = F, col = rgb(.1,.1,.1,.5))
hist(color_trials[color_trials$attended == FALSE,]$color_diff_radians, breaks = 30, freq = F, col = rgb(.9,.9,.9,.5), add = T)
# interaction: attention and probe duration
real_color = aggregate(color_diff_radians ~ attended + onehundredms, data = color_trials, FUN  = mean)

### Color simulated data 
# rho intercept mean
rho_intercept_mean = gg_toj_color_post[gg_toj_color_post$Parameter == "logitRhoMean",]$value
# rho effect mean
rho_effect_mean = gg_toj_color_post[gg_toj_color_post$Parameter == "logitRhoEffectMean",]$value
# rho probe duration effect mean
rho_probe_effect_mean = gg_toj_color_post[gg_toj_color_post$Parameter == "logitRhoProbeEffectMean",]$value
# rho probe duration interaction effect mean
rho_probe_interaction_effect_mean = gg_toj_color_post[gg_toj_color_post$Parameter == "logitRhoProbeInteractionEffectMean",]$value

# rho attend with probe short 
rho_attend_short = plogis(rho_intercept_mean - rho_probe_effect_mean/2  + (rho_effect_mean - rho_probe_interaction_effect_mean)/2)
hist(rho_attend, breaks = 10, freq = F, col = rgb(.1,.1,.1,.5))
# sample from posterior 
rho_attend_short_reps = sample(rho_attend_short, 50, replace = T)
hist(rho_attend_short_reps, breaks = 10, freq = F, col = rgb(.9,.9,.9,.5), add = T)

# rho unattend with probe short 
rho_unattend_short = plogis(rho_intercept_mean - rho_probe_effect_mean/2  - (rho_effect_mean - rho_probe_interaction_effect_mean)/2)
hist(rho_unattend_short, breaks = 10, freq = F, col = rgb(.1,.1,.1,.5))
# sample from posterior 
rho_unattend_short_reps = sample(rho_unattend_short, 50, replace = T)
hist(rho_unattend_short_reps, breaks = 10, freq = F, col = rgb(.9,.9,.9,.5), add = T)

# rho attend with probe long
rho_attend_long = plogis(rho_intercept_mean  + rho_probe_effect_mean/2 + (rho_effect_mean + rho_probe_interaction_effect_mean)/2)
hist(rho_attend_long, breaks = 10, freq = F, col = rgb(.1,.1,.1,.5))
# sample from posterior 
rho_attend_long_reps = sample(rho_attend_long, 50, replace = T)
hist(rho_attend_long_reps, breaks = 10, freq = F, col = rgb(.9,.9,.9,.5), add = T)

# rho unattend with probe long
rho_unattend_long = plogis(rho_intercept_mean  + rho_probe_effect_mean/2 - (rho_effect_mean + rho_probe_interaction_effect_mean)/2)
hist(rho_unattend_long, breaks = 10, freq = F, col = rgb(.1,.1,.1,.5))
# sample from posterior 
rho_unattend_long_reps = sample(rho_unattend_long, 50, replace = T)
hist(rho_unattend_long_reps, breaks = 10, freq = F, col = rgb(.9,.9,.9,.5), add = T)

# kappa intercept mean 
kappa_intercept_mean = gg_toj_color_post[gg_toj_color_post$Parameter == "logKappaMean",]$value
# kappa effect mean
kappa_effect_mean = gg_toj_color_post[gg_toj_color_post$Parameter == "logKappaEffectMean",]$value
# kappa probe duration effect mean
kappa_probe_effect_mean = gg_toj_color_post[gg_toj_color_post$Parameter == "logKappaProbeEffectMean",]$value
# kappa probe duration interaction effect mean
kappa_probe_interaction_effect_mean = gg_toj_color_post[gg_toj_color_post$Parameter == "logKappaProbeInteractionEffectMean",]$value

# rho attend with probe short 
kappa_attend_short = exp(kappa_intercept_mean - kappa_probe_effect_mean/2  + (kappa_effect_mean - kappa_probe_interaction_effect_mean)/2)
hist(kappa_attend_short, breaks = 10, freq = F, col = rgb(.1,.1,.1,.5))
# sample from posterior 
kappa_attend_short_reps = sample(kappa_attend_short, 50, replace = T)
hist(kappa_attend_short_reps, breaks = 10, freq = F, col = rgb(.9,.9,.9,.5), add = T)

# kappa unattend with probe short 
kappa_unattend_short = exp(kappa_intercept_mean - kappa_probe_effect_mean/2  - (kappa_effect_mean - kappa_probe_interaction_effect_mean)/2)
hist(kappa_unattend_short, breaks = 10, freq = F, col = rgb(.1,.1,.1,.5))
# sample from posterior 
kappa_unattend_short_reps = sample(kappa_unattend_short, 50, replace = T)
hist(kappa_unattend_short_reps, breaks = 10, freq = F, col = rgb(.9,.9,.9,.5), add = T)

# kappa attend with probe long
kappa_attend_long = exp(kappa_intercept_mean  + kappa_probe_effect_mean/2 + (kappa_effect_mean + kappa_probe_interaction_effect_mean)/2)
hist(kappa_attend_long, breaks = 10, freq = F, col = rgb(.1,.1,.1,.5))
# sample from posterior 
kappa_attend_long_reps = sample(kappa_attend_long, 50, replace = T)
hist(kappa_attend_long_reps, breaks = 10, freq = F, col = rgb(.9,.9,.9,.5), add = T)

# kappa unattend with probe long
kappa_unattend_long = exp(kappa_intercept_mean  + kappa_probe_effect_mean/2 - (kappa_effect_mean + kappa_probe_interaction_effect_mean)/2)
hist(kappa_unattend_long, breaks = 10, freq = F, col = rgb(.1,.1,.1,.5))
# sample from posterior 
kappa_unattend_long_reps = sample(kappa_unattend_long, 50, replace = T)
hist(kappa_unattend_long_reps, breaks = 10, freq = F, col = rgb(.9,.9,.9,.5), add = T)

### draw from distributions
# look at mixture model
plot( seq(-pi, pi, pi/200), (rho_unattend_short_reps[1])*dvm(seq(-pi, pi, pi/200), 0, kappa_unattend_short_reps[1]) 
      + (1-rho_unattend_short_reps[1]) * dunif(seq(-pi, pi, pi/200), -pi, pi)
      , xlab = "radian deviations", ylab = "density")

# unattend + short
# sample from distributions
hist( 
  c( rvm(1000*(rho_unattend_short_reps[1]), pi, kappa_unattend_short_reps[1]) - pi , runif(1000*(1- rho_unattend_short_reps[1]), -pi, pi) )
  , breaks = 50, col = rgb(.1,.1,.1,.1), xlim = c(-pi, pi), freq = F, ylim = c(0, 2)
  , xlab = "radian deviations", main = "unattend_shorted")
# cycle
for (i in 2:length(kappa_unattend_short_reps)) {
  hist( 
    c( rvm(1000*(rho_unattend_short_reps[i]), pi, kappa_unattend_short_reps[i]) - pi , runif(1000*(1- rho_unattend_short_reps[i]), -pi, pi) )
    , breaks = 50, col = rgb(.1,.1,.1,.1), xlim = c(-pi, pi), freq = F, add = T)
}
hist(color_trials[color_trials$attended == FALSE & color_trials$onehundredms == TRUE,]$color_diff_radians, breaks = 50, freq = F, col = rgb(.9,.9,.9,.5), add = T)

# attend + short
# sample from distributions
hist( 
  c( rvm(1000*(rho_attend_short_reps[1]), pi, kappa_attend_short_reps[1]) - pi , runif(1000*(1- rho_attend_short_reps[1]), -pi, pi) )
  , breaks = 50, col = rgb(.1,.1,.1,.1), xlim = c(-pi, pi), freq = F, ylim = c(0, 2)
  , xlab = "radian deviations", main = "attend_shorted")
# cycle
for (i in 2:length(kappa_attend_short_reps)) {
  hist( 
    c( rvm(1000*(rho_attend_short_reps[i]), pi, kappa_attend_short_reps[i]) - pi , runif(1000*(1- rho_attend_short_reps[i]), -pi, pi) )
    , breaks = 50, col = rgb(.1,.1,.1,.1), xlim = c(-pi, pi), freq = F, add = T)
}
hist(color_trials[color_trials$attended == TRUE & color_trials$onehundredms == TRUE,]$color_diff_radians, breaks = 50, freq = F, col = rgb(.9,.9,.9,.5), add = T)

# unattend + long
# sample from distributions
hist( 
  c( rvm(1000*(rho_unattend_long_reps[1]), pi, kappa_unattend_long_reps[1]) - pi , runif(1000*(1- rho_unattend_long_reps[1]), -pi, pi) )
  , breaks = 50, col = rgb(.1,.1,.1,.1), xlim = c(-pi, pi), freq = F, ylim = c(0, 2)
  , xlab = "radian deviations", main = "unattend_longed")
# cycle
for (i in 2:length(kappa_unattend_long_reps)) {
  hist( 
    c( rvm(1000*(rho_unattend_long_reps[i]), pi, kappa_unattend_long_reps[i]) - pi , runif(1000*(1- rho_unattend_long_reps[i]), -pi, pi) )
    , breaks = 50, col = rgb(.1,.1,.1,.1), xlim = c(-pi, pi), freq = F, add = T)
}
hist(color_trials[color_trials$attended == FALSE & color_trials$onehundredms == FALSE,]$color_diff_radians, breaks = 50, freq = F, col = rgb(.9,.9,.9,.5), add = T)

# attend + long
# sample from distributions
hist( 
  c( rvm(1000*(rho_attend_long_reps[1]), pi, kappa_attend_long_reps[1]) - pi , runif(1000*(1- rho_attend_long_reps[1]), -pi, pi) )
  , breaks = 50, col = rgb(.1,.1,.1,.1), xlim = c(-pi, pi), freq = F, ylim = c(0, 2)
  , xlab = "radian deviations", main = "attend_longed")
# cycle
for (i in 2:length(kappa_attend_long_reps)) {
  hist( 
    c( rvm(1000*(rho_attend_long_reps[i]), pi, kappa_attend_long_reps[i]) - pi , runif(1000*(1- rho_attend_long_reps[i]), -pi, pi) )
    , breaks = 50, col = rgb(.1,.1,.1,.1), xlim = c(-pi, pi), freq = F, add = T)
}
hist(color_trials[color_trials$attended == TRUE & color_trials$onehundredms == FALSE,]$color_diff_radians, breaks = 50, freq = F, col = rgb(.9,.9,.9,.5), add = T)

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
library(reshape)
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
betas$participant = rep(c(1:22), times = 8, each = nrow(betas2))

pssmean2 = data.frame(value = ex_toj_color_post$population_pss_intercept_mean)
pssmean2$iteration = rownames(pssmean2)
pssmean = melt( pssmean2 )$value
pssmean_reps = sample(pssmean, 50, replace = T)

psssd2 = data.frame(value = tan(ex_toj_color_post$zpopulation_pss_intercept_sd))
psssd2$iteration = rownames(psssd2)
psssd = melt( psssd2 )$value
psssd_reps = sample(psssd, 50, replace = T)

pssjudgementtypeeffect2 = data.frame(value = ex_toj_color_post$population_pss_judgement_type_effect_mean)
pssjudgementtypeeffect2$iteration = rownames(pssjudgementtypeeffect2)
pssjudgementtypeeffect = melt( pssjudgementtypeeffect2 )$value
pssjudgementtypeeffect_reps = sample(pssjudgementtypeeffect, 50, replace = T)
judgementtypefactor = ifelse(aggregate(toj_judgement_type~id,data = toj_trials, FUN =unique)$toj_judgement_type == "first", -1, 1)

pssinitialbiaseffect2 = data.frame(value = ex_toj_color_post$population_pss_initial_bias_effect_mean)
pssinitialbiaseffect2$iteration = rownames(pssinitialbiaseffect2)
pssinitialbiaseffect = melt( pssinitialbiaseffect2 )$value
pssinitialbiaseffect_reps = sample(pssinitialbiaseffect, 50, replace = T)
initialbiasfactor = ifelse(aggregate(probe_initial_bias~id,data = toj_trials, FUN =unique)$probe_initial_bias == "RIGHT", -1, 1)

pssprobeeffect2 = data.frame(value = ex_toj_color_post$population_pss_probe_effect_mean)
pssprobeeffect2$iteration = rownames(pssprobeeffect2)
pssprobeeffect = melt( pssprobeeffect2 )$value
pssprobeeffect_reps = sample(pssprobeeffect, 50, replace = T)
probefactor = ifelse(aggregate(onehundredms~id,data = toj_trials, FUN =unique)$onehundredms, -1, 1)


# cycle through each participant 
pss_per_id = ddply(
  .data = betas
  , .variables = .(participant)
  , .fun = function(x){
    i = unique(x$participant)
    x_reps = sample(x[x$parameter == "population_pss_intercept_mean",]$value, 50, replace = TRUE)
    pssintercept = pssmean_reps + psssd_reps*x_reps
      + pssjudgementtypeeffect_reps*judgementtypefactor[i]/2
      + pssinitialbiaseffect_reps*initialbiasfactor[i]/2
      + pssprobeeffect_reps*probefactor[i]/2
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

logjndjudgementtypeeffect2 = data.frame(value = ex_toj_color_post$population_logjnd_judgement_type_effect_mean)
logjndjudgementtypeeffect2$iteration = rownames(logjndjudgementtypeeffect2)
logjndjudgementtypeeffect = melt( logjndjudgementtypeeffect2 )$value
logjndjudgementtypeeffect_reps = sample(logjndjudgementtypeeffect, 50, replace = T)
judgementtypefactor = ifelse(aggregate(toj_judgement_type~id,data = toj_trials, FUN =unique)$toj_judgement_type == "first", -1, 1)

logjndinitialbiaseffect2 = data.frame(value = ex_toj_color_post$population_logjnd_initial_bias_effect_mean)
logjndinitialbiaseffect2$iteration = rownames(logjndinitialbiaseffect2)
logjndinitialbiaseffect = melt( logjndinitialbiaseffect2 )$value
logjndinitialbiaseffect_reps = sample(logjndinitialbiaseffect, 50, replace = T)
initialbiasfactor = ifelse(aggregate(probe_initial_bias~id,data = toj_trials, FUN =unique)$probe_initial_bias == "RIGHT", -1, 1)

logjndprobeeffect2 = data.frame(value = ex_toj_color_post$population_logjnd_probe_effect_mean)
logjndprobeeffect2$iteration = rownames(logjndprobeeffect2)
logjndprobeeffect = melt( logjndprobeeffect2 )$value
logjndprobeeffect_reps = sample(logjndprobeeffect, 50, replace = T)
probefactor = ifelse(aggregate(onehundredms~id,data = toj_trials, FUN =unique)$onehundredms, -1, 1)


jnd_per_id = ddply(
  .data = betas
  , .variables = .(participant)
  , .fun = function(x){
    i = unique(x$participant)
    x_reps = sample(x[x$parameter == "population_logjnd_intercept_mean",]$value, 50, replace = TRUE)
    logjndintercept = logjndmean_reps + logjndsd_reps*x_reps
    + logjndjudgementtypeeffect_reps*judgementtypefactor[i]/2
    + logjndinitialbiaseffect_reps*initialbiasfactor[i]/2
    + logjndprobeeffect_reps*probefactor[i]/2
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
      formula = left_first_TF~soa2
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

# # get PSS effect vs. Rho effect 
# ggplot(
#   data = pos_corr[pos_corr$parameter == "value.2.7",]
#   , aes(x = parameter, y = value)
# )+
#   geom_violin()+
#   labs(x = "Logit \u03C1 vs. PSS Effect Means", y = "Correlation Coefficient (r)")+
#   stat_summary(fun.data = get_95_HDI, size = 0.7)+
#   stat_summary(fun.data = get_50_HDI, size = 2.5)+  
#   geom_hline(yintercept = 0, linetype = 2, size = 1)+
#   theme_gray(base_size = 24)+
#   theme(panel.grid.major = element_line(size = 1.5)
#         ,panel.grid.minor = element_line(size = 1)
#         , axis.text.x = element_blank()
#         , axis.ticks.x = element_blank()) 

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
    pssintercept = median(pssmean) + median(psssd)*median(x_use)
    + median(pssjudgementtypeeffect*judgementtypefactor[i])/2
    + median(pssinitialbiaseffect*initialbiasfactor[i])/2
    + median(pssprobeeffect*probefactor[i])/2    
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
    logjndintercept = median(logjndmean) + median(logjndsd)*median(x_use)
    + median(logjndjudgementtypeeffect*judgementtypefactor[i])/2
    + median(logjndinitialbiaseffect*initialbiasfactor[i])/2
    + median(logjndprobeeffect*probefactor[i])/2
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
  geom_smooth(method = "loess", se = TRUE, size = 1)+
  # geom_smooth(method = "lm", se = FALSE, size = 1)+
  theme_gray(base_size = 24)+
  theme(panel.grid.major = element_line(size = 1.5)
        ,panel.grid.minor = element_line(size = 1))



## ADD other correlations



#### violin plot for key parameters (plot things on same scale) ####
### SOA scale 
pos_SOA_scale = data.frame(  
  value = c(
    ex_toj_color_post$population_pss_intercept_mean * 250
    , exp( ex_toj_color_post$population_logjnd_intercept_mean ) * 250
  )
  , parameter = c(
    rep("PSS Intercept Mean", 250)  
    , rep("JND Intercept Mean", 250)  
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




# ### SOA scale - effects 
# pos_SOA_scale_effects = data.frame(  
#   effect = c(
#     ( (ex_toj_color_post$population_pss_intercept_mean + ex_toj_color_post$population_pss_effect_mean/2) 
#       - (ex_toj_color_post$population_pss_intercept_mean - ex_toj_color_post$population_pss_effect_mean/2) ) * 250
#     , ( exp( ex_toj_color_post$population_logjnd_intercept_mean + ex_toj_color_post$population_logjnd_effect_mean/2 )
#         - exp( ex_toj_color_post$population_logjnd_intercept_mean - ex_toj_color_post$population_logjnd_effect_mean/2  ) ) * 250 
#   )
#   , parameter = c(
#     rep("PSS Effect Mean", 80000)
#     , rep("JND Effect Mean", 80000)
#   )
# )
# 
# ggplot(data = pos_SOA_scale_effects, aes(x = parameter, y = effect))+
#   geom_violin()+
#   stat_summary(fun.data = get_95_HDI, size = 0.7)+
#   stat_summary(fun.data = get_50_HDI, size = 2.5)+
#   labs(x = "", y = "SOA (Base - Glove; ms)")+
#   #  scale_x_discrete(labels = c("PSS Effect Mean", "JND Effect Mean"))+
#   geom_hline(yintercept = 0, linetype = 2, size = 1)+
#   theme_gray(base_size = 30)+
#   theme(panel.grid.major = element_line(size = 1.5)
#         ,panel.grid.minor = element_line(size = 1)
#         , axis.ticks.x = element_blank()) 
# 
# # 95% HDIs
# # pss effect 
# get_95_HDI(ex_toj_color_post$population_pss_effect_mean*250 )  
# # get_95_HDI(tan(ex_toj_color_post$zpopulation_pss_effect_sd)*2*250)  
# # print("ERROR: Does it make sense to multiply the SD by 2?")
# # jnd effect 
# get_95_HDI(
#   ( exp( ex_toj_color_post$population_logjnd_intercept_mean + ex_toj_color_post$population_logjnd_effect_mean/2 )
#     - exp( ex_toj_color_post$population_logjnd_intercept_mean - ex_toj_color_post$population_logjnd_effect_mean/2  ) ) * 250 
# )
# # get_95_HDI( tan(ex_toj_color_post$zpopulation_logjnd_effect_sd)*2) #  (log scale, normalized)
# # print("ERROR: Does it make sense to multiply the SD by 2?")


### SOA scale - Judgement Type effect 
pos_SOA_scale_judgement_type = data.frame(  
  effect = c(
    ( (ex_toj_color_post$population_pss_intercept_mean + ex_toj_color_post$population_pss_judgement_type_effect_mean/2) 
      - (ex_toj_color_post$population_pss_intercept_mean - ex_toj_color_post$population_pss_judgement_type_effect_mean/2) ) * 250
    , ( exp( ex_toj_color_post$population_logjnd_intercept_mean + ex_toj_color_post$population_logjnd_judgement_type_effect_mean/2 )
        - exp( ex_toj_color_post$population_logjnd_intercept_mean - ex_toj_color_post$population_logjnd_judgement_type_effect_mean/2  ) ) * 250 
  )
  , parameter = c(
    rep("PSS Judgement Type Effect Mean", 250)
    , rep("JND Judgement Type Effect Mean", 250)
  )
)

ggplot(data = pos_SOA_scale_judgement_type, aes(x = parameter, y = effect))+
  geom_violin()+
  stat_summary(fun.data = get_95_HDI, size = 0.7)+
  stat_summary(fun.data = get_50_HDI, size = 2.5)+
  labs(x = "", y = "SOA (First - Second; ms)")+
  scale_x_discrete(labels = c("JND Judgement\nType Effect Mean", "PSS Judgement\nType Effect Mean"))+
  geom_hline(yintercept = 0, linetype = 2, size = 1)+
  theme_gray(base_size = 30)+
  theme(panel.grid.major = element_line(size = 1.5)
        ,panel.grid.minor = element_line(size = 1)
        , axis.ticks.x = element_blank()) 

# 95% HDIs
# pss effect 
get_95_HDI(ex_toj_color_post$population_pss_judgement_type_effect_mean*250 )  
# jnd effect 
get_95_HDI(
  ( exp( ex_toj_color_post$population_logjnd_intercept_mean + ex_toj_color_post$population_logjnd_judgement_type_effect_mean/2 )
    - exp( ex_toj_color_post$population_logjnd_intercept_mean - ex_toj_color_post$population_logjnd_judgement_type_effect_mean/2  ) ) * 250 
)

### SOA scale - Initial Bias 
pos_SOA_scale_initial_bias = data.frame(  
  effect = c(
    ( (ex_toj_color_post$population_pss_intercept_mean + ex_toj_color_post$population_pss_initial_bias_effect_mean/2) 
      - (ex_toj_color_post$population_pss_intercept_mean - ex_toj_color_post$population_pss_initial_bias_effect_mean/2) ) * 250
    , ( exp( ex_toj_color_post$population_logjnd_intercept_mean + ex_toj_color_post$population_logjnd_initial_bias_effect_mean/2 )
        - exp( ex_toj_color_post$population_logjnd_intercept_mean - ex_toj_color_post$population_logjnd_initial_bias_effect_mean/2  ) ) * 250 
  )
  , parameter = c(
    rep("PSS Initial Probe Bias Effect Mean", 250)
    , rep("JND Initial Probe Bias Effect Mean", 250)
  )
)

ggplot(data = pos_SOA_scale_initial_bias, aes(x = parameter, y = effect))+
  geom_violin()+
  stat_summary(fun.data = get_95_HDI, size = 0.7)+
  stat_summary(fun.data = get_50_HDI, size = 2.5)+
  labs(x = "", y = "SOA (First - Second; ms)")+
  scale_x_discrete(labels = c("JND Initial Probe\nBias Effect Mean", "PSS Initial Probe\nBias Effect Mean"))+
  geom_hline(yintercept = 0, linetype = 2, size = 1)+
  theme_gray(base_size = 30)+
  theme(panel.grid.major = element_line(size = 1.5)
        ,panel.grid.minor = element_line(size = 1)
        , axis.ticks.x = element_blank()) 

# 95% HDIs
# pss effect 
get_95_HDI(ex_toj_color_post$population_pss_initial_bias_effect_mean*250 )  
# jnd effect 
get_95_HDI(
  ( exp( ex_toj_color_post$population_logjnd_intercept_mean + ex_toj_color_post$population_logjnd_initial_bias_effect_mean/2 )
    - exp( ex_toj_color_post$population_logjnd_intercept_mean - ex_toj_color_post$population_logjnd_initial_bias_effect_mean/2  ) ) * 250 
)

### SOA scale - Probe Duration 
pos_SOA_scale_probe = data.frame(  
  effect = c(
    ( (ex_toj_color_post$population_pss_intercept_mean + ex_toj_color_post$population_pss_probe_effect_mean/2) 
      - (ex_toj_color_post$population_pss_intercept_mean - ex_toj_color_post$population_pss_probe_effect_mean/2) ) * 250
    , ( exp( ex_toj_color_post$population_logjnd_intercept_mean + ex_toj_color_post$population_logjnd_probe_effect_mean/2 )
        - exp( ex_toj_color_post$population_logjnd_intercept_mean - ex_toj_color_post$population_logjnd_probe_effect_mean/2  ) ) * 250 
  )
  , parameter = c(
    rep("PSS Probe Duration Bias Effect Mean", 250)
    , rep("JND Probe Duration Bias Effect Mean", 250)
  )
)

ggplot(data = pos_SOA_scale_probe, aes(x = parameter, y = effect))+
  geom_violin()+
  stat_summary(fun.data = get_95_HDI, size = 0.7)+
  stat_summary(fun.data = get_50_HDI, size = 2.5)+
  labs(x = "", y = "SOA (Long - Short; ms)")+
  scale_x_discrete(labels = c("JND Probe Duration\nBias Effect Mean", "PSS Probe Duration\nBias Effect Mean"))+
  geom_hline(yintercept = 0, linetype = 2, size = 1)+
  theme_gray(base_size = 30)+
  theme(panel.grid.major = element_line(size = 1.5)
        ,panel.grid.minor = element_line(size = 1)
        , axis.ticks.x = element_blank()) 

# 95% HDIs
# pss effect 
get_95_HDI(ex_toj_color_post$population_pss_probe_effect_mean*250 )  
# jnd effect 
get_95_HDI(
  ( exp( ex_toj_color_post$population_logjnd_intercept_mean + ex_toj_color_post$population_logjnd_probe_effect_mean/2 )
    - exp( ex_toj_color_post$population_logjnd_intercept_mean - ex_toj_color_post$population_logjnd_probe_effect_mean/2  ) ) * 250 
)



### Rho scale
pos_rho_scale = data.frame(  
  value = c(
    plogis(ex_toj_color_post$logitRhoMean)
  )
  , parameter = c(
    rep("rhoInterceptMean", 250)
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



### Rho scale - effects
pos_rho_scale_effects = data.frame(  
  effect = c(
    ( plogis(ex_toj_color_post$logitRhoMean + ex_toj_color_post$logitRhoEffectMean/2 )
      - plogis(ex_toj_color_post$logitRhoMean - ex_toj_color_post$logitRhoEffectMean/2 ) )
  )
  , parameter = c(
    rep("rhoEffectMean", 250)
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

### Rho scale - probe effect
pos_rho_scale_probe_effects = data.frame(  
  effect = c(
    ( plogis(ex_toj_color_post$logitRhoMean + ex_toj_color_post$logitRhoProbeEffectMean/2 )
      - plogis(ex_toj_color_post$logitRhoMean - ex_toj_color_post$logitRhoProbeEffectMean/2 ) )
  )
  , parameter = c(
    rep("rhoProbeEffectMean", 250)
  )
)

ggplot(data = pos_rho_scale_probe_effects, aes(x = parameter, y = effect))+
  geom_violin()+
  labs(x = "", y = "\u03C1 (Long - Short)")+
  geom_hline(yintercept = 0, linetype = 2, size = 1)+
  stat_summary(fun.data = get_95_HDI, size = 0.7)+
  stat_summary(fun.data = get_50_HDI, size = 2.5)+
  scale_x_discrete(labels = c("Probability of Memory Probe Effect Mean"))+
  theme_gray(base_size = 30)+
  theme(panel.grid.major = element_line(size = 1.5)
        ,panel.grid.minor = element_line(size = 1)
        , axis.ticks.x = element_blank()) 

# 95% HDIs
# rho effect 
get_95_HDI( 
  ( plogis(ex_toj_color_post$logitRhoMean + ex_toj_color_post$logitRhoProbeEffectMean/2 )
    - plogis(ex_toj_color_post$logitRhoMean - ex_toj_color_post$logitRhoProbeEffectMean/2 ) ) 
) 


### Rho scale - interaction effect
pos_rho_scale_interaction_effects = data.frame(  
  effect = c(
    ( plogis(ex_toj_color_post$logitRhoMean + ex_toj_color_post$logitRhoProbeInteractionEffectMean/2 )
      - plogis(ex_toj_color_post$logitRhoMean - ex_toj_color_post$logitRhoProbeInteractionEffectMean/2 ) )
  )
  , parameter = c(
    rep("rhoInteractionEffectMean", 250)
  )
)

ggplot(data = pos_rho_scale_interaction_effects, aes(x = parameter, y = effect))+
  geom_violin()+
  labs(x = "", y = "\u03C1 (Long & Attended or Short & Unattended\n- Short & Attended or Long & Unattended)")+
  geom_hline(yintercept = 0, linetype = 2, size = 1)+
  stat_summary(fun.data = get_95_HDI, size = 0.7)+
  stat_summary(fun.data = get_50_HDI, size = 2.5)+
  scale_x_discrete(labels = c("Probability of Memory Interaction Effect Mean"))+
  theme_gray(base_size = 30)+
  theme(panel.grid.major = element_line(size = 1.5)
        ,panel.grid.minor = element_line(size = 1)
        , axis.ticks.x = element_blank()) 

# 95% HDIs
# rho effect 
get_95_HDI( 
  ( plogis(ex_toj_color_post$logitRhoMean + ex_toj_color_post$logitRhoProbeInteractionEffectMean/2 )
    - plogis(ex_toj_color_post$logitRhoMean - ex_toj_color_post$logitRhoProbeInteractionEffectMean/2 ) )
) 

### INTUITIVE look at rho interaction
pos_rho_scale_parameters = data.frame(  
  effect = c(
    ( plogis(ex_toj_color_post$logitRhoMean + ex_toj_color_post$logitRhoProbeEffectMean/2
             + (ex_toj_color_post$logitRhoEffectMean + ex_toj_color_post$logitRhoProbeInteractionEffectMean)/2 )
      - plogis(ex_toj_color_post$logitRhoMean + ex_toj_color_post$logitRhoProbeEffectMean/2
              - (ex_toj_color_post$logitRhoEffectMean +  ex_toj_color_post$logitRhoProbeInteractionEffectMean)/2 ) )
    , ( plogis(ex_toj_color_post$logitRhoMean - ex_toj_color_post$logitRhoProbeEffectMean/2
               + (ex_toj_color_post$logitRhoEffectMean - ex_toj_color_post$logitRhoProbeInteractionEffectMean)/2 )
        - plogis(ex_toj_color_post$logitRhoMean - ex_toj_color_post$logitRhoProbeEffectMean/2
                 - (ex_toj_color_post$logitRhoEffectMean -  ex_toj_color_post$logitRhoProbeInteractionEffectMean)/2 ) )
  )
  , parameter = c(
    rep("Rho Attention Effect Given Long", 250)
    , rep("Rho Attention Effect Given Short", 250)
  )
)

ggplot(data = pos_rho_scale_parameters, aes(x = parameter, y = effect))+
  geom_violin()+
  labs(x = "", y = "\u03C1 (Attended - Unattended)")+
  geom_hline(yintercept = 0, linetype = 2, size = 1)+
  stat_summary(fun.data = get_95_HDI, size = 0.7)+
  stat_summary(fun.data = get_50_HDI, size = 2.5)+
  scale_x_discrete(labels = c("Probability of Memory\nAttention Effect\nGiven Long\nProbe Duration"
                              , "Probability of Memory\nAttention Effect\nGiven Short\nProbe Duration"))+
  theme_gray(base_size = 30)+
  theme(panel.grid.major = element_line(size = 1.5)
        ,panel.grid.minor = element_line(size = 1)
        , axis.ticks.x = element_blank()) 

# 95% HDIs
# rho attention effect given long
get_95_HDI( 
  ( plogis(ex_toj_color_post$logitRhoMean + ex_toj_color_post$logitRhoProbeEffectMean/2
           + (ex_toj_color_post$logitRhoEffectMean + ex_toj_color_post$logitRhoProbeInteractionEffectMean)/2 )
    - plogis(ex_toj_color_post$logitRhoMean + ex_toj_color_post$logitRhoProbeEffectMean/2
             - (ex_toj_color_post$logitRhoEffectMean +  ex_toj_color_post$logitRhoProbeInteractionEffectMean)/2 ) )
 ) 
# rho attention effect given short
get_95_HDI(
  ( plogis(ex_toj_color_post$logitRhoMean - ex_toj_color_post$logitRhoProbeEffectMean/2
             + (ex_toj_color_post$logitRhoEffectMean - ex_toj_color_post$logitRhoProbeInteractionEffectMean)/2 )
      - plogis(ex_toj_color_post$logitRhoMean - ex_toj_color_post$logitRhoProbeEffectMean/2
               - (ex_toj_color_post$logitRhoEffectMean -  ex_toj_color_post$logitRhoProbeInteractionEffectMean)/2 ) )
  
)


### Kappa scale
# rad2deg <- function(rad) {(rad * 180) / (pi)}

pos_kappa_scale = data.frame(  
  value = c(
    exp( ex_toj_color_post$logKappaMean ) # regular scale ~ kappa; log scale ~ kappa prime;
  )
  , parameter = c(
    rep("kappaInterceptMean", 250)
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

### kappa scale - probe effect
pos_kappa_scale_probe_effects = data.frame(  
  effect = c(
    ( exp(ex_toj_color_post$logKappaMean + ex_toj_color_post$logKappaProbeEffectMean/2 )
      - exp(ex_toj_color_post$logKappaMean - ex_toj_color_post$logKappaProbeEffectMean/2 ) )
  )
  , parameter = c(
    rep("kappaProbeEffectMean", 250)
  )
)

ggplot(data = pos_kappa_scale_probe_effects, aes(x = parameter, y = effect))+
  geom_violin()+
  labs(x = "", y = "\u03C1 (Long - Short)")+
  geom_hline(yintercept = 0, linetype = 2, size = 1)+
  stat_summary(fun.data = get_95_HDI, size = 0.7)+
  stat_summary(fun.data = get_50_HDI, size = 2.5)+
  scale_x_discrete(labels = c("Fidelity of Memory Probe Effect Mean"))+
  theme_gray(base_size = 30)+
  theme(panel.grid.major = element_line(size = 1.5)
        ,panel.grid.minor = element_line(size = 1)
        , axis.ticks.x = element_blank()) 

# 95% HDIs
# kappa effect 
get_95_HDI( 
  ( exp(ex_toj_color_post$logKappaMean + ex_toj_color_post$logKappaProbeEffectMean/2 )
    - exp(ex_toj_color_post$logKappaMean - ex_toj_color_post$logKappaProbeEffectMean/2 ) ) 
) 


### kappa scale - interaction effect
pos_kappa_scale_interaction_effects = data.frame(  
  effect = c(
    ( exp(ex_toj_color_post$logKappaMean + ex_toj_color_post$logKappaProbeInteractionEffectMean/2 )
      - exp(ex_toj_color_post$logKappaMean - ex_toj_color_post$logKappaProbeInteractionEffectMean/2 ) )
  )
  , parameter = c(
    rep("kappaInteractionEffectMean", 250)
  )
)

ggplot(data = pos_kappa_scale_interaction_effects, aes(x = parameter, y = effect))+
  geom_violin()+
  labs(x = "", y = "\u03C1 (Long & Attended or Short & Unattended\n- Short & Attended or Long & Unattended)")+
  geom_hline(yintercept = 0, linetype = 2, size = 1)+
  stat_summary(fun.data = get_95_HDI, size = 0.7)+
  stat_summary(fun.data = get_50_HDI, size = 2.5)+
  scale_x_discrete(labels = c("Fidelity of Memory Interaction Effect Mean"))+
  theme_gray(base_size = 30)+
  theme(panel.grid.major = element_line(size = 1.5)
        ,panel.grid.minor = element_line(size = 1)
        , axis.ticks.x = element_blank()) 

# 95% HDIs
# kappa effect 
get_95_HDI( 
  ( exp(ex_toj_color_post$logKappaMean + ex_toj_color_post$logKappaProbeInteractionEffectMean/2 )
    - exp(ex_toj_color_post$logKappaMean - ex_toj_color_post$logKappaProbeInteractionEffectMean/2 ) )
) 

### INTUITIVE look at kappa interaction
pos_kappa_scale_parameters = data.frame(  
  effect = c(
    ( exp(ex_toj_color_post$logKappaMean + ex_toj_color_post$logKappaProbeEffectMean/2
             + (ex_toj_color_post$logKappaEffectMean + ex_toj_color_post$logKappaProbeInteractionEffectMean)/2 )
      - exp(ex_toj_color_post$logKappaMean + ex_toj_color_post$logKappaProbeEffectMean/2
               - (ex_toj_color_post$logKappaEffectMean +  ex_toj_color_post$logKappaProbeInteractionEffectMean)/2 ) )
    , ( exp(ex_toj_color_post$logKappaMean - ex_toj_color_post$logKappaProbeEffectMean/2
               + (ex_toj_color_post$logKappaEffectMean - ex_toj_color_post$logKappaProbeInteractionEffectMean)/2 )
        - exp(ex_toj_color_post$logKappaMean - ex_toj_color_post$logKappaProbeEffectMean/2
                 - (ex_toj_color_post$logKappaEffectMean -  ex_toj_color_post$logKappaProbeInteractionEffectMean)/2 ) )
  )
  , parameter = c(
    rep("kappa Attention Effect Given Long", 250)
    , rep("kappa Attention Effect Given Short", 250)
  )
)

ggplot(data = pos_kappa_scale_parameters, aes(x = parameter, y = effect))+
  geom_violin()+
  labs(x = "", y = "\u03C1 (Attended - Unattended)")+
  geom_hline(yintercept = 0, linetype = 2, size = 1)+
  stat_summary(fun.data = get_95_HDI, size = 0.7)+
  stat_summary(fun.data = get_50_HDI, size = 2.5)+
  scale_x_discrete(labels = c("Fidelity of Memory\nAttention Effect\nGiven Long\nProbe Duration"
                              , "Fidelity of Memory\nAttention Effect\nGiven Short\nProbe Duration"))+
  theme_gray(base_size = 30)+
  theme(panel.grid.major = element_line(size = 1.5)
        ,panel.grid.minor = element_line(size = 1)
        , axis.ticks.x = element_blank()) 

# 95% HDIs
# kappa attention effect given long
get_95_HDI( 
  ( exp(ex_toj_color_post$logKappaMean + ex_toj_color_post$logKappaProbeEffectMean/2
           + (ex_toj_color_post$logKappaEffectMean + ex_toj_color_post$logKappaProbeInteractionEffectMean)/2 )
    - exp(ex_toj_color_post$logKappaMean + ex_toj_color_post$logKappaProbeEffectMean/2
             - (ex_toj_color_post$logKappaEffectMean +  ex_toj_color_post$logKappaProbeInteractionEffectMean)/2 ) )
) 
# kappa attention effect given short
get_95_HDI(
  ( exp(ex_toj_color_post$logKappaMean - ex_toj_color_post$logKappaProbeEffectMean/2
           + (ex_toj_color_post$logKappaEffectMean - ex_toj_color_post$logKappaProbeInteractionEffectMean)/2 )
    - exp(ex_toj_color_post$logKappaMean - ex_toj_color_post$logKappaProbeEffectMean/2
             - (ex_toj_color_post$logKappaEffectMean -  ex_toj_color_post$logKappaProbeInteractionEffectMean)/2 ) )
  
)





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




# #### TOJ: recreate NCF using pss intercept mean and jnd intercept mean ####
# # including effects 
# yGlove = pnorm(
#   -250:250
#   , mean = ( median(ex_toj_color_post$population_pss_intercept_mean) - median(ex_toj_color_post$population_pss_effect_mean)/2 ) * 250
#   , sd = ( exp( median(ex_toj_color_post$population_logjnd_intercept_mean) - median(ex_toj_color_post$population_logjnd_effect_mean)/2 )   ) * 250
# )
# yBase = pnorm(
#   -250:250
#   , mean = ( median(ex_toj_color_post$population_pss_intercept_mean) + median(ex_toj_color_post$population_pss_effect_mean)/2 ) * 250
#   , sd = ( exp( median(ex_toj_color_post$population_logjnd_intercept_mean) + median(ex_toj_color_post$population_logjnd_effect_mean)/2 )   ) * 250
#   
# )
# df = data.frame(SOA = -250:250, Prop = c(yGlove, yBase), Attend = c(rep("Glove",501), rep("Base", 501)))
# 
# 
# gg = ggplot(data = df, aes(y = Prop, x = SOA, colour = Attend))+
#   geom_line(size = 1.25)+
#   # scale_color_manual("Attend", values = c("red", "blue"))+
#   scale_color_hue("Attend", l = c(60, 15), c = c(100, 50), h = c(240, 360) ) +
#   labs(x = "SOA (ms)", y = "Proportion of 'Safe' Responses")+
#   theme_gray(base_size = 24)+
#   theme(panel.grid.major = element_line(size = 1.5)
#         ,panel.grid.minor = element_line(size = 1))
# # define text to add
# Text1 = textGrob(label = paste("Out"), gp = gpar(fontsize= 24))
# Text2 = textGrob(label = paste("Safe"), gp = gpar(fontsize= 24)) 
# gg = gg+
#   annotation_custom(grob = Text1,  xmin = -200, xmax = -200, ymin = -0.115, ymax = -0.115)+
#   annotation_custom(grob = Text2,  xmin = 200, xmax = 200, ymin = -0.115, ymax = -0.115)
# # Code to override clipping
# gg2 <- ggplot_gtable(ggplot_build(gg))
# gg2$layout$clip[gg2$layout$name=="panel"] <- "off"
# grid.draw(gg2)





#### Color: plot posterior means with effects ####
# NOTE: Intercepts are misleading here, likely because of logit scale (+- probe duration effect)
pos_rhoMean_WithEffect = data.frame(
  c( 
    plogis(ex_toj_color_post$logitRhoMean + ex_toj_color_post$logitRhoProbeEffectMean/2
           + (ex_toj_color_post$logitRhoEffectMean +  ex_toj_color_post$logitRhoProbeInteractionEffectMean)/2)
    ,  plogis(ex_toj_color_post$logitRhoMean + ex_toj_color_post$logitRhoProbeEffectMean/2
              - (ex_toj_color_post$logitRhoEffectMean +  ex_toj_color_post$logitRhoProbeInteractionEffectMean)/2)
    ,  plogis(ex_toj_color_post$logitRhoMean - ex_toj_color_post$logitRhoProbeEffectMean/2
              + (ex_toj_color_post$logitRhoEffectMean -  ex_toj_color_post$logitRhoProbeInteractionEffectMean)/2)
    ,  plogis(ex_toj_color_post$logitRhoMean - ex_toj_color_post$logitRhoProbeEffectMean/2
              - (ex_toj_color_post$logitRhoEffectMean -  ex_toj_color_post$logitRhoProbeInteractionEffectMean)/2)
  ) 
  , c(rep("AttendedLong",250), rep("UnattendedLong",250), rep("AttendedShort",250), rep("UnattendedShort",250))
)
names(pos_rhoMean_WithEffect) = c("rhoMean", "Effect")


# overlapping
ggplot(pos_rhoMean_WithEffect, aes(x = rhoMean, ..density.., fill = Effect))+
  geom_density(data = pos_rhoMean_WithEffect[pos_rhoMean_WithEffect$Effect == "AttendedLong",],alpha = 0.5)+
  geom_density(data = pos_rhoMean_WithEffect[pos_rhoMean_WithEffect$Effect == "UnattendedLong",],alpha = 0.5)+
  geom_density(data = pos_rhoMean_WithEffect[pos_rhoMean_WithEffect$Effect == "AttendedShort",],alpha = 0.5)+
  geom_density(data = pos_rhoMean_WithEffect[pos_rhoMean_WithEffect$Effect == "UnattendedShort",],alpha = 0.5)+
  scale_fill_hue("Effect", l = c(90, 45, 70, 30) , c = c(100, 50, 100, 50) ) +
  labs(x = "Probability of Memory Population Mean", y = "Density", colour = "")+
  theme_gray(base_size = 24)+
  theme(panel.grid.major = element_line(size = 1.5)
        ,panel.grid.minor = element_line(size = 1))






