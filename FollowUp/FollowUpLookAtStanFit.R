# library(shinystan)
library(coda)
library(ggplot2)
library(ggmcmc)
library(CircStats)
library(grid)

setwd("~/Documents/TOJ/Follow-Up")
load("FollowUptoj_color_post_June13th2016")
load("FollowUp_color_trials.Rdata")
load("FollowUp_toj_trials.Rdata")



############################################################################################
####                                        Diagnostics                                 ####
############################################################################################
# convert stanfit sample to dataframe table 
gg_toj_color_post = ggs(toj_color_post)

# look
gg_toj_color_post

# look structure
str(gg_toj_color_post)

# list of parameters to examine
param_list = c("logitRhoEffectMean"
               , "logitRhoMean"
               , "logitRhoProbeEffectMean"
               , "logitRhoProbeInteractionEffectMean"
               , "logKappaEffectMean"
               , "logKappaMean"
               , "logKappaProbeEffectMean"
               , "logKappaProbeInteractionEffectMean"
               , "population_logjnd_effect_mean"
               , "population_logjnd_initial_bias_effect_mean"
               , "population_logjnd_judgement_type_effect_mean"
               , "population_logjnd_probe_effect_mean"
               , "population_logjnd_intercept_mean"
               , "population_pss_effect_mean"
               , "population_pss_initial_bias_effect_mean"
               , "population_pss_judgement_type_effect_mean"
               , "population_pss_probe_effect_mean"  
               , "population_pss_intercept_mean"
               , "zlogitRhoEffectSD" 
               ,  "zlogitRhoSD"
               , "zlogKappaEffectSD"
               , "zlogKappaSD"
               , "zpopulation_logjnd_effect_sd"
               , "zpopulation_logjnd_intercept_sd"
               , "zpopulation_pss_effect_sd"
               , "zpopulation_pss_intercept_sd")

# look at posteriors
for (param in param_list) {
  ptm = proc.time()
  print( ggs_histogram(gg_toj_color_post, family = param) )
  print( proc.time() - ptm )
}

# posterior by chain
for (param in param_list) {
  ptm = proc.time()
  print( ggs_density(gg_toj_color_post, family = param) )
  print( proc.time() - ptm )
}

# traceplots
for (param in param_list) {
  ptm = proc.time()
  print( ggs_traceplot(gg_toj_color_post, family = param) )
  print( proc.time() - ptm )
}

# running means
for (param in param_list) {
  ptm = proc.time()
  print( ggs_running(gg_toj_color_post, family = param) )
  print( proc.time() - ptm )
}

# compare complete and last part of chains
for (param in param_list) {
  ptm = proc.time()
  print( ggs_compare_partial(gg_toj_color_post, family = param) )
  print( proc.time() - ptm )
}

# autocorrelation
for (param in param_list) {
  ptm = proc.time()
  print( ggs_autocorrelation(gg_toj_color_post, family = param) )
  print( proc.time() - ptm )
}
# NOTE: autocorrelation is not indicative of lack of convergence per se, but is indicative of misbehavior perhaps
# solution to autocorrelation is thinining



############################################################################################
####                         Posterior Predictive Checks                                ####
############################################################################################

#-------------------------------------- TOJ Actual Data -----------------------------------#
real_toj = aggregate(left_first_TF ~ soa2 + block_bias, data = toj_trials, FUN = mean)
real_toj_judgement_type = aggregate(left_first_TF ~ soa2 + toj_judgement_type, data = toj_trials, FUN = mean)
real_toj_initial_bias = aggregate(left_first_TF ~ soa2 +  probe_initial_bias, data = toj_trials, FUN = mean)
real_toj_probe_duration = aggregate(left_first_TF ~ soa2 +  onehundredms, data = toj_trials, FUN = mean)
#-------------------------------------- TOJ Actual Data -----------------------------------#


#-------------------------------------- TOJ Simulated Data --------------------------------#
# function for getting condition-wise samples from calculated mean posteriors 
get_condition_mean_sample = function(intercept, effect, add, space){
  if (add) {
    condition_mean_trans = intercept + effect/2
  } else {
    condition_mean_trans = intercept - effect/2
  }
  if (space == "log") {
    condition_mean = exp(condition_mean_trans)*250
  } else if (space == "null") {
    condition_mean = (condition_mean_trans)*250
  } else if (space == "logit") {
    condition_mean = plogis(condition_mean_trans)
  } else if (space == "log_free") {
    condition_mean = exp(condition_mean_trans)
  } else {
    print("choose 'space' argument from 'log', 'null', 'log_free'")
  }
  condition_mean_reps = sample(condition_mean, 50, replace = T)
  
  return(condition_mean_reps)
}

### Get PSS Parameters
# pss intercept mean
pss_intercept_mean = gg_toj_color_post[gg_toj_color_post$Parameter == "population_pss_intercept_mean",]$value
# pss effect mean
pss_effect_mean = gg_toj_color_post[gg_toj_color_post$Parameter == "population_pss_effect_mean",]$value
# pss judgement type effect mean
pss_judgement_type_effect_mean = gg_toj_color_post[gg_toj_color_post$Parameter == "population_pss_judgement_type_effect_mean",]$value
# pss probe initial bias effect mean
pss_initial_bias_effect_mean = gg_toj_color_post[gg_toj_color_post$Parameter == "population_pss_initial_bias_effect_mean",]$value
# pss probe duration effect mean
pss_probe_duration_effect_mean = gg_toj_color_post[gg_toj_color_post$Parameter == "population_pss_probe_effect_mean",]$value


# pss attend right
pss_right_mean_reps = get_condition_mean_sample(pss_intercept_mean, pss_effect_mean, TRUE, "null")

# pss attend left
pss_left_mean_reps = get_condition_mean_sample(pss_intercept_mean, pss_effect_mean, FALSE, "null")

# pss judgement type first
pss_first_mean_reps = get_condition_mean_sample(pss_intercept_mean, pss_judgement_type_effect_mean, FALSE, "null")

# pss judgement type second
pss_second_mean_reps = get_condition_mean_sample(pss_intercept_mean, pss_judgement_type_effect_mean, TRUE, "null")

# pss initial bias right
pss_initial_right_mean_reps = get_condition_mean_sample(pss_intercept_mean, pss_initial_bias_effect_mean, FALSE, "null")

# pss initial bias second
pss_initial_left_mean_reps = get_condition_mean_sample(pss_intercept_mean, pss_initial_bias_effect_mean, TRUE, "null")

# pss probe duration short (100 ms)
pss_short_mean_reps = get_condition_mean_sample(pss_intercept_mean, pss_probe_duration_effect_mean, FALSE, "null")

# pss probe duration long (200 ms)
pss_long_mean_reps = get_condition_mean_sample(pss_intercept_mean, pss_probe_duration_effect_mean, TRUE, "null")

### Get JND Parameters
# jnd intercept mean
jnd_intercept_mean = gg_toj_color_post[gg_toj_color_post$Parameter == "population_logjnd_intercept_mean",]$value
# jnd effect mean
jnd_effect_mean = gg_toj_color_post[gg_toj_color_post$Parameter == "population_logjnd_effect_mean",]$value
# pss judgement type effect mean
jnd_judgement_type_effect_mean = gg_toj_color_post[gg_toj_color_post$Parameter == "population_logjnd_judgement_type_effect_mean",]$value
# jnd probe initial bias effect mean
jnd_initial_bias_effect_mean = gg_toj_color_post[gg_toj_color_post$Parameter == "population_logjnd_initial_bias_effect_mean",]$value
# jnd probe duration effect mean
jnd_probe_duration_effect_mean = gg_toj_color_post[gg_toj_color_post$Parameter == "population_logjnd_probe_effect_mean",]$value

# jnd attend right
jnd_right_mean_reps = get_condition_mean_sample( jnd_intercept_mean, jnd_effect_mean, TRUE, "log")

# jnd attend left
jnd_left_mean_reps = get_condition_mean_sample( jnd_intercept_mean, jnd_effect_mean, FALSE, "log")

# jnd judgement type first
jnd_first_mean_reps = get_condition_mean_sample( jnd_intercept_mean, jnd_judgement_type_effect_mean, FALSE, "log")

# jnd judgement type second
jnd_second_mean_reps = get_condition_mean_sample( jnd_intercept_mean, jnd_judgement_type_effect_mean, TRUE, "log")

# jnd initial bias right
jnd_initial_right_mean_reps = get_condition_mean_sample( jnd_intercept_mean,  jnd_initial_bias_effect_mean, FALSE, "log")

# jnd initial bias left
jnd_initial_left_mean_reps = get_condition_mean_sample( jnd_intercept_mean,  jnd_initial_bias_effect_mean, TRUE, "log")

# jnd probe duration short (100 ms)
jnd_short_mean_reps = get_condition_mean_sample( jnd_intercept_mean,  jnd_probe_duration_effect_mean, FALSE, "log")

# jnd probe duration long (200 ms)
jnd_long_mean_reps = get_condition_mean_sample( jnd_intercept_mean,  jnd_probe_duration_effect_mean, TRUE, "log")
#-------------------------------------- TOJ Simulated Data --------------------------------#


#-------------------------------------- Do TOJ PPC ----------------------------------------#
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
#-------------------------------------- Do TOJ PPC ----------------------------------------#


#-------------------------------------- Color Actual Data ---------------------------------#
hist(color_trials[color_trials$attended == TRUE,]$color_diff_radians, breaks = 30, freq = F, col = rgb(.1,.1,.1,.5))
hist(color_trials[color_trials$attended == FALSE,]$color_diff_radians, breaks = 30, freq = F, col = rgb(.9,.9,.9,.5), add = T)
# interaction: attention and probe duration
real_color = aggregate(color_diff_radians ~ attended + onehundredms, data = color_trials, FUN  = mean)
#-------------------------------------- Color Actual Data ---------------------------------#


#-------------------------------------- Color Simulated Data ------------------------------#
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
# sample from posterior 
rho_attend_short_reps = sample(rho_attend_short, 50, replace = T)

# rho unattend with probe short 
rho_unattend_short = plogis(rho_intercept_mean - rho_probe_effect_mean/2  - (rho_effect_mean - rho_probe_interaction_effect_mean)/2)
# sample from posterior 
rho_unattend_short_reps = sample(rho_unattend_short, 50, replace = T)

# rho attend with probe long
rho_attend_long = plogis(rho_intercept_mean  + rho_probe_effect_mean/2 + (rho_effect_mean + rho_probe_interaction_effect_mean)/2)
# sample from posterior 
rho_attend_long_reps = sample(rho_attend_long, 50, replace = T)

# rho unattend with probe long
rho_unattend_long = plogis(rho_intercept_mean  + rho_probe_effect_mean/2 - (rho_effect_mean + rho_probe_interaction_effect_mean)/2)
# sample from posterior 
rho_unattend_long_reps = sample(rho_unattend_long, 50, replace = T)

### Get Kappa Parameters
kappa_intercept_mean = gg_toj_color_post[gg_toj_color_post$Parameter == "logKappaMean",]$value
# kappa effect mean
kappa_effect_mean = gg_toj_color_post[gg_toj_color_post$Parameter == "logKappaEffectMean",]$value
# kappa probe duration effect mean
kappa_probe_effect_mean = gg_toj_color_post[gg_toj_color_post$Parameter == "logKappaProbeEffectMean",]$value
# kappa probe duration interaction effect mean
kappa_probe_interaction_effect_mean = gg_toj_color_post[gg_toj_color_post$Parameter == "logKappaProbeInteractionEffectMean",]$value

# rho attend with probe short 
kappa_attend_short = exp(kappa_intercept_mean - kappa_probe_effect_mean/2  + (kappa_effect_mean - kappa_probe_interaction_effect_mean)/2)
# sample from posterior 
kappa_attend_short_reps = sample(kappa_attend_short, 50, replace = T)

# kappa unattend with probe short 
kappa_unattend_short = exp(kappa_intercept_mean - kappa_probe_effect_mean/2  - (kappa_effect_mean - kappa_probe_interaction_effect_mean)/2)
# sample from posterior 
kappa_unattend_short_reps = sample(kappa_unattend_short, 50, replace = T)

# kappa attend with probe long
kappa_attend_long = exp(kappa_intercept_mean  + kappa_probe_effect_mean/2 + (kappa_effect_mean + kappa_probe_interaction_effect_mean)/2)
# sample from posterior 
kappa_attend_long_reps = sample(kappa_attend_long, 50, replace = T)

# kappa unattend with probe long
kappa_unattend_long = exp(kappa_intercept_mean  + kappa_probe_effect_mean/2 - (kappa_effect_mean + kappa_probe_interaction_effect_mean)/2)
# sample from posterior 
kappa_unattend_long_reps = sample(kappa_unattend_long, 50, replace = T)
#-------------------------------------- Color Simulated Data ------------------------------#


#-------------------------------------- Do Color PPC --------------------------------------#
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
#-------------------------------------- Do Color PPC --------------------------------------#



############################################################################################
####                                       Analysis                                     ####
############################################################################################
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


#------------------------------------------------------------------------------------------#
#--------------------------------- Correlations -------------------------------------------#
#------------------------------------------------------------------------------------------#
## ORDER:
# (1) population_pss_intercept_mean      
# (2) population_pss_effect_mean          
# (3) population_logjnd_intercept_mean    
# (4) population_logjnd_effect_mean     
# (5) logitRhoMean                         
# (6) logKappaMean                        
# (7) logitRhoEffectMean                 
# (8) logKappaEffectMean     

# NOTE: for quick look
# not necessarily HDI
ggs_caterpillar(gg_toj_color_post, family = "cor", thick_ci = c(0.25, 0.75) ) + geom_vline(xintercept = 0, col = "red")


#-------------------------------------- Get Betas -----------------------------------------#
# extract samples
detach('package:rstan', unload = T)  # to ensure 
library(rstan)
ex_toj_color_post = extract(toj_color_post)

# for violin plots later
pos_corr2 = data.frame(value = ex_toj_color_post$cor)
pos_corr2$id = rownames(pos_corr2)
pos_corr = melt( pos_corr2 )
names(pos_corr)[2] = c("parameter")

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
betas$participant = rep(c(1:26), times = 8, each = nrow(betas2))
#-------------------------------------- Get Betas -----------------------------------------#


#---------------------------- Rho vs. PSS Effects -----------------------------------------#
psseffect2 = data.frame(value = ex_toj_color_post$population_pss_effect_mean)
psseffect2$iteration = rownames(psseffect2)
psseffect = melt( psseffect2 )$value

psseffectsd2 = data.frame(value = tan(ex_toj_color_post$zpopulation_pss_effect_sd))
psseffectsd2$iteration = rownames(psseffectsd2)
psseffectsd = melt( psseffectsd2 )$value

pssinteraction2 = data.frame(value = ex_toj_color_post$population_pss_probe_interaction_effect_mean)
pssinteraction2$iteration = rownames(pssinteraction2)
pssinteraction = melt(pssinteraction2)$value

probefactor = ifelse(aggregate(onehundredms ~ id, data = color_trials, unique)$onehundred, -1, 1)

psseffect_ids = ddply(
  .data = betas
  , .variables = .(participant)
  , .fun = function(x){
    i = unique(x$participant)
    x_use = x[x$parameter ==  "population_pss_effect_mean",]$value
    psseffect = (median(psseffect)  + median(psseffectsd)*median(x_use) + median(pssinteraction)*probefactor[i])/2
    df = data.frame(psseffect, probefactor[i])
    names(df) = c("psseffect", "probefactor")
    return(df)
  }
)

logitrhomean2 = data.frame(value = ex_toj_color_post$logitRhoMean)
logitrhomean2$iteration = rownames(logitrhomean2)
logitrhomean = melt( logitrhomean2 )$value

logitrhosd2 = data.frame(value = tan(ex_toj_color_post$zlogitRhoSD))
logitrhosd2$iteration = rownames(logitrhosd2)
logitrhosd = melt( logitrhosd2 )$value

logitrhoeffect2 = data.frame(value = ex_toj_color_post$logitRhoEffectMean)
logitrhoeffect2$iteration = rownames(logitrhoeffect2)
logitrhoeffect = melt( logitrhoeffect2 )$value

logitrhoeffectsd2 = data.frame(value = tan(ex_toj_color_post$zlogitRhoEffectSD))
logitrhoeffectsd2$iteration = rownames(logitrhoeffectsd2)
logitrhoeffectsd = melt( logitrhoeffectsd2 )$value

logitrhoprobeeffect2 = data.frame(value = ex_toj_color_post$logitRhoProbeEffectMean)
logitrhoprobeeffect2$iteration = rownames(logitrhoprobeeffect2)
logitrhoprobeeffect = melt( logitrhoprobeeffect2 )$value

logitrhointeractioneffect2 = data.frame(value = ex_toj_color_post$logitRhoProbeInteractionEffectMean)
logitrhointeractioneffect2$iteration = rownames(logitrhointeractioneffect2)
logitrhointeractioneffect = melt( logitrhointeractioneffect2 )$value

rhoeffect_ids = ddply(
  .data = betas
  , .variables = .(participant)
  , .fun = function(x){
    i = unique(x$participant)
    x_use = x[x$parameter == "logitRhoEffectMean",]$value
    logitrhoeffect_use =  median(logitrhoeffect) + median(logitrhoeffectsd)*median(x_use)  +median(logitrhointeractioneffect)*probefactor[i]
    df = data.frame(logitrhoeffect_use, probefactor[i])
    names(df) = c("logitrhoeffect", "probefactor")
    return(df)
  }
)

# psseffect_v_rhoeffect2 = merge(rhoeffect_ids, psseffect_ids)

# # get rid of outliers
# outlier_points = psseffect_v_rhoeffect2[psseffect_v_rhoeffect2$logitrhoeffect > 0.875
#                                         | psseffect_v_rhoeffect2$logitrhoeffect < 0.125
#                                         ,]
# 
# psseffect_v_rhoeffect = psseffect_v_rhoeffect2[!(psseffect_v_rhoeffect2$logitrhoeffect %in% outlier_points$logitrhoeffect),]

psseffect_v_rhoeffect = merge(rhoeffect_ids, psseffect_ids)

# plot
ggplot(data = psseffect_v_rhoeffect, aes(y =psseffect, x = logitrhoeffect, colour = factor(probefactor), shape = factor(probefactor)))+
  scale_y_continuous(name = "Half PSS Effect Mean (Normalized)")+
  scale_x_continuous(name = "Logit \u03C1 Effect Mean")+
  geom_point(size = 3)+
  # geom_smooth(method = "lm", se = FALSE, size = 1)+
  geom_smooth(
    data = psseffect_v_rhoeffect[psseffect_v_rhoeffect$probefactor == +1,]
    , aes(y = psseffect, x = logitrhoeffect)
    , method = "lm", se = FALSE, size = 1, linetype = "dotted")+
  geom_smooth(
    data = psseffect_v_rhoeffect[psseffect_v_rhoeffect$probefactor == -1,]
    , aes(y = psseffect, x = logitrhoeffect)
    , method = "lm", se = FALSE, size = 1, linetype = "dotted")+
  geom_vline(xintercept = 0, linetype = 2, size = 1)+
  geom_hline(yintercept = 0, linetype = 2, size = 1)+
#   geom_point(data = outlier_points, aes(y = psseffect, x = logitrhoeffect), size = 3)+
#   geom_point(data = outlier_points, aes(y = psseffect, x = logitrhoeffect), size = 1.5, colour = "grey90")+
  # annotate("text", label = paste("r=",cor_plot), x = 0.625, y = -0.15, size = 8)+
  theme_gray(base_size = 30)+
  theme(panel.grid.major = element_line(size = 1.5)
        ,panel.grid.minor = element_line(size = 1)
        , legend.position = "none")

### Violin
ggplot(
  data = pos_corr[pos_corr$parameter == "value.2.7",]
  , aes(x = parameter, y = value)
)+
  geom_violin()+
  labs(x = "Logit \u03C1 vs. PSS Effect Means", y = "Correlation Coefficient (r)")+
  stat_summary(fun.data = get_95_HDI, size = 0.7)+
  stat_summary(fun.data = get_50_HDI, size = 2.5)+  
  geom_hline(yintercept = 0, linetype = 2, size = 1)+
  theme_gray(base_size = 30)+
  theme(panel.grid.major = element_line(size = 1.5)
        ,panel.grid.minor = element_line(size = 1)
        , axis.text.x = element_blank()
        , axis.ticks.x = element_blank()) 

# get interval
get_95_HDI( pos_corr[pos_corr$parameter == "value.2.7",]$value)
#---------------------------- Rho vs. PSS Effects -----------------------------------------#



#---------------------------- Kappa vs. PSS Effects ---------------------------------------#
logkappamean2 = data.frame(value = ex_toj_color_post$logKappaMean)
logkappamean2$iteration = rownames(logkappamean2)
logkappamean = melt( logkappamean2 )$value

logkappasd2 = data.frame(value = tan(ex_toj_color_post$zlogKappaSD))
logkappasd2$iteration = rownames(logkappasd2)
logkappasd = melt( logkappasd2 )$value

logkappaeffect2 = data.frame(value = ex_toj_color_post$logKappaEffectMean)
logkappaeffect2$iteration = rownames(logkappaeffect2)
logkappaeffect = melt( logkappaeffect2 )$value

logkappaeffectsd2 = data.frame(value = tan(ex_toj_color_post$zlogKappaEffectSD))
logkappaeffectsd2$iteration = rownames(logkappaeffectsd2)
logkappaeffectsd = melt( logkappaeffectsd2 )$value

logkappaprobeeffect2 = data.frame(value = ex_toj_color_post$logKappaProbeEffectMean)
logkappaprobeeffect2$iteration = rownames(logkappaprobeeffect2)
logkappaprobeeffect = melt( logkappaprobeeffect2 )$value

logkappainteractioneffect2 = data.frame(value = ex_toj_color_post$logKappaProbeInteractionEffectMean)
logkappainteractioneffect2$iteration = rownames(logkappainteractioneffect2)
logkappainteractioneffect = melt( logkappainteractioneffect2 )$value

kappaeffect_ids = ddply(
  .data = betas
  , .variables = .(participant)
  , .fun = function(x){
    i = unique(x$participant)
    x_use = x[x$parameter == "logKappaEffectMean",]$value
    logkappaeffect_use =  median(logkappaeffect) + median(logkappaeffectsd)*median(x_use)  +median(logkappainteractioneffect)*probefactor[i]
    df = data.frame(logkappaeffect_use, probefactor[i])
    names(df) = c("logkappaeffect","probefactor")
    return(df)
  }
)

psseffect_v_kappaeffect2 = merge(kappaeffect_ids, psseffect_ids)

# get rid of outliers
outlier_points = psseffect_v_kappaeffect2[psseffect_v_kappaeffect2$psseffect > 0.035
                                        ,]

psseffect_v_kappaeffect = psseffect_v_kappaeffect2[!(psseffect_v_kappaeffect2$logkappaeffect %in% outlier_points$logkappaeffect),]

# plot
ggplot(data = psseffect_v_kappaeffect, aes(y =psseffect, x = logkappaeffect, colour = probefactor))+
  scale_y_continuous(name = "Half PSS Effect Mean (Normalized)")+
  scale_x_continuous(name = "Log \u03BA Effect Mean")+
  geom_point(size = 3)+
  # geom_smooth(method = "lm", se = FALSE, size = 1)+
    geom_smooth(
      data = psseffect_v_kappaeffect[psseffect_v_kappaeffect$probefactor == +1,]
      , aes(y = psseffect, x = logkappaeffect)
      , method = "lm", se = FALSE, size = 1, linetype = "dotted")+
  geom_smooth(
    data = psseffect_v_kappaeffect[psseffect_v_kappaeffect$probefactor == -1,]
    , aes(y = psseffect, x = logkappaeffect)
    , method = "lm", se = FALSE, size = 1, linetype = "dotted")+
  geom_hline(yintercept = 0, linetype = 2, size = 1)+
  geom_vline(xintercept = 0, linetype = 2, size = 1)+
    geom_point(data = outlier_points, aes(y = psseffect, x = logkappaeffect), size = 3)+
    geom_point(data = outlier_points, aes(y = psseffect, x = logkappaeffect), size = 1.5, colour = "grey90")+
  # annotate("text", label = paste("r=",cor_plot), x = 0.625, y = -0.15, size = 8)+
  theme_gray(base_size = 30)+
  theme(panel.grid.major = element_line(size = 1.5)
        ,panel.grid.minor = element_line(size = 1)
        , legend.position = "none")

### Violin
ggplot(
  data = pos_corr[pos_corr$parameter == "value.2.8",]
  , aes(x = parameter, y = value)
)+
  geom_violin()+
  labs(x = "Log \u03BA vs. PSS Effect Means", y = "Correlation Coefficient (r)")+
  stat_summary(fun.data = get_95_HDI, size = 0.7)+
  stat_summary(fun.data = get_50_HDI, size = 2.5)+  
  geom_hline(yintercept = 0, linetype = 2, size = 1)+
  theme_gray(base_size = 30)+
  theme(panel.grid.major = element_line(size = 1.5)
        ,panel.grid.minor = element_line(size = 1)
        , axis.text.x = element_blank()
        , axis.ticks.x = element_blank()) 

# get interval
get_95_HDI( pos_corr[pos_corr$parameter == "value.2.8",]$value)
#---------------------------- Kappa vs. PSS Effects ---------------------------------------#




#------------------------------------------------------------------------------------------#
#--------------------------------- Parameters ---------------------------------------------#
#------------------------------------------------------------------------------------------#

#---------------------------------- SOA Scale ---------------------------------------------#
### Intercepts
pos_SOA_scale = data.frame(  
  value = c(
    ex_toj_color_post$population_pss_intercept_mean * 250
    , exp( ex_toj_color_post$population_logjnd_intercept_mean ) * 250
  )
  , parameter = c(
    rep("PSS Intercept Mean", 80000)  
    , rep("JND Intercept Mean", 80000)  
  )
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
# jnd intercept 
get_95_HDI(exp( ex_toj_color_post$population_logjnd_intercept_mean ) * 250)

### Effects 
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
  labs(x = "", y = "SOA (Right - Left; ms)")+
  #  scale_x_discrete(labels = c("PSS Effect Mean", "JND Effect Mean"))+
  geom_hline(yintercept = 0, linetype = 2, size = 1)+
  theme_gray(base_size = 30)+
  theme(panel.grid.major = element_line(size = 1.5)
        ,panel.grid.minor = element_line(size = 1)
        , axis.ticks.x = element_blank()) 

# 95% HDIs
# pss effect 
get_95_HDI(ex_toj_color_post$population_pss_effect_mean*250 )  
# jnd effect 
get_95_HDI(
  ( exp( ex_toj_color_post$population_logjnd_intercept_mean + ex_toj_color_post$population_logjnd_effect_mean/2 )
    - exp( ex_toj_color_post$population_logjnd_intercept_mean - ex_toj_color_post$population_logjnd_effect_mean/2  ) ) * 250 
)

### Judgement Type Effect 
pos_SOA_scale_judgement_type = data.frame(  
  effect = c(
    ( (ex_toj_color_post$population_pss_intercept_mean + ex_toj_color_post$population_pss_judgement_type_effect_mean/2) 
      - (ex_toj_color_post$population_pss_intercept_mean - ex_toj_color_post$population_pss_judgement_type_effect_mean/2) ) * 250
    , ( exp( ex_toj_color_post$population_logjnd_intercept_mean + ex_toj_color_post$population_logjnd_judgement_type_effect_mean/2 )
        - exp( ex_toj_color_post$population_logjnd_intercept_mean - ex_toj_color_post$population_logjnd_judgement_type_effect_mean/2  ) ) * 250 
  )
  , parameter = c(
    rep("PSS Judgement Type Effect Mean", 80000)
    , rep("JND Judgement Type Effect Mean", 80000)
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

### Initial Bias 
pos_SOA_scale_initial_bias = data.frame(  
  effect = c(
    ( (ex_toj_color_post$population_pss_intercept_mean + ex_toj_color_post$population_pss_initial_bias_effect_mean/2) 
      - (ex_toj_color_post$population_pss_intercept_mean - ex_toj_color_post$population_pss_initial_bias_effect_mean/2) ) * 250
    , ( exp( ex_toj_color_post$population_logjnd_intercept_mean + ex_toj_color_post$population_logjnd_initial_bias_effect_mean/2 )
        - exp( ex_toj_color_post$population_logjnd_intercept_mean - ex_toj_color_post$population_logjnd_initial_bias_effect_mean/2  ) ) * 250 
  )
  , parameter = c(
    rep("PSS Initial Probe Bias Effect Mean", 80000)
    , rep("JND Initial Probe Bias Effect Mean", 80000)
  )
)

ggplot(data = pos_SOA_scale_initial_bias, aes(x = parameter, y = effect))+
  geom_violin()+
  stat_summary(fun.data = get_95_HDI, size = 0.7)+
  stat_summary(fun.data = get_50_HDI, size = 2.5)+
  labs(x = "", y = "SOA (Left - Right; ms)")+
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

### Probe Duration 
pos_SOA_scale_probe = data.frame(  
  effect = c(
    ( (ex_toj_color_post$population_pss_intercept_mean + ex_toj_color_post$population_pss_probe_effect_mean/2) 
      - (ex_toj_color_post$population_pss_intercept_mean - ex_toj_color_post$population_pss_probe_effect_mean/2) ) * 250
    , ( exp( ex_toj_color_post$population_logjnd_intercept_mean + ex_toj_color_post$population_logjnd_probe_effect_mean/2 )
        - exp( ex_toj_color_post$population_logjnd_intercept_mean - ex_toj_color_post$population_logjnd_probe_effect_mean/2  ) ) * 250 
  )
  , parameter = c(
    rep("PSS Probe Duration Bias Effect Mean", 80000)
    , rep("JND Probe Duration Bias Effect Mean", 80000)
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

### PSS Interaction
pos_SOA_interaction = data.frame(  
  effect = c(
    (ex_toj_color_post$population_pss_probe_interaction_effect_mean) * 250
  )
  , parameter = c(
    rep("PSS Interaction Effect Mean", 80000)
  )
)

ggplot(data = pos_SOA_interaction, aes(x = parameter, y = effect))+
  geom_violin()+
  stat_summary(fun.data = get_95_HDI, size = 0.7)+
  stat_summary(fun.data = get_50_HDI, size = 2.5)+
  labs(x = "", y = "SOA (ms)")+
  geom_hline(yintercept = 0, linetype = 2, size = 1)+
  theme_gray(base_size = 30)+
  theme(panel.grid.major = element_line(size = 1.5)
        ,panel.grid.minor = element_line(size = 1)
        , axis.ticks.x = element_blank()) 

# 95% HDIs
# interaction effect
get_95_HDI(
  (ex_toj_color_post$population_pss_probe_interaction_effect_mean) * 250
)

### JND Interaction
pos_SOA_jnd_interaction = data.frame(  
  effect = c(
    (ex_toj_color_post$population_logjnd_probe_interaction_effect_mean) * 250
  )
  , parameter = c(
    rep("JND Interaction Effect Mean", 80000)
  )
)

ggplot(data = pos_SOA_jnd_interaction, aes(x = parameter, y = effect))+
  geom_violin()+
  stat_summary(fun.data = get_95_HDI, size = 0.7)+
  stat_summary(fun.data = get_50_HDI, size = 2.5)+
  labs(x = "", y = "SOA (ms)")+
  geom_hline(yintercept = 0, linetype = 2, size = 1)+
  theme_gray(base_size = 30)+
  theme(panel.grid.major = element_line(size = 1.5)
        ,panel.grid.minor = element_line(size = 1)
        , axis.ticks.x = element_blank()) 

# 95% HDIs
# interaction effect
get_95_HDI(
  (ex_toj_color_post$population_logjnd_probe_interaction_effect_mean) * 250
)


### INTUITIVE PSS Interaction
pos_SOA_interaction_effects = data.frame(  
  effect = c(
    ( (ex_toj_color_post$population_pss_intercept_mean + (ex_toj_color_post$population_pss_effect_mean + ex_toj_color_post$population_pss_probe_interaction_effect_mean)/2) 
      - (ex_toj_color_post$population_pss_intercept_mean - (ex_toj_color_post$population_pss_effect_mean + ex_toj_color_post$population_pss_probe_interaction_effect_mean)/2 ) ) * 250
    , ( (ex_toj_color_post$population_pss_intercept_mean + (ex_toj_color_post$population_pss_effect_mean - ex_toj_color_post$population_pss_probe_interaction_effect_mean)/2) 
        - (ex_toj_color_post$population_pss_intercept_mean - (ex_toj_color_post$population_pss_effect_mean - ex_toj_color_post$population_pss_probe_interaction_effect_mean)/2 ) ) * 250
  )
  , parameter = c(
    rep("PSS Attention Effect Long", 80000)
    , rep("PSS Attention Effect Short", 80000)
  )
)

ggplot(data = pos_SOA_interaction_effects, aes(x = parameter, y = effect))+
  geom_violin()+
  stat_summary(fun.data = get_95_HDI, size = 0.7)+
  stat_summary(fun.data = get_50_HDI, size = 2.5)+
  labs(x = "", y = "SOA (Attended - Unattended; ms)")+
  scale_x_discrete(labels = c("PSS Attention Effect\nGiven Long\nProbe Duration", "PSS Attention Effect\nGiven Short\nProbe Duration"))+
  geom_hline(yintercept = 0, linetype = 2, size = 1)+
  theme_gray(base_size = 30)+
  theme(panel.grid.major = element_line(size = 1.5)
        ,panel.grid.minor = element_line(size = 1)
        , axis.ticks.x = element_blank()) 

# 95% HDIs
# long effect
get_95_HDI(
  ( (ex_toj_color_post$population_pss_intercept_mean + (ex_toj_color_post$population_pss_effect_mean + ex_toj_color_post$population_pss_probe_interaction_effect_mean)/2) 
             - (ex_toj_color_post$population_pss_intercept_mean - (ex_toj_color_post$population_pss_effect_mean + ex_toj_color_post$population_pss_probe_interaction_effect_mean)/2 ) ) * 250
)  
# short effect 
get_95_HDI(
  ( (ex_toj_color_post$population_pss_intercept_mean + (ex_toj_color_post$population_pss_effect_mean - ex_toj_color_post$population_pss_probe_interaction_effect_mean)/2) 
    - (ex_toj_color_post$population_pss_intercept_mean - (ex_toj_color_post$population_pss_effect_mean - ex_toj_color_post$population_pss_probe_interaction_effect_mean)/2 ) ) * 250
)
#---------------------------------- SOA Scale ---------------------------------------------#


#---------------------------------- Rho Scale ---------------------------------------------#
### Intercepts
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

### Effects
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

### Probe effect
pos_rho_scale_probe_effects = data.frame(  
  effect = c(
    ( plogis(ex_toj_color_post$logitRhoMean + ex_toj_color_post$logitRhoProbeEffectMean/2 )
      - plogis(ex_toj_color_post$logitRhoMean - ex_toj_color_post$logitRhoProbeEffectMean/2 ) )
  )
  , parameter = c(
    rep("rhoProbeEffectMean", 80000)
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

### Interaction effect
pos_rho_scale_interaction_effects = data.frame(  
  effect = c(
    ( plogis(ex_toj_color_post$logitRhoMean + ex_toj_color_post$logitRhoProbeInteractionEffectMean/2 )
      - plogis(ex_toj_color_post$logitRhoMean - ex_toj_color_post$logitRhoProbeInteractionEffectMean/2 ) )
  )
  , parameter = c(
    rep("rhoInteractionEffectMean", 80000)
  )
)

ggplot(data = pos_rho_scale_interaction_effects, aes(x = parameter, y = effect))+
  geom_violin()+
#   labs(x = "", y = "\u03C1 (Long & Attended or Short & Unattended\n- Short & Attended or Long & Unattended)")+
  labs(x = "", y = "\u03C1")+
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
    rep("Rho Attention Effect Given Long", 80000)
    , rep("Rho Attention Effect Given Short", 80000)
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
#---------------------------------- Rho Scale ---------------------------------------------#


#---------------------------------- Kappa Scale -------------------------------------------#
### Intercept
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

### Effects
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

### Probe effect
pos_kappa_scale_probe_effects = data.frame(  
  effect = c(
    ( exp(ex_toj_color_post$logKappaMean + ex_toj_color_post$logKappaProbeEffectMean/2 )
      - exp(ex_toj_color_post$logKappaMean - ex_toj_color_post$logKappaProbeEffectMean/2 ) )
  )
  , parameter = c(
    rep("kappaProbeEffectMean", 80000)
  )
)

ggplot(data = pos_kappa_scale_probe_effects, aes(x = parameter, y = effect))+
  geom_violin()+
  labs(x = "", y = "\u03BA (Long - Short)")+
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

### Interaction effect
pos_kappa_scale_interaction_effects = data.frame(  
  effect = c(
    ( exp(ex_toj_color_post$logKappaMean + ex_toj_color_post$logKappaProbeInteractionEffectMean/2 )
      - exp(ex_toj_color_post$logKappaMean - ex_toj_color_post$logKappaProbeInteractionEffectMean/2 ) )
  )
  , parameter = c(
    rep("kappaInteractionEffectMean", 80000)
  )
)

ggplot(data = pos_kappa_scale_interaction_effects, aes(x = parameter, y = effect))+
  geom_violin()+
#   labs(x = "", y = "\u03BA (Long & Attended or Short & Unattended\n- Short & Attended or Long & Unattended)")+
  labs(x = "", y = "\u03BA")+
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
pos_kappa_interaction = data.frame(  
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
    rep("kappa Attention Effect Given Long", 80000)
    , rep("kappa Attention Effect Given Short", 80000)
  )
)

ggplot(data = pos_kappa_interaction, aes(x = parameter, y = effect))+
  geom_violin()+
  labs(x = "", y = "\u03BA (Attended - Unattended)")+
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




#------------------------------------------------------------------------------------------#
#--------------------------------- Graphs -------------------------------------------------#
#------------------------------------------------------------------------------------------#

#---------------------------------- NCFs --------------------------------------------------#
# including effects 
yLeft = pnorm(
  -250:250
  , mean = ( median(ex_toj_color_post$population_pss_intercept_mean) - median(ex_toj_color_post$population_pss_effect_mean)/2 ) * 250
  , sd = ( exp( median(ex_toj_color_post$population_logjnd_intercept_mean) - median(ex_toj_color_post$population_logjnd_effect_mean)/2 )   ) * 250
)
yRight= pnorm(
  -250:250
  , mean = ( median(ex_toj_color_post$population_pss_intercept_mean) + median(ex_toj_color_post$population_pss_effect_mean)/2 ) * 250
  , sd = ( exp( median(ex_toj_color_post$population_logjnd_intercept_mean) + median(ex_toj_color_post$population_logjnd_effect_mean)/2 )   ) * 250
  
)
df = data.frame(SOA = -250:250, Prop = c(yRight, yLeft), Attend = c(rep("Right",501), rep("Left", 501)))


gg = ggplot(data = df, aes(y = Prop, x = SOA, colour = Attend))+
  geom_line(size = 1.25)+
  # scale_color_manual("Attend", values = c("red", "blue"))+
  scale_color_hue("Attend", l = c(60, 15), c = c(100, 50), h = c(240, 360) ) +
  labs(x = "SOA (ms)", y = "Proportion of 'Left' Responses")+
  theme_gray(base_size = 24)+
  theme(panel.grid.major = element_line(size = 1.5)
        ,panel.grid.minor = element_line(size = 1))
# define text to add
Text1 = textGrob(label = paste("Right"), gp = gpar(fontsize= 24))
Text2 = textGrob(label = paste("Left"), gp = gpar(fontsize= 24)) 
gg = gg+
  annotation_custom(grob = Text1,  xmin = -200, xmax = -200, ymin = -0.115, ymax = -0.115)+
  annotation_custom(grob = Text2,  xmin = 200, xmax = 200, ymin = -0.115, ymax = -0.115)
# Code to override clipping
gg2 <- ggplot_gtable(ggplot_build(gg))
gg2$layout$clip[gg2$layout$name=="panel"] <- "off"
grid.draw(gg2)
#---------------------------------- NCFs --------------------------------------------------#


#---------------------------------- Posteriors --------------------------------------------#
### Rho
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
  , c(rep("AttendedLong",80000), rep("UnattendedLong",80000), rep("AttendedShort",80000), rep("UnattendedShort",80000))
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

### Kappa
pos_kappaMean_WithEffect = data.frame(
  c( 
    plogis(ex_toj_color_post$logKappaMean + ex_toj_color_post$logKappaProbeEffectMean/2
           + (ex_toj_color_post$logKappaEffectMean +  ex_toj_color_post$logKappaProbeInteractionEffectMean)/2)
    ,  plogis(ex_toj_color_post$logKappaMean + ex_toj_color_post$logKappaProbeEffectMean/2
              - (ex_toj_color_post$logKappaEffectMean +  ex_toj_color_post$logKappaProbeInteractionEffectMean)/2)
    ,  plogis(ex_toj_color_post$logKappaMean - ex_toj_color_post$logKappaProbeEffectMean/2
              + (ex_toj_color_post$logKappaEffectMean -  ex_toj_color_post$logKappaProbeInteractionEffectMean)/2)
    ,  plogis(ex_toj_color_post$logKappaMean - ex_toj_color_post$logKappaProbeEffectMean/2
              - (ex_toj_color_post$logKappaEffectMean -  ex_toj_color_post$logKappaProbeInteractionEffectMean)/2)
  ) 
  , c(rep("AttendedLong",80000), rep("UnattendedLong",80000), rep("AttendedShort",80000), rep("UnattendedShort",80000))
)
names(pos_kappaMean_WithEffect) = c("kappaMean", "Effect")


# overlapping
ggplot(pos_rhoMean_WithEffect, aes(x = kappaMean, ..density.., fill = Effect))+
  geom_density(data = pos_kappaMean_WithEffect[pos_kappaMean_WithEffect$Effect == "AttendedLong",],alpha = 0.5)+
  geom_density(data = pos_kappaMean_WithEffect[pos_kappaMean_WithEffect$Effect == "UnattendedLong",],alpha = 0.5)+
  geom_density(data = pos_kappaMean_WithEffect[pos_kappaMean_WithEffect$Effect == "AttendedShort",],alpha = 0.5)+
  geom_density(data = pos_kappaMean_WithEffect[pos_kappaMean_WithEffect$Effect == "UnattendedShort",],alpha = 0.5)+
  scale_fill_hue("Effect", l = c(90, 45, 70, 30) , c = c(100, 50, 100, 50) ) +
  labs(x = "Fidelity of Memory Population Mean", y = "Density", colour = "")+
  theme_gray(base_size = 24)+
  theme(panel.grid.major = element_line(size = 1.5)
        ,panel.grid.minor = element_line(size = 1))
#---------------------------------- Posteriors --------------------------------------------#





