# library(shinystan)
library(coda)
library(ggplot2)
library(ggmcmc)
library(CircStats)
library(grid)
library(sprintfr)
library(plyr)

setwd("~/Documents/TOJ/Follow-Up")
load("FollowUptoj_color_post_June17th2016")
load("FollowUp_color_trials.Rdata")
load("FollowUp_toj_trials.Rdata")
source("../EndogenousVisualPriorEntry-BayesianHierarchicalModel/functions.R")


############################################################################################
####                                        Diagnostics                                 ####
############################################################################################
# convert stanfit sample to dataframe table 
gg_toj_color_post = ggs(toj_color_post)

# list of parameters to examine
param_list = c("logitRhoEffectMean"
               , "logitRhoMean"
               , "logitRhoProbeEffectMean"
               , "logitRhoProbeInteractionEffectMean"
               , "logitRhoJudgementTypeEffectMean"
               , "logitRhoJudgementTypeInteractionEffectMean"
               , "logitRhoInitialBiasEffectMean"
               , "logitRhoInitialBiasInteractionEffectMean"                 
               , "logKappaEffectMean"
               , "logKappaMean"
               , "logKappaProbeEffectMean"
               , "logKappaProbeInteractionEffectMean"
               , "logKappaJudgementTypeEffectMean"
               , "logKappaJudgementTypeInteractionEffectMean"
               , "logKappaInitialBiasEffectMean"
               , "logKappaInitialBiasInteractionEffectMean"
               , "population_logjnd_effect_mean"
               , "population_logjnd_initial_bias_effect_mean"
               , "population_logjnd_initial_bias_interaction_effect_mean"
               , "population_logjnd_judgement_type_effect_mean"
               , "population_logjnd_judgement_type_interaction_effect_mean"
               , "population_logjnd_probe_effect_mean"
               , "population_logjnd_probe_interaction_effect_mean"
               , "population_logjnd_intercept_mean"
               , "population_pss_effect_mean"
               , "population_pss_initial_bias_effect_mean"
               , "population_pss_initial_bias_interaction_effect_mean"
               , "population_pss_judgement_type_effect_mean"
               , "population_pss_judgement_type_interaction_effect_mean"
               , "population_pss_probe_effect_mean" 
               , "population_pss_probe_interaction_effect_mean" 
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
real_toj_judgement_type = aggregate(left_first_TF ~ soa2 + block_bias + toj_judgement_type, data = toj_trials, FUN = mean)
real_toj_judgement_type[,2] = as.character(real_toj_judgement_type[,2])
real_toj_judgement_type[,3] = as.character(real_toj_judgement_type[,3])

real_toj_initial_bias = aggregate(left_first_TF ~ soa2 + block_bias + probe_initial_bias, data = toj_trials, FUN = mean)
real_toj_initial_bias[,2] = as.character(real_toj_initial_bias[,2])
real_toj_initial_bias[,3] = as.character(real_toj_initial_bias[,3])

real_toj_probe_duration = aggregate(left_first_TF ~ soa2 + block_bias + onehundredms, data = toj_trials, FUN = mean)
real_toj_probe_duration[,2] = as.character(real_toj_probe_duration[,2])
real_toj_probe_duration[,3] = as.character(real_toj_probe_duration[,3])
#-------------------------------------- TOJ Actual Data -----------------------------------#


#-------------------------------------- TOJ Simulated Data --------------------------------#
### Get PSS Parameters
pss_intercept_mean = gg_toj_color_post[gg_toj_color_post$Parameter == "population_pss_intercept_mean",]$value
pss_effect_mean = gg_toj_color_post[gg_toj_color_post$Parameter == "population_pss_effect_mean",]$value

pss_judgement_type_effect_mean = gg_toj_color_post[gg_toj_color_post$Parameter == "population_pss_judgement_type_effect_mean",]$value
pss_judgement_type_interaction_effect_mean = gg_toj_color_post[gg_toj_color_post$Parameter == "population_pss_judgement_type_interaction_effect_mean",]$value

pss_initial_bias_effect_mean = gg_toj_color_post[gg_toj_color_post$Parameter == "population_pss_initial_bias_effect_mean",]$value
pss_initial_bias_interaction_effect_mean = gg_toj_color_post[gg_toj_color_post$Parameter == "population_pss_initial_bias_interaction_effect_mean",]$value

pss_probe_duration_effect_mean = gg_toj_color_post[gg_toj_color_post$Parameter == "population_pss_probe_effect_mean",]$value
pss_probe_duration_interaction_effect_mean = gg_toj_color_post[gg_toj_color_post$Parameter == "population_pss_probe_interaction_effect_mean",]$value

# judgement type
pss_right_first_mean_reps = get_condition_mean_sample(
  pss_intercept_mean - pss_judgement_type_effect_mean/2
  , (pss_effect_mean - pss_judgement_type_interaction_effect_mean )
  , TRUE
  , "null"
)

pss_right_second_mean_reps = get_condition_mean_sample(
  pss_intercept_mean + pss_judgement_type_effect_mean/2
  , (pss_effect_mean + pss_judgement_type_interaction_effect_mean )
  , TRUE
  , "null"
)

pss_left_first_mean_reps = get_condition_mean_sample(
  pss_intercept_mean - pss_judgement_type_effect_mean/2
  , (pss_effect_mean - pss_judgement_type_interaction_effect_mean )
  , FALSE
  , "null"
)

pss_left_second_mean_reps = get_condition_mean_sample(
  pss_intercept_mean + pss_judgement_type_effect_mean/2
  , (pss_effect_mean + pss_judgement_type_interaction_effect_mean )
  , FALSE
  , "null"
)

# initial bias
pss_right_right_mean_reps = get_condition_mean_sample(
  pss_intercept_mean - pss_initial_bias_effect_mean/2
  , (pss_effect_mean - pss_initial_bias_interaction_effect_mean )
  , TRUE
  , "null"
)

pss_right_left_mean_reps = get_condition_mean_sample(
  pss_intercept_mean + pss_initial_bias_effect_mean/2
  , (pss_effect_mean + pss_initial_bias_interaction_effect_mean )
  , TRUE
  , "null"
)

pss_left_right_mean_reps = get_condition_mean_sample(
  pss_intercept_mean - pss_initial_bias_effect_mean/2
  , (pss_effect_mean - pss_initial_bias_interaction_effect_mean )
  , FALSE
  , "null"
)

pss_left_left_mean_reps = get_condition_mean_sample(
  pss_intercept_mean + pss_initial_bias_effect_mean/2
  , (pss_effect_mean + pss_initial_bias_interaction_effect_mean )
  , FALSE
  , "null"
)

# probe duration
pss_right_short_mean_reps = get_condition_mean_sample(
  pss_intercept_mean - pss_probe_duration_effect_mean/2
  , (pss_effect_mean - pss_probe_duration_interaction_effect_mean )
  , TRUE
  , "null"
)

pss_right_long_mean_reps = get_condition_mean_sample(
  pss_intercept_mean + pss_probe_duration_effect_mean/2
  , (pss_effect_mean + pss_probe_duration_interaction_effect_mean )
  , TRUE
  , "null"
)

pss_left_short_mean_reps = get_condition_mean_sample(
  pss_intercept_mean - pss_probe_duration_effect_mean/2
  , (pss_effect_mean - pss_probe_duration_interaction_effect_mean )
  , FALSE
  , "null"
)

pss_left_long_mean_reps = get_condition_mean_sample(
  pss_intercept_mean + pss_probe_duration_effect_mean/2
  , (pss_effect_mean + pss_probe_duration_interaction_effect_mean )
  , FALSE
  , "null"
)

### Get JND Parameters
logjnd_intercept_mean = gg_toj_color_post[gg_toj_color_post$Parameter == "population_logjnd_intercept_mean",]$value
logjnd_effect_mean = gg_toj_color_post[gg_toj_color_post$Parameter == "population_logjnd_effect_mean",]$value

logjnd_judgement_type_effect_mean = gg_toj_color_post[gg_toj_color_post$Parameter == "population_logjnd_judgement_type_effect_mean",]$value
logjnd_judgement_type_interaction_effect_mean = gg_toj_color_post[gg_toj_color_post$Parameter == "population_logjnd_judgement_type_interaction_effect_mean",]$value

logjnd_initial_bias_effect_mean = gg_toj_color_post[gg_toj_color_post$Parameter == "population_logjnd_initial_bias_effect_mean",]$value
logjnd_initial_bias_interaction_effect_mean = gg_toj_color_post[gg_toj_color_post$Parameter == "population_logjnd_initial_bias_interaction_effect_mean",]$value

logjnd_probe_duration_effect_mean = gg_toj_color_post[gg_toj_color_post$Parameter == "population_logjnd_probe_effect_mean",]$value
logjnd_probe_duration_interaction_effect_mean = gg_toj_color_post[gg_toj_color_post$Parameter == "population_logjnd_probe_interaction_effect_mean",]$value

# judgement type
jnd_right_first_mean_reps = get_condition_mean_sample(
  logjnd_intercept_mean - logjnd_judgement_type_effect_mean/2
  , (logjnd_effect_mean - logjnd_judgement_type_interaction_effect_mean )
  , TRUE
  , "log"
)

jnd_right_second_mean_reps = get_condition_mean_sample(
  logjnd_intercept_mean + logjnd_judgement_type_effect_mean/2
  , (logjnd_effect_mean + logjnd_judgement_type_interaction_effect_mean )
  , TRUE
  , "log"
)

jnd_left_first_mean_reps = get_condition_mean_sample(
  logjnd_intercept_mean - logjnd_judgement_type_effect_mean/2
  , (logjnd_effect_mean - logjnd_judgement_type_interaction_effect_mean )
  , FALSE
  , "log"
)

jnd_left_second_mean_reps = get_condition_mean_sample(
  logjnd_intercept_mean + logjnd_judgement_type_effect_mean/2
  , (logjnd_effect_mean + logjnd_judgement_type_interaction_effect_mean )
  , FALSE
  , "log"
)

# initial bias
jnd_right_right_mean_reps = get_condition_mean_sample(
  logjnd_intercept_mean - logjnd_initial_bias_effect_mean/2
  , (logjnd_effect_mean - logjnd_initial_bias_interaction_effect_mean )
  , TRUE
  , "log"
)

jnd_right_left_mean_reps = get_condition_mean_sample(
  logjnd_intercept_mean + logjnd_initial_bias_effect_mean/2
  , (logjnd_effect_mean + logjnd_initial_bias_interaction_effect_mean )
  , TRUE
  , "log"
)

jnd_left_right_mean_reps = get_condition_mean_sample(
  logjnd_intercept_mean - logjnd_initial_bias_effect_mean/2
  , (logjnd_effect_mean - logjnd_initial_bias_interaction_effect_mean )
  , FALSE
  , "log"
)

jnd_left_left_mean_reps = get_condition_mean_sample(
  logjnd_intercept_mean + logjnd_initial_bias_effect_mean/2
  , (logjnd_effect_mean + logjnd_initial_bias_interaction_effect_mean )
  , FALSE
  , "log"
)

# probe duration
jnd_right_short_mean_reps = get_condition_mean_sample(
  logjnd_intercept_mean - logjnd_probe_duration_effect_mean/2
  , (logjnd_effect_mean - logjnd_probe_duration_interaction_effect_mean )
  , TRUE
  , "log"
)

jnd_right_long_mean_reps = get_condition_mean_sample(
  logjnd_intercept_mean + logjnd_probe_duration_effect_mean/2
  , (logjnd_effect_mean + logjnd_probe_duration_interaction_effect_mean )
  , TRUE
  , "log"
)

jnd_left_short_mean_reps = get_condition_mean_sample(
  logjnd_intercept_mean - logjnd_probe_duration_effect_mean/2
  , (logjnd_effect_mean - logjnd_probe_duration_interaction_effect_mean )
  , FALSE
  , "log"
)

jnd_left_long_mean_reps = get_condition_mean_sample(
  logjnd_intercept_mean + logjnd_probe_duration_effect_mean/2
  , (logjnd_effect_mean + logjnd_probe_duration_interaction_effect_mean )
  , FALSE
  , "log"
)#-------------------------------------- TOJ Simulated Data --------------------------------#


#-------------------------------------- Do TOJ PPC ----------------------------------------#
SOAs = c(-250, -150, -100, -50, -17, 17, 50, 100, 150, 250)

# judgement type
do_toj_ppc(
  pss_right_first_mean_reps
  , jnd_right_first_mean_reps
  , "'which first?' and attend right"
  , c("toj_judgement_type", "block_bias")
  , c("first", "RIGHT")
  , "left proportion"
  , real = real_toj_judgement_type
)

do_toj_ppc(
  pss_right_second_mean_reps
  , jnd_right_second_mean_reps
  , "'which second?' and attend right"
  , c("toj_judgement_type", "block_bias")
  , c("second", "RIGHT")
  , "left proportion"
  , real = real_toj_judgement_type
)

do_toj_ppc(
  pss_left_first_mean_reps
  , jnd_left_first_mean_reps
  , "'which first?' and attend left"
  , c("toj_judgement_type", "block_bias")
  , c("first", "LEFT")
  , "left proportion"
  , real = real_toj_judgement_type
)

do_toj_ppc(
  pss_left_second_mean_reps
  , jnd_left_second_mean_reps
  , "'which second?' and attend left"
  , c("toj_judgement_type", "block_bias")
  , c("second", "LEFT")
  , "left proportion"
  , real = real_toj_judgement_type
)

# intial bias
do_toj_ppc(
  pss_right_right_mean_reps
  , jnd_right_right_mean_reps
  , "initial right and attend right"
  , c("probe_initial_bias", "block_bias")
  , c("RIGHT", "RIGHT")
  , "left proportion"
  , real = real_toj_initial_bias
)

do_toj_ppc(
  pss_right_left_mean_reps
  , jnd_right_left_mean_reps
  , "initial left and attend right"
  , c("probe_initial_bias", "block_bias")
  , c("LEFT", "RIGHT")
  , "left proportion"
  , real = real_toj_initial_bias
)

do_toj_ppc(
  pss_left_right_mean_reps
  , jnd_left_right_mean_reps
  , "initial right and attend left"
  , c("probe_initial_bias", "block_bias")
  , c("RIGHT", "LEFT")
  , "left proportion"
  , real = real_toj_initial_bias
)

do_toj_ppc(
  pss_left_left_mean_reps
  , jnd_left_left_mean_reps
  , "initial left and attend left"
  , c("probe_initial_bias", "block_bias")
  , c("LEFT", "LEFT")
  , "left proportion"
  , real = real_toj_initial_bias
)

# probe duration
do_toj_ppc(
  pss_right_short_mean_reps
  , jnd_right_short_mean_reps
  , "short duration and attend right"
  , c("onehundredms", "block_bias")
  , c("TRUE", "RIGHT")
  , "left proportion"
  , real = real_toj_probe_duration
)

do_toj_ppc(
  pss_right_long_mean_reps
  , jnd_right_long_mean_reps
  , "long duration and attend right"
  , c("onehundredms", "block_bias")
  , c("FALSE", "RIGHT")
  , "left proportion"
  , real = real_toj_probe_duration
)

do_toj_ppc(
  pss_left_short_mean_reps
  , jnd_left_short_mean_reps
  , "short duration and attend left"
  , c("onehundredms", "block_bias")
  , c("TRUE", "LEFT")
  , "left proportion"
  , real = real_toj_probe_duration
)

do_toj_ppc(
  pss_left_long_mean_reps
  , jnd_left_long_mean_reps
  , "long duration and attend left"
  , c("onehundredms", "block_bias")
  , c("FALSE", "LEFT")
  , "left proportion"
  , real = real_toj_probe_duration
)
#-------------------------------------- Do TOJ PPC ----------------------------------------#


#-------------------------------------- Color Actual Data ---------------------------------#
hist(color_trials[color_trials$attended == TRUE,]$color_diff_radians, breaks = 30, freq = F, col = rgb(.1,.1,.1,.5))
hist(color_trials[color_trials$attended == FALSE,]$color_diff_radians, breaks = 30, freq = F, col = rgb(.9,.9,.9,.5), add = T)
#-------------------------------------- Color Actual Data ---------------------------------#


#-------------------------------------- Color Simulated Data ------------------------------#
### Get Rho Parameters
rho_intercept_mean = gg_toj_color_post[gg_toj_color_post$Parameter == "logitRhoMean",]$value
rho_effect_mean = gg_toj_color_post[gg_toj_color_post$Parameter == "logitRhoEffectMean",]$value

rho_judgement_type_effect_mean = gg_toj_color_post[gg_toj_color_post$Parameter == "logitRhoJudgementTypeEffectMean",]$value
rho_judgement_type_interaction_effect_mean = gg_toj_color_post[gg_toj_color_post$Parameter == "logitRhoJudgementTypeInteractionEffectMean",]$value

rho_initial_bias_effect_mean = gg_toj_color_post[gg_toj_color_post$Parameter == "logitRhoInitialBiasEffectMean",]$value
rho_initial_bias_interaction_effect_mean = gg_toj_color_post[gg_toj_color_post$Parameter == "logitRhoInitialBiasInteractionEffectMean",]$value

rho_probe_duration_effect_mean = gg_toj_color_post[gg_toj_color_post$Parameter == "logitRhoProbeEffectMean",]$value
rho_probe_duration_interaction_effect_mean = gg_toj_color_post[gg_toj_color_post$Parameter == "logitRhoProbeInteractionEffectMean",]$value

# judgement type
rho_attend_first_mean_reps = get_condition_mean_sample(
  rho_intercept_mean - rho_judgement_type_effect_mean/2
  , (rho_effect_mean - rho_judgement_type_interaction_effect_mean )
  , TRUE
  , "logit"
)

rho_attend_second_mean_reps = get_condition_mean_sample(
  rho_intercept_mean + rho_judgement_type_effect_mean/2
  , (rho_effect_mean + rho_judgement_type_interaction_effect_mean )
  , TRUE
  , "logit"
)

rho_unattend_first_mean_reps = get_condition_mean_sample(
  rho_intercept_mean - rho_judgement_type_effect_mean/2
  , (rho_effect_mean - rho_judgement_type_interaction_effect_mean )
  , FALSE
  , "logit"
)

rho_unattend_second_mean_reps = get_condition_mean_sample(
  rho_intercept_mean + rho_judgement_type_effect_mean/2
  , (rho_effect_mean + rho_judgement_type_interaction_effect_mean )
  , FALSE
  , "logit"
)

# initial bias
rho_attend_right_mean_reps = get_condition_mean_sample(
  rho_intercept_mean - rho_initial_bias_effect_mean/2
  , (rho_effect_mean - rho_initial_bias_interaction_effect_mean )
  , TRUE
  , "logit"
)

rho_attend_left_mean_reps = get_condition_mean_sample(
  rho_intercept_mean + rho_initial_bias_effect_mean/2
  , (rho_effect_mean + rho_initial_bias_interaction_effect_mean )
  , TRUE
  , "logit"
)

rho_unattend_right_mean_reps = get_condition_mean_sample(
  rho_intercept_mean - rho_initial_bias_effect_mean/2
  , (rho_effect_mean - rho_initial_bias_interaction_effect_mean )
  , FALSE
  , "logit"
)

rho_unattend_left_mean_reps = get_condition_mean_sample(
  rho_intercept_mean + rho_initial_bias_effect_mean/2
  , (rho_effect_mean + rho_initial_bias_interaction_effect_mean )
  , FALSE
  , "logit"
)

# probe duration
rho_attend_short_mean_reps = get_condition_mean_sample(
  rho_intercept_mean - rho_probe_duration_effect_mean/2
  , (rho_effect_mean - rho_probe_duration_interaction_effect_mean )
  , TRUE
  , "logit"
)

rho_attend_long_mean_reps = get_condition_mean_sample(
  rho_intercept_mean + rho_probe_duration_effect_mean/2
  , (rho_effect_mean + rho_probe_duration_interaction_effect_mean )
  , TRUE
  , "logit"
)

rho_unattend_short_mean_reps = get_condition_mean_sample(
  rho_intercept_mean - rho_probe_duration_effect_mean/2
  , (rho_effect_mean - rho_probe_duration_interaction_effect_mean )
  , FALSE
  , "logit"
)

rho_unattend_long_mean_reps = get_condition_mean_sample(
  rho_intercept_mean + rho_probe_duration_effect_mean/2
  , (rho_effect_mean + rho_probe_duration_interaction_effect_mean )
  , FALSE
  , "logit"
)

### Get Kappa Parameters
kappa_intercept_mean = gg_toj_color_post[gg_toj_color_post$Parameter == "logKappaMean",]$value
kappa_effect_mean = gg_toj_color_post[gg_toj_color_post$Parameter == "logKappaEffectMean",]$value

kappa_judgement_type_effect_mean = gg_toj_color_post[gg_toj_color_post$Parameter == "logKappaJudgementTypeEffectMean",]$value
kappa_judgement_type_interaction_effect_mean = gg_toj_color_post[gg_toj_color_post$Parameter == "logKappaJudgementTypeInteractionEffectMean",]$value

kappa_initial_bias_effect_mean = gg_toj_color_post[gg_toj_color_post$Parameter == "logKappaInitialBiasEffectMean",]$value
kappa_initial_bias_interaction_effect_mean = gg_toj_color_post[gg_toj_color_post$Parameter == "logKappaInitialBiasInteractionEffectMean",]$value

kappa_probe_duration_effect_mean = gg_toj_color_post[gg_toj_color_post$Parameter == "logKappaProbeEffectMean",]$value
kappa_probe_duration_interaction_effect_mean = gg_toj_color_post[gg_toj_color_post$Parameter == "logKappaProbeInteractionEffectMean",]$value

# judgement type
kappa_attend_first_mean_reps = get_condition_mean_sample(
  kappa_intercept_mean - kappa_judgement_type_effect_mean/2
  , (kappa_effect_mean - kappa_judgement_type_interaction_effect_mean )
  , TRUE
  , "log_free"
)

kappa_attend_second_mean_reps = get_condition_mean_sample(
  kappa_intercept_mean + kappa_judgement_type_effect_mean/2
  , (kappa_effect_mean + kappa_judgement_type_interaction_effect_mean )
  , TRUE
  , "log_free"
)

kappa_unattend_first_mean_reps = get_condition_mean_sample(
  kappa_intercept_mean - kappa_judgement_type_effect_mean/2
  , (kappa_effect_mean - kappa_judgement_type_interaction_effect_mean )
  , FALSE
  , "log_free"
)

kappa_unattend_second_mean_reps = get_condition_mean_sample(
  kappa_intercept_mean + kappa_judgement_type_effect_mean/2
  , (kappa_effect_mean + kappa_judgement_type_interaction_effect_mean )
  , FALSE
  , "log_free"
)

# initial bias
kappa_attend_right_mean_reps = get_condition_mean_sample(
  kappa_intercept_mean - kappa_initial_bias_effect_mean/2
  , (kappa_effect_mean - kappa_initial_bias_interaction_effect_mean )
  , TRUE
  , "log_free"
)

kappa_attend_left_mean_reps = get_condition_mean_sample(
  kappa_intercept_mean + kappa_initial_bias_effect_mean/2
  , (kappa_effect_mean + kappa_initial_bias_interaction_effect_mean )
  , TRUE
  , "log_free"
)

kappa_unattend_right_mean_reps = get_condition_mean_sample(
  kappa_intercept_mean - kappa_initial_bias_effect_mean/2
  , (kappa_effect_mean - kappa_initial_bias_interaction_effect_mean )
  , FALSE
  , "log_free"
)

kappa_unattend_left_mean_reps = get_condition_mean_sample(
  kappa_intercept_mean + kappa_initial_bias_effect_mean/2
  , (kappa_effect_mean + kappa_initial_bias_interaction_effect_mean )
  , FALSE
  , "log_free"
)

# probe duration
kappa_attend_short_mean_reps = get_condition_mean_sample(
  kappa_intercept_mean - kappa_probe_duration_effect_mean/2
  , (kappa_effect_mean - kappa_probe_duration_interaction_effect_mean )
  , TRUE
  , "log_free"
)

kappa_attend_long_mean_reps = get_condition_mean_sample(
  kappa_intercept_mean + kappa_probe_duration_effect_mean/2
  , (kappa_effect_mean + kappa_probe_duration_interaction_effect_mean )
  , TRUE
  , "log_free"
)

kappa_unattend_short_mean_reps = get_condition_mean_sample(
  kappa_intercept_mean - kappa_probe_duration_effect_mean/2
  , (kappa_effect_mean - kappa_probe_duration_interaction_effect_mean )
  , FALSE
  , "log_free"
)

kappa_unattend_long_mean_reps = get_condition_mean_sample(
  kappa_intercept_mean + kappa_probe_duration_effect_mean/2
  , (kappa_effect_mean + kappa_probe_duration_interaction_effect_mean )
  , FALSE
  , "log_free"
)
#-------------------------------------- Color Simulated Data ------------------------------#


#-------------------------------------- Do Color PPC --------------------------------------#
do_color_ppc(
  rho_attend_first_mean_reps
  , kappa_attend_first_mean_reps
  , "'which first?' and attend"
  , c("toj_judgement_type", "attended")
  , c("first", "TRUE")
)

do_color_ppc(
  rho_attend_second_mean_reps
  , kappa_attend_second_mean_reps
  , "'which second?' and attend"
  , c("toj_judgement_type", "attended")
  , c("second", "TRUE")
)

do_color_ppc(
  rho_unattend_first_mean_reps
  , kappa_unattend_first_mean_reps
  , "'which first?' and unattend"
  , c("toj_judgement_type", "attended")
  , c("first", "FALSE")
)

do_color_ppc(
  rho_unattend_second_mean_reps
  , kappa_unattend_second_mean_reps
  , "'which second?' and unattend"
  , c("toj_judgement_type", "attended")
  , c("second", "FALSE")
)

# initial bias
do_color_ppc(
  rho_attend_right_mean_reps
  , kappa_attend_right_mean_reps
  , "initial right and attend"
  , c("probe_initial_bias", "attended")
  , c("RIGHT", "TRUE")
)

do_color_ppc(
  rho_attend_left_mean_reps
  , kappa_attend_left_mean_reps
  , "initial left and attend"
  , c("probe_initial_bias", "attended")
  , c("LEFT", "TRUE")
)

do_color_ppc(
  rho_unattend_right_mean_reps
  , kappa_unattend_right_mean_reps
  , "initial right and unattend"
  , c("probe_initial_bias", "attended")
  , c("RIGHT", "FALSE")
)

do_color_ppc(
  rho_unattend_left_mean_reps
  , kappa_unattend_left_mean_reps
  , "initial left and unattend"
  , c("probe_initial_bias", "attended")
  , c("LEFT", "FALSE")
)

# probe duration
do_color_ppc(
  rho_attend_short_mean_reps
  , kappa_attend_short_mean_reps
  , "short and attend"
  , c("onehundredms", "attended")
  , c("TRUE", "TRUE")
)

do_color_ppc(
  rho_attend_long_mean_reps
  , kappa_attend_long_mean_reps
  , "long and attend"
  , c("onehundredms", "attended")
  , c("FALSE", "TRUE")
)

do_color_ppc(
  rho_unattend_short_mean_reps
  , kappa_unattend_short_mean_reps
  , "short and unattend"
  , c("onehundredms", "attended")
  , c("TRUE", "FALSE")
)

do_color_ppc(
  rho_unattend_long_mean_reps
  , kappa_unattend_long_mean_reps
  , "long and unattend"
  , c("onehundredms", "attended")
  , c("FALSE", "FALSE")
)
#-------------------------------------- Do Color PPC --------------------------------------#



############################################################################################
####                                       Analysis                                     ####
############################################################################################

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
betas$participant = rep(c(1:29), times = 8, each = nrow(betas2))
#-------------------------------------- Get Betas -----------------------------------------#


#---------------------------- Rho vs. PSS Effects -----------------------------------------#
psseffect = extract_samples("population_pss_effect_mean")

psseffectsd = extract_samples("zpopulation_pss_effect_sd", TRUE)

pssinteraction = extract_samples("population_pss_probe_interaction_effect_mean")

pssjudgementinteraction = extract_samples("population_pss_judgement_type_interaction_effect_mean")

pssinitialbiasinteraction = extract_samples("population_pss_initial_bias_interaction_effect_mean")

probefactor = ifelse(aggregate(onehundredms ~ id, data = color_trials, unique)$onehundred, -1, 1)

judgementfactor = ifelse(aggregate(toj_judgement_type ~ id, data = toj_trials, FUN = unique)$toj_judgement_type == "first", -1, 1)

initialbiasfactor = ifelse(aggregate(probe_initial_bias ~ id, data = toj_trials, FUN = unique)$probe_initial_bias == "RIGHT", -1, 1)

psseffect_ids = ddply(
  .data = betas
  , .variables = .(participant)
  , .fun = function(x){
    i = unique(x$participant)
    x_use = x[x$parameter ==  "population_pss_effect_mean",]$value
    psseffect_use = median(psseffect)  + median(psseffectsd)*median(x_use) + median(pssinteraction)*probefactor[i]+ median(pssjudgementinteraction)*judgementfactor[i]+ median(pssinitialbiasinteraction)*initialbiasfactor[i]
    df = data.frame(psseffect_use*250, probefactor[i], judgementfactor[i],initialbiasfactor[i])
    names(df) = c("psseffect", "probefactor", "judgementfactor", "initialbiasfactor")
    return(df)
  }
)

logitrhoeffect = extract_samples("logitRhoEffectMean")

logitrhoeffectsd = extract_samples("zlogitRhoEffectSD", TRUE)

logitrhointeractioneffect = extract_samples("logitRhoProbeInteractionEffectMean")

logitrhojudgementinteractioneffect = extract_samples("logitRhoJudgementTypeInteractionEffectMean")

logitrhoinitialbiasinteractioneffect = extract_samples("logitRhoInitialBiasInteractionEffectMean")

rhoeffect_ids = ddply(
  .data = betas
  , .variables = .(participant)
  , .fun = function(x){
    i = unique(x$participant)
    x_use = x[x$parameter == "logitRhoEffectMean",]$value
    logitrhoeffect_use =  median(logitrhoeffect) + median(logitrhoeffectsd)*median(x_use)+ median(logitrhointeractioneffect)*probefactor[i] + median(logitrhojudgementinteractioneffect)*judgementfactor[i]+ median(logitrhoinitialbiasinteractioneffect)*initialbiasfactor[i]
    df = data.frame(logitrhoeffect_use, probefactor[i], judgementfactor[i], initialbiasfactor[i])
    names(df) = c("value", "probefactor", "judgementfactor", "initialbiasfactor")
    return(df)
  }
)

psseffect_v_rhoeffect = merge(rhoeffect_ids, psseffect_ids)

ggplot(data = psseffect_v_rhoeffect, aes(y = psseffect, x = value, colour = factor(probefactor), shape = factor(probefactor)))+ #, fill = factor(judgementfactor)))+
  scale_y_continuous(name = "PSS Effect Mean")+
  scale_x_continuous(name = "Logit \u03C1 Effect Mean")+
  geom_vline(xintercept = 0, linetype = 2, size = 1)+
  geom_hline(yintercept = 0, linetype = 2, size = 1)+
  geom_point(size = 4)+
  scale_shape_manual(name = "Probe\nDuration", labels = c("Short", "Long") , values = c(16,17) )+
  # scale_fill_manual(name = "Judgement\nType", labels = c("Second", "First"), values = c("white", "black"))+
  # scale_fill_manual(name = "Initial\nBias", labels = c("Left", "Right"), values = c("white", "black"))+
  scale_colour_manual(name = "Probe\nDuration", labels =c("Short", "Long"), values = c("red", "blue") )+
  theme_gray(base_size = 30)+
  theme(panel.grid.major = element_line(size = 1.5)
        ,panel.grid.minor = element_line(size = 1))

get_corr(
  "value.2.7"
  , "Logit \u03C1 vs. PSS Effect Means"
)
#---------------------------- Rho vs. PSS Effects -----------------------------------------#


#---------------------------- Kappa vs. PSS Effects ---------------------------------------#
logkappaeffect = extract_samples("logKappaEffectMean")

logkappaeffectsd = extract_samples("zlogKappaEffectSD", TRUE)

logkappainteractioneffect = extract_samples("logKappaProbeInteractionEffectMean")

logkappajudgementtypeinteractioneffect = extract_samples("logKappaJudgementTypeInteractionEffectMean")

logkappainitialbiasinteractioneffect = extract_samples("logKappaInitialBiasInteractionEffectMean")

kappaeffect_ids = ddply(
  .data = betas
  , .variables = .(participant)
  , .fun = function(x){
    i = unique(x$participant)
    x_use = x[x$parameter == "logKappaEffectMean",]$value
    logkappaeffect_use =  median(logkappaeffect) + median(logkappaeffectsd)*median(x_use)  +median(logkappainteractioneffect)*probefactor[i] + median(logkappajudgementtypeinteractioneffect)*judgementfactor[i] + median(logkappainitialbiasinteractioneffect)*initialbiasfactor[i]
    df = data.frame(logkappaeffect_use, probefactor[i], judgementfactor[i], initialbiasfactor[i])
    names(df) = c("value","probefactor", "judgementfactor", "initialbiasfactor")
    return(df)
  }
)

psseffect_v_kappaeffect = merge(kappaeffect_ids, psseffect_ids)

ggplot(data = psseffect_v_kappaeffect, aes(y = psseffect, x = value, colour = factor(probefactor), shape = factor(probefactor)))+ #, fill = factor(initialbiasfactor)))+
  scale_y_continuous(name = "PSS Effect Mean")+
  scale_x_continuous(name = "Log \u03BA Effect Mean")+
  geom_vline(xintercept = 0, linetype = 2, size = 1)+
  geom_hline(yintercept = 0, linetype = 2, size = 1)+
  geom_point(size = 4)+
  scale_shape_manual(name = "Probe\nDuration", labels = c("Short", "Long") , values = c(16,17) )+
  # scale_fill_manual(name = "Judgement\nType", labels = c("Second", "First"), values = c("white", "black"))+
  # scale_fill_manual(name = "Initial\nBias", labels = c("Left", "Right"), values = c("white", "black"))+
  scale_colour_manual(name = "Probe\nDuration", labels =c("Short", "Long"), values = c("red", "blue") )+
  theme_gray(base_size = 30)+
  theme(panel.grid.major = element_line(size = 1.5)
        ,panel.grid.minor = element_line(size = 1))

get_corr(
  "value.2.8"
  , "Log \u03BA vs. PSS Effect Means"
)        
#---------------------------- Kappa vs. PSS Effects ---------------------------------------#


#------------------------- Rho vs. JND Intercept Means ------------------------------------#
logitrhomean = extract_samples("logitRhoMean")

logitrhosd = extract_samples("zlogitRhoSD")

logitrhojudgementeffect = extract_samples("logitRhoJudgementTypeEffectMean")

logitrhoprobeeffect = extract_samples("logitRhoProbeEffectMean")

logitrhoinitialbiaseffect = extract_samples("logitRhoInitialBiasEffectMean")

rhointercept_ids = ddply(
  .data = betas
  , .variables = .(participant)
  , .fun = function(x){
    i = unique(x$participant)
    x_use = x[x$parameter == "logitRhoMean",]$value
    logitrhointercept_use =  median(logitrhomean) + median(logitrhosd)*median(x_use)+ median(logitrhoprobeeffect)*probefactor[i] + median(logitrhojudgementeffect)*judgementfactor[i]+ median(logitrhoinitialbiaseffect)*initialbiasfactor[i]
    df = data.frame(logitrhointercept_use, probefactor[i], judgementfactor[i], initialbiasfactor[i])
    names(df) = c("logitrhointercepts", "probefactor", "judgementfactor", "initialbiasfactor")
    return(df)
  }
)

logjndmean = extract_samples("population_logjnd_intercept_mean")

logjndsd = extract_samples("zpopulation_logjnd_intercept_sd")

logjndjudgementeffect = extract_samples("population_logjnd_judgement_type_effect_mean")

logjndprobeeffect = extract_samples("population_logjnd_probe_effect_mean")

logjndinitialbiaseffect = extract_samples("population_logjnd_initial_bias_effect_mean")

jndintercept_ids = ddply(
  .data = betas
  , .variables = .(participant)
  , .fun = function(x){
    i = unique(x$participant)
    x_use = x[x$parameter == "population_logjnd_intercept_mean",]$value
    logjndintercept_use =  median(logjndmean) + median(logjndsd)*median(x_use)+ median(logjndprobeeffect)*probefactor[i] + median(logjndjudgementeffect)*judgementfactor[i]+ median(logjndinitialbiaseffect)*initialbiasfactor[i]
    df = data.frame(logjndintercept_use, probefactor[i], judgementfactor[i], initialbiasfactor[i])
    names(df) = c("logjndintercepts", "probefactor", "judgementfactor", "initialbiasfactor")
    return(df)
  }
)

jndintercept_v_rhointercept = merge(rhointercept_ids, jndintercept_ids)

ggplot(data = jndintercept_v_rhointercept, aes(y = logjndintercepts, x = logitrhointercepts, colour = factor(probefactor), shape = factor(probefactor)))+ #, fill = factor(judgementfactor)) )+
  scale_y_continuous(name = "Log JND Intercept Mean")+
  scale_x_continuous(name = "Logit \u03C1 Intercept Mean")+
  geom_vline(xintercept = 0, linetype = 2, size = 1)+
  geom_hline(yintercept = 0, linetype = 2, size = 1)+
  geom_point(size = 4)+
  scale_shape_manual(name = "Probe\nDuration", labels = c("Short", "Long") , values = c(21,22) )+
  # scale_fill_manual(name = "Judgement\nType", labels = c("Second", "First"), values = c("white", "black"))+
  # scale_fill_manual(name = "Initial\nBias", labels = c("Left", "Right"), values = c("white", "black"))+
  scale_colour_manual(name = "Probe\nDuration", labels =c("Short", "Long"), values = c("red", "blue") )+
  theme_gray(base_size = 30)+
  theme(panel.grid.major = element_line(size = 1.5)
        ,panel.grid.minor = element_line(size = 1))

get_corr(
  "value.3.5"
  , "Logit \u03C1 vs. JND Intercept Means"
)  
#------------------------- Rho vs. JND Intercept Means ------------------------------------#


#------------------------------------------------------------------------------------------#
#--------------------------------- Parameters ---------------------------------------------#
#------------------------------------------------------------------------------------------#

#---------------------------------- SOA Intercepts ----------------------------------------#
get_violin(
  ex_toj_color_post$population_pss_intercept_mean * 250
  , "PSS Intercept Mean"
  , exp( ex_toj_color_post$population_logjnd_intercept_mean ) * 250
  , "JND Intercept Mean"
  , y_lab = "SOA (ms)"
  , hline = FALSE
  , facet = TRUE
  , samps = 70000
)
#---------------------------------- SOA Intercepts ----------------------------------------#


#---------------------------------- SOA Attention Effects ---------------------------------#
# effect of attention on PSS and JND
get_violin(
  ( (ex_toj_color_post$population_pss_intercept_mean + ex_toj_color_post$population_pss_effect_mean/2) 
    - (ex_toj_color_post$population_pss_intercept_mean - ex_toj_color_post$population_pss_effect_mean/2) ) * 250
  , "PSS Effect Mean"
  , y_lab = "SOA (Right - Left; ms)"
  , samps = 70000
)

get_violin(
  ( exp( ex_toj_color_post$population_logjnd_intercept_mean + ex_toj_color_post$population_logjnd_effect_mean/2 )
      - exp( ex_toj_color_post$population_logjnd_intercept_mean - ex_toj_color_post$population_logjnd_effect_mean/2  ) ) * 250 
  , "JND Effect Mean"
  , y_lab = "SOA (Right - Left; ms)"
  , samps = 70000
)
#---------------------------------- SOA Attention Effects ---------------------------------#


#---------------------------------- SOA Judgement Effects ---------------------------------#
# effect of judgement type (Q) on PSS and JND
get_violin(
  ( ex_toj_color_post$population_pss_judgement_type_effect_mean ) * 250 
  , "PSS Judgement\nType Effect Mean"
  , ( exp( ex_toj_color_post$population_logjnd_intercept_mean + ex_toj_color_post$population_logjnd_judgement_type_effect_mean/2 )
      - exp( ex_toj_color_post$population_logjnd_intercept_mean - ex_toj_color_post$population_logjnd_judgement_type_effect_mean/2  ) ) * 250 
  , "JND Judgement\nType Effect Mean"
  , y_lab = "SOA (Second - First; ms)"
  , samps = 70000
)

#  effect of interaction between judgement type and attention on PSS 
get_violin(
  (ex_toj_color_post$population_pss_judgement_type_interaction_effect_mean) * 250
  , "PSS Judgement Type\nInteraction Effect Mean"
  , y_lab = "SOA (ms)"
  , samps = 70000
)

#  effect of interaction between judgement type and attention on JND
get_violin(
  ( exp( ex_toj_color_post$population_logjnd_intercept_mean + ex_toj_color_post$population_logjnd_judgement_type_interaction_effect_mean/2 )
    - exp( ex_toj_color_post$population_logjnd_intercept_mean - ex_toj_color_post$population_logjnd_judgement_type_interaction_effect_mean/2  ) ) * 250 
  , "JND Judgement Type\nInteraction Effect Mean"
  , y_lab = "SOA (ms)"
  , samps = 70000
)

# # effect of attention on PSS by judgement type
# get_violin(
#   ( (ex_toj_color_post$population_pss_intercept_mean + (ex_toj_color_post$population_pss_effect_mean + ex_toj_color_post$population_pss_judgement_type_interaction_effect_mean)/2) 
#     - (ex_toj_color_post$population_pss_intercept_mean - (ex_toj_color_post$population_pss_effect_mean + ex_toj_color_post$population_pss_judgement_type_interaction_effect_mean)/2 ) ) * 250
#   , "PSS Attention Effect\nGiven Second"
#   , ( (ex_toj_color_post$population_pss_intercept_mean + (ex_toj_color_post$population_pss_effect_mean - ex_toj_color_post$population_pss_judgement_type_interaction_effect_mean)/2) 
#       - (ex_toj_color_post$population_pss_intercept_mean - (ex_toj_color_post$population_pss_effect_mean - ex_toj_color_post$population_pss_judgement_type_interaction_effect_mean)/2 ) ) * 250
#   , "PSS Attention Effect\nGiven First"
#   , y_lab = "SOA (Attended - Unattended; ms)"
#   , samps = 70000
# )
#---------------------------------- SOA Judgement Effects ---------------------------------#


#------------------------------- SOA Initial Bias Effects ---------------------------------#
# effect of initial bias on PSS and JND
get_violin(
  ( ex_toj_color_post$population_pss_initial_bias_effect_mean ) * 250
  , "PSS Initial Probe\nBias Effect Mean"
  , ( exp( ex_toj_color_post$population_logjnd_intercept_mean + ex_toj_color_post$population_logjnd_initial_bias_effect_mean/2 )
      - exp( ex_toj_color_post$population_logjnd_intercept_mean - ex_toj_color_post$population_logjnd_initial_bias_effect_mean/2  ) ) * 250 
  , "JND Initial Probe\nBias Effect Mean"
  , y_lab = "SOA (Left - Right; ms)"
  , samps = 70000
)

#  effect of interaction between judgement type and attention on PSS 
get_violin(
  (ex_toj_color_post$population_pss_initial_bias_interaction_effect_mean) * 250
  , "PSS Initial Probe Bias\nInteraction Effect Mean"
  , y_lab = "SOA (ms)"
  , samps = 70000
)

#  effect of interaction between judgement type and attention on JND
get_violin(
  ( exp( ex_toj_color_post$population_logjnd_intercept_mean + ex_toj_color_post$population_logjnd_initial_bias_interaction_effect_mean/2 )
    - exp( ex_toj_color_post$population_logjnd_intercept_mean - ex_toj_color_post$population_logjnd_initial_bias_interaction_effect_mean/2  ) ) * 250 
  , "JND Initial Probe Bias\nInteraction Effect Mean"
  , y_lab = "SOA (ms)"
  , samps = 70000
)
#------------------------------- SOA Initial Bias Effects ---------------------------------#


#------------------------------- SOA Probe Duration Effects -------------------------------#
# effect of probe duration on PSS and JND
get_violin(
  ( ex_toj_color_post$population_pss_probe_effect_mean ) * 250
  , "PSS Probe Duration\nBias Effect Mean"
  , ( exp( ex_toj_color_post$population_logjnd_intercept_mean + ex_toj_color_post$population_logjnd_probe_effect_mean/2 )
      - exp( ex_toj_color_post$population_logjnd_intercept_mean - ex_toj_color_post$population_logjnd_probe_effect_mean/2  ) ) * 250 
  , "JND Probe Duration\nBias Effect Mean"
  , y_lab = "SOA (Long - Short; ms)"
  , samps = 70000
)

# effect of interaction between probe duration and attention on PSS
get_violin(
  (ex_toj_color_post$population_pss_probe_interaction_effect_mean) * 250
  , "PSS Probe Duration\nInteraction Effect Mean"
  , y_lab = "SOA (ms)"
  , samps = 70000
)

# effect of interaction between probe duration and attention on JND
get_violin(
  ( exp( ex_toj_color_post$population_logjnd_intercept_mean + ex_toj_color_post$population_logjnd_probe_interaction_effect_mean/2 )
    - exp( ex_toj_color_post$population_logjnd_intercept_mean - ex_toj_color_post$population_logjnd_probe_interaction_effect_mean/2  ) ) * 250 
  , "JND Probe Duration\nInteraction Effect Mean"
  , y_lab = "SOA (ms)"
  , samps = 70000
)

# # effect of attention on PSS by judgement type
# get_violin(
#   ( (ex_toj_color_post$population_pss_intercept_mean + (ex_toj_color_post$population_pss_effect_mean + ex_toj_color_post$population_pss_probe_interaction_effect_mean)/2) 
#     - (ex_toj_color_post$population_pss_intercept_mean - (ex_toj_color_post$population_pss_effect_mean + ex_toj_color_post$population_pss_probe_interaction_effect_mean)/2 ) ) * 250
#   , "PSS Attention Effect\nGiven Long\nProbe Duration"
#   , ( (ex_toj_color_post$population_pss_intercept_mean + (ex_toj_color_post$population_pss_effect_mean - ex_toj_color_post$population_pss_probe_interaction_effect_mean)/2) 
#       - (ex_toj_color_post$population_pss_intercept_mean - (ex_toj_color_post$population_pss_effect_mean - ex_toj_color_post$population_pss_probe_interaction_effect_mean)/2 ) ) * 250
#   , "PSS Attention Effect\nGiven Short\nProbe Duration"
#   , y_lab = "SOA (Attended - Unattended; ms)"
#   , samps = 70000
# )
#------------------------------- SOA Probe Duration Effects -------------------------------#


#---------------------------------- Rho Intercept -----------------------------------------#
get_violin(
  plogis(ex_toj_color_post$logitRhoMean)
  , "Probability of Memory Intercept Mean"
  , y_lab = "\u03C1"
  , hline = FALSE
  , samps = 70000
)
#---------------------------------- Rho Intercept -----------------------------------------#


#-------------------------------- Rho Attention Effect ------------------------------------#
get_violin(
  ( plogis(ex_toj_color_post$logitRhoMean + ex_toj_color_post$logitRhoEffectMean/2 )
    - plogis(ex_toj_color_post$logitRhoMean - ex_toj_color_post$logitRhoEffectMean/2 ) )
  , "Probability of Memory Intercept Mean"
  , y_lab = "\u03C1 (Attended - Unattended)"
  , samps = 70000
)
#-------------------------------- Rho Attention Effect ------------------------------------#


#-------------------------------- Rho Judgement Effects -----------------------------------#
get_violin(
  ( plogis(ex_toj_color_post$logitRhoMean + ex_toj_color_post$logitRhoJudgementTypeEffectMean/2 )
    - plogis(ex_toj_color_post$logitRhoMean - ex_toj_color_post$logitRhoJudgementTypeEffectMean/2 ) )
  , "Probability of Memory\nJudgement Type Effect Mean"
  , y_lab = "\u03C1 (Second - First)"
  , samps = 70000
)

get_violin(
  ( plogis(ex_toj_color_post$logitRhoMean + ex_toj_color_post$logitRhoJudgementTypeInteractionEffectMean/2 )
    - plogis(ex_toj_color_post$logitRhoMean - ex_toj_color_post$logitRhoJudgementTypeInteractionEffectMean/2 ) )
  , "Probability of Memory\nJudgement Type Interaction Effect Mean"
  , y_lab = "\u03C1"
  , samps = 70000
)

# get_violin(
#   ( plogis(ex_toj_color_post$logitRhoMean + ex_toj_color_post$logitRhoJudgementTypeEffectMean/2
#            + (ex_toj_color_post$logitRhoEffectMean + ex_toj_color_post$logitRhoJudgementTypeInteractionEffectMean)/2 )
#     - plogis(ex_toj_color_post$logitRhoMean + ex_toj_color_post$logitRhoJudgementTypeEffectMean/2
#              - (ex_toj_color_post$logitRhoEffectMean +  ex_toj_color_post$logitRhoJudgementTypeInteractionEffectMean)/2 ) )
#   , "Probability of Memory\nAttention Effect\nGiven Second"
#   , ( plogis(ex_toj_color_post$logitRhoMean - ex_toj_color_post$logitRhoJudgementTypeEffectMean/2
#              + (ex_toj_color_post$logitRhoEffectMean - ex_toj_color_post$logitRhoJudgementTypeInteractionEffectMean)/2 )
#       - plogis(ex_toj_color_post$logitRhoMean - ex_toj_color_post$logitRhoJudgementTypeEffectMean/2
#                - (ex_toj_color_post$logitRhoEffectMean -  ex_toj_color_post$logitRhoJudgementTypeInteractionEffectMean)/2 ) )
#   , "Probability of Memory\nAttention Effect\nGiven First"
#   , y_lab = "\u03C1 (Attended - Unattended)"
#   , samps = 70000
# )
#-------------------------------- Rho Judgement Effects -----------------------------------#


#----------------------------- Rho Initial Bias Effects -----------------------------------#
get_violin(
  ( plogis(ex_toj_color_post$logitRhoMean + ex_toj_color_post$logitRhoInitialBiasEffectMean/2 )
    - plogis(ex_toj_color_post$logitRhoMean - ex_toj_color_post$logitRhoInitialBiasEffectMean/2 ) )
  , "Probability of Memory\nInitial Probe Bias Effect Mean"
  , y_lab = "\u03C1 (Left - Right)"
  , samps = 70000
)

get_violin(
  ( plogis(ex_toj_color_post$logitRhoMean + ex_toj_color_post$logitRhoInitialBiasInteractionEffectMean/2 )
    - plogis(ex_toj_color_post$logitRhoMean - ex_toj_color_post$logitRhoInitialBiasInteractionEffectMean/2 ) )
  , "Probability of Memory\nInitial Probe Bias\nInteraction Effect Mean"
  , y_lab = "\u03C1"
  , samps = 70000
)

# get_violin(
#   ( plogis(ex_toj_color_post$logitRhoMean + ex_toj_color_post$logitRhoInitialBiasEffectMean/2
#            + (ex_toj_color_post$logitRhoEffectMean + ex_toj_color_post$logitRhoInitialBiasInteractionEffectMean)/2 )
#     - plogis(ex_toj_color_post$logitRhoMean + ex_toj_color_post$logitRhoInitialBiasEffectMean/2
#              - (ex_toj_color_post$logitRhoEffectMean +  ex_toj_color_post$logitRhoInitialBiasInteractionEffectMean)/2 ) )
#   , "Probability of Memory\nAttention Effect\nGiven Initial Bias Left"
#   , ( plogis(ex_toj_color_post$logitRhoMean - ex_toj_color_post$logitRhoInitialBiasEffectMean/2
#              + (ex_toj_color_post$logitRhoEffectMean - ex_toj_color_post$logitRhoInitialBiasInteractionEffectMean)/2 )
#       - plogis(ex_toj_color_post$logitRhoMean - ex_toj_color_post$logitRhoInitialBiasEffectMean/2
#                - (ex_toj_color_post$logitRhoEffectMean -  ex_toj_color_post$logitRhoInitialBiasInteractionEffectMean)/2 ) )
#   , "Probability of Memory\nAttention Effect\nGiven Initial Bias Right"
#   , y_lab = "\u03C1 (Attended - Unattended)"
#   , samps = 70000
# )
#----------------------------- Rho Initial Bias Effects -----------------------------------#


#-------------------------------- Rho Probe Effects ---------------------------------------#
get_violin(
  ( plogis(ex_toj_color_post$logitRhoMean + ex_toj_color_post$logitRhoProbeEffectMean/2 )
    - plogis(ex_toj_color_post$logitRhoMean - ex_toj_color_post$logitRhoProbeEffectMean/2 ) )
  , "Probability of Memory\nProbe Duration Effect Mean"
  , y_lab = "\u03C1 (Long - Short)"
  , samps = 70000
)

get_violin(
  ( plogis(ex_toj_color_post$logitRhoMean + ex_toj_color_post$logitRhoProbeInteractionEffectMean/2 )
    - plogis(ex_toj_color_post$logitRhoMean - ex_toj_color_post$logitRhoProbeInteractionEffectMean/2 ) )
  , "Probability of Memory\nProbe Duration Interaction Effect Mean"
  , y_lab = "\u03C1"
  , samps = 70000
)

get_violin(
  ( plogis(ex_toj_color_post$logitRhoMean + ex_toj_color_post$logitRhoProbeEffectMean/2
           + (ex_toj_color_post$logitRhoEffectMean + ex_toj_color_post$logitRhoProbeInteractionEffectMean)/2 )
    - plogis(ex_toj_color_post$logitRhoMean + ex_toj_color_post$logitRhoProbeEffectMean/2
             - (ex_toj_color_post$logitRhoEffectMean +  ex_toj_color_post$logitRhoProbeInteractionEffectMean)/2 ) )
  , "Probability of Memory\nAttention Effect\nGiven Long\nProbe Duration"
  , ( plogis(ex_toj_color_post$logitRhoMean - ex_toj_color_post$logitRhoProbeEffectMean/2
             + (ex_toj_color_post$logitRhoEffectMean - ex_toj_color_post$logitRhoProbeInteractionEffectMean)/2 )
      - plogis(ex_toj_color_post$logitRhoMean - ex_toj_color_post$logitRhoProbeEffectMean/2
               - (ex_toj_color_post$logitRhoEffectMean -  ex_toj_color_post$logitRhoProbeInteractionEffectMean)/2 ) )
  , "Probability of Memory\nAttention Effect\nGiven Short\nProbe Duration"
  , y_lab = "\u03C1 (Attended - Unattended)"
  , samps = 70000
)
#-------------------------------- Rho Probe Effects ---------------------------------------#


#---------------------------------- Kappa Intercept ---------------------------------------#
get_violin(
  exp( ex_toj_color_post$logKappaMean ) 
  , "Fidelity of Memory Intercept Mean"
  , y_lab = "\u03BA"
  , samps = 70000
)
#---------------------------------- Kappa Intercept ---------------------------------------#


#-------------------------------- Kappa Attention Effect ----------------------------------#
get_violin(
  ( exp(ex_toj_color_post$logKappaMean + ex_toj_color_post$logKappaEffectMean/2) 
    - exp(ex_toj_color_post$logKappaMean - ex_toj_color_post$logKappaEffectMean/2) )
  , "Fidelity of Memory Effect Mean"
  , y_lab = "\u03BA (Attended - Unattended)"
  , samps = 70000
)
#-------------------------------- Kappa Attention Effect ----------------------------------#


#--------------------------- Kappa Initial Bias Effects -----------------------------------#
get_violin(
  ( exp(ex_toj_color_post$logKappaMean + ex_toj_color_post$logKappaInitialBiasEffectMean/2 )
    - exp(ex_toj_color_post$logKappaMean - ex_toj_color_post$logKappaInitialBiasEffectMean/2 ) )
  , "Fidelity of Memory\nInitial Bias Effect Mean"
  , y_lab = "\u03BA (Second - First)"
  , samps = 70000
)

get_violin(
  ( exp(ex_toj_color_post$logKappaMean + ex_toj_color_post$logKappaInitialBiasInteractionEffectMean/2 )
    - exp(ex_toj_color_post$logKappaMean - ex_toj_color_post$logKappaInitialBiasInteractionEffectMean/2 ) )
  , "Fidelity of Memory\nInitial Bias\nInteraction Effect Mean"
  , y_lab = "\u03BA"
  , samps = 70000
)

# get_violin(
#   ( exp(ex_toj_color_post$logKappaMean + ex_toj_color_post$logKappaInitialBiasEffectMean/2
#         + (ex_toj_color_post$logKappaEffectMean + ex_toj_color_post$logKappaInitialBiasInteractionEffectMean)/2 )
#     - exp(ex_toj_color_post$logKappaMean + ex_toj_color_post$logKappaInitialBiasEffectMean/2
#           - (ex_toj_color_post$logKappaEffectMean +  ex_toj_color_post$logKappaInitialBiasInteractionEffectMean)/2 ) )
#   , "Fidelity of Memory\nAttention Effect\nGiven Left"
#   , ( exp(ex_toj_color_post$logKappaMean - ex_toj_color_post$logKappaInitialBiasEffectMean/2
#           + (ex_toj_color_post$logKappaEffectMean - ex_toj_color_post$logKappaInitialBiasInteractionEffectMean)/2 )
#       - exp(ex_toj_color_post$logKappaMean - ex_toj_color_post$logKappaInitialBiasEffectMean/2
#             - (ex_toj_color_post$logKappaEffectMean -  ex_toj_color_post$logKappaInitialBiasInteractionEffectMean)/2 ) )
#   , "Fidelity of Memory\nAttention Effect\nGiven Right"
#   , y_lab = "\u03BA (Attended - Unattended)"
#   , samps = 70000
# )
#--------------------------- Kappa Initial Bias Effects -----------------------------------#



#-------------------------------- Kappa Judgement Effects ---------------------------------#
get_violin(
  ( exp(ex_toj_color_post$logKappaMean + ex_toj_color_post$logKappaJudgementTypeEffectMean/2 )
    - exp(ex_toj_color_post$logKappaMean - ex_toj_color_post$logKappaJudgementTypeEffectMean/2 ) )
  , "Fidelity of Memory Judgement Effect Mean"
  , y_lab = "\u03BA (Second - First)"
  , samps = 70000
)

get_violin(
  ( exp(ex_toj_color_post$logKappaMean + ex_toj_color_post$logKappaJudgementTypeInteractionEffectMean/2 )
    - exp(ex_toj_color_post$logKappaMean - ex_toj_color_post$logKappaJudgementTypeInteractionEffectMean/2 ) )
  , "Fidelity of Memory Judgement Type\nInteraction Effect Mean"
  , y_lab = "\u03BA"
  , samps = 70000
)

# get_violin(
#   ( exp(ex_toj_color_post$logKappaMean + ex_toj_color_post$logKappaJudgementTypeEffectMean/2
#         + (ex_toj_color_post$logKappaEffectMean + ex_toj_color_post$logKappaJudgementTypeInteractionEffectMean)/2 )
#     - exp(ex_toj_color_post$logKappaMean + ex_toj_color_post$logKappaJudgementTypeEffectMean/2
#           - (ex_toj_color_post$logKappaEffectMean +  ex_toj_color_post$logKappaJudgementTypeInteractionEffectMean)/2 ) )
#   , "Fidelity of Memory\nAttention Effect\nGiven Second"
#   , ( exp(ex_toj_color_post$logKappaMean - ex_toj_color_post$logKappaJudgementTypeEffectMean/2
#           + (ex_toj_color_post$logKappaEffectMean - ex_toj_color_post$logKappaJudgementTypeInteractionEffectMean)/2 )
#       - exp(ex_toj_color_post$logKappaMean - ex_toj_color_post$logKappaJudgementTypeEffectMean/2
#             - (ex_toj_color_post$logKappaEffectMean -  ex_toj_color_post$logKappaJudgementTypeInteractionEffectMean)/2 ) )
#   , "Fidelity of Memory\nAttention Effect\nGiven First"
#   , y_lab = "\u03BA (Attended - Unattended)"
#   , samps = 70000
# )
#-------------------------------- Kappa Judgement Effects ---------------------------------#



#-------------------------------- Kappa Probe Effects -------------------------------------#
get_violin(
  ( exp(ex_toj_color_post$logKappaMean + ex_toj_color_post$logKappaProbeEffectMean/2 )
    - exp(ex_toj_color_post$logKappaMean - ex_toj_color_post$logKappaProbeEffectMean/2 ) )
  , "Fidelity of Memory Probe Effect Mean"
  , y_lab = "\u03BA (Long - Short)"
  , samps = 70000
)

get_violin(
  ( exp(ex_toj_color_post$logKappaMean + ex_toj_color_post$logKappaProbeInteractionEffectMean/2 )
    - exp(ex_toj_color_post$logKappaMean - ex_toj_color_post$logKappaProbeInteractionEffectMean/2 ) )
  , "Fidelity of Memory Probe Duration\nInteraction Effect Mean"
  , y_lab = "\u03BA"
  , samps = 70000
)

get_violin(
  ( exp(ex_toj_color_post$logKappaMean + ex_toj_color_post$logKappaProbeEffectMean/2
        + (ex_toj_color_post$logKappaEffectMean + ex_toj_color_post$logKappaProbeInteractionEffectMean)/2 )
    - exp(ex_toj_color_post$logKappaMean + ex_toj_color_post$logKappaProbeEffectMean/2
          - (ex_toj_color_post$logKappaEffectMean +  ex_toj_color_post$logKappaProbeInteractionEffectMean)/2 ) )
  , "Fidelity of Memory\nAttention Effect\nGiven Long\nProbe Duration"
  , ( exp(ex_toj_color_post$logKappaMean - ex_toj_color_post$logKappaProbeEffectMean/2
          + (ex_toj_color_post$logKappaEffectMean - ex_toj_color_post$logKappaProbeInteractionEffectMean)/2 )
      - exp(ex_toj_color_post$logKappaMean - ex_toj_color_post$logKappaProbeEffectMean/2
            - (ex_toj_color_post$logKappaEffectMean -  ex_toj_color_post$logKappaProbeInteractionEffectMean)/2 ) )
  , "Fidelity of Memory\nAttention Effect\nGiven Short\nProbe Duration"
  , y_lab = "\u03BA (Attended - Unattended)"
  , samps = 70000
)
#-------------------------------- Kappa Probe Effects -------------------------------------#



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


# #---------------------------------- Posteriors --------------------------------------------#
# ### Rho
# pos_rhoMean_WithEffect = data.frame(
#   c( 
#     plogis(ex_toj_color_post$logitRhoMean + ex_toj_color_post$logitRhoProbeEffectMean/2
#            + (ex_toj_color_post$logitRhoEffectMean +  ex_toj_color_post$logitRhoProbeInteractionEffectMean)/2)
#     ,  plogis(ex_toj_color_post$logitRhoMean + ex_toj_color_post$logitRhoProbeEffectMean/2
#               - (ex_toj_color_post$logitRhoEffectMean +  ex_toj_color_post$logitRhoProbeInteractionEffectMean)/2)
#     ,  plogis(ex_toj_color_post$logitRhoMean - ex_toj_color_post$logitRhoProbeEffectMean/2
#               + (ex_toj_color_post$logitRhoEffectMean -  ex_toj_color_post$logitRhoProbeInteractionEffectMean)/2)
#     ,  plogis(ex_toj_color_post$logitRhoMean - ex_toj_color_post$logitRhoProbeEffectMean/2
#               - (ex_toj_color_post$logitRhoEffectMean -  ex_toj_color_post$logitRhoProbeInteractionEffectMean)/2)
#   ) 
#   , c(rep("AttendedLong",80000), rep("UnattendedLong",80000), rep("AttendedShort",80000), rep("UnattendedShort",80000))
# )
# names(pos_rhoMean_WithEffect) = c("rhoMean", "Effect")
# 
# 
# # overlapping
# ggplot(pos_rhoMean_WithEffect, aes(x = rhoMean, ..density.., fill = Effect))+
#   geom_density(data = pos_rhoMean_WithEffect[pos_rhoMean_WithEffect$Effect == "AttendedLong",],alpha = 0.5)+
#   geom_density(data = pos_rhoMean_WithEffect[pos_rhoMean_WithEffect$Effect == "UnattendedLong",],alpha = 0.5)+
#   geom_density(data = pos_rhoMean_WithEffect[pos_rhoMean_WithEffect$Effect == "AttendedShort",],alpha = 0.5)+
#   geom_density(data = pos_rhoMean_WithEffect[pos_rhoMean_WithEffect$Effect == "UnattendedShort",],alpha = 0.5)+
#   scale_fill_hue("Effect", l = c(90, 45, 70, 30) , c = c(100, 50, 100, 50) ) +
#   labs(x = "Probability of Memory Population Mean", y = "Density", colour = "")+
#   theme_gray(base_size = 24)+
#   theme(panel.grid.major = element_line(size = 1.5)
#         ,panel.grid.minor = element_line(size = 1))
# 
# ### Kappa
# pos_kappaMean_WithEffect = data.frame(
#   c( 
#     plogis(ex_toj_color_post$logKappaMean + ex_toj_color_post$logKappaProbeEffectMean/2
#            + (ex_toj_color_post$logKappaEffectMean +  ex_toj_color_post$logKappaProbeInteractionEffectMean)/2)
#     ,  plogis(ex_toj_color_post$logKappaMean + ex_toj_color_post$logKappaProbeEffectMean/2
#               - (ex_toj_color_post$logKappaEffectMean +  ex_toj_color_post$logKappaProbeInteractionEffectMean)/2)
#     ,  plogis(ex_toj_color_post$logKappaMean - ex_toj_color_post$logKappaProbeEffectMean/2
#               + (ex_toj_color_post$logKappaEffectMean -  ex_toj_color_post$logKappaProbeInteractionEffectMean)/2)
#     ,  plogis(ex_toj_color_post$logKappaMean - ex_toj_color_post$logKappaProbeEffectMean/2
#               - (ex_toj_color_post$logKappaEffectMean -  ex_toj_color_post$logKappaProbeInteractionEffectMean)/2)
#   ) 
#   , c(rep("AttendedLong",80000), rep("UnattendedLong",80000), rep("AttendedShort",80000), rep("UnattendedShort",80000))
# )
# names(pos_kappaMean_WithEffect) = c("kappaMean", "Effect")
# 
# 
# # overlapping
# ggplot(pos_rhoMean_WithEffect, aes(x = kappaMean, ..density.., fill = Effect))+
#   geom_density(data = pos_kappaMean_WithEffect[pos_kappaMean_WithEffect$Effect == "AttendedLong",],alpha = 0.5)+
#   geom_density(data = pos_kappaMean_WithEffect[pos_kappaMean_WithEffect$Effect == "UnattendedLong",],alpha = 0.5)+
#   geom_density(data = pos_kappaMean_WithEffect[pos_kappaMean_WithEffect$Effect == "AttendedShort",],alpha = 0.5)+
#   geom_density(data = pos_kappaMean_WithEffect[pos_kappaMean_WithEffect$Effect == "UnattendedShort",],alpha = 0.5)+
#   scale_fill_hue("Effect", l = c(90, 45, 70, 30) , c = c(100, 50, 100, 50) ) +
#   labs(x = "Fidelity of Memory Population Mean", y = "Density", colour = "")+
#   theme_gray(base_size = 24)+
#   theme(panel.grid.major = element_line(size = 1.5)
#         ,panel.grid.minor = element_line(size = 1))
# #---------------------------------- Posteriors --------------------------------------------#





