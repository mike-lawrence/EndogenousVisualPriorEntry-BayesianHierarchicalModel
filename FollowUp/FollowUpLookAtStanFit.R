# library(shinystan)
library(coda)
library(ggplot2)
library(ggmcmc)
library(CircStats)
library(grid)
library(sprintfr)

setwd("~/Documents/TOJ/Follow-Up")
load("FollowUptoj_color_post_June15th2016")
load("FollowUp_color_trials.Rdata")
load("FollowUp_toj_trials.Rdata")



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
real_toj_block_bias = aggregate(left_first_TF ~ soa2 + block_bias, data = toj_trials, FUN = mean)
real_toj_judgement_type = aggregate(left_first_TF ~ soa2 + toj_judgement_type, data = toj_trials, FUN = mean)
real_toj_initial_bias = aggregate(left_first_TF ~ soa2 +  probe_initial_bias, data = toj_trials, FUN = mean)
real_toj_probe_duration = aggregate(left_first_TF ~ soa2 +  onehundredms, data = toj_trials, FUN = mean)

real_toj = data.frame(
  SOA = rep(real_toj_block_bias$soa2,4)
  , factor = c(
    rep(names(real_toj_block_bias)[2], 20)
    , rep(names(real_toj_judgement_type)[2], 20)
    , rep(names(real_toj_initial_bias)[2], 20)
    , rep(names(real_toj_probe_duration)[2], 20)
    )
  , level = c(
    as.character(real_toj_block_bias$block_bias)
    , as.character(real_toj_judgement_type$toj_judgement_type)
    , as.character(real_toj_initial_bias$probe_initial_bias)
    , as.character(real_toj_probe_duration$onehundredms)
    ) 
  , left_first_TF = c(
    real_toj_block_bias$left_first_TF
    , real_toj_judgement_type$left_first_TF
    , real_toj_initial_bias$left_first_TF
    , real_toj_probe_duration$left_first_TF
  )
)
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
pss_intercept_mean = gg_toj_color_post[gg_toj_color_post$Parameter == "population_pss_intercept_mean",]$value
pss_effect_mean = gg_toj_color_post[gg_toj_color_post$Parameter == "population_pss_effect_mean",]$value
pss_judgement_type_effect_mean = gg_toj_color_post[gg_toj_color_post$Parameter == "population_pss_judgement_type_effect_mean",]$value
pss_initial_bias_effect_mean = gg_toj_color_post[gg_toj_color_post$Parameter == "population_pss_initial_bias_effect_mean",]$value
pss_probe_duration_effect_mean = gg_toj_color_post[gg_toj_color_post$Parameter == "population_pss_probe_effect_mean",]$value

pss_right_mean_reps = get_condition_mean_sample(pss_intercept_mean, pss_effect_mean, TRUE, "null")
pss_left_mean_reps = get_condition_mean_sample(pss_intercept_mean, pss_effect_mean, FALSE, "null")

pss_first_mean_reps = get_condition_mean_sample(pss_intercept_mean, pss_judgement_type_effect_mean, FALSE, "null")
pss_second_mean_reps = get_condition_mean_sample(pss_intercept_mean, pss_judgement_type_effect_mean, TRUE, "null")

pss_initial_right_mean_reps = get_condition_mean_sample(pss_intercept_mean, pss_initial_bias_effect_mean, FALSE, "null")
pss_initial_left_mean_reps = get_condition_mean_sample(pss_intercept_mean, pss_initial_bias_effect_mean, TRUE, "null")

pss_short_mean_reps = get_condition_mean_sample(pss_intercept_mean, pss_probe_duration_effect_mean, FALSE, "null")
pss_long_mean_reps = get_condition_mean_sample(pss_intercept_mean, pss_probe_duration_effect_mean, TRUE, "null")

### Get JND Parameters
jnd_intercept_mean = gg_toj_color_post[gg_toj_color_post$Parameter == "population_logjnd_intercept_mean",]$value
jnd_effect_mean = gg_toj_color_post[gg_toj_color_post$Parameter == "population_logjnd_effect_mean",]$value
jnd_judgement_type_effect_mean = gg_toj_color_post[gg_toj_color_post$Parameter == "population_logjnd_judgement_type_effect_mean",]$value
jnd_initial_bias_effect_mean = gg_toj_color_post[gg_toj_color_post$Parameter == "population_logjnd_initial_bias_effect_mean",]$value
jnd_probe_duration_effect_mean = gg_toj_color_post[gg_toj_color_post$Parameter == "population_logjnd_probe_effect_mean",]$value

jnd_right_mean_reps = get_condition_mean_sample( jnd_intercept_mean, jnd_effect_mean, TRUE, "log")
jnd_left_mean_reps = get_condition_mean_sample( jnd_intercept_mean, jnd_effect_mean, FALSE, "log")

jnd_first_mean_reps = get_condition_mean_sample( jnd_intercept_mean, jnd_judgement_type_effect_mean, FALSE, "log")
jnd_second_mean_reps = get_condition_mean_sample( jnd_intercept_mean, jnd_judgement_type_effect_mean, TRUE, "log")

jnd_initial_right_mean_reps = get_condition_mean_sample( jnd_intercept_mean,  jnd_initial_bias_effect_mean, FALSE, "log")
jnd_initial_left_mean_reps = get_condition_mean_sample( jnd_intercept_mean,  jnd_initial_bias_effect_mean, TRUE, "log")

jnd_short_mean_reps = get_condition_mean_sample( jnd_intercept_mean,  jnd_probe_duration_effect_mean, FALSE, "log")
jnd_long_mean_reps = get_condition_mean_sample( jnd_intercept_mean,  jnd_probe_duration_effect_mean, TRUE, "log")
#-------------------------------------- TOJ Simulated Data --------------------------------#


#-------------------------------------- Do TOJ PPC ----------------------------------------#
SOAs = c(-250, -150, -100, -50, -17, 17, 50, 100, 150, 250)

do_toj_ppc = function(pss, jnd, main, facteur, level) {
  df = NULL
  for (i in 1:length(pss)) {
    df_temp = data.frame(SOA = SOAs, left_prop = pnorm( SOAs, mean = pss[i], sd = jnd[i]) )
    df = rbind(df, df_temp)
  }
  
  gg = ggplot(data = df, aes(x = SOA, y = left_prop))+
    geom_point(alpha = 0.3, colour = "turquoise")+
    ylab("left proportion")+
    xlab("SOA")+
    ggtitle(main)
  
  real = data.frame(SOA = SOAs, left_prop = real_toj[real_toj[,"factor"] == facteur & real_toj[,"level"] == level,]$left_first_TF) 
  
  gg = gg + geom_point(data = real, aes(x = SOA, y = left_prop), colour = "blue")

  return(gg)
}

do_toj_ppc(pss_right_mean_reps, jnd_right_mean_reps, "attend right", "block_bias", "RIGHT")

do_toj_ppc(pss_left_mean_reps, jnd_left_mean_reps, "attend left", "block_bias", "LEFT")

do_toj_ppc(pss_first_mean_reps, jnd_first_mean_reps, "which came first?", "toj_judgement_type", "first")

do_toj_ppc(pss_second_mean_reps, jnd_second_mean_reps, "which came second?", "toj_judgement_type", "second")

do_toj_ppc(pss_initial_right_mean_reps, jnd_initial_right_mean_reps, "initial right", "probe_initial_bias", "RIGHT")

do_toj_ppc(pss_initial_left_mean_reps, jnd_initial_left_mean_reps, "initial left", "probe_initial_bias", "LEFT")

do_toj_ppc(pss_short_mean_reps, jnd_short_mean_reps, "short probe duration", "onehundredms", "TRUE")

do_toj_ppc(pss_long_mean_reps, jnd_long_mean_reps, "long probe duration", "onehundredms", "FALSE")
#-------------------------------------- Do TOJ PPC ----------------------------------------#


#-------------------------------------- Color Actual Data ---------------------------------#
hist(color_trials[color_trials$attended == TRUE,]$color_diff_radians, breaks = 30, freq = F, col = rgb(.1,.1,.1,.5))
hist(color_trials[color_trials$attended == FALSE,]$color_diff_radians, breaks = 30, freq = F, col = rgb(.9,.9,.9,.5), add = T)
#-------------------------------------- Color Actual Data ---------------------------------#


#-------------------------------------- Color Simulated Data ------------------------------#
rho_intercept_mean = gg_toj_color_post[gg_toj_color_post$Parameter == "logitRhoMean",]$value
rho_effect_mean = gg_toj_color_post[gg_toj_color_post$Parameter == "logitRhoEffectMean",]$value
rho_probe_effect_mean = gg_toj_color_post[gg_toj_color_post$Parameter == "logitRhoProbeEffectMean",]$value
rho_probe_interaction_effect_mean = gg_toj_color_post[gg_toj_color_post$Parameter == "logitRhoProbeInteractionEffectMean",]$value

rho_attend_short = plogis(rho_intercept_mean - rho_probe_effect_mean/2  + (rho_effect_mean - rho_probe_interaction_effect_mean)/2)
rho_attend_short_reps = sample(rho_attend_short, 50, replace = T)

rho_unattend_short = plogis(rho_intercept_mean - rho_probe_effect_mean/2  - (rho_effect_mean - rho_probe_interaction_effect_mean)/2)
rho_unattend_short_reps = sample(rho_unattend_short, 50, replace = T)

rho_attend_long = plogis(rho_intercept_mean  + rho_probe_effect_mean/2 + (rho_effect_mean + rho_probe_interaction_effect_mean)/2)
rho_attend_long_reps = sample(rho_attend_long, 50, replace = T)

rho_unattend_long = plogis(rho_intercept_mean  + rho_probe_effect_mean/2 - (rho_effect_mean + rho_probe_interaction_effect_mean)/2)
rho_unattend_long_reps = sample(rho_unattend_long, 50, replace = T)

### Get Kappa Parameters
kappa_intercept_mean = gg_toj_color_post[gg_toj_color_post$Parameter == "logKappaMean",]$value
kappa_effect_mean = gg_toj_color_post[gg_toj_color_post$Parameter == "logKappaEffectMean",]$value
kappa_probe_effect_mean = gg_toj_color_post[gg_toj_color_post$Parameter == "logKappaProbeEffectMean",]$value
kappa_probe_interaction_effect_mean = gg_toj_color_post[gg_toj_color_post$Parameter == "logKappaProbeInteractionEffectMean",]$value

kappa_attend_short = exp(kappa_intercept_mean - kappa_probe_effect_mean/2  + (kappa_effect_mean - kappa_probe_interaction_effect_mean)/2)
kappa_attend_short_reps = sample(kappa_attend_short, 50, replace = T)

kappa_unattend_short = exp(kappa_intercept_mean - kappa_probe_effect_mean/2  - (kappa_effect_mean - kappa_probe_interaction_effect_mean)/2)
kappa_unattend_short_reps = sample(kappa_unattend_short, 50, replace = T)

kappa_attend_long = exp(kappa_intercept_mean  + kappa_probe_effect_mean/2 + (kappa_effect_mean + kappa_probe_interaction_effect_mean)/2)
kappa_attend_long_reps = sample(kappa_attend_long, 50, replace = T)

kappa_unattend_long = exp(kappa_intercept_mean  + kappa_probe_effect_mean/2 - (kappa_effect_mean + kappa_probe_interaction_effect_mean)/2)
kappa_unattend_long_reps = sample(kappa_unattend_long, 50, replace = T)
#-------------------------------------- Color Simulated Data ------------------------------#


#-------------------------------------- Do Color PPC --------------------------------------#
# look at mixture model
plot( seq(-pi, pi, pi/200), (rho_unattend_short_reps[1])*dvm(seq(-pi, pi, pi/200), 0, kappa_unattend_short_reps[1]) 
      + (1-rho_unattend_short_reps[1]) * dunif(seq(-pi, pi, pi/200), -pi, pi)
      , xlab = "radian deviations", ylab = "density")

do_color_ppc = function(rho, kappa, main, attention_level, probe_level) {
  df = NULL
  for (i in 1:length(rho)) {
    color_dev = c( 
      rvm(1000*(rho[i]), pi, kappa[i]) - pi
      , runif(1000*(1- rho[i]), -pi, pi) 
    )
    df_temp = data.frame(
      repi = rep(i, length(color_dev))
      , color_dev = color_dev
    )
    df = rbind(df, df_temp)
  }
  
  gg = ggplot(data = df, aes(color_dev, group = factor(repi)))+
    geom_density(colour = "turquoise")+
    xlab("radian deviation")+
    ggtitle(main)+
    theme(legend.position = "none")
  
  color_dev = color_trials[color_trials[,"attended"]==attention_level & color_trials[,"onehundredms"]==probe_level,]$color_diff_radians
  real = data.frame(
    repi = rep(i, length(color_dev))
    , color_dev = color_dev
  )
  
  gg = gg + geom_density(data = real, aes(color_dev, group = factor(repi)), colour = "blue")
  
  return(gg)
}

do_color_ppc(rho_unattend_short_reps, kappa_unattend_short_reps, "unattend short", FALSE, TRUE)

do_color_ppc(rho_attend_short_reps, kappa_unattend_short_reps, "attend short", TRUE, TRUE)

do_color_ppc(rho_unattend_long_reps, kappa_unattend_long_reps, "unattend long", FALSE, FALSE)

do_color_ppc(rho_attend_long_reps, kappa_attend_long_reps,"attend long", TRUE, FALSE)
#-------------------------------------- Do Color PPC --------------------------------------#



############################################################################################
####                                       Analysis                                     ####
############################################################################################
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
extract_samples = function(parameter, SD = FALSE) {
  if (SD) {
    df2 = data.frame(value = tan(ex_toj_color_post[[parameter]]))
  } else {
    df2 = data.frame(value = ex_toj_color_post[[parameter]])
  }
  df2$iteration = rownames(df2)
  df = melt( df2 )$value
  return(df)
}

psseffect = extract_samples("population_pss_effect_mean")

psseffectsd = extract_samples("zpopulation_pss_effect_sd", TRUE)

pssinteraction = extract_samples("population_pss_probe_interaction_effect_mean")

pssjudgementinteraction = extract_samples("population_pss_judgement_type_interaction_effect_mean")

probefactor = ifelse(aggregate(onehundredms ~ id, data = color_trials, unique)$onehundred, -1, 1)

judgementfactor = ifelse(aggregate(toj_judgement_type ~ id, data = toj_trials, FUN = unique)$toj_judgement_type == "first", -1, 1)

psseffect_ids = ddply(
  .data = betas
  , .variables = .(participant)
  , .fun = function(x){
    i = unique(x$participant)
    x_use = x[x$parameter ==  "population_pss_effect_mean",]$value
    psseffect = (median(psseffect)  + median(psseffectsd)*median(x_use) 
                 + median(pssinteraction)*probefactor[i]
                 + median(pssjudgementinteraction)*judgementfactor[i])/2
    df = data.frame(psseffect, probefactor[i], judgementfactor[i])
    names(df) = c("psseffect", "probefactor", "judgementfactor")
    return(df)
  }
)

logitrhomean = extract_samples("logitRhoMean")

logitrhosd = extract_samples("zlogitRhoSD", TRUE)

logitrhoeffect = extract_samples("logitRhoEffectMean")

logitrhoeffectsd = extract_samples("zlogitRhoEffectSD", TRUE)

logitrhoprobeeffect = extract_samples("logitRhoProbeEffectMean")

logitrhointeractioneffect = extract_samples("logitRhoProbeInteractionEffectMean")

# logitrhojudgementinteractioneffect = extract_samples("logitRhoJudgementTypeInteractionEffectMean")

rhoeffect_ids = ddply(
  .data = betas
  , .variables = .(participant)
  , .fun = function(x){
    i = unique(x$participant)
    x_use = x[x$parameter == "logitRhoEffectMean",]$value
    logitrhoeffect_use =  median(logitrhoeffect) + median(logitrhoeffectsd)*median(x_use)  +median(logitrhointeractioneffect)*probefactor[i]
    df = data.frame(logitrhoeffect_use, probefactor[i])
    names(df) = c("value", "probefactor")
    return(df)
  }
)

psseffect_v_rhoeffect = merge(rhoeffect_ids, psseffect_ids)

get_scat = function(data, x_lab, y_lab, cor_val, cor_lab) {
  data_forgg = data  
  
#   fn = function(probefactor, judgementfactor) {
#     if (probefactor == 1) {
#       if (judgementfactor == 1) {
#         factor = "long & second"
#       } else {
#         factor = "long & first"
#       }
#     } else {
#       if (judgementfactor == 1) {
#         factor = "short & second"
#       } else {
#         factor = "short & first"
#       }
#     }
#    
#     return(factor)
#   }
#   
#   data_forgg = ddply(
#     .data = data
#     , .variables = .(participant)
#     , .fun = transform 
#     , factor = fn(probefactor, judgementfactor)     
#   )
  
  gg1 = ggplot(data = data_forgg, aes(y = psseffect, x = value, colour = factor(probefactor), shape = factor(probefactor), fill = factor(judgementfactor)))+
    scale_y_continuous(name = y_lab)+
    scale_x_continuous(name = x_lab)+
    geom_vline(xintercept = 0, linetype = 2, size = 1)+
    geom_hline(yintercept = 0, linetype = 2, size = 1)+
    geom_point(size = 4)+
    scale_shape_manual(name = "Probe\nDuration", labels = c("Short", "Long") , values = c(21,22) )+
    scale_fill_manual(name = "Judgement\nType", labels = c("Second", "First"), values = c("white", "black"))+
    scale_colour_manual(name = "Probe\nDuration", labels =c("Short", "Long"), values = c("red", "blue") )+
    theme_gray(base_size = 30)+
    theme(panel.grid.major = element_line(size = 1.5)
          ,panel.grid.minor = element_line(size = 1))
          # , legend.position = "none")
  
  print(gg1)
  
  ### Violin
  gg2 = ggplot(
    data = pos_corr[pos_corr$parameter == cor_val,]
    , aes(x = parameter, y = value)
  )+
    geom_violin()+
    labs(x = cor_lab, y = "Correlation Coefficient (r)")+
    stat_summary(fun.data = get_95_HDI, size = 0.7)+
    stat_summary(fun.data = get_50_HDI, size = 2.5)+  
    geom_hline(yintercept = 0, linetype = 2, size = 1)+
    theme_gray(base_size = 30)+
    theme(panel.grid.major = element_line(size = 1.5)
          ,panel.grid.minor = element_line(size = 1)
          , axis.text.x = element_blank()
          , axis.ticks.x = element_blank()) 
  
  print(gg2)
  
  HDI95 = get_95_HDI( pos_corr[pos_corr$parameter == cor_val,]$value ) 
  
  return(HDI95)
}

get_scat(psseffect_v_rhoeffect
         , "Logit \u03C1 Effect Mean"
         , "PSS Effect Mean"
         , "value.2.7"
         , "Logit \u03C1 vs. PSS Effect Means"
         )
#---------------------------- Rho vs. PSS Effects -----------------------------------------#


#---------------------------- Kappa vs. PSS Effects ---------------------------------------#
logkappamean = extract_samples("logKappaMean")

logkappasd = extract_samples("zlogKappaSD", TRUE)

logkappaeffect = extract_samples("logKappaEffectMean")

logkappaeffectsd = extract_samples("zlogKappaEffectSD", TRUE)

logkappaprobeeffect = extract_samples("logKappaProbeEffectMean")

logkappainteractioneffect = extract_samples("logKappaProbeInteractionEffectMean")

kappaeffect_ids = ddply(
  .data = betas
  , .variables = .(participant)
  , .fun = function(x){
    i = unique(x$participant)
    x_use = x[x$parameter == "logKappaEffectMean",]$value
    logkappaeffect_use =  median(logkappaeffect) + median(logkappaeffectsd)*median(x_use)  +median(logkappainteractioneffect)*probefactor[i]
    df = data.frame(logkappaeffect_use, probefactor[i])
    names(df) = c("value","probefactor")
    return(df)
  }
)

psseffect_v_kappaeffect = merge(kappaeffect_ids, psseffect_ids)

get_scat(psseffect_v_kappaeffect
         , "Log \u03BA Effect Mean"
         , "Half PSS Effect Mean (Normalized)"
         , "value.2.8"
         , "Log \u03BA vs. PSS Effect Means"
)
#---------------------------- Kappa vs. PSS Effects ---------------------------------------#



#------------------------------------------------------------------------------------------#
#--------------------------------- Parameters ---------------------------------------------#
#------------------------------------------------------------------------------------------#

get_violin = function(value1, label1, value2 = NULL, label2 = NULL, y_lab, hline = TRUE, facet = FALSE) {
  df = data.frame(  
    value = c(
      value1
      , value2
    )
    , parameter = c(
      rep(label1, 60000)  
      , rep(label2, 60000)  
    )
  )
  
  print("is 60,000 instead of 80,000 right now")
  
  gg = ggplot(data = df)+
    geom_violin(aes(x = parameter, y = value))+
    labs(x = "", y = y_lab)+
    stat_summary(aes(x = parameter, y = value), fun.data = get_95_HDI, size = 0.7)+  
    stat_summary(aes(x = parameter, y = value), fun.data = get_50_HDI, size = 2.5)
  
  if (hline) {
    gg = gg + geom_hline(yintercept = 0, linetype = 2, size = 1)
  }
  
  if (facet) {
    gg = gg + facet_wrap(~parameter, scales = "free")
  } 
  
  gg = gg + theme_gray(base_size = 30)+
    theme(
      panel.grid.major = element_line(size = 1.5)
      , panel.grid.minor = element_line(size = 1)
      , strip.background = element_blank()
      , strip.text.x = element_blank() 
      , axis.ticks.x = element_blank() 
    ) 
  
  print(gg)
  
  print("value 1"); print(get_95_HDI(value1) )
  if ( is.null(value2) ) {
    to_print = NA
  } else {
    to_print = get_95_HDI(value2)
  } 
  print("value 2"); print( to_print )
}

#---------------------------------- SOA Intercepts ----------------------------------------#
get_violin(
  ex_toj_color_post$population_pss_intercept_mean * 250
  , "PSS Intercept Mean"
  , exp( ex_toj_color_post$population_logjnd_intercept_mean ) * 250
  , "JND Intercept Mean"
  , y_lab = "SOA (ms)"
  , hline = FALSE
  , facet = TRUE
)
#---------------------------------- SOA Intercepts ----------------------------------------#


#---------------------------------- SOA Attention Effects ---------------------------------#
# effect of attention on PSS and JND
get_violin(
  ( (ex_toj_color_post$population_pss_intercept_mean + ex_toj_color_post$population_pss_effect_mean/2) 
    - (ex_toj_color_post$population_pss_intercept_mean - ex_toj_color_post$population_pss_effect_mean/2) ) * 250
  , "PSS Effect Mean"
  , ( exp( ex_toj_color_post$population_logjnd_intercept_mean + ex_toj_color_post$population_logjnd_effect_mean/2 )
      - exp( ex_toj_color_post$population_logjnd_intercept_mean - ex_toj_color_post$population_logjnd_effect_mean/2  ) ) * 250 
  , "JND Effect Mean"
  , y_lab = "SOA (Right - Left; ms)"
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
)

#  effect of interaction between judgement type and attention on PSS 
get_violin(
  (ex_toj_color_post$population_pss_judgement_type_interaction_effect_mean) * 250
  , "PSS Judgement Type\nInteraction Effect Mean"
  , y_lab = "SOA (ms)"
)

#  effect of interaction between judgement type and attention on JND
get_violin(
  ( exp( ex_toj_color_post$population_logjnd_intercept_mean + ex_toj_color_post$population_logjnd_judgement_type_interaction_effect_mean/2 )
    - exp( ex_toj_color_post$population_logjnd_intercept_mean - ex_toj_color_post$population_logjnd_judgement_type_interaction_effect_mean/2  ) ) * 250 
  , "JND Judgement Type\nInteraction Effect Mean"
  , y_lab = "SOA (ms)"
)

# effect of attention on PSS by judgement type
get_violin(
  ( (ex_toj_color_post$population_pss_intercept_mean + (ex_toj_color_post$population_pss_effect_mean + ex_toj_color_post$population_pss_judgement_type_interaction_effect_mean)/2) 
    - (ex_toj_color_post$population_pss_intercept_mean - (ex_toj_color_post$population_pss_effect_mean + ex_toj_color_post$population_pss_judgement_type_interaction_effect_mean)/2 ) ) * 250
  , "PSS Attention Effect\nGiven Second"
  , ( (ex_toj_color_post$population_pss_intercept_mean + (ex_toj_color_post$population_pss_effect_mean - ex_toj_color_post$population_pss_judgement_type_interaction_effect_mean)/2) 
      - (ex_toj_color_post$population_pss_intercept_mean - (ex_toj_color_post$population_pss_effect_mean - ex_toj_color_post$population_pss_judgement_type_interaction_effect_mean)/2 ) ) * 250
  , "PSS Attention Effect\nGiven First"
  , y_lab = "SOA (Attended - Unattended; ms)"
)
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
)

# effect of interaction between probe duration and attention on PSS
get_violin(
  (ex_toj_color_post$population_pss_probe_interaction_effect_mean) * 250
  , "PSS Probe Duration\nInteraction Effect Mean"
  , y_lab = "SOA (ms)"
)

# effect of interaction between probe duration and attention on JND
get_violin(
  ( exp( ex_toj_color_post$population_logjnd_intercept_mean + ex_toj_color_post$population_logjnd_probe_interaction_effect_mean/2 )
    - exp( ex_toj_color_post$population_logjnd_intercept_mean - ex_toj_color_post$population_logjnd_probe_interaction_effect_mean/2  ) ) * 250 
  , "JND Probe Duration\nInteraction Effect Mean"
  , y_lab = "SOA (ms)"
)

# effect of attention on PSS by judgement type
get_violin(
  ( (ex_toj_color_post$population_pss_intercept_mean + (ex_toj_color_post$population_pss_effect_mean + ex_toj_color_post$population_pss_probe_interaction_effect_mean)/2) 
    - (ex_toj_color_post$population_pss_intercept_mean - (ex_toj_color_post$population_pss_effect_mean + ex_toj_color_post$population_pss_probe_interaction_effect_mean)/2 ) ) * 250
  , "PSS Attention Effect\nGiven Long\nProbe Duration"
  , ( (ex_toj_color_post$population_pss_intercept_mean + (ex_toj_color_post$population_pss_effect_mean - ex_toj_color_post$population_pss_probe_interaction_effect_mean)/2) 
      - (ex_toj_color_post$population_pss_intercept_mean - (ex_toj_color_post$population_pss_effect_mean - ex_toj_color_post$population_pss_probe_interaction_effect_mean)/2 ) ) * 250
  , "PSS Attention Effect\nGiven Short\nProbe Duration"
  , y_lab = "SOA (Attended - Unattended; ms)"
)
#------------------------------- SOA Probe Duration Effects -------------------------------#


#---------------------------------- Rho Intercept -----------------------------------------#
get_violin(
  plogis(ex_toj_color_post$logitRhoMean)
  , "Probability of Memory Intercept Mean"
  , y_lab = "\u03C1"
  , hline = FALSE
)
#---------------------------------- Rho Intercept -----------------------------------------#


#-------------------------------- Rho Attention Effect ------------------------------------#
get_violin(
  ( plogis(ex_toj_color_post$logitRhoMean + ex_toj_color_post$logitRhoEffectMean/2 )
    - plogis(ex_toj_color_post$logitRhoMean - ex_toj_color_post$logitRhoEffectMean/2 ) )
  , "Probability of Memory Intercept Mean"
  , y_lab = "\u03C1 (Attended - Unattended)"
)
#-------------------------------- Rho Attention Effect ------------------------------------#


#-------------------------------- Rho Judgement Effects -----------------------------------#
get_violin(
  ( plogis(ex_toj_color_post$logitRhoMean + ex_toj_color_post$logitRhoJudgementTypeEffectMean/2 )
    - plogis(ex_toj_color_post$logitRhoMean - ex_toj_color_post$logitRhoJudgementTypeEffectMean/2 ) )
  , "Probability of Memory\nJudgement Type Effect Mean"
  , y_lab = "\u03C1 (Second - First)"
)

get_violin(
  ( plogis(ex_toj_color_post$logitRhoMean + ex_toj_color_post$logitRhoJudgementTypeInteractionEffectMean/2 )
    - plogis(ex_toj_color_post$logitRhoMean - ex_toj_color_post$logitRhoJudgementTypeInteractionEffectMean/2 ) )
  , "Probability of Memory\nJudgement Type Interaction Effect Mean"
  , y_lab = "\u03C1"
)

get_violin(
  ( plogis(ex_toj_color_post$logitRhoMean + ex_toj_color_post$logitRhoJudgementTypeEffectMean/2
           + (ex_toj_color_post$logitRhoEffectMean + ex_toj_color_post$logitRhoJudgementTypeInteractionEffectMean)/2 )
    - plogis(ex_toj_color_post$logitRhoMean + ex_toj_color_post$logitRhoJudgementTypeEffectMean/2
             - (ex_toj_color_post$logitRhoEffectMean +  ex_toj_color_post$logitRhoJudgementTypeInteractionEffectMean)/2 ) )
  , "Probability of Memory\nAttention Effect\nGiven Second"
  , ( plogis(ex_toj_color_post$logitRhoMean - ex_toj_color_post$logitRhoJudgementTypeEffectMean/2
             + (ex_toj_color_post$logitRhoEffectMean - ex_toj_color_post$logitRhoJudgementTypeInteractionEffectMean)/2 )
      - plogis(ex_toj_color_post$logitRhoMean - ex_toj_color_post$logitRhoJudgementTypeEffectMean/2
               - (ex_toj_color_post$logitRhoEffectMean -  ex_toj_color_post$logitRhoJudgementTypeInteractionEffectMean)/2 ) )
  , "Probability of Memory\nAttention Effect\nGiven First"
  , y_lab = "\u03C1 (Attended - Unattended)"
)
#-------------------------------- Rho Judgement Effects -----------------------------------#


#-------------------------------- Rho Probe Effects ---------------------------------------#
get_violin(
  ( plogis(ex_toj_color_post$logitRhoMean + ex_toj_color_post$logitRhoProbeEffectMean/2 )
    - plogis(ex_toj_color_post$logitRhoMean - ex_toj_color_post$logitRhoProbeEffectMean/2 ) )
  , "Probability of Memory\nProbe Duration Effect Mean"
  , y_lab = "\u03C1 (Long - Short)"
)

get_violin(
  ( plogis(ex_toj_color_post$logitRhoMean + ex_toj_color_post$logitRhoProbeInteractionEffectMean/2 )
    - plogis(ex_toj_color_post$logitRhoMean - ex_toj_color_post$logitRhoProbeInteractionEffectMean/2 ) )
  , "Probability of Memory\nProbe Duration Interaction Effect Mean"
  , y_lab = "\u03C1"
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
)
#-------------------------------- Rho Probe Effects ---------------------------------------#


#---------------------------------- Kappa Intercept ---------------------------------------#
get_violin(
  exp( ex_toj_color_post$logKappaMean ) 
  , "Fidelity of Memory Intercept Mean"
  , y_lab = "\u03BA"
)
#---------------------------------- Kappa Intercept ---------------------------------------#


#-------------------------------- Kappa Attention Effect ----------------------------------#
get_violin(
  ( exp(ex_toj_color_post$logKappaMean + ex_toj_color_post$logKappaEffectMean/2) 
    - exp(ex_toj_color_post$logKappaMean - ex_toj_color_post$logKappaEffectMean/2) )
  , "Fidelity of Memory Effect Mean"
  , y_lab = "\u03BA (Attended - Unattended)"
)
#-------------------------------- Kappa Attention Effect ----------------------------------#


#-------------------------------- Kappa Judgement Effects ---------------------------------#
get_violin(
  ( exp(ex_toj_color_post$logKappaMean + ex_toj_color_post$logKappaJudgementTypeEffectMean/2 )
    - exp(ex_toj_color_post$logKappaMean - ex_toj_color_post$logKappaJudgementTypeEffectMean/2 ) )
  , "Fidelity of Memory Judgement Effect Mean"
  , y_lab = "\u03BA (Second - First)"
)

get_violin(
  ( exp(ex_toj_color_post$logKappaMean + ex_toj_color_post$logKappaJudgementTypeInteractionEffectMean/2 )
    - exp(ex_toj_color_post$logKappaMean - ex_toj_color_post$logKappaJudgementTypeInteractionEffectMean/2 ) )
  , "Fidelity of Memory Judgement Type\nInteraction Effect Mean"
  , y_lab = "\u03BA"
)

get_violin(
  ( exp(ex_toj_color_post$logKappaMean + ex_toj_color_post$logKappaJudgementTypeEffectMean/2
        + (ex_toj_color_post$logKappaEffectMean + ex_toj_color_post$logKappaJudgementTypeInteractionEffectMean)/2 )
    - exp(ex_toj_color_post$logKappaMean + ex_toj_color_post$logKappaJudgementTypeEffectMean/2
          - (ex_toj_color_post$logKappaEffectMean +  ex_toj_color_post$logKappaJudgementTypeInteractionEffectMean)/2 ) )
  , "Fidelity of Memory\nAttention Effect\nGiven Second"
  , ( exp(ex_toj_color_post$logKappaMean - ex_toj_color_post$logKappaJudgementTypeEffectMean/2
          + (ex_toj_color_post$logKappaEffectMean - ex_toj_color_post$logKappaJudgementTypeInteractionEffectMean)/2 )
      - exp(ex_toj_color_post$logKappaMean - ex_toj_color_post$logKappaJudgementTypeEffectMean/2
            - (ex_toj_color_post$logKappaEffectMean -  ex_toj_color_post$logKappaJudgementTypeInteractionEffectMean)/2 ) )
  , "Fidelity of Memory\nAttention Effect\nGiven First"
  , y_lab = "\u03BA (Attended - Unattended)"
)
#-------------------------------- Kappa Judgement Effects ---------------------------------#


#-------------------------------- Kappa Probe Effects -------------------------------------#
get_violin(
  ( exp(ex_toj_color_post$logKappaMean + ex_toj_color_post$logKappaProbeEffectMean/2 )
    - exp(ex_toj_color_post$logKappaMean - ex_toj_color_post$logKappaProbeEffectMean/2 ) )
  , "Fidelity of Memory Probe Effect Mean"
  , y_lab = "\u03BA (Long - Short)"
)

get_violin(
  ( exp(ex_toj_color_post$logKappaMean + ex_toj_color_post$logKappaProbeInteractionEffectMean/2 )
    - exp(ex_toj_color_post$logKappaMean - ex_toj_color_post$logKappaProbeInteractionEffectMean/2 ) )
  , "Fidelity of Memory Probe Duration\nInteraction Effect Mean"
  , y_lab = "\u03BA"
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





