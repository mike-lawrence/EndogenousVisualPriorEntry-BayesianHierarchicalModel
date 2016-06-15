# library(shinystan)
library(coda)
library(ggplot2)
library(ggmcmc)
library(CircStats)
library(reshape2)
library(plyr)
library(grid)

setwd("/Users/ghislaindentremont/Documents/TOJ/Baseball")
load("toj_color_post_June4th2016" )  # Rstan results
load("toj_trials.Rdata")  # actual toj data
load("color_trials.Rdata")  # actual color data



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
               , "logKappaEffectMean"
               , "logKappaMean"
               , "population_logjnd_effect_mean"
               , "population_logjnd_convention_effect_mean"
               , "population_logjnd_intercept_mean"
               , "population_pss_effect_mean"
               , "population_pss_convention_effect_mean" 
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
real_toj = aggregate(safe ~ soa2 + base_probe_dist, data = toj_trials, FUN = mean)
real_toj_convention = aggregate(safe ~ soa2 + know_tie_goes_runner, data = toj_trials, FUN = mean)
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
# pss convention effect mean
pss_convention_effect_mean = gg_toj_color_post[gg_toj_color_post$Parameter == "population_pss_convention_effect_mean",]$value

# pss glove
pss_glove_mean_reps = get_condition_mean_sample(pss_intercept_mean, pss_effect_mean, FALSE, "null")

# pss base
pss_base_mean_reps = get_condition_mean_sample(pss_intercept_mean, pss_effect_mean, TRUE, "null")

# pss true convention
pss_know_mean_reps = get_condition_mean_sample(pss_intercept_mean, pss_convention_effect_mean, FALSE, "null")

# pss false convention
pss_dontknow_mean_reps = get_condition_mean_sample(pss_intercept_mean, pss_convention_effect_mean, TRUE, "null")

### Get JND Parameters
# jnd intercept mean
jnd_intercept_mean = gg_toj_color_post[gg_toj_color_post$Parameter == "population_logjnd_intercept_mean",]$value
# jnd effect mean
jnd_effect_mean = gg_toj_color_post[gg_toj_color_post$Parameter == "population_logjnd_effect_mean",]$value
# jnd convention effect mean
jnd_convention_effect_mean = gg_toj_color_post[gg_toj_color_post$Parameter == "population_logjnd_convention_effect_mean",]$value

# jnd glove
jnd_glove_mean_reps = get_condition_mean_sample(jnd_intercept_mean, jnd_effect_mean, FALSE, "log")

# jnd base
jnd_base_mean_reps = get_condition_mean_sample(jnd_intercept_mean, jnd_effect_mean, TRUE, "log")

# jnd true convention
jnd_know_mean_reps = get_condition_mean_sample(jnd_intercept_mean, jnd_convention_effect_mean, FALSE, "log")

# jnd false convention
jnd_dontknow_mean_reps = get_condition_mean_sample(jnd_intercept_mean, jnd_convention_effect_mean, TRUE, "log")
#-------------------------------------- TOJ Simulated Data --------------------------------#


#-------------------------------------- Do TOJ PPC ----------------------------------------#
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

# true convention
plot(SOAs, pnorm( SOAs, mean = pss_know_mean_reps[1], sd = jnd_know_mean_reps[1]), main = "know convention", ylab = "safe proportion", xlab = "SOA", col = alpha("turquoise", 0.5) )
for (i in 2:length(pss_know_mean_reps)) {
  points(SOAs, pnorm(SOAs, mean = pss_know_mean_reps[i], sd = jnd_know_mean_reps[i]), ylab = "safe proportion", xlab = "SOA", col = alpha("turquoise", 0.5) )
}
# real true convention
real_know = real_toj_convention[real_toj_convention$know_tie_goes_runner == TRUE,]$safe
points(SOAs,real_know, col = "blue", pch = 15 )

# false convention
plot( SOAs, pnorm( SOAs, mean = pss_dontknow_mean_reps[1], sd = jnd_dontknow_mean_reps[1]), main = "don't know convention", ylab = "safe proportion", xlab = "SOA", col = alpha("pink", 0.5) )
for (i in 2:length(pss_dontknow_mean_reps)) {
  points( SOAs, pnorm( SOAs, mean = pss_dontknow_mean_reps[i], sd = jnd_dontknow_mean_reps[i]), col = alpha("pink", 0.5) )
}
# real false convention
real_dontknow = real_toj_convention[real_toj_convention$know_tie_goes_runner == FALSE,]$safe
points(SOAs,real_dontknow, col = "red", pch = 15 )
#-------------------------------------- Do TOJ PPC ----------------------------------------#


#-------------------------------------- Color Actual Data ---------------------------------#
hist(color_trials[color_trials$attended == TRUE,]$color_diff_radians, breaks = 30, freq = F, col = rgb(.1,.1,.1,.5))
hist(color_trials[color_trials$attended == FALSE,]$color_diff_radians, breaks = 30, freq = F, col = rgb(.9,.9,.9,.5), add = T)
real_color = aggregate(color_diff_radians ~ attended, data = color_trials, FUN  = mean)
#-------------------------------------- Color Actual Data ---------------------------------#


#-------------------------------------- Color Simulated Data ------------------------------#
# rho intercept mean
rho_intercept_mean = gg_toj_color_post[gg_toj_color_post$Parameter == "logitRhoMean",]$value
# rho effect mean
rho_effect_mean = gg_toj_color_post[gg_toj_color_post$Parameter == "logitRhoEffectMean",]$value

# rho attend 
rho_attend_reps = get_condition_mean_sample(rho_intercept_mean, rho_effect_mean, TRUE, "logit")

# rho unattend
rho_unattend_reps = get_condition_mean_sample(rho_intercept_mean, rho_effect_mean, FALSE, "logit")

### Get Kappa Parameters
# kappa intercept mean 
kappa_intercept_mean = gg_toj_color_post[gg_toj_color_post$Parameter == "logKappaMean",]$value
# kappa effect mean
kappa_effect_mean = gg_toj_color_post[gg_toj_color_post$Parameter == "logKappaEffectMean",]$value

# kappa attend 
kappa_attend_reps = get_condition_mean_sample(kappa_intercept_mean, kappa_effect_mean, TRUE, "log_free")

# kappa unattend
kappa_unattend_reps = get_condition_mean_sample(kappa_intercept_mean, kappa_effect_mean, FALSE, "log_free")
#-------------------------------------- Color Simulated Data ------------------------------#


#-------------------------------------- Do Color PPC --------------------------------------#
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

# version so nothing gets flipped twice by accident 
ex_toj_color_post2 = extract(toj_color_post)

# flip pss and jnd effects to make positive values indicate predicted results
ex_toj_color_post = ex_toj_color_post2
ex_toj_color_post$population_pss_effect_mean = -ex_toj_color_post2$population_pss_effect_mean
ex_toj_color_post$population_logjnd_effect_mean = -ex_toj_color_post2$population_logjnd_effect_mean

# get correlation posteriors 
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
betas2 = data.frame(value = ex_toj_color_post$beta)
betas2$iteration = rownames(betas2)

# melt
ptm = proc.time()
betas3 = melt( betas2 )  # this takes a while
proc.time() - ptm

betas3$parameter = rep( c(
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
, each = nrow(betas2)*length(unique(betas3$variable))/8  # 8 is number of parameters 
)  
betas3$participant = rep(c(1:44), times = 8, each = nrow(betas2))

# flip betas 
betas = betas3
betas[betas$parameter=="population_pss_effect_mean",]$value = -betas3[betas3$parameter=="population_pss_effect_mean" ,]$value
betas[betas$parameter=="population_logjnd_effect_mean",]$value = -betas3[betas3$parameter=="population_logjnd_effect_mean",]$value
#-------------------------------------- Get Betas -----------------------------------------#


#---------------------------- JND vs. PSS Intercepts --------------------------------------#
pssmean2 = data.frame(value = ex_toj_color_post$population_pss_intercept_mean)
pssmean2$iteration = rownames(pssmean2)
pssmean = melt( pssmean2 )$value

psssd2 = data.frame(value = tan(ex_toj_color_post$zpopulation_pss_intercept_sd))
psssd2$iteration = rownames(psssd2)
psssd = melt( psssd2 )$value

pssconventioneffect2 = data.frame(value = ex_toj_color_post$population_pss_convention_effect_mean)
pssconventioneffect2$iteration = rownames(pssconventioneffect2)
pssconventioneffect = melt( pssconventioneffect2 )$value
conventionfactor = ifelse(aggregate(know_tie_goes_runner~id,data = toj_trials, FUN =unique)$know_tie_goes_runner, -1, 1)

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

logjndmean2 = data.frame(value = ex_toj_color_post$population_logjnd_intercept_mean)
logjndmean2$iteration = rownames(logjndmean2)
logjndmean = melt( logjndmean2 )$value

logjndsd2 = data.frame(value = tan(ex_toj_color_post$zpopulation_logjnd_intercept_sd))
logjndsd2$iteration = rownames(logjndsd2)
logjndsd = melt( logjndsd2 )$value

logjndconventioneffect2 = data.frame(value = ex_toj_color_post$population_logjnd_convention_effect_mean)
logjndconventioneffect2$iteration = rownames(logjndconventioneffect2)
logjndconventioneffect = melt( logjndconventioneffect2 )$value

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

ggplot(data = pss_v_jnd, aes(x =pssintercept, y = jndintercept))+
  scale_x_continuous(name = "PSS Intercept Mean (Normalized)")+
  scale_y_continuous(name = "Log JND Intercept Mean (Normalized)")+
  geom_point(size = 2)+
  # geom_smooth(method = "lm", se = FALSE, size = 1)+
  geom_smooth(span = 2)+
  theme_gray(base_size = 24)+
  theme(panel.grid.major = element_line(size = 1.5)
        ,panel.grid.minor = element_line(size = 1))

### Violin
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
#---------------------------- JND vs. PSS Intercepts --------------------------------------#


#---------------------------- Rho vs. PSS Effects -----------------------------------------#
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

logitrhoeffect2 = data.frame(value = ex_toj_color_post$logitRhoEffectMean)
logitrhoeffect2$iteration = rownames(logitrhoeffect2)
logitrhoeffect = melt( logitrhoeffect2 )$value

logitrhoeffectsd2 = data.frame(value = tan(ex_toj_color_post$zlogitRhoEffectSD))
logitrhoeffectsd2$iteration = rownames(logitrhoeffectsd2)
logitrhoeffectsd = melt( logitrhoeffectsd2 )$value

rhoeffect_ids = ddply(
  .data = betas
  , .variables = .(participant)
  , .fun = function(x){
    i = unique(x$participant)
    x_use = x[x$parameter == "logitRhoEffectMean",]$value
    logitrhoeffect_use = median(logitrhoeffect) + median(logitrhoeffectsd)*median(x_use)
    df = data.frame(logitrhoeffect_use)
    names(df) = "logitrhoeffect"
    return(df)
  }
)

psseffect_v_rhoeffect2 = merge(rhoeffect_ids, psseffect_ids)

# get rid of outliers
outlier_points = psseffect_v_rhoeffect2[psseffect_v_rhoeffect2$psseffect < -0.2,]

psseffect_v_rhoeffect = psseffect_v_rhoeffect2[!(psseffect_v_rhoeffect2$logitrhoeffect %in% outlier_points$logitrhoeffect),]

ggplot(data = psseffect_v_rhoeffect, aes(x = logitrhoeffect, y =psseffect))+
  scale_y_continuous(name = "Half PSS Effect Mean (Normalized)")+
  scale_x_continuous(name = "Logit \u03C1 Effect Mean")+
  geom_hline(yintercept = 0, linetype = 2, size = 1)+
  geom_vline(xintercept = 0, linetype = 2, size = 1)+
  geom_point(size = 3)+
  geom_smooth(method = "lm", se = FALSE, size = 1)+
  geom_smooth(
    data = psseffect_v_rhoeffect2
    , aes(y = psseffect, x = logitrhoeffect)
    , method = "lm", se = FALSE, size = 1, linetype = "dotted")+
  geom_point(data = outlier_points, aes(y = psseffect, x = logitrhoeffect), size = 3)+
  geom_point(data = outlier_points, aes(y = psseffect, x = logitrhoeffect), size = 1.5, colour = "grey90")+
  theme_gray(base_size = 30)+
  theme(panel.grid.major = element_line(size = 1.5)
        ,panel.grid.minor = element_line(size = 1)
        , legend.position = "none")
 
### Violin
# must take negative because I flipped pss effect sign
pos_corr_f = pos_corr
pos_corr_f[pos_corr_f$parameter == "value.2.7",]$value = -pos_corr[pos_corr$parameter == "value.2.7",]$value    
# plot
ggplot(
  data = pos_corr_f[pos_corr_f$parameter == "value.2.7",]  
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

# get 95% HDI
print("NOTE: make sure to flip sign (see above code)")
get_95_HDI(pos_corr_f[pos_corr_f$parameter == "value.2.7",]$value)
#---------------------------- Rho vs. PSS Effects -----------------------------------------#


#---------------------------- Kappa vs. PSS Effects ---------------------------------------#
logkappaeffect2 = data.frame(value = ex_toj_color_post$logKappaEffectMean)
logkappaeffect2$iteration = rownames(logkappaeffect2)
logkappaeffect = melt( logkappaeffect2 )$value

logkappaeffectsd2 = data.frame(value = tan(ex_toj_color_post$zlogKappaEffectSD))
logkappaeffectsd2$iteration = rownames(logkappaeffectsd2)
logkappaeffectsd = melt( logkappaeffectsd2 )$value

kappaeffect_ids = ddply(
  .data = betas
  , .variables = .(participant)
  , .fun = function(x){
    i = unique(x$participant)
    x_use = x[x$parameter == "logKappaEffectMean",]$value
    logkappaeffect_use = median(logkappaeffect) + median(logkappaeffectsd)*median(x_use)
    df = data.frame(logkappaeffect_use)
    names(df) = "logkappaeffect"
    return(df)
  }
)

psseffect_v_kappaeffect2 = merge(kappaeffect_ids, psseffect_ids)

# get rid of outliers
outlier_points = psseffect_v_kappaeffect2[psseffect_v_kappaeffect2$psseffect < -0.2,]

psseffect_v_kappaeffect = psseffect_v_kappaeffect2[!(psseffect_v_kappaeffect2$logkappaeffect %in% outlier_points$logkappaeffect),]

ggplot(data = psseffect_v_kappaeffect, aes(x = logkappaeffect, y =psseffect))+
  scale_y_continuous(name = "Half PSS Effect Mean (Normalized)")+
  scale_x_continuous(name = "Log \u03BA Effect Mean")+
  geom_hline(yintercept = 0, linetype = 2, size = 1)+
  geom_vline(xintercept = 0, linetype = 2, size = 1)+
  geom_point(size = 3)+
  geom_smooth(method = "lm", se = FALSE, size = 1)+
  geom_smooth(
    data = psseffect_v_kappaeffect2
    , aes(y = psseffect, x = logkappaeffect)
    , method = "lm", se = FALSE, size = 1, linetype = "dotted")+
  geom_point(data = outlier_points, aes(y = psseffect, x = logkappaeffect), size = 3)+
  geom_point(data = outlier_points, aes(y = psseffect, x = logkappaeffect), size = 1.5, colour = "grey90")+
  theme_gray(base_size = 30)+
  theme(panel.grid.major = element_line(size = 1.5)
        ,panel.grid.minor = element_line(size = 1)
        , legend.position = "none")

### Violin
# must take negative because I flipped pss effect sign
pos_corr_f = pos_corr
pos_corr_f[pos_corr_f$parameter == "value.2.8",]$value = -pos_corr[pos_corr$parameter == "value.2.8",]$value    
# plot
ggplot(
  data = pos_corr_f[pos_corr_f$parameter == "value.2.8",]  
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

# get 95% HDI
print("NOTE: make sure to flip sign (see above code)")
get_95_HDI(pos_corr_f[pos_corr_f$parameter == "value.2.8",]$value)
#---------------------------- Kappa vs. PSS Effects ---------------------------------------#




# #---------------------------- JND Effect vs. PSS Intercept --------------------------------#
# logjndeffect2 = data.frame(value = ex_toj_color_post$population_logjnd_effect_mean) # must devide by two for SD to make sense 
# logjndeffect2$iteration = rownames(logjndeffect2)
# logjndeffect = melt( logjndeffect2 )$value
# 
# logjndeffectsd2 = data.frame(value = tan(ex_toj_color_post$zpopulation_logjnd_effect_sd))
# logjndeffectsd2$iteration = rownames(logjndeffectsd2)
# logjndeffectsd = melt( logjndeffectsd2 )$value
# 
# jndeffect_ids = ddply(
#   .data = betas
#   , .variables = .(participant)
#   , .fun = function(x){
#     i = unique(x$participant)
#     x_use = x[x$parameter == "population_logjnd_effect_mean",]$value
#     logjndeffect = median(logjndeffect)/2 + median(logjndeffectsd)*median(x_use)
#     df = data.frame(logjndeffect)
#     names(df) = "jndeffect"
#     return(df)
#   }
# )
# 
# pss_v_jndeffect2 = merge(jndeffect_ids, pss_ids)
# 
# outlier_jndeffect_points = pss_v_jndeffect2[pss_v_jndeffect2$pssintercept > 0.25
#                                         | pss_v_jndeffect2$pssintercept < -0.50
#                                         ,]
# 
# pss_v_jndeffect = pss_v_jndeffect2[!(pss_v_jndeffect2$pssintercept %in% outlier_jndeffect_points$pssintercept),]
# 
# ggplot(data = pss_v_jndeffect, aes(x =pssintercept, y = jndeffect))+
#   scale_x_continuous(name = "PSS Intercept Mean (Normalized)")+
#   scale_y_continuous(name = "Half Log JND Effect Mean (Normalized)")+
#   geom_point(size = 3)+
#   geom_smooth(method = "lm", se = FALSE, size = 1)+
#   geom_smooth(
#     data = pss_v_jndeffect2
#     , aes(x = pssintercept, y = jndeffect)
#     , method = "lm", se = FALSE, size = 1, linetype = "dotted")+
#   geom_point(data = outlier_jndeffect_points, aes(x = pssintercept, y = jndeffect), size = 3)+
#   geom_point(data = outlier_jndeffect_points, aes(x = pssintercept, y = jndeffect), size = 1.5, colour = "grey90")+
#   theme_gray(base_size = 30)+
#   theme(panel.grid.major = element_line(size = 1.5)
#         ,panel.grid.minor = element_line(size = 1)
#         , legend.position = "none")
# 
# ### Violin
# # flip sign because I flipped jnd effect sign
# pos_corr_f[pos_corr_f$parameter == "value.1.4",]$value = -pos_corr[pos_corr$parameter == "value.1.4",]$value
# # plot
# ggplot(
#   data = pos_corr_f[pos_corr_f$parameter == "value.1.4",]
#   , aes(x = parameter, y = value)
# )+
#   geom_violin()+
#   labs(x = "Log JND Effect Means vs. PSS Intercept Means", y = "Correlation Coefficient (r)")+
#   stat_summary(fun.data = get_95_HDI, size = 0.7)+
#   stat_summary(fun.data = get_50_HDI, size = 2.5)+  
#   geom_hline(yintercept = 0, linetype = 2, size = 1)+
#   theme_gray(base_size = 24)+
#   theme(panel.grid.major = element_line(size = 1.5)
#         ,panel.grid.minor = element_line(size = 1)
#         , axis.text.x = element_blank()
#         , axis.ticks.x = element_blank())
# #---------------------------- JND Effect vs. PSS Intercept --------------------------------#



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
  labs(x = "", y = "SOA (Glove - Base; ms)")+
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
  scale_x_discrete(labels = c("JND Convention\nEffect Mean", "PSS Convention\nEffect Mean"))+
  geom_hline(yintercept = 0, linetype = 2, size = 1)+
  theme_gray(base_size = 30)+
  theme(panel.grid.major = element_line(size = 1.5)
        ,panel.grid.minor = element_line(size = 1)
        , axis.ticks.x = element_blank()) 

# 95% HDIs
# pss effect 
get_95_HDI(ex_toj_color_post$population_pss_convention_effect_mean*250 )  # multiply by two for full difference
# jnd effect 
get_95_HDI(
  ( exp( ex_toj_color_post$population_logjnd_intercept_mean + ex_toj_color_post$population_logjnd_convention_effect_mean/2 )
    - exp( ex_toj_color_post$population_logjnd_intercept_mean - ex_toj_color_post$population_logjnd_convention_effect_mean/2  ) ) * 250 
)
#---------------------------------- SOA Scale ---------------------------------------------#


#---------------------------------- Rho Scale ---------------------------------------------#
### Intercept
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

### Effect
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

### Effect
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
#---------------------------------- Kappa Scale -------------------------------------------#



#------------------------------------------------------------------------------------------#
#--------------------------------- Graphs -------------------------------------------------#
#------------------------------------------------------------------------------------------#

#---------------------------------- NCFs --------------------------------------------------#
# including effects 
yGlove = pnorm(
  -250:250
  , mean = ( median(ex_toj_color_post$population_pss_intercept_mean) + median(ex_toj_color_post$population_pss_effect_mean)/2 ) * 250
  , sd = ( exp( median(ex_toj_color_post$population_logjnd_intercept_mean) + median(ex_toj_color_post$population_logjnd_effect_mean)/2 )   ) * 250
)
yBase = pnorm(
  -250:250
  , mean = ( median(ex_toj_color_post$population_pss_intercept_mean) - median(ex_toj_color_post$population_pss_effect_mean)/2 ) * 250
  , sd = ( exp( median(ex_toj_color_post$population_logjnd_intercept_mean) - median(ex_toj_color_post$population_logjnd_effect_mean)/2 )   ) * 250
  
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
#---------------------------------- NCFs --------------------------------------------------#


#---------------------------------- Posteriors --------------------------------------------#
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
#---------------------------------- Posteriors --------------------------------------------#
