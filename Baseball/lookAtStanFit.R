# library(shinystan)
library(coda)
library(ggplot2)
library(ggmcmc)
library(CircStats)
library(reshape2)
library(plyr)
library(grid)

setwd("/Users/ghislaindentremont/Documents/TOJ/Baseball")
load("toj_color_post_June16th2016" )  # Rstan results
load("toj_trials.Rdata")  # actual toj data
load("color_trials.Rdata")  # actual color data
source("../EndogenousVisualPriorEntry-BayesianHierarchicalModel/functions.R")


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
               , "logitRhoConventionEffectMean"
               , "logitRhoConventionInteractionEffectMean"
               , "logKappaEffectMean"
               , "logKappaConventionEffectMean"
               , "logKappaConventionInteractionEffectMean"
               , "logKappaMean"
               , "population_logjnd_effect_mean"
               , "population_logjnd_convention_effect_mean"
               , "population_logjnd_intercept_mean"
               , "population_logjnd_convention_interaction_effect_mean"
               , "population_pss_effect_mean"
               , "population_pss_convention_effect_mean" 
               , "population_pss_intercept_mean"
               , "population_pss_convention_interaction_effect_mean"
               , "zlogitRhoEffectSD" 
               , "zlogitRhoSD"
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
real_toj = aggregate(safe ~ soa2 + base_probe_dist + know_tie_goes_runner, data = toj_trials, FUN = mean)
real_toj[,2] = as.character(real_toj[,2])
real_toj[,3] = as.character(real_toj[,3])
#-------------------------------------- TOJ Actual Data -----------------------------------#


#-------------------------------------- TOJ Simulated Data --------------------------------#
### Get PSS Parameters
pss_intercept_mean = gg_toj_color_post[gg_toj_color_post$Parameter == "population_pss_intercept_mean",]$value
pss_effect_mean = gg_toj_color_post[gg_toj_color_post$Parameter == "population_pss_effect_mean",]$value
pss_convention_effect_mean = gg_toj_color_post[gg_toj_color_post$Parameter == "population_pss_convention_effect_mean",]$value
pss_convention_interaction_effect_mean = gg_toj_color_post[gg_toj_color_post$Parameter == "population_pss_convention_interaction_effect_mean",]$value

pss_glove_know_mean_reps = get_condition_mean_sample(
  pss_intercept_mean - pss_convention_effect_mean/2
  , (pss_effect_mean - pss_convention_interaction_effect_mean )
  , FALSE
  , "null"
)

pss_glove_dontknow_mean_reps = get_condition_mean_sample(
  pss_intercept_mean + pss_convention_effect_mean/2
  , (pss_effect_mean + pss_convention_interaction_effect_mean )
  , FALSE
  , "null"
)

pss_base_know_mean_reps = get_condition_mean_sample(
  pss_intercept_mean - pss_convention_effect_mean/2
  , (pss_effect_mean - pss_convention_interaction_effect_mean )
  , TRUE
  , "null"
)

pss_base_dontknow_mean_reps = get_condition_mean_sample(
  pss_intercept_mean + pss_convention_effect_mean/2
  , (pss_effect_mean + pss_convention_interaction_effect_mean )
  , TRUE
  , "null"
)


### Get JND Parameters
jnd_intercept_mean = gg_toj_color_post[gg_toj_color_post$Parameter == "population_logjnd_intercept_mean",]$value
jnd_effect_mean = gg_toj_color_post[gg_toj_color_post$Parameter == "population_logjnd_effect_mean",]$value
jnd_convention_effect_mean = gg_toj_color_post[gg_toj_color_post$Parameter == "population_logjnd_convention_effect_mean",]$value
jnd_convention_interaction_effect_mean = gg_toj_color_post[gg_toj_color_post$Parameter == "population_logjnd_convention_interaction_effect_mean",]$value

jnd_glove_know_mean_reps = get_condition_mean_sample(
  jnd_intercept_mean - jnd_convention_effect_mean/2
  , (jnd_effect_mean - jnd_convention_interaction_effect_mean )
  , FALSE
  , "log"
)

jnd_glove_dontknow_mean_reps = get_condition_mean_sample(
  jnd_intercept_mean + jnd_convention_effect_mean/2
  , (jnd_effect_mean + jnd_convention_interaction_effect_mean )
  , FALSE
  , "log"
)

jnd_base_know_mean_reps = get_condition_mean_sample(
  jnd_intercept_mean - jnd_convention_effect_mean/2
  , (jnd_effect_mean - jnd_convention_interaction_effect_mean )
  , TRUE
  , "log"
)

jnd_base_dontknow_mean_reps = get_condition_mean_sample(
  jnd_intercept_mean + jnd_convention_effect_mean/2
  , (jnd_effect_mean + jnd_convention_interaction_effect_mean )
  , TRUE
  , "log"
)
#-------------------------------------- TOJ Simulated Data --------------------------------#


#-------------------------------------- Do TOJ PPC ----------------------------------------#
SOAs = c(-250, -150, -100, -50, -17, 17, 50, 100, 150, 250)

do_toj_ppc(
  pss_glove_know_mean_reps
  , jnd_glove_know_mean_reps
  , "know convention and attend glove"
  , c("know_tie_goes_runner", "base_probe_dist")
  , c("TRUE", "0.2")
  , "safe proportion"
)

do_toj_ppc(
  pss_glove_dontknow_mean_reps
  , jnd_glove_dontknow_mean_reps
  , "don't know convention and attend glove"
  , c("know_tie_goes_runner", "base_probe_dist")
  , c("FALSE", "0.2")
  , "safe proportion"
)

do_toj_ppc(
  pss_base_know_mean_reps
  , jnd_base_know_mean_reps
  , "know convention and attend base"
  , c("know_tie_goes_runner", "base_probe_dist")
  , c("TRUE", "0.8")
  , "safe proportion"
)

do_toj_ppc(
  pss_base_dontknow_mean_reps
  , jnd_base_dontknow_mean_reps
  , "don't know convention and attend base"
  , c("know_tie_goes_runner", "base_probe_dist")
  , c("FALSE", "0.8")
  , "safe proportion"
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
rho_convention_effect_mean = gg_toj_color_post[gg_toj_color_post$Parameter == "logitRhoConventionEffectMean",]$value
rho_convention_interaction_effect_mean = gg_toj_color_post[gg_toj_color_post$Parameter == "logitRhoConventionInteractionEffectMean",]$value

rho_attend_know_reps = get_condition_mean_sample(
  rho_intercept_mean - rho_convention_effect_mean/2
  , (rho_effect_mean - rho_convention_interaction_effect_mean)
  , TRUE
  , "logit"
  )

rho_attend_dontknow_reps = get_condition_mean_sample(
  rho_intercept_mean + rho_convention_effect_mean/2
  , (rho_effect_mean + rho_convention_interaction_effect_mean)
  , TRUE
  , "logit"
)

rho_unattend_know_reps = get_condition_mean_sample(
  rho_intercept_mean - rho_convention_effect_mean/2
  , (rho_effect_mean - rho_convention_interaction_effect_mean)
  , FALSE
  , "logit"
)

rho_unattend_dontknow_reps = get_condition_mean_sample(
  rho_intercept_mean + rho_convention_effect_mean/2
  , (rho_effect_mean + rho_convention_interaction_effect_mean)
  , FALSE
  , "logit"
)

### Get Kappa Parameters
kappa_intercept_mean = gg_toj_color_post[gg_toj_color_post$Parameter == "logKappaMean",]$value
kappa_effect_mean = gg_toj_color_post[gg_toj_color_post$Parameter == "logKappaEffectMean",]$value
kappa_convention_effect_mean = gg_toj_color_post[gg_toj_color_post$Parameter == "logKappaConventionEffectMean",]$value
kappa_convention_interaction_effect_mean = gg_toj_color_post[gg_toj_color_post$Parameter == "logKappaConventionInteractionEffectMean",]$value

kappa_attend_know_reps = get_condition_mean_sample(
  kappa_intercept_mean - kappa_convention_effect_mean/2
  , (kappa_effect_mean - kappa_convention_interaction_effect_mean)
  , TRUE
  , "log_free"
)

kappa_attend_dontknow_reps = get_condition_mean_sample(
  kappa_intercept_mean + kappa_convention_effect_mean/2
  , (kappa_effect_mean + kappa_convention_interaction_effect_mean)
  , TRUE
  , "log_free"
)

kappa_unattend_know_reps = get_condition_mean_sample(
  kappa_intercept_mean - kappa_convention_effect_mean/2
  , (kappa_effect_mean - kappa_convention_interaction_effect_mean)
  , FALSE
  , "log_free"
)

kappa_unattend_dontknow_reps = get_condition_mean_sample(
  kappa_intercept_mean + kappa_convention_effect_mean/2
  , (kappa_effect_mean + kappa_convention_interaction_effect_mean)
  , FALSE
  , "log_free"
)
#-------------------------------------- Color Simulated Data ------------------------------#


#-------------------------------------- Do Color PPC --------------------------------------#
# look at mixture model
plot( seq(-pi, pi, pi/200), (rho_unattend_reps[1])*dvm(seq(-pi, pi, pi/200), 0, kappa_unattend_reps[1]) 
      + (1-rho_unattend_reps[1]) * dunif(seq(-pi, pi, pi/200), -pi, pi)
      , xlab = "radian deviations", ylab = "density")

do_color_ppc(
  rho_attend_know_reps
  , kappa_attend_know_reps
  , "know convention and attend"
  , c("know_tie_goes_runner", "attended")
  , c("TRUE", "TRUE")
)

do_color_ppc(
  rho_attend_dontknow_reps
  , kappa_attend_dontknow_reps
  , "don't know convention and attend"
  , c("know_tie_goes_runner", "attended")
  , c("FALSE", "TRUE")
)

do_color_ppc(
  rho_unattend_know_reps
  , kappa_unattend_know_reps
  , "know convention and unattend"
  , c("know_tie_goes_runner", "attended")
  , c("TRUE", "FALSE")
)

do_color_ppc(
  rho_unattend_dontknow_reps
  , kappa_unattend_dontknow_reps
  , "don't know convention and unattend"
  , c("know_tie_goes_runner", "attended")
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

# version so nothing gets flipped twice by accident 
ex_toj_color_post2 = extract(toj_color_post)

# flip pss and jnd effects to make positive values indicate predicted results
ex_toj_color_post = ex_toj_color_post2

ex_toj_color_post$population_pss_effect_mean = -ex_toj_color_post2$population_pss_effect_mean
ex_toj_color_post$population_pss_convention_interaction_effect_mean = -ex_toj_color_post2$population_pss_convention_interaction_effect_mean

ex_toj_color_post$population_logjnd_effect_mean = -ex_toj_color_post2$population_logjnd_effect_mean
ex_toj_color_post$population_logjnd_convention_interaction_effect_mean = -ex_toj_color_post2$population_logjnd_convention_interaction_effect_mean


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


#---------------------------- Rho vs. PSS Effects -----------------------------------------#
psseffect = extract_samples("population_pss_effect_mean")

psseffectsd = extract_samples("zpopulation_pss_effect_sd", TRUE)

pssinteractioneffect = extract_samples("population_pss_convention_interaction_effect_mean")

conventionfactor = ifelse(aggregate(know_tie_goes_runner~id,data = toj_trials, FUN =unique)$know_tie_goes_runner, -1, 1)

psseffect_ids = ddply(
  .data = betas
  , .variables = .(participant)
  , .fun = function(x){
    i = unique(x$participant)
    x_use = x[x$parameter ==  "population_pss_effect_mean",]$value
    psseffect = median(psseffect)  + median(psseffectsd)*median(x_use) 
    + median(pssinteractioneffect)*conventionfactor[i]
    df = data.frame(psseffect*250, conventionfactor[i])
    names(df) = c("psseffect", "conventionfactor")
    return(df)
  }
)

logitrhoeffect = extract_samples("logitRhoEffectMean")

logitrhoeffectsd = extract_samples("zlogitRhoEffectSD", TRUE)

logitrhointeractioneffect = extract_samples("logitRhoConventionInteractionEffectMean")

rhoeffect_ids = ddply(
  .data = betas
  , .variables = .(participant)
  , .fun = function(x){
    i = unique(x$participant)
    x_use = x[x$parameter == "logitRhoEffectMean",]$value
    logitrhoeffect_use = median(logitrhoeffect) + median(logitrhoeffectsd)*median(x_use)+ median(logitrhointeractioneffect)*conventionfactor[i]
    df = data.frame(logitrhoeffect_use, conventionfactor[i])
    names(df) = c("logitrhoeffect", "conventionfactor")
    return(df)
  }
)

psseffect_v_rhoeffect = merge(rhoeffect_ids, psseffect_ids)

ggplot(data = psseffect_v_rhoeffect, aes(x = logitrhoeffect, y =psseffect, colour = factor(conventionfactor), shape = factor(conventionfactor)))+
  scale_y_continuous(name = "PSS Effect Mean")+
  scale_x_continuous(name = "Logit \u03C1 Effect Mean")+
  geom_hline(yintercept = 0, linetype = 2, size = 1)+
  geom_vline(xintercept = 0, linetype = 2, size = 1)+
  geom_point(size = 4)+
  scale_shape_manual(name = "Know\nConvention", labels = c("True", "False"), values = c(16,17) )+
  scale_colour_manual(name = "Know\nConvention", labels =c("True", "False") , values = c("red", "blue") )+
  theme_gray(base_size = 30)+
  theme(panel.grid.major = element_line(size = 1.5)
        ,panel.grid.minor = element_line(size = 1))
 
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
logkappaeffect = extract_samples("logKappaEffectMean")

logkappaeffectsd = extract_samples("zlogKappaEffectSD", TRUE)

logkappainteractioneffect = extract_samples("logKappaConventionInteractionEffectMean")

kappaeffect_ids = ddply(
  .data = betas
  , .variables = .(participant)
  , .fun = function(x){
    i = unique(x$participant)
    x_use = x[x$parameter == "logKappaEffectMean",]$value
    logkappaeffect_use = median(logkappaeffect) + median(logkappaeffectsd)*median(x_use) + median(logkappainteractioneffect)*conventionfactor[i]
    df = data.frame(logkappaeffect_use, conventionfactor[i])
    names(df) = c("logkappaeffect", "conventionfactor")
    return(df)
  }
)

psseffect_v_kappaeffect = merge(kappaeffect_ids, psseffect_ids)

ggplot(data = psseffect_v_kappaeffect, aes(x = logkappaeffect, y =psseffect, colour = factor(conventionfactor), shape = factor(conventionfactor)))+
  scale_y_continuous(name = "PSS Effect Mean")+
  scale_x_continuous(name = "Log \u03BA Effect Mean")+
  geom_hline(yintercept = 0, linetype = 2, size = 1)+
  geom_vline(xintercept = 0, linetype = 2, size = 1)+
  geom_point(size = 4)+
  scale_shape_manual(name = "Know\nConvention", labels = c("True", "False"), values = c(16,17) )+
  scale_colour_manual(name = "Know\nConvention", labels =c("True", "False") , values = c("red", "blue") )+
  theme_gray(base_size = 30)+
  theme(panel.grid.major = element_line(size = 1.5)
        ,panel.grid.minor = element_line(size = 1))

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
)
#---------------------------------- SOA Intercepts ----------------------------------------#


#---------------------------------- SOA Attention Effects ---------------------------------#
# effect of attention on PSS and JND
get_violin(
  ( (ex_toj_color_post$population_pss_intercept_mean + ex_toj_color_post$population_pss_effect_mean/2) 
    - (ex_toj_color_post$population_pss_intercept_mean - ex_toj_color_post$population_pss_effect_mean/2) ) * 250
  , "PSS Attention Effect Mean"
  , y_lab = "SOA (Glove - Base; ms)"
)

get_violin(
  ( exp( ex_toj_color_post$population_logjnd_intercept_mean + ex_toj_color_post$population_logjnd_effect_mean/2 )
      - exp( ex_toj_color_post$population_logjnd_intercept_mean - ex_toj_color_post$population_logjnd_effect_mean/2  ) ) * 250 
  , "JND Attention Effect Mean"
  , y_lab = "SOA (Glove - Base; ms)"
)
#---------------------------------- SOA Attention Effects ---------------------------------#


#--------------------------------- SOA Convention Effects ---------------------------------#
# effect of convention knowlege on PSS and JND
get_violin(
  ( (ex_toj_color_post$population_pss_intercept_mean + ex_toj_color_post$population_pss_convention_effect_mean/2) 
    - (ex_toj_color_post$population_pss_intercept_mean - ex_toj_color_post$population_pss_convention_effect_mean/2) ) * 250
  , "PSS Convention\nEffect Mean"
  , ( exp( ex_toj_color_post$population_logjnd_intercept_mean + ex_toj_color_post$population_logjnd_convention_effect_mean/2 )
      - exp( ex_toj_color_post$population_logjnd_intercept_mean - ex_toj_color_post$population_logjnd_convention_effect_mean/2  ) ) * 250 
  , "JND Convention\nEffect Mean"
  , y_lab = "SOA (Don't Know - Know; ms)"
)

# effect of interaction between convention knowledge and attention on PSS
get_violin(
  ( (ex_toj_color_post$population_pss_intercept_mean + ex_toj_color_post$population_pss_convention_interaction_effect_mean/2) 
    - (ex_toj_color_post$population_pss_intercept_mean - ex_toj_color_post$population_pss_convention_interaction_effect_mean/2) ) * 250
  , "PSS Convention Knowledge\nInteraction Effect Mean"
  , y_lab = "SOA (ms)"
)

# effect of interaction between convention knowledge and attention on JND
get_violin(
  ( exp(ex_toj_color_post$population_logjnd_intercept_mean + ex_toj_color_post$population_logjnd_convention_interaction_effect_mean/2) 
    - exp(ex_toj_color_post$population_logjnd_intercept_mean - ex_toj_color_post$population_logjnd_convention_interaction_effect_mean/2) ) * 250
  , "JND Convention Knowledge\nInteraction Effect Mean"
  , y_lab = "SOA (ms)"
)

# effect of attention on JND by knowledge of convention
get_violin(
  ( (ex_toj_color_post$population_logjnd_intercept_mean + (ex_toj_color_post$population_logjnd_effect_mean + ex_toj_color_post$population_logjnd_convention_interaction_effect_mean)/2) 
    - (ex_toj_color_post$population_logjnd_intercept_mean - (ex_toj_color_post$population_logjnd_effect_mean + ex_toj_color_post$population_logjnd_convention_interaction_effect_mean)/2 ) ) * 250
  , "JND Attention Effect\nGiven Don't Know"
  , ( (ex_toj_color_post$population_logjnd_intercept_mean + (ex_toj_color_post$population_logjnd_effect_mean - ex_toj_color_post$population_logjnd_convention_interaction_effect_mean)/2) 
      - (ex_toj_color_post$population_logjnd_intercept_mean - (ex_toj_color_post$population_logjnd_effect_mean - ex_toj_color_post$population_logjnd_convention_interaction_effect_mean)/2 ) ) * 250
  , "JND Attention Effect\nGiven Know"
  , y_lab = "SOA (Attended - Unattended; ms)"
)
#--------------------------------- SOA Convention Effects ---------------------------------#


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
  , "Probability of Memory Effect Mean"
  , y_lab = "\u03C1 (Attended - Unattended)"
)
#-------------------------------- Rho Attention Effect ------------------------------------#


#-------------------------------- Rho Convention Effects ----------------------------------#
get_violin(
  ( plogis(ex_toj_color_post$logitRhoMean + ex_toj_color_post$logitRhoConventionEffectMean/2 )
    - plogis(ex_toj_color_post$logitRhoMean - ex_toj_color_post$logitRhoConventionEffectMean/2 ) )
  , "Probability of Memory\nConvention Effect Mean"
  , y_lab = "\u03C1 (Don't Know - Know)"
)

get_violin(
  ( plogis(ex_toj_color_post$logitRhoMean + ex_toj_color_post$logitRhoConventionInteractionEffectMean/2 )
    - plogis(ex_toj_color_post$logitRhoMean - ex_toj_color_post$logitRhoConventionInteractionEffectMean/2 ) )
  , "Probability of Memory\nConvention Knowledge\nInteraction Effect Mean"
  , y_lab = "\u03C1"
)

# effect of attention on rho for each convention knowledge level
get_violin(
  ( plogis(ex_toj_color_post$logitRhoMean + ex_toj_color_post$logitRhoConventionEffectMean/2
           + (ex_toj_color_post$logitRhoEffectMean + ex_toj_color_post$logitRhoConventionInteractionEffectMean)/2 )
    - plogis(ex_toj_color_post$logitRhoMean + ex_toj_color_post$logitRhoConventionEffectMean/2
             - (ex_toj_color_post$logitRhoEffectMean +  ex_toj_color_post$logitRhoConventionInteractionEffectMean)/2 ) )
  , "Probability of Memory\nAttention Effect Given\nDon't Know Convention"
  , ( plogis(ex_toj_color_post$logitRhoMean - ex_toj_color_post$logitRhoConventionEffectMean/2
             + (ex_toj_color_post$logitRhoEffectMean - ex_toj_color_post$logitRhoConventionInteractionEffectMean)/2 )
      - plogis(ex_toj_color_post$logitRhoMean - ex_toj_color_post$logitRhoConventionEffectMean/2
               - (ex_toj_color_post$logitRhoEffectMean -  ex_toj_color_post$logitRhoConventionInteractionEffectMean)/2 ) )
  , "Probability of Memory\nAttention Effect Given\nKnow Convention"
  , y_lab = "\u03C1 (Attended - Unattended)"
)
#-------------------------------- Rho Convention Effects ----------------------------------#


#---------------------------------- Kappa Intercept ---------------------------------------#
get_violin(
  exp( ex_toj_color_post$logKappaMean ) 
  , "Fidelity of Memory Intercept Mean"
  , y_lab = "\u03BA"
  , hline = FALSE
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


#------------------------------- Kappa Convention Effects ---------------------------------#
get_violin(
  ( exp(ex_toj_color_post$logKappaMean + ex_toj_color_post$logKappaConventionEffectMean/2 )
    - exp(ex_toj_color_post$logKappaMean - ex_toj_color_post$logKappaConventionEffectMean/2 ) )
  , "Fidelity of Memory\nConvention Effect Mean"
  , y_lab = "\u03BA (Don't Know - Know)"
)

get_violin(
  ( exp(ex_toj_color_post$logKappaMean + ex_toj_color_post$logKappaConventionInteractionEffectMean/2 )
    - exp(ex_toj_color_post$logKappaMean - ex_toj_color_post$logKappaConventionInteractionEffectMean/2 ) )
  , "Fidelity of Memory\nConvention Knowledge\nInteraction Effect Mean"
  , y_lab = "\u03BA"
)

get_violin(
  ( exp(ex_toj_color_post$logKappaMean + ex_toj_color_post$logKappaConventionEffectMean/2
        + (ex_toj_color_post$logKappaEffectMean + ex_toj_color_post$logKappaConventionInteractionEffectMean)/2 )
    - exp(ex_toj_color_post$logKappaMean + ex_toj_color_post$logKappaConventionEffectMean/2
          - (ex_toj_color_post$logKappaEffectMean +  ex_toj_color_post$logKappaConventionInteractionEffectMean)/2 ) )
  , "Fidelity of Memory\nAttention Effect\nGiven Don't Know"
  , ( exp(ex_toj_color_post$logKappaMean - ex_toj_color_post$logKappaConventionEffectMean/2
          + (ex_toj_color_post$logKappaEffectMean - ex_toj_color_post$logKappaConventionInteractionEffectMean)/2 )
      - exp(ex_toj_color_post$logKappaMean - ex_toj_color_post$logKappaConventionEffectMean/2
            - (ex_toj_color_post$logKappaEffectMean -  ex_toj_color_post$logKappaConventionInteractionEffectMean)/2 ) )
  , "Fidelity of Memory\nAttention Effect\nGiven Know"
  , y_lab = "\u03BA (Attended - Unattended)"
)
#------------------------------- Kappa Convention Effects ---------------------------------#



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


# #---------------------------------- Posteriors --------------------------------------------#
# pos_rhoMean_WithEffect = data.frame(
#   c( 
#     plogis(ex_toj_color_post$logitRhoMean + ex_toj_color_post$logitRhoEffectMean/2)
#     , plogis(ex_toj_color_post$logitRhoMean - ex_toj_color_post$logitRhoEffectMean/2)
#   ) 
#   , c(rep("Attended",80000), rep("Unattended", 80000))
# )
# names(pos_rhoMean_WithEffect) = c("rhoMean", "Effect")
# 
# # overlapping
# ggplot(pos_rhoMean_WithEffect, aes(x = rhoMean, ..density.., fill = Effect))+
#   geom_density(data = pos_rhoMean_WithEffect[pos_rhoMean_WithEffect$Effect == "Attended",],alpha = 0.5)+
#   geom_density(data = pos_rhoMean_WithEffect[pos_rhoMean_WithEffect$Effect == "Unattended",],alpha = 0.5)+
#   scale_fill_hue("Effect", l = c(90, 45), c = c(100, 50) ) +
#   labs(x = "Probability of Memory Population Mean", y = "Density", colour = "")+
#   theme_gray(base_size = 24)+
#   theme(panel.grid.major = element_line(size = 1.5)
#         ,panel.grid.minor = element_line(size = 1))
# #---------------------------------- Posteriors --------------------------------------------#
