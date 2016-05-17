# library(shinystan)
library(coda)
library(ggplot2)

# look at parameter distribution estimates  
toj_color_post

# visualize
plot(toj_color_post)

# extract samples
ex_toj_color_post = extract(toj_color_post)

# check chain behavior (want hairy catapillar)
# for toj
plot(ex_toj_color_post$population_pss_intercept_mean, type = 'l')
plot(ex_toj_color_post$population_pss_effect_mean, type = 'l')
plot(ex_toj_color_post$population_logjnd_intercept_mean, type = 'l')
plot(ex_toj_color_post$population_logjnd_effect_mean, type = 'l')
plot(ex_toj_color_post$zpopulation_pss_intercept_sd, type = 'l')
plot(ex_toj_color_post$zpopulation_pss_effect_sd, type = 'l')
plot(ex_toj_color_post$zpopulation_logjnd_intercept_sd, type = 'l')
plot(ex_toj_color_post$zpopulation_logjnd_effect_sd, type = 'l')

# for color
plot(ex_toj_color_post$logitRhoMean, type = 'l')
plot(ex_toj_color_post$logKappaMean, type = 'l')
plot(ex_toj_color_post$logitRhoEffectMean, type = 'l')
plot(ex_toj_color_post$logKappaEffectMean, type = 'l')
plot(ex_toj_color_post$zlogitRhoSD, type = 'l')
plot(ex_toj_color_post$zlogKappaSD, type = 'l')
plot(ex_toj_color_post$zlogitRhoEffectSD, type = 'l')
plot(ex_toj_color_post$zlogKappaEffectSD, type = 'l')



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
)

# plot
ggplot(data = pos_SOA_scale, aes(x = parameter, y = value))+
  geom_violin()+
  labs(x = "", y = "SOA (ms)")+
  stat_summary(fun.data = get_95_HDI, size = 0.5)+
  stat_summary(fun.data = get_50_HDI, size = 1.5)+
#  scale_x_discrete(labels = c("PSS Intercept Mean", "JND Intercept Mean"))+
  theme_gray(base_size = 18) 

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
  stat_summary(fun.data = get_50_HDI, size = 1.5)+
  labs(x = "", y = "SOA (Base - Glove; ms)")+
#  scale_x_discrete(labels = c("PSS Effect Mean", "JND Effect Mean"))+
  geom_hline(yintercept = 0, linetype = 2)+
  theme_gray(base_size = 18) 

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
  stat_summary(fun.data = get_50_HDI, size = 1.5)+
  labs(x = "", y = "\u03C1")+
  scale_x_discrete(labels = c("Probability of Memory Intercept Mean"))+
  theme_gray(base_size = 18) 

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
  geom_hline(yintercept = 0, linetype = 2)+
  stat_summary(fun.data = get_95_HDI, size = 0.5)+
  stat_summary(fun.data = get_50_HDI, size = 1.5)+
  scale_x_discrete(labels = c("Probability of Memory Effect Mean"))+
  theme_gray(base_size = 18) 

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
  stat_summary(fun.data = get_50_HDI, size = 1.5)+
  scale_x_discrete(labels = c("Fidelity of Memory Intercept Mean"))+
#  scale_y_continuous(limits = c(0,20) ) +
  theme_gray(base_size = 18) 

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
  geom_hline(yintercept = 0, linetype = 2 ) +
  stat_summary(fun.data = get_95_HDI, size = 0.5)+
  stat_summary(fun.data = get_50_HDI, size = 1.5)+
  scale_x_discrete(labels = c("Fidelity of Memory Effect Mean"))+
  theme_gray(base_size = 18) 

# 95% HDIs
# kappa effects (*radians*)
get_95_HDI( 
  exp(ex_toj_color_post$logKappaMean + ex_toj_color_post$logKappaEffectMean/2) 
                     - exp(ex_toj_color_post$logKappaMean - ex_toj_color_post$logKappaEffectMean/2) 
)
# get_95_HDI(tan(ex_toj_color_post$zlogKappaEffectSD)) # (log scale)




#### TOJ: plot posterior pss means with effects ####
pos_pssMean_WithEffect = data.frame(
  c( 
    ex_toj_color_post$population_pss_intercept_mean + ex_toj_color_post$population_pss_effect_mean
    , ex_toj_color_post$population_pss_intercept_mean - ex_toj_color_post$population_pss_effect_mean
  ) * 250
  , c(rep("Base",40000), rep("Glove", 40000))
)
names(pos_pssMean_WithEffect) = c("pssMean", "Effect")

# overlapping
ggplot(pos_pssMean_WithEffect, aes(x = pssMean, ..density.., fill = Effect))+
  geom_density(data = pos_pssMean_WithEffect[pos_pssMean_WithEffect$Effect == "Base",], alpha = 0.5) + 
  geom_density(data = pos_pssMean_WithEffect[pos_pssMean_WithEffect$Effect == "Glove",], alpha = 0.5) + 
  geom_vline(xintercept = 0, linetype = 2)+
  labs(x = "PSS Population Mean", y = "Density")+
  theme_gray(base_size = 18)  # recall that negative SOAs are glove first




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

ggplot(data = df, aes(y = Prop, x = SOA, colour = Attend))+
  geom_line()+
  labs(x = "SOA (ms)", y = "Proportion of 'Safe' Responses")+
  theme_gray(base_size = 18)




#### Color: plot posterior means with effects ####
pos_rhoMean_WithEffect = data.frame(
  c( 
    plogis( ex_toj_color_post$logitRhoMean + ex_toj_color_post$logitRhoEffectMean/2) 
    , plogis( ex_toj_color_post$logitRhoMean - ex_toj_color_post$logitRhoEffectMean/2)
  ) 
  , c(rep("Attended",40000), rep("Unattended", 40000))
)
names(pos_rhoMean_WithEffect) = c("rhoMean", "Effect")


# overlapping
ggplot(pos_rhoMean_WithEffect, aes(x = rhoMean, ..density.., fill = Effect))+
  geom_density(data = pos_rhoMean_WithEffect[pos_rhoMean_WithEffect$Effect == "Attended",],alpha = 0.5)+
  geom_density(data = pos_rhoMean_WithEffect[pos_rhoMean_WithEffect$Effect == "Unattended",],alpha = 0.5)+
  labs(x = "Probability of Memory Population Mean", y = "Density", colour = "")+
  theme_gray(base_size = 18)
