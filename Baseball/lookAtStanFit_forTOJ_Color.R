# library(shinystan)
library(coda)
library(ggplot2)

# read in stan fit
load("toj_color_post_May11th2016")

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



### violin plot for key parameters (plot things on same scale)
# SOA scale 
pos_SOA_scale = data.frame(  
  value = c(
  ex_toj_color_post$population_pss_intercept_mean * 250
  , tan(ex_toj_color_post$zpopulation_pss_intercept_sd) * 250
  , exp( ex_toj_color_post$population_logjnd_intercept_mean ) * 250
  , exp( tan(ex_toj_color_post$zpopulation_logjnd_intercept_sd) ) * 250  
  )
  , parameter = c(
    rep("pssInterceptMean", 40000)
    , rep("pssInterceptSD", 40000)
    , rep("jndInterceptMean", 40000)
    , rep("jndInterceptSD", 40000)
    )
)

ggplot(data = pos_SOA_scale, aes(x = parameter, y = value))+
  geom_violin()+
  labs(x = "", y = "SOA (ms)")+
  theme_gray(base_size = 18) 



# SOA scale - effects 
pos_SOA_scale_effects = data.frame(  
  effect = c(
    ( (ex_toj_color_post$population_pss_intercept_mean + ex_toj_color_post$population_pss_effect_mean) 
    - (ex_toj_color_post$population_pss_intercept_mean - ex_toj_color_post$population_pss_effect_mean) ) * 250
    , ( (tan(ex_toj_color_post$zpopulation_pss_intercept_sd) + tan(ex_toj_color_post$zpopulation_pss_effect_sd)) 
        - ( tan(ex_toj_color_post$zpopulation_pss_intercept_sd) - tan(ex_toj_color_post$zpopulation_pss_effect_sd)) ) * 250  
    , ( exp( ex_toj_color_post$population_logjnd_intercept_mean + ex_toj_color_post$population_logjnd_effect_mean )
        - exp( ex_toj_color_post$population_logjnd_intercept_mean - ex_toj_color_post$population_logjnd_effect_mean  ) ) * 250 
    , ( exp( tan(ex_toj_color_post$zpopulation_logjnd_intercept_sd) + tan(ex_toj_color_post$zpopulation_logjnd_effect_sd) ) 
        - exp( tan(ex_toj_color_post$zpopulation_logjnd_intercept_sd) - tan(ex_toj_color_post$zpopulation_logjnd_effect_sd) ) ) * 250 
  )
  , parameter = c(
    rep("pssEffectMean", 40000)
    , rep("pssEffectSD", 40000)
    , rep("jndEffectMean", 40000)
    , rep("jndEffectSD", 40000)
  )
)

ggplot(data = pos_SOA_scale_effects, aes(x = parameter, y = effect))+
  geom_violin()+
  labs(x = "", y = "SOA (Base - Glove; ms)")+
  geom_hline(yintercept = 0, linetype = 2)+
  theme_gray(base_size = 18) 



# Rho scale
pos_rho_scale = data.frame(  
  value = c(
    plogis(ex_toj_color_post$logitRhoMean)
    , plogis( tan(ex_toj_color_post$zlogitRhoSD) )
  )
  , parameter = c(
    rep("rhoInterceptMean", 40000)
    , rep("rhoInterceptSD", 40000)
  )
)

ggplot(data = pos_rho_scale, aes(x = parameter, y = value))+
  geom_violin()+
  labs(x = "", y = "Probability of Memory")+
  theme_gray(base_size = 18) 



# Rho scale - effects
pos_rho_scale_effects = data.frame(  
  effect = c(
    ( plogis(ex_toj_color_post$logitRhoMean + ex_toj_color_post$logitRhoEffectMean/2 )
    - plogis(ex_toj_color_post$logitRhoMean - ex_toj_color_post$logitRhoEffectMean/2 ) )
    , ( plogis( tan(ex_toj_color_post$zlogitRhoSD) + tan(ex_toj_color_post$zlogitRhoEffectSD)/2 ) 
        - plogis( tan(ex_toj_color_post$zlogitRhoSD) - tan(ex_toj_color_post$zlogitRhoEffectSD)/2 ) )
  )
  , parameter = c(
    rep("rhoEffectMean", 40000)
    , rep("rhoEffectSD", 40000)
  )
)

ggplot(data = pos_rho_scale_effects, aes(x = parameter, y = effect))+
  geom_violin()+
  labs(x = "", y = "Probability of Memory (Attended - Unattended)")+
  geom_hline(yintercept = 0, linetype = 2)+
  theme_gray(base_size = 18) 



# Kappa scale
pos_kappa_scale = data.frame(  
  value = c(
    exp(ex_toj_color_post$logKappaMean)
    , exp( tan(ex_toj_color_post$zlogKappaSD) )
  )
  , parameter = c(
    rep("kappaInterceptMean", 40000)
    , rep("kappaInterceptSD", 40000)
  )
)

ggplot(data = pos_kappa_scale, aes(x = parameter, y = value))+
  geom_violin()+
  labs(x = "", y = "Fidelity of Memory")+  # are there any units here?
  scale_y_continuous(limits = c(0,20) ) +
  theme_gray(base_size = 18) 


# Kappa scale - effects
pos_kappa_scale_effects = data.frame(  
  value = c(
    ( exp(ex_toj_color_post$logKappaMean + ex_toj_color_post$logKappaEffectMean/2) 
      - exp(ex_toj_color_post$logKappaMean - ex_toj_color_post$logKappaEffectMean/2) )
    , ( exp( tan(ex_toj_color_post$zlogKappaSD) + tan(ex_toj_color_post$zlogKappaEffectSD)/2 )
        - exp( tan(ex_toj_color_post$zlogKappaSD) - tan(ex_toj_color_post$zlogKappaEffectSD)/2 ) )
  )
  , parameter = c(
    rep("kappaEffectMean", 40000)
    , rep("kappaEffectSD", 40000)
  )
)

ggplot(data = pos_kappa_scale_effects, aes(x = parameter, y = value))+
  geom_violin()+
  labs(x = "", y = "Fidelity of Memory (Attended - Unattended)")+  # are there any units here?
  geom_hline(yintercept = 0, linetype = 2 ) +
  theme_gray(base_size = 18) 



### TOJ: plot posterior means with effects 
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
  labs(x = "PSS Population Mean", y = "Density")+
  theme_gray(base_size = 18)  # recall that negative SOAs are glove first



### Color: plot posterior means with effects
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
