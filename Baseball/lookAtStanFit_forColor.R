# library(shinystan)
library(coda)

# read in stan fit
load("color_post_May6th2016")

# look at parameter distribution estimates  
color_post

# visualize
plot(color_post)

# extract samples
ex_color_post = extract(color_post)

# check chain behavior (want hairy catapillar)
plot(ex_color_post$logitRhoMean, type = 'l')
plot(ex_color_post$logKappaMean, type = 'l')
plot(ex_color_post$logitRhoEffectMean, type = 'l')
plot(ex_color_post$logKappaEffectMean, type = 'l')
plot(ex_color_post$zlogitRhoSD, type = 'l')
plot(ex_color_post$zlogKappaSD, type = 'l')
plot(ex_color_post$zlogitRhoEffectSD, type = 'l')
plot(ex_color_post$zlogKappaEffectSD, type = 'l')



# ### plot posterior means 
# pos_RhoMean = as.data.frame( logistic( ex_color_post$logitRhoMean) ) 
# names(pos_RhoMean) = "RhoMean"
# 
# ggplot(pos_RhoMean, aes(x = RhoMean, ..density..))+
#   geom_histogram(bins = 100)+
#   geom_density(colour = "red")
# 
# pos_KappaMean = as.data.frame( exp( ex_color_post$logKappaMean) ) 
# names(pos_KappaMean) = "KappaMean"
# 
# ggplot(pos_KappaMean, aes(x = KappaMean, ..density..))+
#   geom_histogram(bins = 100)+
#   geom_density(colour = "red")



### plot posterior means with effects BECAUSE CREDIBLE EFFECTS FOR MEAN AND SD
# I am not sure if it makes sense to add the markov chains like this - I assume that it is not meaningful...
pos_RhoMean_WithEffect = data.frame(
  c( 
  logistic( ex_color_post$logitRhoMean + ex_color_post$logitRhoEffectMean/2) 
  , logistic( ex_color_post$logitRhoMean - ex_color_post$logitRhoEffectMean/2)
  ) 
  , c(rep("Attended",40000), rep("Unattended", 40000))
)
names(pos_RhoMean_WithEffect) = c("RhoMean", "Effect")

# # stacked 
# ggplot(pos_RhoMean_WithEffect, aes(x = RhoMean, ..density.., group = Effect, colour = Effect))+
#   geom_histogram(bins = 100)+
#   geom_density(colour = "yellow")

# overlapping
ggplot(pos_RhoMean_WithEffect, aes(x = RhoMean, ..density.., colour = Effect))+
  geom_histogram(data = pos_RhoMean_WithEffect[pos_RhoMean_WithEffect$Effect == "Attended",], bins = 100, alpha = 0.5)+
  geom_density(data = pos_RhoMean_WithEffect[pos_RhoMean_WithEffect$Effect == "Attended",],colour = "red")+
  geom_histogram(data = pos_RhoMean_WithEffect[pos_RhoMean_WithEffect$Effect == "Unattended",], bins = 100, alpha = 0.5)+
  geom_density(data = pos_RhoMean_WithEffect[pos_RhoMean_WithEffect$Effect == "Unattended",],colour = "blue")+
  labs(x = "Probability of Memory Population Mean", y = "Density", colour = "")+
  theme_gray(base_size = 18)
  

