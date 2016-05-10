# library(shinystan)
library(coda)

# read in stan fit
load("toj_post_May10th2016")

# look at parameter distribution estimates  
toj_post

# visualize
plot(toj_post)

# extract samples
ex_toj_post = extract(toj_post)

# check chain behavior (want hairy catapillar)
plot(ex_toj_post$population_pss_intercept_mean, type = 'l')
plot(ex_toj_post$population_pss_effect_mean, type = 'l')
plot(ex_toj_post$population_logjnd_intercept_mean, type = 'l')
plot(ex_toj_post$population_logjnd_effect_mean, type = 'l')
plot(ex_toj_post$zpopulation_pss_intercept_sd, type = 'l')
plot(ex_toj_post$zpopulation_pss_effect_sd, type = 'l')
plot(ex_toj_post$zpopulation_logjnd_intercept_sd, type = 'l')
plot(ex_toj_post$zpopulation_logjnd_effect_sd, type = 'l')



### plot posterior means 
pos_pssMean = as.data.frame( ex_toj_post$population_pss_intercept_mean * 250) 
names(pos_pssMean) = "pssMean"

ggplot(pos_pssMean, aes(x = pssMean, ..density..))+
  geom_histogram(bins = 100)+
  geom_density(colour = "red")

pos_jndMean = as.data.frame( exp( ex_toj_post$population_logjnd_intercept_mean ) * 250/2) 
names(pos_jndMean) = "jndMean"

ggplot(pos_jndMean, aes(x = jndMean, ..density..))+
  geom_histogram(bins = 100)+
  geom_density(colour = "red")  # I am having difficulty interpreting this



### plot posterior means with effects TO SEE NEAR CREDIBLE INVERSE PSS EFFECT
# I am not sure if it makes sense to add the markov chains like this - I assume that it is not meaningful...
pos_pssMean_WithEffect = data.frame(
  c( 
    ex_toj_post$population_pss_intercept_mean + ex_toj_post$population_pss_effect_mean
    , ex_toj_post$population_pss_intercept_mean - ex_toj_post$population_pss_effect_mean
  ) 
  , c(rep("Base",40000), rep("Glove", 40000))
)
names(pos_pssMean_WithEffect) = c("pssMean", "Effect")

# # stacked 
# ggplot(pos_pssMean_WithEffect, aes(x = pssMean, ..density.., group = Effect, colour = Effect))+
#   geom_histogram(bins = 100)+
#   geom_density(colour = "yellow")

# overlapping
ggplot(pos_pssMean_WithEffect, aes(x = pssMean, ..density.., colour = Effect))+
  geom_histogram(data = pos_pssMean_WithEffect[pos_pssMean_WithEffect$Effect == "Base",], bins = 100, alpha = 0.5)+
  geom_density(data = pos_pssMean_WithEffect[pos_pssMean_WithEffect$Effect == "Base",],colour = "red")+
  geom_histogram(data = pos_pssMean_WithEffect[pos_pssMean_WithEffect$Effect == "Glove",], bins = 100, alpha = 0.5)+
  geom_density(data = pos_pssMean_WithEffect[pos_pssMean_WithEffect$Effect == "Glove",],colour = "blue")+
  labs(x = "PSS Population Mean", y = "Density", colour = "")+
  theme_gray(base_size = 18)  # recall that negative SOAs are glove first






### recreate NCF using pss intercept mean and jnd intercept mean
# group average 
y = pnorm(-250:250, mean = -0.01*250, sd = exp(0.02)*125 )
plot(-250:250, y, type = 'l')

# including effects 
yGlove = pnorm(-250:250, mean = (-0.01-0.02)*250, sd = exp(0.02 -(-0.05))*125 )
yBase = pnorm(-250:250, mean = (-0.01+0.02)*250, sd = exp(0.02 +(-0.05))*125 )
df = data.frame(SOA = -250:250, Prop = c(yGlove, yBase), Attend = c(rep("Glove",501), rep("Base", 501)))

ggplot(data = df, aes(y = Prop, x = SOA, colour = Attend))+
  geom_line()
