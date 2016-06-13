#### Libraries ####
library(plyr)
library(ggplot2)
library(grid)
library(rstan)



##########################################
####          Data Import           #### 
##########################################

check_before_read = function(file){
	temp = tools::md5sum(file)
	if(temp %in% checksums){
		return(NULL)
	}else{
		checksums <<- c(checksums,temp)
		to_return = read.table(
			file
			, header = T
			, sep = '\t'
			, skip = 1 
		)
		return(to_return)
	}
}

checksums = NULL

a = ldply(
  .data = list.files(
    pattern = ".txt"
    , full.names = T
    , path = './100msOrLess'  # includes folders for 50ms, 75ms, 100ms
    , recursive = T
  )
  , .fun = check_before_read
  , .progress = 'text'
)
names(a)[1] = "id"
a$onehundredms = TRUE

b = ldply(
  .data = list.files(
	  	pattern = ".txt"
  		, full.names = T
  		, path = './200ms'
	  	# , path = './50ms'
  	)
  , .fun = check_before_read
  , .progress = 'text'
)
names(b)[1] = "id"
b$onehundredms = FALSE

a = rbind(a,b)

length(unique(a$id))

length(checksums)

#count trials per id
temp = data.frame(table(id=a$id))

#discard those with too few trials
keep = temp$id[temp$Freq==600]  # 40 * 3 + 240 * 2 = 600
a = a[a$id %in% keep,]

length(unique(a$id))



#### Type ####
#As factor 
a$id = as.factor(a$id)
a$sex = as.factor(a$sex)
a$handedness = as.factor(a$handedness)
a$block_num = as.factor(a$block_num)
a$trial_num = as.factor(a$trial_num)
#a$soa = as.factor(a$soa)
a$trial_type = as.factor(a$trial_type)
a$probe_loc = as.factor(a$probe_loc)
a$t1_loc = as.factor(a$t1_loc)
a$t1_type = as.factor(a$t1_type)
a$probe_initial_bias = as.factor(a$probe_initial_bias)
a$block_bias = as.factor(a$block_bias)  
a$toj_judgement = as.factor(a$toj_judgement)

#As numeric
#a$probe_color = as.numeric(a$probe_color)
#a$probe_judgement_color = as.numeric(a$probe_judgement_color)
a$probe_angle = as.numeric(a$probe_angle)
a$rotation = as.numeric(a$rotation)
a$probe_judgement = as.numeric(a$probe_judgement)  # angle of color judgement (0 to 360)
a$probe_rt = as.numeric(a$probe_rt)
a$p_minus_j = as.numeric(a$p_minus_j)
a$toj_rt = as.numeric(a$toj_rt)
a$age = as.numeric(a$age)



#### Summary ####
summary(a)

# double check 20/80
table(a$probe_loc, a$block_num, a$id)    
# continue 
aggregate(trial_num ~ block_num, data = a, FUN = length) # good
table(a$t1_loc, a$block_num) # good
table(a$t1_type, a$block_num) # good

# get experimental blocks (3, 5)
a_exp = a[a$block_num %in% c(3,5),]



#### TOJ ####
toj_trials = a_exp
toj_trials = toj_trials[toj_trials$trial_type == "TARGET", ]
toj_trials$toj_judgement = as.factor(as.character(toj_trials$toj_judgement))

# looking left condition since we looked at 'safe' (not out) proportion and since that event occured on the left side
# interested in their perception of which came first, even when they were asked which came second
toj_trials$left_first_TF = FALSE


toj_trials = ddply(
  .data = toj_trials
  , .variable = .(id)
  , .fun = function(x) {
    if ( unique(x$toj_judgement_type) == "first" ) {
      
      x$correct_judgement = x$t1_type == x$toj_judgement
      x$toj_judgement_side = "NA"
      x[x$correct_judgement,]$toj_judgement_side = as.character( x[x$correct_judgement,]$t1_loc )
      x[!x$correct_judgement,]$toj_judgement_side = ifelse(x[!x$correct_judgement,]$t1_loc == "LEFT", "RIGHT", "LEFT")
      x$toj_judgement_side = as.factor(x$toj_judgement_side)
      
      x$left_first_TF[x$toj_judgement_side == "LEFT"] = TRUE 
      
    } else if ( unique(x$toj_judgement_type) == "second" ) {
      
      x$correct_judgement = x$t1_type != x$toj_judgement
      x$toj_judgement_side = "NA"
      x[!x$correct_judgement,]$toj_judgement_side =  as.character( x[!x$correct_judgement,]$t1_loc )
      x[x$correct_judgement,]$toj_judgement_side = ifelse(x[x$correct_judgement,]$t1_loc == "LEFT", "RIGHT", "LEFT")
      x$toj_judgement_side = as.factor(x$toj_judgement_side)
      
      x$left_first_TF[x$toj_judgement_side == "RIGHT"] = TRUE 
      
    } else {
      print("What is the toj_judgement_type? It is not 'first' or 'second'.")
    }
    return(x)
  }

)



### SOAs 
toj_trials$soa2 = toj_trials$soa
# correct soas 
toj_trials[toj_trials$soa2 == "15",]$soa2 = 17
toj_trials[toj_trials$soa2 == "45",]$soa2 = 50
toj_trials[toj_trials$soa2 == "90",]$soa2 = 100
toj_trials[toj_trials$soa2 == "135",]$soa2 = 150
toj_trials[toj_trials$soa2 == "240",]$soa2 = 250
# Negative SOAs means RIGHT first 
toj_trials[toj_trials$t1_loc == "RIGHT",]$soa2 = -toj_trials[toj_trials$t1_loc == "RIGHT",]$soa2

# save
save(toj_trials, file = "FollowUp_toj_trials.Rdata")

### Plot Psychometric Functions 
toj_means_by_id_by_condition = ddply(
  .data = toj_trials
  , .variables = .(id, block_bias, soa2)
  , .fun = function(x){
    to_return = data.frame(
      value = mean(x$left_first_TF)
    )
    return(to_return)
  }
)
toj_means_by_id_by_condition$soa2 = as.numeric(as.character(toj_means_by_id_by_condition$soa2))

ggplot(
  data = toj_means_by_id_by_condition
  , mapping = aes(
    x = soa2
    , y =  value
    , shape = block_bias
    , linetype = block_bias
    , group = block_bias
  )
)+
  facet_wrap(
    ~ id
  )+
  geom_smooth(
    method = "glm"
    , method.args = list(family = "binomial")
    , formula = y ~ splines::ns(x,2)
    , se = FALSE
  )+
  labs(x = "Stimulus Onset Asynchony (Negative Means First Line Appeared on the Right)", y = "Proportion of 'LEFT' Responses")+
  geom_point(size = 4)+
  geom_point(colour = "grey90")
  # +theme(legend.position = "none")  # to be blind to the condition 

# do this
toj_means_all = ddply(
  .data = toj_trials
  , .variables = .(block_bias, soa2)
  , .fun = function(x){
    to_return = data.frame(
      value = mean(x$left_first_TF)
    )
    return(to_return)
  }
)
toj_means_all$soa2 = as.numeric(as.character(toj_means_all$soa2))

ggplot(
  data = toj_means_all
  , mapping = aes(
    x = soa2
    , y =  value
    , shape = block_bias
    , linetype = block_bias
    , group = block_bias
  )
)+
  geom_smooth(
    method = "glm"
    , method.args = list(family = "binomial")
    , formula = y ~ splines::ns(x,2)
    , se = FALSE
  )+
  labs(x = "Stimulus Onset Asynchony (Negative Means First Line Appeared on the Right)", y = "Proportion of 'LEFT' Responses")+
  geom_point(size = 4)+
  geom_point(colour = "grey90")



# get pss and jnds for bewteen subject factors
toj_by_condition = ddply(
  .data = toj_trials
  , .variables = .(id, toj_judgement_type, probe_initial_bias)  
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
# look at descriptive statistics (mean values) of raw data to compare with parameter estimates of model later
aggregate(pss ~ toj_judgement_type, data = toj_by_condition, FUN = mean) 
plot(toj_by_condition$pss) # sense of outliers
aggregate(jnd ~ toj_judgement_type, data = toj_by_condition, FUN = mean) 
plot(toj_by_condition$jnd)  # get a sense of outliers
aggregate(pss ~ probe_initial_bias, data = toj_by_condition, FUN = mean)
aggregate(jnd ~ probe_initial_bias, data = toj_by_condition, FUN = mean)

# Does probe duration effect PSS indirectly?
toj_by_probe = ddply(
  .data = toj_trials
  , .variables = .(id, onehundredms)  
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
aggregate(pss~onehundredms, data=toj_by_probe, FUN=mean)
aggregate(jnd~onehundredms, data=toj_by_probe, FUN=mean)



#### Color ####
short_angle = function(x, y)
{
	return(((x - y + 180) %% 360) - 180)
}

degree_to_rad = function(x)
{
  return(x*pi / 180)
}


rad_to_degrees = function(x)
{
  return(x*180 / pi)
}

color_trials = a_exp
color_trials = color_trials[color_trials$trial_type == "PROBE", ]
hist(color_trials$p_minus_j,br=100)
color_trials[color_trials$p_minus_j > 180,]$p_minus_j = - (360 - color_trials[color_trials$p_minus_j > 180,]$p_minus_j)
color_trials[color_trials$p_minus_j < (-180),]$p_minus_j = color_trials[color_trials$p_minus_j < (-180),]$p_minus_j + 360
hist(color_trials$p_minus_j,br=100)

color_trials$attended = FALSE
color_trials$attended[ (color_trials$block_bias == "LEFT" & color_trials$probe_loc == "LEFT") | (color_trials$block_bias == "RIGHT" & color_trials$probe_loc == "RIGHT")] = TRUE

color_trials$color_diff_radians = color_trials$p_minus_j*pi/180

# save
save(color_trials, file = "FollowUp_color_trials.Rdata")

### mixture model
# rho and kappa by participant + condition
source("../fit_uvm.R")
fitted_by_condition = ddply(
  .data = color_trials
  , .variables = .(id, attended)
  , .fun = function(piece_of_df){
    fit = fit_uvm(piece_of_df$color_diff_radians, do_mu = TRUE)
    to_return = data.frame(
      kappa_prime = fit$kappa_prime
      , rho = fit$rho
    )
    return(to_return)
  }
  , .progress = 'time'
)
aggregate(rho~attended, data = fitted_by_condition, FUN = mean)
aggregate(kappa_prime~attended, data = fitted_by_condition, FUN = mean)

# rho and kappa by participant + probe duration 
# to check validity of obetween-subject effectin stan model
fitted_by_probe_duration = ddply(
  .data = color_trials
  , .variables = .(id, onehundredms)
  , .fun = function(piece_of_df){
    fit = fit_uvm(piece_of_df$color_diff_radians, do_mu = TRUE)
    to_return = data.frame(
      kappa_prime = fit$kappa_prime
      , rho = fit$rho
    )
    return(to_return)
  }
  , .progress = 'time'
)
aggregate(rho~onehundredms, data = fitted_by_probe_duration, FUN = mean)
aggregate(kappa_prime~onehundredms, data = fitted_by_probe_duration, FUN = mean)

# check for potential interaction between probe duration and rho effect!
# if no sign of interaction, consider just adding simple effects into model for simplicity
fitted_by_both_conditions = ddply(
  .data = color_trials
  , .variables = .(id, onehundredms, attended)
  , .fun = function(piece_of_df){
    fit = fit_uvm(piece_of_df$color_diff_radians, do_mu = TRUE)
    to_return = data.frame(
      kappa_prime = fit$kappa_prime
      , rho = fit$rho
    )
    return(to_return)
  }
  , .progress = 'time'
)
aggregate(rho~onehundredms + attended, data = fitted_by_both_conditions, FUN = mean)
aggregate(kappa_prime~onehundredms + attended, data = fitted_by_both_conditions, FUN = mean)

# rho and kappa by participant
fitted_all =  ddply(
  .data = color_trials
  , .variables = .(id)
  , .fun = function(piece_of_df){
    fit = fit_uvm(piece_of_df$color_diff_radians, do_mu = TRUE)
    to_return = data.frame(
      kappa_prime = fit$kappa_prime
      , rho = fit$rho
    )
    return(to_return)
  }
  , .progress = 'time'
)

# how many participants are perfect across the board?
perfection_count = sum(fitted_all$rho == 1)
perfection_count
perfection_rate = perfection_count/nrow(fitted_all)
perfection_rate

ggplot(
  data = fitted_all
  , mapping = aes(rho)  #, fill = attended)
)+ 
  geom_histogram(bins = 50)+
  labs(x = "Probability of Memory", y = "Frequency")+
  theme_gray(base_size = 24)+
  theme(panel.grid.major = element_line(size = 1.5)
        ,panel.grid.minor = element_line(size = 1))



#### Stan ####
toj_color_data_for_stan = list(
  N_toj = length(unique(toj_trials$id))
  , L_toj = nrow(toj_trials)
  , y_toj = as.numeric(toj_trials$left_first_TF)  
  , x_toj = (as.numeric(toj_trials$soa2))/250  # we normalize soas, and therefore pss
  , id_toj = as.numeric(factor(toj_trials$id))
  # Flipped from Baseball 'Analysis' so no need to change in 'stananalysis'
  # Will give positive effect for predicted results, which we want
  , condition_toj = ifelse(toj_trials$block_bias=="LEFT",-1,1)  # LEFT is -1 and RIGHT is +1 
  , N_color = length(unique(color_trials$id))
  , L_color = nrow(color_trials)
  , unit_color = as.numeric(factor(color_trials$id))
  , condition_color = as.numeric(as.factor(color_trials$attended)) # TRUE is 2, FALSE is 1 
  , condition_probe = ifelse(aggregate(onehundredms ~ id, data = color_trials, unique)$onehundred, -1, 1) # 100 ms is -1, and 200 ms is +1
  , condition_initial_bias = ifelse(aggregate(probe_initial_bias ~ id, data = toj_trials, FUN = unique)$probe_initial_bias == "RIGHT", -1, 1) # RIGHT is -1, and LEFT is +1
  , condition_judgement_type = ifelse(aggregate(toj_judgement_type ~ id, data = toj_trials, FUN = unique)$toj_judgement_type == "first", -1, 1) # first is -1, and second is +1
  , y_color = pi+degree_to_rad(color_trials$p_minus_j)  # want from 0 to 360 instead of -180 to 180
)

toj_color_model = stan_model(
 file = '../EndogenousVisualPriorEntry-BayesianHierarchicalModel/FollowUp/FollowUptoj_color.stan'
  # file = '../EndogenousVisualPriorEntry-BayesianHierarchicalModel/Baseball/toj_color.stan'
  )

toj_color_post = sampling(
  object = toj_color_model
  , data = toj_color_data_for_stan
<<<<<<< HEAD
  , iter = 2e4
  , chains = 8
  , cores = 8
  , pars = c('trial_prob', 'p'
             #,'population_pss_effect_mean'
             #, 'population_logjnd_effect_mean'
             #, 'zpopulation_logjnd_effect_sd'
             #, 'zpopulation_pss_effect_sd' 
=======
  , iter = 1e2
  , chains = 1
  , cores = 1
  , pars = c('trial_prob', 'p'
#              ,'population_pss_effect_mean'
#              , 'population_logjnd_effect_mean'
#              , 'zpopulation_logjnd_effect_sd'
#              , 'zpopulation_pss_effect_sd' 
>>>>>>> origin/master
             )  # blind to the TOJ effects as I collect data
  , include = FALSE
)
print(toj_color_post)
