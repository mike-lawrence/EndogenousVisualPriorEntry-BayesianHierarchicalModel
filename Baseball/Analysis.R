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
		)
		return(to_return)
	}
}

checksums = NULL

a = ldply(
  .data = list.files(
	  	pattern = ".txt"
  		, full.names = T
  		, path = './Data_alpha'
  	)
  , .fun = check_before_read
  , .progress = 'text'
)
a$id = paste('alpha',a$participant_id,a$created)
	
b = ldply(
	.data = list.files(
		pattern = ".txt"
		, full.names = T
		, path = './Data_delta'
	)
	, .fun = check_before_read
	, .progress = 'text'
)
b$id = paste('delta',b$participant_id,b$created)

a = rbind(a,b)
length(unique(a$id))

length(checksums)

#count trials per id
temp = data.frame(table(id=a$id))

#discard those with too few trials
keep = temp$id[temp$Freq==480]
a = a[a$id %in% keep,]

length(unique(a$id))



#toss baseball-naive Ss
a = a[substr(a$id,1,8)!='alpha 12',]
a = a[substr(a$id,1,7)!='delta 7',]
a = a[substr(a$id,1,8)!='delta 20',]
length(unique(a$id))  # count files


#### Type ####
#As factor 
a$gender = as.factor(a$gender)
a$age = as.factor(a$age)
a$handedness = as.factor(a$handedness)
a$created = as.factor(a$created)
a$block_num = as.factor(a$block_num)
a$trial_num = as.factor(a$trial_num)
#a$soa = as.factor(a$soa)
a$baserun_offset = as.factor(a$baserun_offset)
a$first_arrival = as.factor(a$first_arrival)
a$probed_trial = as.factor(a$probed_trial)
a$glove_probe_dist = as.factor(a$glove_probe_dist)
a$base_probe_dist = as.factor(a$base_probe_dist)
a$probe_location = as.factor(a$probe_location)
a$toj_response = as.factor(a$toj_response)

#As numeric
#a$probe_color = as.numeric(a$probe_color)
#a$color_response = as.numeric(a$color_response)
a$color_diff = as.numeric(a$color_diff)
a$response_time = as.numeric(a$response_time)



#### Summary ####
summary(a)

# double check 20/80
table(a$probe_location, a$base_probe_dist)  # by base
table(a$probe_location, a$glove_probe_dist)  # by glove. Should be symetric 
toss = NULL
for(i in unique(a$id)){
	temp = with(
		a[a$id==i,]
		,data.frame(table(
			probe_location, glove_probe_dist
		))
	)
	if(
		(!all(temp$Freq==c(64,16,0,0,16,64,0,0))) &
		(!all(temp$Freq==c(0,0,64,16,0,0,16,64)))
	){
		toss = c(toss,i)
		print(i)
		print(temp)
	}
}

a = a[!(a$id %in% toss),]

length(unique(a$id))

### add at 'tie goes to runner' summary
a$know_tie_goes_runner = FALSE
a[a$id == "alpha 4 2015-02-04 13:52:56"
  | a$id == "alpha 8 2015-02-09 13:38:03"
  | a$id == "alpha 15 2015-02-10 17:07:37"
  | a$id == "delta 3 2014-12-03 09:03:59"
  | a$id == "delta 4 2014-12-03 10:11:45"
  | a$id == "delta 5 2014-12-04 10:10:39"
  | a$id == "delta 6 2014-12-04 11:07:22"
  | a$id == "delta 11 2014-12-05 09:58:42"
  | a$id == "delta 14 2014-12-05 13:04:02"
  | a$id == "delta 26 2014-12-10 15:18:43"
  | a$id == "delta 28 2015-02-02 13:41:26"
  | a$id == "delta 29 2015-02-02 14:42:42",]$know_tie_goes_runner = TRUE

a$use_tie_goes_runner = FALSE
a[a$id == "alpha 4 2015-02-04 13:52:56"
  | a$id == "alpha 15 2015-02-10 17:07:37"
  | a$id == "delta 3 2014-12-03 09:03:59"
  | a$id == "delta 4 2014-12-03 10:11:45"
  | a$id == "delta 5 2014-12-04 10:10:39"
  | a$id == "delta 6 2014-12-04 11:07:22"
  | a$id == "delta 14 2014-12-05 13:04:02"
  | a$id == "delta 28 2015-02-02 13:41:26"
  | a$id == "delta 29 2015-02-02 14:42:42",]$use_tie_goes_runner = TRUE


# get rid of delta 8 because psychometric function
a = a[a$id != "delta 8 2014-12-04 14:23:37",]

length(unique(a$id))



#### TOJ ####
toj_trials = a
toj_trials = toj_trials[!is.na(toj_trials$toj_response), ]
toj_trials$safe = FALSE
toj_trials$safe[toj_trials$toj_response == "safe"] = TRUE

### SOAs 
toj_trials$soa2 = toj_trials$soa
# correct soas 
toj_trials[toj_trials$soa2 == "15",]$soa2 = 17
toj_trials[toj_trials$soa2 == "45",]$soa2 = 50
toj_trials[toj_trials$soa2 == "90",]$soa2 = 100
toj_trials[toj_trials$soa2 == "135",]$soa2 = 150
toj_trials[toj_trials$soa2 == "240",]$soa2 = 250
# Negative SOAs means Ball first 
toj_trials$soa2[toj_trials$first_arrival == "ball"] = -toj_trials$soa2[toj_trials$first_arrival == "ball"]



### save toj trials for posterior predictive check
# aggregate with respect to id and condition 
# save(toj_trials, file = "../toj_trials.Rdata")


### Graph participant-wise psychometric functions 
toj_means_by_id_by_condition = ddply(
  .data = toj_trials
  , .variables = .(id,base_probe_dist, soa2)
  , .fun = function(x){
    to_return = data.frame(
      value = mean(x$safe)
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
    , shape = base_probe_dist
    , linetype = base_probe_dist
    , group = base_probe_dist
  )
)+
  facet_wrap(
    ~ id
  )+
  geom_smooth(
    method = "glm"
    , method.args = list(family = binomial(link="probit"))
    , se = FALSE
  ) +
  geom_point()

### compare tie goes to runner with not 
toj_means_by_id_by_convention = ddply(
  .data = toj_trials
  , .variables = .(base_probe_dist, soa2, know_tie_goes_runner)
  , .fun = function(x){
    to_return = data.frame(
      value = mean(x$safe)
    )
    return(to_return)
  }
)
toj_means_by_id_by_condition$soa2 = as.numeric(as.character(toj_means_by_id_by_condition$soa2))

ggplot(
  data = toj_means_by_id_by_convention
  , mapping = aes(
    x = soa2
    , y =  value
    , shape = base_probe_dist
    , linetype = base_probe_dist
    , group = base_probe_dist
  )
)+
  facet_wrap(
    ~ know_tie_goes_runner
  )+
  geom_smooth(
    method = "glm"
    , method.args = list(family = binomial(link="probit"))
    , se = FALSE
  ) +
  geom_point()

### Get PSSs and JNDs
toj_by_condition = ddply(
  .data = toj_trials
  , .variables = .(id, base_probe_dist, know_tie_goes_runner, use_tie_goes_runner)  
  , .fun = function(x){
    fit = glm(
      formula = safe~soa2
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
pss_means_by_condition = aggregate(pss ~ base_probe_dist, data = toj_by_condition, FUN = mean) 
names(pss_means_by_condition)[1] = "attend"
levels(pss_means_by_condition$attend) = c("glove", "base")
pss_means_by_condition
plot(toj_by_condition$pss) # sense of outliers

jnd_means_by_condition = aggregate(jnd ~ base_probe_dist, data = toj_by_condition, FUN = mean) 
names(jnd_means_by_condition)[1] = "attend"
levels(jnd_means_by_condition$attend) = c("glove", "base")
jnd_means_by_condition
plot(toj_by_condition$jnd)  # get a sense of outliers

# look at 'tie goes to runner'
# use MEDIAN because outlier
pss_means_by_convention = aggregate(pss ~ base_probe_dist + know_tie_goes_runner, data = toj_by_condition, FUN = median) 
names(pss_means_by_convention)[1] = "attend"
levels(pss_means_by_convention$attend) = c("glove", "base")
pss_means_by_convention
ggplot(data = toj_by_condition
       , aes(y=pss
             , x =  know_tie_goes_runner
             , colour =base_probe_dist
             )
       )+
  stat_summary() # default to mean + se I think 

jnd_means_by_convention = aggregate(jnd ~ base_probe_dist + know_tie_goes_runner, data = toj_by_condition, FUN = median) 
names(jnd_means_by_convention)[1] = "attend"
levels(jnd_means_by_convention$attend) = c("glove", "base")
jnd_means_by_convention
ggplot(data = toj_by_condition
       , aes(y=jnd
             , x =  know_tie_goes_runner
             , colour =base_probe_dist
       )
)+
  stat_summary() # default to mean + se I think 
# No apparent interaction! Do not include, for simplicity

# look at JND and PSS intercept scatter plot 
# for attend glove
pss_by_id_attend_glove = toj_by_condition[toj_by_condition$base_probe_dist == 0.2,]$pss
jnd_by_id_attend_glove = toj_by_condition[toj_by_condition$base_probe_dist == 0.2,]$jnd
plot(
  pss_by_id_attend_glove
  , jnd_by_id_attend_glove
)
abline(lm(jnd_by_id_attend_glove~pss_by_id_attend_glove))
# for attend base
pss_by_id_attend_base = toj_by_condition[toj_by_condition$base_probe_dist == 0.8,]$pss
jnd_by_id_attend_base = toj_by_condition[toj_by_condition$base_probe_dist == 0.8,]$jnd
plot(
  pss_by_id_attend_base
  , jnd_by_id_attend_base
)
abline(lm(jnd_by_id_attend_base~pss_by_id_attend_base))



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

color_trials = a
color_trials$probe_location[color_trials$probe_location == "[1088, 896]"] = "base"
color_trials$probe_location[color_trials$probe_location == "[1328, 581]"] = "glove"
color_trials = color_trials[!is.na(color_trials$color_diff), ]
hist(color_trials$color_diff,br=100)
color_trials[color_trials$color_diff > 180,]$color_diff = - (360 - color_trials[color_trials$color_diff > 180,]$color_diff)
color_trials[color_trials$color_diff < (-180),]$color_diff = color_trials[color_trials$color_diff < (-180),]$color_diff + 360
hist(color_trials$color_diff,br=100)

color_trials$attended = FALSE
color_trials$attended[ (color_trials$base_probe_dist == 0.8 & color_trials$probe_location == "base") | (color_trials$base_probe_dist == 0.2 & color_trials$probe_location == "glove")] = TRUE

color_trials$color_diff_radians = color_trials$color_diff*pi/180



### save color trials for posterior predicitve check 
# save(color_trials, file = "../color_trials.Rdata")



# mixture model - rho and kappa by participant
source("../../fit_uvm.R")
fitted_all = ddply(
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

# get rho and kappa by condition 
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

# look at descriptive statistics (mean values) of raw data to compare with parameter estimates of model later
rho_means_by_condition = aggregate(rho ~ attended, data = fitted_by_condition, FUN = mean)
rho_means_by_condition
plot(fitted_by_condition$rho)  # get a sense of outliers

kappa_means_by_condition = aggregate(kappa_prime ~ attended, data = fitted_by_condition, FUN = mean)
kappa_means_by_condition$kappa_prime = exp(kappa_means_by_condition$kappa_prime)
names(kappa_means_by_condition)[2] = "kappa"
kappa_means_by_condition



#### Stan ####
toj_color_data_for_stan = list(
  N_toj = length(unique(toj_trials$id))
  , L_toj = nrow(toj_trials)
  , y_toj = as.numeric(toj_trials$safe)  
  , x_toj = (as.numeric(toj_trials$soa2))/250  # we normalize soas, and therefore pss
  , id_toj = as.numeric(factor(toj_trials$id))
  , condition_toj = ifelse(toj_trials$glove_probe_dist==.8,-1,1)  # glove is -1 and base is +1
  , N_color = length(unique(color_trials$id))
  , L_color = nrow(color_trials)
  , unit_color = as.numeric(factor(color_trials$id))
  , condition_color = as.numeric(as.factor(color_trials$attended)) # TRUE is 2, FALSE is 1 
  , condition_convention = ifelse(aggregate(know_tie_goes_runner~id,data = toj_trials, FUN =unique)$know_tie_goes_runner, -1, 1) # know ('safe' bias) is -1...
  , y_color = pi+degree_to_rad(color_trials$color_diff)  # want from 0 to 360 instead of -180 to 180
)
  
toj_color_model = stan_model(
  file = './EndogenousVisualPriorEntry-BayesianHierarchicalModel/Baseball/toj_color.stan'
)

toj_color_post = sampling(
  object = toj_color_model
  , data = toj_color_data_for_stan
  , iter = 1e4*2
  , chains = 8
  , cores = 8
  , pars = c('trial_prob', 'p')
  , include = FALSE
)
print(toj_color_post)






  
  