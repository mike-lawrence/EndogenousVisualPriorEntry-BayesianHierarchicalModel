# set directory 
setwd("~/Documents/TOJ")

#### Libraries ####
library(plyr)
library(ggplot2)
library(grid)



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
  		, path = './Follow-Up/Analysis'
  	)
  , .fun = check_before_read
  , .progress = 'text'
)
names(a)[1] = "id"
	
# b = ldply(
# 	.data = list.files(
# 		pattern = ".txt"
# 		, full.names = T
# 		, path = './Baseball/baseballtojdata/Data_delta'
# 	)
# 	, .fun = check_before_read
# 	, .progress = 'text'
# )
# b$id = paste('delta',b$participant_id,b$created)

# a = rbind(a,b)

length(unique(a$id))

length(checksums)

#count trials per id
temp = data.frame(table(id=a$id))

#discard those with too few trials
keep = temp$id[temp$Freq==588]  # 36 * 3 + 240 * 2 = 588 
a = a[a$id %in% keep,]

length(unique(a$id))



#### Type ####
#As factor 
a$id = as.factor(a$id)
a$sex = as.factor(a$sex)
a$age = as.factor(a$age)
a$handedness = as.factor(a$handedness)
a$block_num = as.factor(a$block_num)
a$trial_num = as.factor(a$trial_num)
#a$soa = as.factor(a$soa)
a$trial_type = as.factor(a$trial_type)
a$probe_loc = as.factor(a$probe_loc)
a$t1_loc = as.factor(a$t1_loc)
a$t1_type = as.factor(a$t1_type)

#As numeric
#a$probe_color = as.numeric(a$probe_color)
#a$probe_judgement_color = as.numeric(a$probe_judgement_color)
a$probe_angle = as.numeric(a$probe_angle)
a$rotation = as.numeric(a$rotation)
a$probe_judgement = as.numeric(a$probe_judgement)  # angle of color judgement (0 to 360)
a$probe_rt = as.numeric(a$probe_rt)


# create color diff column 
a$color_diff = as.numeric(a$probe_angle - a$probe_judgement)



#### Summary ####
summary(a)

# double check 20/80
table(a$probe_loc, a$block_num)  # don't have a block bias factor yet  
aggregate(trial_num ~ block_num, data = a, FUN = length)
table(a$t1_loc, a$block_num)
table(a$t1_type, a$block_num)

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




#### TOJ ####
toj_trials = a
toj_trials = toj_trials[!is.na(toj_trials$toj_response), ]
toj_trials$safe = FALSE
toj_trials$safe[toj_trials$toj_response == "safe"] = TRUE
toj_trials$soa2 = toj_trials$soa
# Negative SOAs means Ball first 
toj_trials$soa2[toj_trials$first_arrival == "ball"] = -toj_trials$soa2[toj_trials$first_arrival == "ball"]


toj_data_for_stan = list(
	N = length(unique(toj_trials$id))
	, L = nrow(toj_trials)
	, y = as.numeric(toj_trials$safe)
	, x = (as.numeric(toj_trials$soa2))/240  # we normalize soas, and therefore pss
	, id = as.numeric(factor(toj_trials$id))
	, condition = ifelse(toj_trials$glove_probe_dist==.8,-1,1)
)

library(rstan)
toj_model = stan_model(
	file = './BayesMay4th/toj.stan'
)

toj_post = sampling(
	object = toj_model
	, data = toj_data_for_stan
	, iter = 1e2
	, chains = 1
	, pars = 'trial_prob'
	, include = FALSE
)
print(toj_post)



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

color_data_for_stan = list(
	N = length(unique(color_trials$id))
	, L = nrow(color_trials)
	, unit = as.numeric(factor(color_trials$id))
	, condition = as.numeric(as.factor(color_trials$attended))
	, y = pi+degree_to_rad(color_trials$color_diff)  # want from 0 to 360 instead of -180 to 180
)

color_model = stan_model(file='./BayesMay4th/color.stan')
color_post <- sampling(
	object = color_model
	, data = color_data_for_stan
	, iter = 1e2
	, chains = 1
	, cores = 1
	, pars = c( 
		'logitRhoMean'
		,'logKappaMean'
		,'logitRhoEffectMean'
		,'logKappaEffectMean'
		,'zlogitRhoSD'
		,'zlogKappaSD'
		,'zlogitRhoEffectSD'
		,'zlogKappaEffectSD'
		, 'cors'
		, 'betas'
	)
)
print(color_post)