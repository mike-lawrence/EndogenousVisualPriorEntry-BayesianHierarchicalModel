data{
	int N ;
	int L ;
	int y[L] ;
	real x[L] ;
	int id[L] ;
	int condition[L] ;
}
transformed data{
	vector[4] zeros ;
	for(i in 1:4){
		zeros[i] <- 0 ;
	}
}
parameters{
	//population means
	real population_pss_intercept_mean ;
	real population_pss_effect_mean ; 
	real<lower=0> population_logjnd_intercept_mean ;
	real population_logjnd_effect_mean ;
	//population sds
	real<lower=0,upper=pi()/2> zpopulation_pss_intercept_sd ;
	real<lower=0,upper=pi()/2> zpopulation_pss_effect_sd ; 
	real<lower=0,upper=pi()/2> zpopulation_logjnd_intercept_sd ;
	real<lower=0,upper=pi()/2> zpopulation_logjnd_effect_sd ; 
	// correlation
	corr_matrix[4] cor ; 
	//dummy variable for matt trick
	vector[4] beta[N]; 
}
transformed parameters{
	real trial_prob[L] ;
	{
		real pss_intercept_per_id[N] ; 
		real pss_effect_per_id[N] ; 
		real logjnd_intercept_per_id[N] ; 
		real logjnd_effect_per_id[N] ; 
		real trial_pss[L] ; 
		real trial_logjnd[L] ; 
		real population_pss_intercept_sd ;
		real population_pss_effect_sd ; 
		real population_logjnd_intercept_sd ;
		real population_logjnd_effect_sd ; 
		population_pss_intercept_sd <- tan(zpopulation_pss_intercept_sd) ;
		population_pss_effect_sd <- tan(zpopulation_pss_effect_sd) ;
		population_logjnd_intercept_sd <- tan(zpopulation_logjnd_intercept_sd);
		population_logjnd_effect_sd <- tan(zpopulation_logjnd_effect_sd) ;
		for(this_id in 1:N){
			pss_intercept_per_id[this_id] <- beta[this_id,1]*population_pss_intercept_sd + population_pss_intercept_mean ;
			pss_effect_per_id[this_id] <- beta[this_id,2]*population_pss_effect_sd + population_pss_effect_mean ;
			logjnd_intercept_per_id[this_id] <- beta[this_id,3]*population_logjnd_intercept_sd + population_logjnd_intercept_mean ;
			logjnd_effect_per_id[this_id] <- beta[this_id,4]*population_logjnd_effect_sd + population_logjnd_effect_mean ;
		}
		for(this_obs in 1:L){
			trial_pss[this_obs] <- pss_intercept_per_id[id[this_obs]] + pss_effect_per_id[id[this_obs]]*condition[this_obs] ;  // glove is -1, base is +1
			trial_logjnd[this_obs] <- logjnd_intercept_per_id[id[this_obs]] + logjnd_effect_per_id[id[this_obs]]*condition[this_obs] ; // glove is -1, base is +1
			trial_prob[this_obs] <- Phi_approx((x[this_obs]-trial_pss[this_obs])/exp(trial_logjnd[this_obs])) ;
		}
	}	
}
model{
	population_pss_intercept_mean ~ normal(0,1) ;
	population_pss_effect_mean ~ normal(0,1) ;

	population_logjnd_intercept_mean ~ normal(-1,.5) ;
	population_logjnd_effect_mean ~ normal(0,1) ;

	
	#sample the betas from standard multivariate normal
	for(this_id in 1:N){
	  beta[this_id] ~ multi_student_t(1,zeros,cor) ;
	}
	y ~ bernoulli(trial_prob) ;
	
}
