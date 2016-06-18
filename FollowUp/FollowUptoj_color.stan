data{
	int N_toj ;
	int L_toj ;
	int y_toj[L_toj] ;
	real x_toj[L_toj] ;
	int id_toj[L_toj] ;
	int condition_toj[L_toj] ;
	int<lower=0> N_color;                      
  int<lower=0> L_color;                         
  int<lower=0> unit_color[L_color];                 
  int<lower=1,upper=2> condition_color[L_color];   
  int condition_probe[N_color];
  int condition_initial_bias[N_toj];
  int condition_judgement_type[N_toj];
  real<lower=0,upper=2*pi()> y_color[L_color]; 
}
transformed data{
	vector[8] zeros ;
	real neglog2pi;
  neglog2pi <- -log(2.0 * pi()); // log-probability of uniform component (i.e. data invariant)
	for(i in 1:8){
		zeros[i] <- 0 ;
	}
}
parameters{
	//population means
	real population_pss_intercept_mean ;
	real population_pss_effect_mean ; 
	real population_pss_initial_bias_effect_mean;
	real population_pss_judgement_type_effect_mean;
	real population_pss_probe_effect_mean;
	real population_pss_probe_interaction_effect_mean;
	real population_pss_judgement_type_interaction_effect_mean;
	real population_pss_initial_bias_interaction_effect_mean;
	real population_logjnd_intercept_mean ;
	real population_logjnd_effect_mean ;
	real population_logjnd_initial_bias_effect_mean;
	real population_logjnd_judgement_type_effect_mean;
	real population_logjnd_probe_effect_mean;
	real population_logjnd_probe_interaction_effect_mean;
	real population_logjnd_judgement_type_interaction_effect_mean;
	real population_logjnd_initial_bias_interaction_effect_mean;
	//population sds
	real<lower=0,upper=pi()/2> zpopulation_pss_intercept_sd ;
	real<lower=0,upper=pi()/2> zpopulation_pss_effect_sd ; 
	real<lower=0,upper=pi()/2> zpopulation_logjnd_intercept_sd ;
	real<lower=0,upper=pi()/2> zpopulation_logjnd_effect_sd ; 
	
	//population parameters
  real logitRhoMean;
  real logKappaMean;
  real logitRhoEffectMean;
  real logKappaEffectMean;
  // between subject, probe duration effect
  real logitRhoProbeEffectMean;
  real logKappaProbeEffectMean;
  real logitRhoJudgementTypeEffectMean;
  real logKappaJudgementTypeEffectMean;
  real logitRhoInitialBiasEffectMean;
  real logKappaInitialBiasEffectMean;
  // between vs. within subject interaction terms
  real logitRhoProbeInteractionEffectMean;
  real logKappaProbeInteractionEffectMean;
  real logitRhoJudgementTypeInteractionEffectMean;
  real logKappaJudgementTypeInteractionEffectMean;
  real logitRhoInitialBiasInteractionEffectMean;
  real logKappaInitialBiasInteractionEffectMean;
  // SDs for population parameters
  real<lower=0,upper=pi()/2> zlogitRhoSD;
  real<lower=0,upper=pi()/2> zlogKappaSD;
  real<lower=0,upper=pi()/2> zlogitRhoEffectSD;
  real<lower=0,upper=pi()/2> zlogKappaEffectSD;
  
	// correlation
	corr_matrix[8] cor ; 
	//dummy variable for matt trick
	vector[8] beta[N_toj]; // N_toj = N_color
}

transformed parameters{
	real trial_prob[L_toj] ;
  vector[L_color] p ; // for storing log-probabilities
	{
	  // local inits for TOJ
		real pss_intercept_per_id[N_toj] ; 
		real pss_effect_per_id[N_toj] ; 
		real logjnd_intercept_per_id[N_toj] ; 
		real logjnd_effect_per_id[N_toj] ; 
		real trial_pss[L_toj] ; 
		real trial_logjnd[L_toj] ; 
		real population_pss_intercept_sd ;
		real population_pss_effect_sd ; 
		real population_logjnd_intercept_sd ;
		real population_logjnd_effect_sd ; 
		
    //local inits for color wheel
    real logitRhoSD ;
    real logKappaSD ;
    real logitRhoEffectSD ;
    real logKappaEffectSD ;
    vector[N_color] logRho[2]; //subject-level log-rho for each condition
    vector[N_color] log1mrho_neglog2pi[2]; //subject-level log(1-rho) for each condition
    vector[N_color] kappa[2]; //eventually used in the model block
    vector[N_color] rho[2]; //intermediate value
    vector[N_color] logitRho; //intermediate value
    vector[N_color] logKappa; //intermediate value
    vector[N_color] logitRhoEffect; //intermediate value
    vector[N_color] logKappaEffect; //intermediate value
		
		// computations for TOJ
		population_pss_intercept_sd <- tan(zpopulation_pss_intercept_sd) ;
		population_pss_effect_sd <- tan(zpopulation_pss_effect_sd) ;
		population_logjnd_intercept_sd <- tan(zpopulation_logjnd_intercept_sd);
		population_logjnd_effect_sd <- tan(zpopulation_logjnd_effect_sd) ;
		for(this_id in 1:N_toj){
			pss_intercept_per_id[this_id] <- beta[this_id,1]*population_pss_intercept_sd + population_pss_intercept_mean
			+ population_pss_judgement_type_effect_mean*condition_judgement_type[this_id]/2 
			+ population_pss_initial_bias_effect_mean*condition_initial_bias[this_id]/2 
			+ population_pss_probe_effect_mean*condition_probe[this_id]/2;
			pss_effect_per_id[this_id] <- ( beta[this_id,2]*population_pss_effect_sd 
			+ population_pss_effect_mean
		    + population_pss_probe_interaction_effect_mean*condition_probe[this_id]
		    + population_pss_judgement_type_interaction_effect_mean*condition_judgement_type[this_id]
		    + population_pss_initial_bias_interaction_effect_mean*condition_initial_bias[this_id] )/2;
			logjnd_intercept_per_id[this_id] <- beta[this_id,3]*population_logjnd_intercept_sd + population_logjnd_intercept_mean
			+ population_logjnd_judgement_type_effect_mean*condition_judgement_type[this_id]/2 
			+ population_logjnd_initial_bias_effect_mean*condition_initial_bias[this_id]/2
			+ population_logjnd_probe_effect_mean*condition_probe[this_id]/2;
			logjnd_effect_per_id[this_id] <- ( beta[this_id,4]*population_logjnd_effect_sd 
			+ population_logjnd_effect_mean
			+ population_logjnd_probe_interaction_effect_mean*condition_probe[this_id]
			+ population_logjnd_judgement_type_interaction_effect_mean*condition_judgement_type[this_id]
			+ population_logjnd_initial_bias_interaction_effect_mean*condition_initial_bias[this_id] )/2;
		}
		for(this_obs in 1:L_toj){
			trial_pss[this_obs] <- pss_intercept_per_id[id_toj[this_obs]] + pss_effect_per_id[id_toj[this_obs]]*condition_toj[this_obs];  // glove, RIGHT is -1... base, LEFT is +1 
			trial_logjnd[this_obs] <- logjnd_intercept_per_id[id_toj[this_obs]] + logjnd_effect_per_id[id_toj[this_obs]]*condition_toj[this_obs]; // glove, RIGHT is -1... base, LEFT is +1 
			trial_prob[this_obs] <- Phi_approx((x_toj[this_obs]-trial_pss[this_obs])/exp(trial_logjnd[this_obs])) ;
		}
		
  	// computations for color wheel
    logitRhoSD <- tan(zlogitRhoSD) ;
    logKappaSD <- tan(zlogKappaSD) ;
    logitRhoEffectSD <- tan(zlogitRhoEffectSD) ;
    logKappaEffectSD <- tan(zlogKappaEffectSD) ;
    // compute unit-level parameters
    for(n in 1:N_color){
      logitRho[n] <- logitRhoMean + beta[n,5]*logitRhoSD 
      + condition_probe[n]*logitRhoProbeEffectMean/2
      + condition_judgement_type[n]*logitRhoJudgementTypeEffectMean/2
      + condition_initial_bias[n]*logitRhoInitialBiasEffectMean/2; 
      logKappa[n] <- logKappaMean + beta[n,6]*logKappaSD 
      + condition_probe[n]*logKappaProbeEffectMean/2
      + condition_judgement_type[n]*logKappaJudgementTypeEffectMean/2 
      + condition_initial_bias[n]*logKappaInitialBiasEffectMean/2;
      logitRhoEffect[n] <- logitRhoEffectMean + beta[n,7]*logitRhoEffectSD 
      + condition_probe[n]*logitRhoProbeInteractionEffectMean
      + condition_judgement_type[n]*logitRhoJudgementTypeInteractionEffectMean
      + condition_initial_bias[n]*logitRhoInitialBiasInteractionEffectMean;
      logKappaEffect[n] <- logKappaEffectMean + beta[n,8]*logKappaEffectSD 
      + condition_probe[n]*logKappaProbeInteractionEffectMean
      + condition_judgement_type[n]*logKappaJudgementTypeInteractionEffectMean
      + condition_initial_bias[n]*logKappaInitialBiasInteractionEffectMean;
      rho[1,n] <- inv_logit( logitRho[n] - logitRhoEffect[n]/2 ) ;  // unattended is 1, is minus
      rho[2,n] <- inv_logit( logitRho[n] + logitRhoEffect[n]/2 ) ;  // attended is 2, is plus
      kappa[1,n] <- exp( logKappa[n] - logKappaEffect[n]/2  ) ;
      kappa[2,n] <- exp( logKappa[n] + logKappaEffect[n]/2 );
      logRho[1,n] <- log(rho[1,n]);
      logRho[2,n] <- log(rho[2,n]);
      log1mrho_neglog2pi[1,n] <- log1m(rho[1,n]) + neglog2pi;
      log1mrho_neglog2pi[2,n] <- log1m(rho[2,n]) + neglog2pi;
    }
  	// compute trial-level parameters
    for (i in 1:L_color){
      p[i] <- log_sum_exp(  // Ghis: likelihood 
                            logRho[condition_color[i],unit_color[i]] + von_mises_log(y_color[i],pi(),kappa[condition_color[i],unit_color[i]])   
                            , log1mrho_neglog2pi[condition_color[i],unit_color[i]]
      ) ;
    }
	}
}
model{
	population_pss_intercept_mean ~ normal(0,1) ;
	population_pss_effect_mean ~ normal(0,1) ;
	population_pss_initial_bias_effect_mean ~ normal(0,1);
	population_pss_judgement_type_effect_mean ~ normal(0,1);
	population_pss_probe_effect_mean ~ normal(0,1);
	population_pss_probe_interaction_effect_mean ~ normal(0,1);
	population_pss_judgement_type_interaction_effect_mean ~ normal(0,1);
	population_pss_initial_bias_interaction_effect_mean ~ normal(0,1) ; 
	population_logjnd_intercept_mean ~ normal(-1,.5) ;
	population_logjnd_effect_mean ~ normal(0,1) ;
	population_logjnd_initial_bias_effect_mean ~ normal(0,1);
	population_logjnd_judgement_type_effect_mean ~ normal(0,1);
	population_logjnd_probe_effect_mean ~ normal(0,1);
	population_logjnd_probe_interaction_effect_mean ~ normal(0,1);
	population_logjnd_judgement_type_interaction_effect_mean ~ normal(0,1);
	population_logjnd_initial_bias_interaction_effect_mean ~ normal(0,1);


  //set priors on population parameters
  logitRhoMean ~ normal(3,3);
  logKappaMean ~ normal(3,3);
  logitRhoEffectMean ~ normal(0,3) ;#normal(0,3);
  logKappaEffectMean ~ normal(0,3) ;#normal(0,3);
  logitRhoProbeEffectMean ~ normal(0,3) ;
  logKappaProbeEffectMean ~ normal(0,3) ;
  logitRhoProbeInteractionEffectMean ~ normal(0,3);
  logKappaProbeInteractionEffectMean ~ normal(0,3);
  logitRhoJudgementTypeEffectMean ~ normal(0,3);
  logKappaJudgementTypeEffectMean ~ normal(0,3);
  logitRhoJudgementTypeInteractionEffectMean ~ normal(0,3);
  logKappaJudgementTypeInteractionEffectMean ~ normal(0,3);
  logitRhoInitialBiasEffectMean ~ normal(0,3);
  logKappaInitialBiasEffectMean ~ normal(0,3);
  logitRhoInitialBiasInteractionEffectMean ~ normal(0,3);
  logKappaInitialBiasInteractionEffectMean ~ normal(0,3);
  // logitRhoSD ~ weibull(2,2);#student_t(4,0,2);
  // logKappaSD ~ weibull(2,2);#student_t(4,0,1);
  // logitRhoEffectSD ~ weibull(2,2);#student_t(4,0,1);
  // logKappaEffectSD ~ weibull(2,2);#student_t(4,0,1);

  cor ~ lkj_corr(4) ;  // prior on correlation matrix 
	
	#sample the betas from standard multivariate normal
	for(this_id in 1:N_toj){
	  beta[this_id] ~ multi_student_t(1,zeros,cor) ;
	}
	
	y_toj ~ bernoulli(trial_prob) ;
	//update the log-probability from p (defined in transformed parameters)
  increment_log_prob(p) ;
}
