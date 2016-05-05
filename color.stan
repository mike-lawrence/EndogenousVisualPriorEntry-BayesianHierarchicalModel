#docs suggestion: don't use "beta" as parameter name
#"This way, both the mu and sigma parameters are shared. Which way works out to be more efficient will depend on the shape of the data; if the sizes are very small, the simple vectorization may be faster, but for moderate to large sized groups, the full expansion should be faster."

#reparameterizations:
# with_correlations: multivariate student-t to cholesky as on page 219
# no_correlations: cauchy to uniform+tan

#optimizations:
#- trialKappa/trialLogRho
#   Might not cause speed-up because there's no vectorized mixture statement anyway
#- aggregating common expressions
#		Already done


#try no overflow compensation
#or put it in transformed parameters


data {
	int<lower=0> N;
	int<lower=0> L;
	int<lower=0> unit[L];
	int<lower=1,upper=2> condition[L];
	real<lower=0,upper=2*pi()> y[L];
}
transformed data{
	vector[4] zeros;
	real neglog2pi;
	neglog2pi <- -log(2.0 * pi()); #log-probability of uniform component (i.e. data invariant)
	for(i in 1:4){
		zeros[i] <- 0 ;
	}
}
parameters {
	//population parameters
	real logitRhoMean;
	real logKappaMean;
	real logitRhoEffectMean;
	real logKappaEffectMean;
	// SDs for population parameters
	real<lower=0,upper=pi()/2> zlogitRhoSD;
	real<lower=0,upper=pi()/2> zlogKappaSD;
	real<lower=0,upper=pi()/2> zlogitRhoEffectSD;
	real<lower=0,upper=pi()/2> zlogKappaEffectSD;
	corr_matrix[4] cors ;
	// dummy parameter to employ the matt trick
	// matrix[N,4] betas ;
	vector[4] betas[N] ;
}
transformed parameters {
	//variable inits
	vector[L] p ; // for storing log-probabilities
	{
		//local inits
		real logitRhoSD ;
		real logKappaSD ;
		real logitRhoEffectSD ;
		real logKappaEffectSD ;
		vector[N] logRho[2]; //subject-level log-rho for each condition
		vector[N] log1mrho_neglog2pi[2]; //subject-level log(1-rho) for each condition
		vector[N] kappa[2]; //eventually used in the model block
		vector[N] rho[2]; //intermediate value
		vector[N] logitRho; //intermediate value
		vector[N] logKappa; //intermediate value
		vector[N] logitRhoEffect; //intermediate value
		vector[N] logKappaEffect; //intermediate value
		// computations
		logitRhoSD <- tan(zlogitRhoSD) ;
		logKappaSD <- tan(zlogKappaSD) ;
		logitRhoEffectSD <- tan(zlogitRhoEffectSD) ;
		logKappaEffectSD <- tan(zlogKappaEffectSD) ;
		// compute unit-level parameters
		for(n in 1:N){
			logitRho[n] <- logitRhoMean + betas[n,1]*logitRhoSD;
			logKappa[n] <- logKappaMean + betas[n,2]*logKappaSD;
			logitRhoEffect[n] <- logitRhoEffectMean + betas[n,3]*logitRhoEffectSD;
			logKappaEffect[n] <- logKappaEffectMean + betas[n,4]*logKappaEffectSD;
			rho[1,n] <- inv_logit( logitRho[n] - logitRhoEffect[n]/2 );
			rho[2,n] <- inv_logit( logitRho[n] + logitRhoEffect[n]/2 );
			kappa[1,n] <- exp( logKappa[n] - logKappaEffect[n]/2 );
			kappa[2,n] <- exp( logKappa[n] + logKappaEffect[n]/2 );
			logRho[1,n] <- log(rho[1,n]);
			logRho[2,n] <- log(rho[2,n]);
			log1mrho_neglog2pi[1,n] <- log1m(rho[1,n]) + neglog2pi;
			log1mrho_neglog2pi[2,n] <- log1m(rho[2,n]) + neglog2pi;
		}
		// compute trial-level parameters
		for (i in 1:L){
			p[i] <- log_sum_exp(
				logRho[condition[i],unit[i]] + von_mises_log(y[i],pi(),kappa[condition[i],unit[i]])
				, log1mrho_neglog2pi[condition[i],unit[i]]
			) ;
		}
	}
}
model {
	
	//set priors on population parameters
	logitRhoMean ~ normal(3,3);
	logKappaMean ~ normal(3,3);
	logitRhoEffectMean ~ normal(0,3) ;#normal(0,3);
	logKappaEffectMean ~ normal(0,3) ;#normal(0,3);
	// logitRhoSD ~ weibull(2,2);#student_t(4,0,2);
	// logKappaSD ~ weibull(2,2);#student_t(4,0,1);
	// logitRhoEffectSD ~ weibull(2,2);#student_t(4,0,1);
	// logKappaEffectSD ~ weibull(2,2);#student_t(4,0,1);
	cors ~ lkj_corr(4) ;
	
	//standard normal sampling of betas (for matt trick, see transformed parameters)
	for(n in 1:N){
		betas[n] ~ multi_student_t(1,zeros,cors) ;
	}
	
	//update the log-probability from p (defined in transformed parameters)
	increment_log_prob(p) ;
}
