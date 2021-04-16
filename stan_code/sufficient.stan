data{

	// n: number of subj
	int<lower=1> n ;

	// k: number of coefficients modelled as intercorrelated
	int<lower=1> k ;

	// y: array of values for each subj & coef
	row_vector[k] y[n] ;

	// num_cors: number of correlations implied by the number of coefficients; user-supplied bc Stan is
	// strict in not permitting conversion from real to int.
	//   Must be: ((k^2)-k)/2
	int num_cors ;

}
transformed data{

	// y_mat: just a reformat of subj_coef_array for convenience
	matrix[n,k] y_mat ;
	for(this_n in 1:n){
		y_mat[this_n] = y[this_n] ;
	}

	// compute the empirical means & SDs
	vector[k] obs_means ;
	vector[k] obs_sds ;
	for(this_k in 1:k){
		obs_means[this_k] = mean(y_mat[,this_k]);
		obs_sds[this_k] = sd(y_mat[,this_k]);
	}
	vector[k] obs_vars = pow(obs_sds,2);

	real sqrt_n = sqrt(n) ;
	real sd_gamma_shape = (n - 1) / 2 ;

	// compute the empirical means & SDs
	// obs_z_div_se: empirical correlations, on Fisher's Z scale then divided by the SE
	vector[num_cors] obs_z ;
	real corz_se = 1/sqrt(n-3) ;
	{
		// corz_se: standard error for correlations on Fisher's Z scale
		vector[num_cors] obs_r ;
		int i = 0 ;
		// looping over lower-tri to compute correlation between each column in subj_coef
		for(this_k in 1:(k-1)){
			for(this_other_k in (this_k+1):k){
				i += 1 ;
				obs_r[i] = (
					sum(
						( y_mat[,this_k] - mean(y_mat[,this_k]) )
						.* ( y_mat[,this_other_k] - mean(y_mat[,this_other_k]) )
					)
				) / (
					(n-1) * sd(y_mat[,this_k]) * sd(y_mat[,this_other_k])
				) ;
			}
		}
		obs_z = atanh(obs_r) ;
	}
}
parameters{

	// pop_means: mean (across subj) for each coefficient
	vector[k] pop_means ;

	// pop_sds: sd (across subj) for each coefficient
	vector<lower=0>[k] pop_sds ;

	// corz_vec: population-level correlations (on Fisher's-Z scale) among within-subject predictors
	vector[num_cors] corz_vec ;

}
model{

	////
	// Priors
	////

	// normal(0,1) priors on all means
	pop_means ~ std_normal() ;

	// weibull(2,1) priors on all sds (peaked at ~.8)
	pop_sds ~ weibull(2,1) ;

	// normal(0,.74) priors on all corz
	// yields flat prior on abs(cor)<.7, and diminishing to zero by abs(cor)==1
	// view in R via: hist(tanh(abs(rnorm(1e7)*.74)),br=1e4,xlim=c(0,1))
	corz_vec ~ normal(0,.74) ;

	////
	// Likelihood
	////
	obs_means ~ normal( pop_means , pop_sds/sqrt_n ) ;
    obs_vars  ~ gamma( sd_gamma_shape , sd_gamma_shape ./ pow(pop_sds,2) ) ;

	// expressing the structure for the pairwise correlations
	obs_z ~ normal(corz_vec,corz_se) ;

}
generated quantities{

	// cor_vec: the upper-tri of the correlation matrix, flattened to a vector for efficient storage
	vector[num_cors] cor_vec = tanh(corz_vec) ;

}
