#include mvn_functions.stan
data{

	// n: number of subj
	int<lower=1> n ;

	// k: number of coefficients modelled as intercorrelated
	int<lower=1> k ;

	// y: array of values for each subj & coef
	vector[k] y[n] ;

}
parameters{

	// means: mean (across subj) for each coefficient
	vector[k] means ;

	// sds: sd (across subj) for each coefficient
	vector<lower=0>[k] sds ;

	// chol_corr: population-level correlations (on cholesky factor scale) amongst within-subject predictors
	cholesky_factor_corr[k] chol_corr ;

}
model{

	////
	// Priors
	////

	// normal(0,1) priors on all means
	means ~ std_normal() ;

	// weibull(2,1) priors on all sds (peaked at ~.8)
	sds ~ weibull(2,1) ;

	// flat prior across sets of correlation matrices
	chol_corr ~ lkj_corr_cholesky(1) ;

	////
	// Likelihood
	////

	// y as multivariate normal
	y ~ multi_normal_cholesky(
		rep_array(means,n)
		, diag_pre_multiply(sds,chol_corr)
	) ;


}
generated quantities{

	// cor_vec: the lower-tri of the correlation matrix, flattened to a vector for efficient storage
	vector[(k*(k-1))/2] cor_vec = flatten_lower_tri(
		multiply_lower_tri_self_transpose(chol_corr)
	) ;

}
