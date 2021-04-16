#include mvn_functions.stan
data{

	// n: number of subj
	int<lower=1> n ;

	// k: number of variables modelled as intercorrelated
	int<lower=1> k ;

}
parameters{
	real dummy_ ;
}
generated quantities{

	// means: mean (across subj) for each coefficient
	vector[k] means ;
	for(i_k in 1:k){
		means[i_k] = std_normal_rng() ;
	}

	// sds: sd (across subj) for each coefficient
	vector[k] sds ;
	for(i_k in 1:k){
		sds[i_k] = weibull_rng(2,1) ;
	}

	// chol_corr: population-level correlations (on cholesky factor scale) amongst within-subject predictors
	matrix[k,k] chol_corr = lkj_corr_cholesky_rng(k,1) ;

	// y: array of values for each subj & coef
	vector[k] y[n] ;
	for(i_n in 1:n){
		y[i_n] = multi_normal_cholesky_rng(
			means
			, diag_pre_multiply(sds,chol_corr)
		) ;
	}

	// cor_vec: the lower-tri of the correlation matrix, flattened to a vector for efficient storage
	vector[(k*(k-1))/2] cor_vec = flatten_lower_tri(
		multiply_lower_tri_self_transpose(chol_corr)
	) ;

}
