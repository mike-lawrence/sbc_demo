generate_true_and_data = function(n,k,iteration,stan_file){
	n = 1e3
	k = 1e3
	iteration = 1
	stan_file = 'stan_code/mvn_generate.stan'
	mod = cmdstanr::cmdstan_model(
		stan_file
		, include_paths = './stan_code'
	)
	quantities = mod$generate_quantities(
		data = lst(n,k)
		, fitted_params = posterior::as_draws_array(tibble(dummy_=0))
		, seed = abs(digest::digest2int(paste0(n,k,iteration)))
	)
	#get data
	(
		quantities$draws('y')
		%>% posterior::as_draws_rvars()
	) -> y

	#package data
	(
		lst(
			n = n
			, k = k
			, y = posterior::draws_of(y$y)[1,,]
		)
	) -> data_for_stan

	#get parameters
	(
		quantities$draws()
		%>% posterior::as_draws_df()
		%>% as_tibble()
		%>% select(-.chain,-.iteration,-.draw)
		%>% pivot_longer(everything())
		%>% filter(
			!str_starts(name,'y')
		)
		%>% rename(true=value)
	) -> true_pars

	true_and_data = lst(n,k,iteration,true_pars,data_for_stan)
	return(true_and_data)
}
