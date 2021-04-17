compile_stan_file = function(stan_file){
	mod = cmdstanr::cmdstan_model(
		stan_file
		, include_paths = './stan_code'
		, dir = './stan_temp'
	)
	return(mod)
}

generate_true_and_data = function(n,k,iteration,stan_mod_for_generating){
	gen_args = tibble(n,k,iteration)
	data_for_stan = lst(n,k)
	gq = stan_mod_for_generating$generate_quantities(
		data = data_for_stan
		, fitted_params = posterior::as_draws_array(tibble(dummy_=0))
		, seed = abs(digest::digest2int(digest::digest(gen_args,algo='xxhash64')))
	)
	#add generated y to data (probably a more straight-forward way to do this!)
	(
		gq$draws('y')
		%>% posterior::as_draws_rvars()
		%>% (function(y){
			posterior::draws_of(y$y)[1,,]
		})
	) -> data_for_stan$y
	#get parameters
	(
		gq$draws()
		%>% posterior::as_draws_df()
		%>% as_tibble()
		%>% select(-.chain,-.iteration,-.draw)
		%>% pivot_longer(everything())
		%>% filter(
			!str_starts(name,'y')
		)
		%>% rename(true=value)
	) -> true_pars
	#bundle for output
	lst(
		gen_args
		, true_pars
		, data_for_stan
	) -> generated
	return(generated)
}
