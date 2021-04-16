generate_true_and_data = function(n,k,iteration,generate_stan_file){
	gen_args = tibble(n,k,iteration,generate_stan_file)
	data_for_stan = lst(n,k)
	generate_mod = cmdstanr::cmdstan_model(
		stan_file
		, include_paths = './stan_code'
		, dir = './stan_temp'
	)
	gq = generate_mod$generate_quantities(
		data = data_for_stan
		, fitted_params = posterior::as_draws_array(tibble(dummy_=0))
		, seed = abs(digest::digest2int(digest::digest(gen_args,algo='xxhash64')))
	)
	#add generated y to data (probably a more straight-forward way to do this!)
	data_for_stan$y = (
		gq$draws('y')
		%>% posterior::as_draws_rvars()
		%>% (function(y){
			posterior::draws_of(y$y)[1,,]
		})
	)
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
