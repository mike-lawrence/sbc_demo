sample_and_summarise = function(generated,sample_stan_file){
	csv_base = paste(as.list(generated$gen_args),collapse='_')
	sample_mod = cmdstanr::cmdstan_model(
		sample_stan_file
		, include_paths = './stan_code'
		, dir = './stan_temp'
	)
	sampled = sample_mod$sample(
		data = true_and_data$data_for_stan
		, chains = parallel::detectCores()/2
		, parallel_chains = parallel::detectCores()/2
		, show_messages = F
		, refresh = 1
		, output_dir = 'stan_temp'
		, output_basename = csv_base
		, seed = abs(digest::digest2int(digest::digest(generated$data_for_stan,algo='xxhash64')))
	)
	#get ranks & other summaries
	(
		sampled$draws()
		%>% posterior::as_draws_df()
		%>% as_tibble()
		%>% select(-.chain,-.iteration)
		%>% pivot_longer(-.draw)
		%>% left_join(generated$true_pars,by='name')
		%>% filter(
			!is.na(true)
		)
		%>% rename(variable=name)
		%>% group_by(variable)
		%>% summarise(
			rank = mean(true>value)
			, true = true[1]
			, .groups = 'drop'
		)
		%>% left_join(
			(
				sampled$draws()
				%>% posterior::summarise_draws(
					posterior::default_convergence_measures()
				)
			)
			, by = 'variable'
		)
		%>% left_join(
			(
				sampled$draws()
				%>% posterior::summarise_draws(
					~posterior::quantile2(.x,probs=c(.1,.25,.5,.75,.9))
				)
			)
			, by = 'variable'
		)
	) -> posterior_summary
	(
		sampled$sampler_diagnostics()
		%>% posterior::as_draws_df()
		%>% summarise(
			max_treedepth = max(treedepth__)
			, num_divergent = sum(divergent__)
			, var_energy = var(energy__)
		)
		%>% mutate(
			time = sampled$time()$total
			, var_summary = list(posterior_summary)
			, model = basename(sample_stan_file)
		)
	) -> to_return
	#delete the csvs
	system(paste0('rm stan_temp/',csv_base,'*'))
	return(to_return)
}
