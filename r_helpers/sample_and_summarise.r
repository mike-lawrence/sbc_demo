# true_and_data = targets::tar_read(true_and_data,branches=6000)[[1]]
# stan_file = 'stan_code/mvn.stan'
# sample_and_summarise(true_and_data,stan_file)
sample_and_summarise = function(true_and_data,stan_file){
	model = cmdstanr::cmdstan_model(
		stan_file
		, include_paths = './stan_code'
		, dir = './stan_temp'
	)
	csv_base = with(true_and_data,paste(n,k,iteration,sep='_'))
	sampled <- model$sample(
		data = true_and_data$data_for_stan
		, chains = parallel::detectCores()/2
		, parallel_chains = parallel::detectCores()/2
		, show_messages = F
		, refresh = 1
		, output_dir = 'stan_temp'
		, output_basename = csv_base
		, seed = abs(digest::digest2int(csv_base))
		, init = .1
	)
	# get the diagnostics-check output (will parse later)
	diagnostics = sampled$cmdstan_diagnose()$stdout #annoyingly not quiet-able
	#get ranks
	(
		sampled$draws()
		%>% posterior::as_draws_df()
		%>% as_tibble()
		%>% select(-.chain,-.iteration)
		%>% pivot_longer(-.draw)
		%>% left_join(true_and_data$true_pars,by='name')
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
		%>% bind_cols(
			tibble(
				treedepth_maxed = stringr::str_detect(diagnostics,'transitions hit the maximum')
				, ebfmi_low = stringr::str_detect(diagnostics,' is below the nominal threshold')
				, essp_low = stringr::str_detect(diagnostics,'The following parameters had fewer than')
				, rhat_high = stringr::str_detect(diagnostics,'The following parameters had split R-hat greater than')
			)
		)
	) -> posterior_summary
	#delete the csvs
	system(paste0('rm stan_temp/',csv_base,'*'))
	return(posterior_summary)
}
