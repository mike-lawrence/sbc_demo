source('_imports.r')
source('r_helpers/generate_true_and_data.r')
source('r_helpers/sample_and_summarise.r')
options(tidyverse.quiet = TRUE)
library(targets)

tar_option_set(
	iteration = 'list'
	#, packages = required_packages
	, format = 'qs'
	, resources = list(
		preset = 'fast'
	)
	, error = 'workspace'
	# , error = 'continue'
	, memory = 'transient'
)

list(
	tar_target(
		name = iteration
		, command = {1:1e3} #probably want this to be 1:1e3 for real work
	)

	, tar_target(
		name = n
		, command = {c(1e1,1e2)}
	)

	, tar_target(
		name = k
		, command = {c(2)}
	)

	, tar_target(
		name = stan_file_for_generating
		, command = {'stan_code/mvn_generate.stan'}
		, format = 'file'
	)
	, tar_target(
		name = stan_mod_for_generating
		, command = compile_stan_file(stan_file_for_generating)
	)
	, tar_target(
		name = generated
		, command = generate_true_and_data(n,k,iteration,stan_mod_for_generating)
		, pattern = cross(n,k,iteration)
	)

	, tar_target(
		name = stan_file_for_sampling_name
		, command = {c('stan_code/mvn.stan','stan_code/sufficient.stan')}
	)
	, tar_target(
		name = stan_file_for_sampling
		, command = identity(stan_file_for_sampling_name)
		, format = 'file'
		, pattern = map(stan_file_for_sampling_name)
	)
	, tar_target(
		name = stan_mod_for_sampling
		, command = compile_stan_file(stan_file_for_sampling)
		, pattern = map(stan_file_for_sampling)
	)
	, tar_target(
		name = summaries
		, command = sample_and_summarise(generated,stan_mod_for_sampling)
		, pattern = cross(generated,stan_mod_for_sampling)
		, deployment = 'main' #typically want this unparallel bc cmdstanr will do chains in parallel itself
	)

)
