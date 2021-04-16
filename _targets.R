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
		, command = {1:1e3}
	)

	, tar_target(
		name = n
		, command = {c(1e1,1e2)}
	)

	, tar_target(
		name = k
		, command = {c(2,5,15)}
	)

	, tar_target(
		name = generated
		, command = generate_true_and_data(n,k,iteration,'stan_code/mvn_generate.stan')
		, pattern = cross(n,k,iteration)
	)

	, tar_target(
		name = summaries
		, command = sample_and_summarise(generated,'stan_code/mvn.stan')
		, pattern = map(generated)
		, deployment = 'main'
	)

)
