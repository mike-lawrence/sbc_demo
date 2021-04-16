#run imports
source('_imports.r')

#quick viz of the pipeline
tar_glimpse()
#tar_visnetwork() #less quick but more info


#make the targets as a rstudio job
(
	"targets::tar_make_clustermq(
		workers = parallel::detectCores()/2
		, reporter = 'summary'
	)"
	%>% (function(x){
		temp_file = tempfile()
		write(x,file=temp_file)
		return(temp_file)
	})
	%>% rstudioapi::jobRunScript(
		workingDir = getwd()
	)
)


#open a shiny dashboard to watch the progress
tar_watch(
	seconds = 10
	, outdated = FALSE
	, targets_only = TRUE
	, port = 59627 #just a random port
)

#when done, summarize:
# (this can actually be run during the make for intermediate results)

#get the timing info
(
	#first get the metadata
	tar_meta()
	%>% filter(
		!is.na(seconds)
		, !is.na(parent)
		# , parent=='true_and_data'
		# , str_starts(name,'out_mvn_') | str_starts(name,'out_pairwise_') | str_starts(name,'out_sufficient_')
	)
	# %>% View())
	%>% ggplot()
	+ facet_grid(
		parent ~ .
	)
	+ geom_histogram(
		aes(
			x = seconds
		)
	))
	%>% select(
		name
		, seconds
	)
	%>% separate(
		name
		, into = c('out','model','N','K','rep')
		, sep = '_'
		, fill = 'right'
	)
	# %>% View())
	%>% filter(
		!is.na(rep)
	)
	%>% mutate(
		N = as.numeric(str_replace(N,'e.','e'))
		, K = as.numeric(K)
	)
	%>% group_by(model,N,K)
	%>% summarise(
		med = median(seconds)
		, mad = mad(seconds)
		, q25 = quantile(seconds,.25)
		, q75 = quantile(seconds,.75)
		, iqr = q75-q25
		, n = n()
		, .groups = 'drop'
	)
) -> timings
print(timings)

#viz
(
	timings
	%>% mutate(
		N = paste('N:',N)
		, model = case_when(
			model=='mvn' ~ 'MVN'
			, model=='sufficient' ~ 'Pairwise'
		)
	)
	%>% ggplot()
	+facet_wrap(~N)
	+ geom_ribbon(
		mapping = aes(
			x = K
			, ymin = q25
			, ymax = q75
			, fill = model
		)
		, colour = 'transparent'
		, alpha = .5
	)
	# + scale_x_log10(
	# 	breaks = c(1e1,1e2,1e3,1e4,1e5)
	# 	, labels = c('10','100','1 000','10 000','100 000')
	# )
	+ scale_y_log10()
	+ labs(
		y = 'Seconds\n(n.b. log-scale)\n'
		, fill = 'Model'
	)
	+ theme(
		aspect.ratio = 1
		, panel.background = element_rect(
			fill = 'white'
			, colour = 'grey80'
		)
	)
)
ggsave(
	file = 'plots/time.png'
	, width = 8
	, height = 4
)



#gather summaries and summarize
(
	#get the names of summary objects
	tar_meta()
	%>% filter(
		type=='pattern'
		, !str_detect(name,'_data')
	)
	%>% pull(name)
	#read them in and parse the name
	%>% map_dfr(
		.f = function(x){(
			x
			%>% tar_read_raw()
			%>% mutate(name=x)
			%>% filter(
				variable!='lp__'
			)
		)}
	)
	# %>% View())
	%>% separate(
		name
		, into = c('out','model','N','K')
		, sep = '_'
	)
	# %>% View())
	%>% mutate(
		N = as.numeric(N)
		, K = as.numeric(K)
		, var_type = case_when(
			str_detect(variable,'mean') ~ 'mean'
			, str_detect(variable,'sd') ~ 'sd'
			, str_detect(variable,'cor') ~ 'cor'
		)
		, bias_ = case_when(
			var_type!='cor' ~ q50 - .join_data
			, T ~ atanh(q50) - atanh(.join_data)
		)
		, uncertainty_ = case_when(
			var_type!='cor' ~ q75 - q25
			, T ~ atanh(q75) - atanh(q25)
		)
		, p50_ = (q25<.join_data) & (.join_data<q75)
		, p80_ = (q10<.join_data) & (.join_data<q90)
		, temp = str_replace(variable,'pop_means','')
		, temp = str_replace(temp,'pop_sds','')
		, temp = str_replace(temp,'cor_vec','')
		, temp = str_replace(temp,fixed('['),'')
		, temp = str_replace(temp,fixed(']'),'')
		, var_subindex = as.numeric(temp)
		, is_rel = var_subindex %in% cumsum(c(K,K*2-(1:(K-1))))
		, var_type = case_when(
			(var_type=='cor') & is_rel ~ 'rel'
			, T ~ var_type
		)
	)
	%>% select(-temp,-is_rel)
	# %>% View())
	%>% group_by(model,N,K,var_type)
	%>% summarise(
		across(ends_with('_'),mean)
		, n_reps = n()
		, .groups = 'drop'
	)
	# %>% View())
) -> recovery_dat

#quick glance at the summary
summary(recovery_dat)

#viz rates:
(
	recovery_dat
	%>% select(model,N,K,var_type,p50_,p80_,n_reps)
	%>% pivot_longer(
		cols = c(p50_,p80_)
		, names_to = 'prob'
		, names_transform = list(
			prob = function(x){
				as.numeric(substr(x,2,2))/10
			}
		)
	)
	%>% mutate(
		var_type = factor(var_type,levels=c('mean','sd','cor','rel'))
		, var_type = factor(
			paste('Variable type:',var_type)
			, levels = paste('Variable type:',levels(var_type))
		)
		, N = paste('N:',N)
		, model = case_when(
			model=='mvn' ~ 'MVN'
			, model=='sufficient' ~ 'Pairwise'
		)
		, fprob = factor(prob,levels=c('0.8','0.5'))
	)
	%>% ggplot()
	# + facet_wrap(
	# 	~N*var_type
	# 	, ncol = 4
	# )
	+ facet_grid(
		N~var_type
		# , scales = 'free_y'
	)
	+ geom_rect(
		data = (
			.
			%>% group_by(prob)
			%>% summarise(
				#compute 89% interval given expected prob and n_reps
				se = qnorm(1-(1-.89)/2)*sqrt(prob[1]*(1-prob[1])/min(n_reps))
				, lo = prob-se
				, hi = prob+se
				, .groups = 'drop'
			)
		)
		, mapping = aes(
			xmin = -Inf
			, xmax = Inf
			, ymin = lo
			, ymax = hi
			, group = prob
		)
		, fill = 'grey80'
	)
	+ geom_line(
		mapping = aes(
			x = K
			, y = value
			, colour = model
			, group = interaction(model,prob)
			, linetype = fprob
		)
		, alpha = .5
	)
	+ geom_point(
		mapping = aes(
			x = K
			, y = value
			, colour = model
			, shape = fprob
		)
		, alpha = .5
	)
	+ labs(
		y = '\nRecovery rate\n'
		, x = 'K'
		, colour = 'Model'
		, linetype = 'Expected rate'
		, shape = 'Expected rate'
	)
	# + scale_y_continuous(
	# 	limits = c(0,1)
	# 	, expand = c(0,0)
	# )
	+ theme(
		aspect.ratio = 1
		, panel.background = element_rect(
			fill = 'white'
			, colour = 'grey80'
		)
	)
)
ggsave(
	file = 'plots/recovery.png'
	, width = 8
	, height = 4
)



#viz bias:
(
	recovery_dat
	%>% mutate(
		var_type = factor(var_type,levels=c('mean','sd','cor','rel'))
		, var_type = factor(
			paste('Variable type:',var_type)
			, levels = paste('Variable type:',levels(var_type))
		)
		, N = paste('N:',N)
		, model = case_when(
			model=='mvn' ~ 'MVN'
			, model=='sufficient' ~ 'Pairwise'
		)
	)
	%>% ggplot()
	# + facet_wrap(
	# 	~N*var_type
	# 	, ncol = 4
	# )
	+ facet_grid(
		N~var_type
		, scales = 'free_y'
	)
	+ geom_hline(
		yintercept = 0
		, linetype = 3
	)
	+ geom_line(
		mapping = aes(
			x = K
			, y = bias_
			, colour = model
		)
		, alpha = .5
	)
	+ geom_point(
		mapping = aes(
			x = K
			, y = bias_
			, colour = model
		)
		, alpha = .5
	)
	+ labs(
		y = 'Bias\n(n.b. scale varies by row)\n'
		, x = 'K'
		, colour = 'Model'
	)
	+ theme(
		aspect.ratio = 1
		, panel.background = element_rect(
			fill = 'white'
			, colour = 'grey80'
		)
	)
)
ggsave(
	file = 'plots/bias.png'
	, width = 8
	, height = 4
)

#viz uncertainty:
(
	recovery_dat
	%>% mutate(
		var_type = factor(var_type,levels=c('mean','sd','cor','rel'))
		, var_type = factor(
			paste('Variable type:',var_type)
			, levels = paste('Variable type:',levels(var_type))
		)
		, N = paste('N:',N)
		, model = case_when(
			model=='mvn' ~ 'MVN'
			, model=='sufficient' ~ 'Pairwise'
		)
	)
	%>% ggplot()
	# + facet_wrap(
	# 	~N*var_type
	# 	, ncol = 4
	# )
	+ facet_grid(
		N~var_type
		, scales = 'free_y'
	)
	+ geom_line(
		mapping = aes(
			x = K
			, y = uncertainty_
			, colour = model
		)
		, alpha = .5
	)
	+ geom_point(
		mapping = aes(
			x = K
			, y = uncertainty_
			, colour = model
		)
		, alpha = .5
	)
	+ labs(
		y = 'Uncertainty\n(n.b. scale varies by row)\n'
		, x = 'K'
		, colour = 'Model'
	)
	+ theme(
		aspect.ratio = 1
		, panel.background = element_rect(
			fill = 'white'
			, colour = 'grey80'
		)
	)
)
ggsave(
	file = 'plots/uncertainty.png'
	, width = 8
	, height = 4
)
