#run imports
source('_imports.r')

#quick viz of the pipeline
tar_glimpse()
#tar_visnetwork() #less quick but more info


#make the targets as a rstudio job
(
	"targets::tar_make()"
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

#when done, gather results (could add all the below as new targets too)
a = bind_rows(tar_read(summaries))

#show diagnostics & timing
(
	a
	%>% select(-var_summary)
	%>% pivot_longer(cols=c(-model,-n,-k,-iteration))
	%>% ggplot()
	+ facet_wrap(
		~name
		, scales = 'free'
	)
	+ geom_histogram(
		aes(
			x = value
			, fill = interaction(model,n,k)
		)
		, alpha = .5 #for when there are multiple models being compared
		, position = 'identity' #default is stacked, which sucks
	)
)

#show the per-parameter diagnostics
(
	a
	%>% filter(
		num_divergent==0 # possibly questionable!!
	)
	%>% select(var_summary,model,n,k,iteration)
	%>% unnest(var_summary)
	%>% filter(
		!is.na(rhat) # some parameters are constant in the mvn model
	)
	%>% select(rank,rhat,ess_bulk,ess_tail,model,n,k,iteration)
	%>% pivot_longer(cols=c(-model,-n,-k,-iteration))
	%>% ggplot()
	+ facet_wrap(
		~name
		, scales = 'free'
	)
	+ geom_histogram(
		aes(
			x = value
			, fill = interaction(model,n,k)
		)
		, alpha = .5 #for when there are multiple models being compared
		, position = 'identity' #default is stacked, which sucks
	)
)




# define a function to compute the dECDF confidence ellipse
#   very possibly not the right thing to do! (came up with this myself)
bootstrap_decdf_ci = function(n,bootstrap_iterations=1e5,interval_perc=.9){
	rank_rank = (1:n)/n - (1/n/2)
	deltas = matrix(
		NA
		, nrow = n
		, ncol = bootstrap_iterations
	)
	for(i in 1:bootstrap_iterations){
		deltas[,i] = sort(runif(n)) - rank_rank
	}
	lo = apply(deltas,1,quantile,probs=(1-interval_perc)/2)
	hi = apply(deltas,1,quantile,probs=1-(1-interval_perc)/2)
	return(tibble(lo,hi))
}

#show the dECDF for a specific parameter
# plus the possibly-erroneous bootstrapped CI ellipse
(
	a
	%>% filter(
		num_divergent==0 # possibly questionable!!
	)
	%>% select(var_summary,model,n,k,iteration)
	%>% unnest(var_summary)
	%>% filter(
		variable=='means[1]'
		, rhat<1.01 # possibly questionable!!
	)
	%>% group_by(variable,model,n,k)
	%>% arrange(rank)
	%>% mutate(
		rank_rank = (1:n())/n() - (1/n()/2)
		, delta = rank - rank_rank
		, ci = bootstrap_decdf_ci(n())
	)
	%>% ggplot()
	+ geom_ribbon(
		aes(
			x = rank_rank
			, ymin = ci$lo
			, ymax = ci$hi
			, colour = interaction(variable,model,n,k)
		)
		, fill = 'transparent'
		, linetype = 3
	)
	+ geom_line(
		aes(
			x = rank_rank
			, y = delta
			, colour = interaction(variable,model,n,k)
		)
	)
	+ scale_x_continuous(limits=c(0,1),expand=c(0,0))
	+ theme(
		legend.position='top'
		, aspect.ratio = 1
	)
)
