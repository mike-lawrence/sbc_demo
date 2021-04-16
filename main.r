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
a = bind_cols(tar_read(summaries))

#show diagnostics & timing
(
	a
	%>% select(-var_summary)
	%>% pivot_longer(-model)
	%>% ggplot()
	+ facet_wrap(
		~name
		, scales = 'free'
	)
	+ geom_histogram(
		aes(
			x = value
			, fill = model
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
	%>% select(model,var_summary)
	%>% unnest(var_summary)
	%>% filter(
		!is.na(rhat) # some parameters are constant in the mvn model
	)
	%>% select(model,rank,rhat,ess_bulk,ess_tail)
	%>% pivot_longer(-model)
	%>% ggplot()
	+ facet_wrap(
		~name
		, scales = 'free'
	)
	+ geom_histogram(
		aes(
			x = value
			, fill = model
		)
		, alpha = .5 #for when there are multiple models being compared
		, position = 'identity' #default is stacked, which sucks
	)
)

#show the dECDF for a specific parameter
# (need to work out how to compute/add the confidence ellipse)
(
	a
	%>% filter(
		num_divergent==0 # possibly questionable!!
	)
	%>% select(model,var_summary)
	%>% unnest(var_summary)
	%>% filter(variable=='means[1]')
	%>% filter(
		rhat<1.01 # possibly questionable!!
	)
	%>% group_by(model)
	%>% arrange(rank)
	%>% mutate(
		rank_rank = (1:n())/n() - (1/n()/2)
		, delta = rank - rank_rank
	)
	%>% ggplot()
	+ geom_point(
		aes(x=rank,y=delta,colour=model)
		, alpha = .5
	)
	+ scale_x_continuous(limits=c(0,1),expand=c(0,0))
	+ scale_y_continuous(limits=c(-.5,.5),expand=c(0,0))
	+ theme(
		legend.position='top'
		, aspect.ratio = 1
	)
)
