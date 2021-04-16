options(warn=1) # really should be default in R

# specify the packages used:
required_packages = c(
	'import' # for explicit imports
	, 'github.com/ropensci/targets'
	, 'github.com/ropensci/tarchetypes'
	, 'github.com/stan-dev/posterior'
	, 'github.com/stan-dev/cmdstanr'
	, 'digest' # for digest2int
	, 'tidyverse' # for all that is good and holy
)

# load the helper functions:
source('r_helpers/install_if_missing.r') # defines install_if_missing()

# install any required packages not already present
install_if_missing(required_packages)

# load the tidyverse (instead of imports bc lazy)
library(tidyverse)

#non-tidyverse imports
import::from(parallel,detectCores) # part of base, hence not in `required_packages`
import::from(digest,digest,digest2int)
import::from(
	targets
	, tar_option_set
	, tar_glimpse
	, tar_visnetwork
	, tar_watch
	, tar_read_raw
	, tar_meta
	, tar_destroy
	, tar_target
	, tar_make
)
import::from(
	tarchetypes
	, tar_map
)
