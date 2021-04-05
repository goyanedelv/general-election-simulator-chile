	packages <- c("openxlsx", "dplyr", "coalitions", "jsonlite", "reshape", "doParallel", "foreach", "parallelsugar", "ggplot2", "pbapply", "yaml", "waffle", "RColorBrewer")

	install.packages(setdiff(packages, rownames(installed.packages()))) 
	
	library(openxlsx)
	library(dplyr)
	library(coalitions)
	library(jsonlite)
	library(reshape)
	library(doParallel)
	library(foreach)
	library(parallelsugar)
    library(ggplot2)
    library(pbapply)
    library(yaml)
	library(waffle)
	library(RColorBrewer)

