## R state:
library(RPostgreSQL)
library(lubridate)
library(integrator)
library(parallel)
library(reshape2)
library(ggplot2)
#library(cruftery)
options(stringsAsFactors=FALSE)
options(check.names=FALSE)

options(mc.cores=1)
# Shared data:

shared_data <- local(expr={
  
  date.format='mdy' #input for 'orders' in the function parse_date_time that matches input csv date format 
  
  #link <- db_connector("~/credentials/pgsql-pass-salmonids-local-db.rds")
	link <- db_connector("~/wb_credentials.rds") 

	root_data_dir <- '~/process-data/data_store'
	original_data_dir <- file.path(root_data_dir,'original_data')
	adjusted_data_dir <- file.path(root_data_dir,'adjusted_data')
	processed_data_dir <- file.path(root_data_dir,'processed_data')
	
  
	tag_data_names <- c(
		"tags_antenna", "tags_dead", 
		"tags_salmon_wb", "tags_trout_wb", "tags_tribs_wb"
	)

	csv_files <- paste(file.path(adjusted_data_dir, tag_data_names), '.csv', sep='')
	names(csv_files) <- tag_data_names
	standardize_files <- paste(file.path(adjusted_data_dir, tag_data_names), '_standardize.R', sep='')
	names(standardize_files) <- tag_data_names


	return(environment(NULL)) 
})




