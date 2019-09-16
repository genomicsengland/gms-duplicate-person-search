# some testing of participant matching process in 100k data
rm(list = objects())
options(stringsAsFactors = FALSE,
	scipen = 200)
library(wrangleR)
library(tidyverse)
library(RPostgreSQL)
library(rmarkdown)

#-- connect to MIS database
drv <- dbDriver("PostgreSQL")
p <- getprofile(c("mis_con", "indx_con"))
mis_con <- dbConnect(drv,
             dbname = "gel_mi",
             host     = p$mis_con$host,
             port     = p$mis_con$port,
             user     = p$mis_con$user,
             password = p$mis_con$password)
indx_con <- dbConnect(drv,
             dbname = "metrics",
             host     = p$indx_con$host,
             port     = p$indx_con$port,
             user     = p$indx_con$user,
             password = p$indx_con$password)


#-- get participant data
d <- dbGetQuery(mis_con, "
select upper(forenames) as fname
	,upper(surname) as lname
	,extract(year from date_of_birth) as by
	,extract(month from date_of_birth) as bm
	,extract(day from date_of_birth) as bd
	,nhs_number
	,participant_id 
from cdm.vw_participant_level_data
;")

#-- get the duplicate participant list
#-- person_id 16 does not seem to exist any longer
dup_p <- dbGetQuery(indx_con, "
select *
from dict.duplicate_participants
where person_id != 16
;")

#-- create the identity vector
ident <- seq(from = 1000, length.out = nrow(d))
#-- replace the values in ident with person_id from duplicate participants
ident[match(dup_p$participant_id, d$participant_id)] <- dup_p$person_id

source('search-funcs.r')
id_vec = ident
bf_list = list(c('by', 'bm'), c('bm', 'bd'), c('by', 'bd'))
excl_list = c('participant_id') 
threshold = 10
participant_detail_cols = colnames(d) 
export = T

render('duplicate-search-analysis.rmd', output_file = 'test.html')

