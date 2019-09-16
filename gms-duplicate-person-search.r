#-- try to identify cases of duplicate persons in GMS database
rm(list = objects())
options(stringsAsFactors = FALSE,
	scipen = 200)
library(wrangleR)
library(tidyverse)
library(RPostgreSQL)
library(rmarkdown)

#-- scenarios
#-- 1. two rows, same person, one has nhs number other doesn't
#-- 2. two rows, same person, neither has nhs number
#-- 3. two rows, same person, different nhs numbers

##-- connect to GR database
drv <- dbDriver("PostgreSQL")
p <- getprofile("local_postgres_con")
con <- dbConnect(drv,
             dbname = "name_dedup_test",
             host     = p$host,
             port     = p$port,
             user     = p$user,
             password = p$password)

##-- get all people from the GR database
#persons <- dbGetQuery(con,"
#select person.uid as person_uid
#	,upper(person_first_name) as first_name
#	,upper(person_middle_name) as middle_name
#	,upper(person_family_name) as family_name
#from public.person
#;")
#
##-- get all the NHS or CHI numbers
#chi_nhs_number_concept_cid <- dbGetQuery(con,"
#select uid
#from public.concept
#where concept_code in ('chinumber', 'nhsnumber') and codesystem_uri in ('identifier_type')
#;")$uid
#nhs_numbers <- dbGetQuery(con, paste0("
#select patient_uid
#	,value as nhs_num
#from public.identifier
#where type_cid in ('",
#paste(chi_nhs_number_concept_cid, collapse = "','"),
#"');", collapse = '')
#)
#
##-- get all the dobs
#dobs <- dbGetQuery(con, "
#select uid as patient_uid
#	,person_uid
#	,patient_date_of_birth as dob
#from public.patient
#;")
#
##-- check for duplicate dob records in dobs tables
##-- dobs$duplicate <- duplicated(dobs$person_uid) | duplicated(dobs$person_uid, fromLast = TRUE)
#
##-- merge all the records together, search is being done at patient level so happy with having duplicated persons in end data
#d <- merge(persons, dobs, by = 'person_uid')
#d <- merge(d, nhs_numbers, by = 'patient_uid')
#
##-- process names to increase chances of a match
##-- concatenate fore, middle and surname
##-- remove non-letter characters
#d$name <- gsub('[^A-Z]', '', paste0(d$first_name, d$middle_name, d$family_name))

source('search-funcs.r')
d <- dbGetQuery(con, "select * from public.people;")
id_vec = NA
bf_list = list(c('by', 'bm'), c('bm', 'bd'), c('by', 'bd'))
excl_list = c('match_in_dataset', 'nhs_number', 'pid')
threshold = 10
participant_detail_cols = colnames(d) 
export = T

render('duplicate-search-analysis.rmd', output_file = 'test.html')




