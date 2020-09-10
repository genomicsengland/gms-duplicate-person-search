#-- some testing of participant matching process in 100k data
rm(list = objects())
options(stringsAsFactors = FALSE,
	scipen = 200)
library(wrangleR)
library(tidyverse)
library(rmarkdown)
library(RPostgreSQL)

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
	,participant_stated_gender as gender
from cdm.vw_participant_level_data
where upper(surname) != 'KEWAY' and upper(forenames) != 'JEN'
;")
d$dob <- paste0(d$by, '-', sprintf('%02d', d$bm), '-', sprintf('%02d', d$bd))

#-- get the duplicate participant list
#-- person_id 16 does not seem to exist any longer
dup_p <- dbGetQuery(indx_con, "
select *
from dict.duplicate_participants
;")
dup_p <- dup_p[dup_p$participant_id %in% d$participant_id,]

#-- create the identity vector
ident <- seq(from = 1000, length.out = nrow(d))
#-- replace the values in ident with person_id from duplicate participants
ident[match(dup_p$participant_id, d$participant_id)] <- dup_p$person_id

#-- do the record linkage
source('search-funcs.r')
id_vec = ident
excl_list = c('participant_id', 'nhs_number', 'dob', 'gender') 
threshold = c(15, 10)
participant_detail_cols = colnames(d) 

#-- do the deduplication and populate with participant details
e <- bind_rows(
			   generate_link_participant_table(rl_dedup(d, id = id_vec, bf= list(c('by', 'bm', 'bd')), excl = excl_list, thold = threshold), colnames(d)),
			   generate_link_participant_table(rl_dedup(d, id = id_vec, bf= list(c('lname', 'by', 'bm')), excl = c(excl_list, 'bd'), thold = threshold), colnames(d)),
			   generate_link_participant_table(rl_dedup(d, id = id_vec, bf= list(c('lname', 'by', 'bd')), excl = c(excl_list, 'bm'), thold = threshold), colnames(d))
			   )

#-- do some deduplication and merging
f <- e %>% group_by(id1, id2) %>% summarise(similarity_score = sum(link_strength))
pd <- unique(e[,c('id1', 'id2',  'is_match', 'fname.a', 'lname.a', 'dob.a', 'nhs_number.a',  'participant_id.a', 'fname.b', 'lname.b', 'dob.b', 'nhs_number.b', 'participant_id.b')])
g <- merge(f, pd, by = c('id1', 'id2'), all = T)
g$known_duplicate <- as.logical(g$is_match)
g <- g[order(g$similarity_score, decreasing = T),!colnames(g) %in% c('is_match')]

#-- make a separate dataframe of those with same NHS numbers
known_dummy_nhs_numbers <- c('0000000000', '2222222222', '3333333333', '4444444444')
h <- merge(d[!is.na(d$nhs_number),], d[!is.na(d$nhs_number),], by = 'nhs_number', suffixes = c('.a', '.b'))
h <- h[h$participant_id.a != h$participant_id.b & !h$nhs_number %in% known_dummy_nhs_numbers,]
h <- h[!duplicated(h$nhs_number), !colnames(h) %in% c('by.a', 'bm.a', 'bd.a', 'by.b', 'bm.b', 'bd.b')]
known_dups <- merge(dup_p, dup_p, by = 'person_id', suffixes = c('.a', '.b'))
known_dups$known_duplicate <- TRUE
h <- merge(h, known_dups[,!colnames(known_dups) %in% c('person_id')], by = c('participant_id.a', 'participant_id.b'), all.x = T)
h$known_duplicate[is.na(h$known_duplicate)] <- FALSE

#-- output to Excel spreadsheet for output
writeXlsx <- function(d_a, d_b, fn, sheet_names = c('Fuzzy Matches', 'Duplicate NHS Numbers')){
	require(openxlsx)
	# creates xlsx workbook containing data
	# header styling
	hs1 <- createStyle(fgFill = "#4F81BD", halign = "LEFT", textDecoration = "Bold", border = "Bottom", fontColour = "white")
	cs1 <- createStyle(wrapText = TRUE, halign = 'LEFT', valign = 'top')
	wb <- createWorkbook() 
	addWorksheet(wb, sheetName=sheet_names[1])
	addWorksheet(wb, sheetName=sheet_names[2])
	setColWidths(wb, 1, 1:ncol(d_a), 'auto')
	setColWidths(wb, 2, 1:ncol(d_b), 'auto')
	freezePane(wb, 1, firstRow = TRUE)
	freezePane(wb, 2, firstRow = TRUE)
	writeData(wb, 1, d_a, headerStyle = hs1)
	writeData(wb, 2, d_b, headerStyle = hs1)
	addStyle(wb, 1, style = cs1, rows = 2:(nrow(d_a) + 1), cols = 1:ncol(d_a), gridExpand = TRUE)
	addStyle(wb, 2, style = cs1, rows = 2:(nrow(d_b) + 1), cols = 1:ncol(d_b), gridExpand = TRUE)
	saveWorkbook(wb, fn, overwrite = TRUE)
}
writeXlsx(g, h, 'duplicate_people.xlsx')

#-- disconnect all the dbs
dbdisconnectall()

