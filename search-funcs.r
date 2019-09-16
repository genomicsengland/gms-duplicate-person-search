# strcmp = does comparison on similarity of strings, without it just does straightforward - 0/1/ for whether same or not.
# blockfld = list of column combinations where identical entries between individuals suggests a pairing that should be pursued - only looking for pairings in this subset of participants
# exclude = don't include these columns in the matching
# identity = vector of true identities of people in dataset, only relevant if you've got a traiing set sort of situation   
# frequencies in compare result gives the average frequency of values (i.e. the average frequency of the different names in name column etc.)
# emWeights on the compare.dedup result runs EM algorithm on and gives extra data ultimately, gives the weights calculating during EM.
# can then use emClassify to classify the results into N - non-link (below threshold), P - possible link (between two thresholds), L - link (above threshold)

library(RecordLinkage)
library(tictoc)

rl_dedup <- function(d, id = NA, bf = NA, excl = FALSE, thold){
	tic()
	cat('Running compare.dedup:\n')
	cat(paste('    Nrows:', nrow(d), '\n'))
	cat(paste('    Threshold:', paste(thold, collapse = '-'), '\n'))
	cat(paste('    Block fields:',
			  paste(
			  		sapply(bf, function(x) paste(x, collapse = '/')),
			  		collapse = ' - '
					), '\n\n`'))	
	#-- do the comparison
	rp <- compare.dedup(d,
						blockfld = bf,
						strcmp = TRUE,
						identity = id,
						exclude = excl)
	cat('Running emWeight')
	#-- get emWeights
	rpem <- emWeights(rp)
	cat('Running emClassify\n')
	#-- do classification using given thresholds (either N,P.L or N,L)
	if(length(thold) == 1){
		res <- emClassify(rpem, thold)
	} else {
		res <- emClassify(rpem, thold[1], thold[2])
	}
	toc()
	return(res)
}

generate_link_participant_table <- function(d, participant_detail_cols){
	#-- pull out those with linkages
	of_interest <- which(as.character(d$prediction) %in% c('P', 'L'))
	links <- d$pairs[of_interest, c('id1', 'id2')]
	links$link_type <- d$prediction[of_interest]
	links$link_strength <- d$Wdata[of_interest]
	#-- merge in first participant data
	out <- merge(links, d$data[,participant_detail_cols], by.x = 'id1', by.y = "row.names", all.x = T)
	#-- merge in linked participant data
	out <- merge(out, d$data[, participant_detail_cols], by.x = 'id2', by.y = "row.names", all.x = T, suffixes = c('.a', '.b'))
	return(out)
}
