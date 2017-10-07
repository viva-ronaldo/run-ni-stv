#Sketch out a non-ballot-based STV function
library(stringr)
library(dplyr)

#Currently not getting any benefit from historical local transfers
#  e.g. 2016w11 (tuned) miss on EA,UB (stdev=0)
#  EA is overestimating UUP vs SF - nw FPs would say +2.3% UUP - NO
#  UB is overestimating SDLP vs SF - nw FPs would say +0.7% SDLP - NO
#  So adjusting transfers received by nw FPs doesn't seem to be the answer.
#    And adjusting by change in local FPs doesn't seem to work either.
#    From nw all, sum(received) shows SDLP, UUP, DUP, UKIP were up in 2016, SF, Ind, All down.

#TODO: try more accurate comb method
#TODO: improve Ind
#  - transfer to In as uni/nat as well as from
#  - maybe adjust transfer to based on FPs or scale down from full uni/nat?
#TODO: could try to be more accurate on multi-cand party splits
#TODO: check whole algorithm


find_eliminated <- function(votes) {
  minv <- min(ifelse(votes==0,NA,votes),na.rm=T)
  minInds <- which(votes == minv)
  if (length(minInds) == 1) {
    return(minInds)
  } else {
    return(sample(minInds,1))
  }
}

correct_votes_for_multiple_parties <- function(cands,parties,adjusts,votes_transferred) {
  partyCounts <- table(parties)
  maxPartyCounts <- max(partyCounts)
  if (maxPartyCounts >= 2) {
    for (m in 2:maxPartyCounts) {
      multiples <- names(partyCounts)[partyCounts==m]
      for (p in multiples) {
        sum_adjusts_left <- sum(adjusts[adjusts$cand %in% cands[parties == p],'adjust'])
        #cat(p,as.integer(partyCounts[p]),'\n')
        #votes_transferred[parties == p] <- votes_transferred[parties == p] / as.integer(partyCounts[p])
        
        #at same time adjust for unequal share among candidates from that party
        for (i in which(parties == p)) {
          votes_transferred[i] <- votes_transferred[i] * 
            adjusts[adjusts$cand == cands[i],'adjust'] / sum_adjusts_left
        }          
      }
    }
  }
  #print(votes_transferred)
  votes_transferred
}

perturb_tra_grid <- function(grid,stdev) {
  pert_grid <- grid
  for (i in seq(nrow(grid))) {
    pert_grid[i,] <- grid[i,] * pmax(rnorm(ncol(grid),mean=1,sd=stdev), 0.01)
    pert_grid[i,] <- pert_grid[i,] / sum(pert_grid[i,])
  }
  pert_grid
}

adjust_independent_grids <- function(firstPrefs,constitName,year,transfersSelf,transfersRest) {
  new_unionist_pc <- getUnionistPercent(firstPrefs)
  new_nationalist_pc <- getNationalistPercent(firstPrefs)
  new_ind_pc <- getIndependentPercent(firstPrefs)
  prev_year <- list('2017'=2016,'2016'=2011,'2011'=2007)[[as.character(year)]]
  prev_resultsFile <- paste0('fp',prev_year,'/alt/',
                             str_replace_all(tolower(constitName),' ',''),prev_year,'alt.csv')
  prev_first_prefs <- getFirstPrefs(prev_resultsFile)
  prev_unionist_pc <- getUnionistPercent(prev_first_prefs)
  prev_nationalist_pc <- getNationalistPercent(prev_first_prefs)
  prev_ind_pc <- getIndependentPercent(prev_first_prefs)
  cat('Change in unionist pc: ',new_unionist_pc-prev_unionist_pc,'\n')
  cat('Change in nationalist pc: ',new_nationalist_pc-prev_nationalist_pc,'\n')
  cat('Change in independent pc: ',new_ind_pc-prev_ind_pc,'\n')
  uni_down <- FALSE
  nat_down <- FALSE
  if ((new_unionist_pc-prev_unionist_pc) < -2 & (new_nationalist_pc-prev_nationalist_pc) < -2) {
    if ((new_unionist_pc-prev_unionist_pc) < (new_nationalist_pc-prev_nationalist_pc)) {
      uni_down <- TRUE
    } else { nat_down <- TRUE }
  } else if ((new_unionist_pc-prev_unionist_pc) < -2) { uni_down <- TRUE 
  } else if ((new_nationalist_pc-prev_nationalist_pc) < -2) { nat_down <- TRUE }
  ind_up <- (new_ind_pc - prev_ind_pc > 2)
  if (uni_down & ind_up) {
    transfersSelf['Independent',] <- transfersSelf['Ulster Unionist Party',]
    transfersSelf['Independent','Ulster Unionist Party'] <- transfersSelf['Democratic Unionist Party','Ulster Unionist Party']
    transfersRest['Independent',] <- transfersRest['Ulster Unionist Party',]
    transfersRest['Independent','Ulster Unionist Party'] <- transfersRest['Democratic Unionist Party','Ulster Unionist Party']
    cat('Treating Ind as unionist\n')
  } else if (nat_down & ind_up) {
    transfersSelf['Independent',] <- transfersSelf['Sinn Fein',]
    transfersSelf['Independent','Sinn Fein'] <- transfersSelf['Social Democratic and Labour Party','Sinn Fein']
    transfersRest['Independent',] <- transfersRest['Sinn Fein',]
    transfersRest['Independent','Sinn Fein'] <- transfersRest['Social Democratic and Labour Party','Sinn Fein']
    cat('Treating Ind as nationalist\n')
  }
  return(list(self=transfersSelf,rest=transfersRest))
}

#---
noBallot_runConstit <- function(constitName,year,transfersSelf,transfersRest) {
  infer_independent <- ifelse(year == 2007, FALSE, TRUE)
  
  resultsFile <- paste0('fp',year,'/alt/',str_replace_all(tolower(constitName),' ',''),year,'alt.csv')
  firstPrefs <- getFirstPrefs(resultsFile)
  
  parties <- unique(as.character(sapply(firstPrefs$cand, function(p) partyMap[as.character(p)])))
  firstPrefs$adjust <- 1
  for (party in parties) {
    firstPrefs$adjust[grepl(party,firstPrefs$cand)] <- firstPrefs[grepl(party,firstPrefs$cand),]$percent / 
      mean(firstPrefs[grepl(party,firstPrefs$cand),]$percent)
  }
  
  cands <- as.character(firstPrefs$cand)
  parties <- as.character(sapply(cands, function(p) partyMap[as.character(p)]))
  transfersSelf <- transfersSelf[unique(parties),]
  transfersRest <- transfersRest[unique(parties),]
  votes <- firstPrefs$number
  nSeats <- ifelse(year >= 2017, 5, 6)
  res3 <- data.frame()
  
  if (infer_independent) {
    returnstuff <- adjust_independent_grids(firstPrefs,constitName,year,transfersSelf,transfersRest)
    transfersSelf <- returnstuff[[1]]
    transfersRest <- returnstuff[[2]]
  }
  
  traGrid <- transfersSelf
  quota <- floor(sum(votes)/(nSeats+1) + 1)
  stillIn <- rep(TRUE,length(parties))
  elected <- c()
  
  for (i in seq(20)) {
    free_seats <- nSeats - length(elected)
    assertthat::are_equal(free_seats+length(elected),nSeats)
    assertthat::assert_that(sum(votes[!stillIn]) == 0)
    
    cat(sprintf('Round %i: %i seats to fill, %i parties still in\n',i,free_seats,sum(stillIn)))
    print(sum(votes) + quota*length(elected))
    for (i in seq(length(votes[stillIn]))) {
      cat(sprintf('%37s %g\n',as.character(cands[stillIn][order(-votes[stillIn])][i]),
                  votes[stillIn][order(-votes[stillIn])][i]))
    }
    above_quota <- votes >= quota
    if (sum(above_quota) > 0) {
      surpluses <- pmax(votes - quota, 0)
      stillIn[above_quota] <- FALSE
      for (i in seq(length(cands))[above_quota]) {
        cat(as.character(cands[i]),'elected\n')
        elected <- c(elected,cands[i])
        transferrer <- parties[i]
        
        reduced_traGrid <- traGrid[,c(unique(parties[stillIn]),'votes_lost')]
        row_totals <- apply(reduced_traGrid, 1, sum)
        reduced_traGrid <- reduced_traGrid / row_totals
        print(reduced_traGrid[transferrer,])
        
        votes_transferred <- unlist(sapply(parties[stillIn], function(p) reduced_traGrid[transferrer,p]))
        print(sum(votes_transferred))
        #print(table(parties[stillIn]))
        votes_transferred <- correct_votes_for_multiple_parties(cands[stillIn],parties[stillIn],
                                                                firstPrefs[,c('cand','adjust')],
                                                                votes_transferred)
        print(sum(votes_transferred))
        
        votes[i] <- 0
        votes[stillIn] <- votes[stillIn] + surpluses[i]*as.numeric(votes_transferred)
        free_seats <- free_seats - 1
      }
    } else if (sum(stillIn) == (free_seats+1)) {
      print('Everyone except bottom one is in')
      eliminated <- find_eliminated(votes)  #which.min(ifelse(votes==0,NA,votes))
      stillIn[eliminated] <- FALSE
      elected <- c(elected,cands[stillIn])
      free_seats <- 0
    } else {
      #eliminate someone
      eliminated <- find_eliminated(votes)
      stillIn[eliminated] <- FALSE
      surplus <- votes[eliminated]
      cat(sprintf('%s eliminated; transferring %g votes\n',cands[eliminated],surplus))
      votes[eliminated] <- 0
      
      reduced_traGrid <- traGrid[,c(unique(parties[stillIn]),'votes_lost')]
      row_totals <- apply(reduced_traGrid, 1, sum)
      reduced_traGrid <- reduced_traGrid / row_totals
      print(reduced_traGrid[parties[eliminated],])
      
      votes_transferred <- unlist(sapply(parties[stillIn], 
                                         function(p) reduced_traGrid[parties[eliminated],p]))
      print((votes_transferred))
      votes_transferred <- correct_votes_for_multiple_parties(cands[stillIn],parties[stillIn],
                                                              firstPrefs[,c('cand','adjust')],
                                                              votes_transferred)
      print((votes_transferred))
      
      votes[stillIn] <- votes[stillIn] + surplus*as.numeric(votes_transferred)
    }
    if (free_seats == 0) break
  }
  by_party <- table(as.character(sapply(elected,function(p) 
    party_short_names[as.character(partyMap[as.character(p)])])))
  by_party <- arrange(data.frame(by_party),-Freq)
  paste(paste(by_party$Freq,by_party$Var1),collapse=', ')
}
  
noBallot_runConstitEnsemble <- function(constitName, year, transfersSelf, transfersRest,
                                        nSample, stdev) {
  resE <- data.frame()
  for (e in seq(nSample)) {
    capture.output(resE <- rbind(resE, data.frame(result = noBallot_runConstit(constitName,year,
                                                           perturb_tra_grid(transfersSelf,stdev),
                                                           transfersRest))))
  }
  names(resE) <- c('result')
  table_results <- table(resE$result)/nrow(resE)
  arrange(data.frame(result=names(table_results),prob=as.numeric(table_results)),-prob)
}

noBallot_runAllConstits <- function(constituencies, year, transfersSelf, transfersRest,
                                    localTransfersAll, alpha,
                                    nSample, stdev,
                                    rough_combine = TRUE) {
  total_correct <- 0
  sum_briers <- 0
  for (constit in constituencies) {
    constit_lower <- str_replace_all(tolower(constit),' ','')
    constit_lowerdash <- str_replace_all(tolower(constit),' ','-')
    resultsFile <- paste0('fp',year,'/alt/',constit_lower,year,'alt.csv')
    firstPrefs <- getFirstPrefs(resultsFile)
    capture.output(res <- noBallot_runConstitEnsemble(constit,year,
                                                      combine_transfer_grids(transfersSelf,
                                                                             localTransfersAll[[constit_lowerdash]],
                                                                             alpha),
                                                      combine_transfer_grids(transfersRest,
                                                                             localTransfersAll[[constit_lowerdash]],
                                                                             alpha),
                                                      nSample,stdev))
    brier <- get_brier_for_constit(res,constit,year)
    sum_briers <- sum_briers + brier
    right <- brier <= 0.04
    total_correct <- total_correct + right
    cat(sprintf('%s: %s; %.0f%% %s\n',constit,as.character(res$result[1]),
                as.numeric(res$prob[1])*100,
                ifelse(right,'','----WRONG----')))
  }
  sum_briers <- sum_briers / length(constituencies)
  cat(sprintf('\n%i/18 right (>80%%)\n',total_correct))
  return(list(num_right=total_correct,brier=sum_briers))
}

#With equal division of multi-party and infer_independent:
# - 16/18 right 2016w16 with all nw
#   - miss UB; correct with comb at 0.6 or below
#   - miss SA; correct result with comb at 0.9, correct seq at 0.6 or below
# - 14/18 right 2011w11 with all nw
#   - miss BN; correct with comb 0.9 or below
#   - miss UB; correct with comb 0.8 or below
#   - miss NwA; correct with comb 0.6 or below
#   - miss ELD; correct with comb 0.9 or below
# - 13/18 right 2007w11 with all nw
#   - miss BE (40% with stdev=0.5), comb doesn't fix
#   - miss Str (40% with stdev=0.5), comb doesn't fix
#   - miss UB (30% with stdev=0.5), correct with comb 0.8 or below
#   - miss ND (60% with stdev=0.2), correct with comb 0.9 or below
#   - miss WT (would be helped by adjusting Ind transfers based on FPs or hist local transfer fracs); comb doesn't fix

# - 16/18 right 2016w11 with all nw
#   - miss UB; not fixed by local comb
#   - miss EA; just fixed by local comb 0.1

#TODO try proper comb of local

tune_stdev <- function(constituencies, year, transfersSelf, transfersRest,
                       localTransfersAll, alpha,
                       stdevs) {
  res_all <- data.frame()
  for (stdev in stdevs) {
    res <- noBallot_runAllConstits(constituencies, year, transfersSelf, transfersRest,
                                   localTransfersAll, alpha,
                                   50, stdev)
    res_all <- rbind(res_all, data.frame(stdev=stdev,num_right=res$num_right,brier=res$brier))
  }
  res_all
}
#Min Brier for stdev =
#2016w16: 0.2 (0.069)
#2011w11: 0.4+ (0.174!)
#2007w07:
#2016w11: 0/0.05 (0.111)
#2011w07: 0.4+ (0.323)

tune_alpha_and_stdev <- function(constituencies, year, transfersSelf, transfersRest,
                       localTransfersAll, alphas) {
  res_all <- data.frame()
  for (alpha in alphas) {
    res <- tune_stdev(constituencies, year, transfersSelf, transfersRest,
                      localTransfersAll, alpha, c(0,0.1,0.2,0.3,0.4,0.5,0.6))
    res_all <- rbind(res_all, data.frame(res,alpha=alpha))
  }
  res_all
}

#Rough comb:
#2016w16: stdev 0.1, alpha=0.5 or less (<0.01)
#         stdev 0.2, alpha=0.2 (0.014)
tunecomb2016w16 <- tune_alpha_and_stdev(constituencies, 2016, fullTra2016All, fullTra2016Rest, 
                                        localTransfersAll2016, c(1,0.9,0.7,0.5,0.2,0.1))
#brier=0 for stdev=0, alpha=0.2,0.5,0.7; next best stdev=0.1, alpha=0.2,0.5,0.7.
tunecomb2011w11 <- tune_alpha_and_stdev(constituencies, 2011, fullTra2011All, fullTra2011Rest, 
                                        localTransfersAll2011, c(1,0.9,0.7,0.5,0.2,0.1))
#brier=0 for stdev=0, alpha=0.5; best are stdev=0-0.2, alpha=0.1-0.5
tunecomb2007w07 <- tune_alpha_and_stdev(constituencies, 2007, fullTra2007All, fullTra2007Rest, 
                                        localTransfersAll2007, c(1,0.9,0.7,0.5,0.2,0.1))
#brier=0.100 for stdev=0.4, alpha=0.2; best are stdev>=0.3, alpha<=0.5.
#So unfortunately 'difficulty' of each year is quite different. Probably have to
#  lean towards larger stdev, even though this makes 2011, 2016 less accurate than they could be.

tunecomb2016w11 <- tune_alpha_and_stdev(constituencies, 2016, fullTra2011All, fullTra2011Rest, 
                                        localTransfersAll2011, c(1,0.9,0.7,0.5,0.2,0.1))
#brier=0.111 for stdev=0, alpha=0.9; best for alpha>=0.9.
tunecomb2011w07 <- tune_alpha_and_stdev(constituencies, 2011, fullTra2007All, fullTra2007Rest, 
                                        localTransfersAll2007, c(1,0.9,0.7,0.5,0.2,0.1))
#brier=0.295 for stdev=0.6, alpha=1.0; best for stdev>=0.4,alpha>=0.5.

