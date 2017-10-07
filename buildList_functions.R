#The better function, with separate self and rest grids and a default
#  of using chain transfers. Slow to run.
buildList <- function(vote, candidates, adjusts, transfersSelf, transfersRest,
                      use_party_1_for_transfers=FALSE) {
  #TODO: convert votes to partyMap versions before passing in here  
  #TODO: might be able to combine grids if rest grid probs are just self grid
  #  ones scaled up - which they probably should be
  
  #TODO: check working correctly after changes
  
  num_candidates <- length(candidates)
  selfs_that_we_have <- rownames(transfersSelf)
  
  stillToUse <- setdiff(candidates,vote)
  stillToUse <- union(stillToUse,'votes_lost')
  stillToUseParties <- as.character(sapply(stillToUse,function(p) partyMap[[p]]))
  
  while (length(vote) < num_candidates) {
    
    if (use_party_1_for_transfers) {
      transferring_party <- partyMap[[vote[1]]]
    } else {
      transferring_party <- partyMap[[vote[length(vote)]]]  
    }
    
    #An alternative is to always transfer from vote[1]
    
    if (transferring_party %in% stillToUseParties) {
      if (transferring_party %in% selfs_that_we_have) {
        #probs <- unlist(sapply(stillToUseParties, 
        #                       function(p) { transfersSelf[transferring_party,p] }))
        probs <- unlist(transfersSelf[transferring_party,stillToUseParties])
        #renames parties but OK as only ever index for votes_lost
      } else {
        print(paste("Don't know self transfer prob for ",vote[length(vote)],
                    "Using 0.75 for self and dividing rest evenly"))
        numNonSelfCands <- sum(unlist(sapply(stillToUse, function(p) 
          ifelse(unlist(partyMap[p])==unlist(partyMap[vote[length(vote)]]),0,1))))
        probs <- unlist(sapply(stillToUse, function(p) 
          ifelse(unlist(partyMap[p])==unlist(partyMap[vote[length(vote)]]),0.75,0.25/numNonSelfCands)))
      }
    } else {
      #probs <- unlist(sapply(stillToUseParties, 
      #                       function(p) { transfersRest[transferring_party,p] }))
      probs <- unlist(transfersRest[transferring_party, stillToUseParties])
    }
    
    #if (vote[length(vote)] == 'Alliance Party') {
    #  print('---1---')
    #  print(probs)
    #}
    #If same party present more than once, probs must be divided
    partyCounts <- table(stillToUseParties)
    maxPartyCounts <- max(partyCounts)
    if (maxPartyCounts >= 2) {
      for (m in 2:maxPartyCounts) {
        multiples <- names(partyCounts)[partyCounts==m]
        for (p in multiples) {
          the_multiple_party <- stillToUseParties == p
          sum_adjusts_left <- sum(adjusts[adjusts$cand %in% stillToUse[the_multiple_party],'adjust'])
          #probs[the_multiple_party] <- probs[the_multiple_party] / m
          
          #at same time adjust for unequal share among candidates from that party
          for (i in which(the_multiple_party)) {
            probs[i] <- probs[i] * adjusts[adjusts$cand==stillToUse[i],'adjust'] / sum_adjusts_left
          }          
        }
      }
    }
    #Not otherwise adjusting for first prefs - should do that in grid in advance?
    #TODO: Or better to make use of this year first pref information?
    
    #for now use a low-ish default. 
    # cat(sum(is.na(probs)),'NAs\n')
    # if (sum(is.na(probs)) > 0) {
    #   print(vote[length(vote)])
    #   print(probs)
    # }
    #probs[is.na(probs)] <- 0.05  #Shouldn't need this now have filled gaps
    
    #votes_lost stays at its true value and rest are normalised around it
    not_votes_lost <- names(probs) != 'votes_lost'
    probs[not_votes_lost] <- probs[not_votes_lost] * 
      (1-probs[['votes_lost']]) / sum(probs[not_votes_lost])
    #if (vote[length(vote)] == 'Sinn Fein 2') {
    #  print('---3---')
    #  print(probs)
    #}
    
    #nextpref <- sample(stillToUse,1,prob=probs)
    nextprefInd <- sample.int(length(stillToUse),1,prob=probs)
    nextpref <- stillToUse[nextprefInd]
    if (nextpref == 'votes_lost') { 
      break
    } else {
      vote <- c(vote,nextpref)
      tokeepInds <- which(seq(length(stillToUse)) != nextprefInd)
      stillToUse <- stillToUse[tokeepInds]
      stillToUseParties <- stillToUseParties[tokeepInds]
    }
  }
  list(vote)
}
#orig 0.5s for 100 reps; some simplifications to 0.43s; 
#  merged 2 adjust parts, to 0.32s; update stillToUse each time, to 0.28s;
#  multiple index grids directly, to 0.26s;
#Getting stillToUseParties only takes 0.0036s.
#_1grid currently at 0.06s


#When using FP for all subsequent transfers, don't get the realistic grouping
#  of other parties as unlikely to hit same one in succession. Means one of 
#  multi-party can be under-represented if the other multi-party is already
#  out, as the latter's transfers are largely not carried through to the former.
#  Instead slide all extra multi-parties back to follow first one, keeping
#  internal multi-party order which should maintain the FP dependence of their
#  transfer sharing.
#Something weird may be happening with guaranteed self transfers even when other parties present
#TODO: or better, use self-transfer prob to limit the multi-party group- could do the grouping only 75% of time?
buildList_1grid <- function(vote, candidates, adjusts, transfersAll) {
  #TODO: convert votes to partyMap versions before passing in here  
  #TODO: might be able to do in one go with sample at least for no self options
  #TODO: might be able to combine grids if rest grid probs are just self grid
  #  ones scaled up - which they probably should be
  
  #TODO: lists coming out shorter on average - to do with votes_lost and normalisation
  stillToUse <- setdiff(candidates,vote[1])
  stillToUse <- union(stillToUse,'votes_lost')
  stillToUseParties <- as.character(sapply(stillToUse,function(p) partyMap[[p]]))
  probs <- unlist(transfersAll[partyMap[[vote[1]]], stillToUseParties])
  #If same party present more than once, probs must be divided
  partyCounts <- table(stillToUseParties)
  #print(partyCounts)
  maxPartyCounts <- max(partyCounts)
  if (maxPartyCounts >= 2) {
    for (m in 2:maxPartyCounts) {
      multiples <- names(partyCounts)[partyCounts==m]
      for (p in multiples) {
        the_multiple_party <- stillToUseParties == p 
        sum_adjusts_left <- sum(adjusts[adjusts$cand %in% stillToUse[the_multiple_party],'adjust'])
        #probs[the_multiple_party] <- probs[the_multiple_party] / m
        
        #also adjust for unequal share among candidates from that party
        for (i in which(the_multiple_party)) {
          probs[i] <- probs[i] * adjusts[adjusts$cand==stillToUse[i],'adjust'] / sum_adjusts_left
        }
      }
    }
  }
  print(probs/sum(probs))
  #vote <- c(vote[1],sample(stillToUse,length(stillToUse),prob=probs))
  voteInds <- sample.int(length(stillToUse),length(stillToUse),prob=probs)
  vote <- c(vote[1], stillToUse[voteInds])
  voteParties <- c(partyMap[[vote[1]]], stillToUseParties[voteInds])
  indeps <- which(voteParties=='Independent')
  voteParties[indeps] <- paste(voteParties[indeps],c('a','b','c','d','e','f')[seq(length(indeps))],sep='-')
  
  if (runif(1) > 0.2) vote <- vote[grouping(voteParties)]

  vote <- vote[1:(which(vote=='votes_lost')-1)]
  
  list(vote)
}


