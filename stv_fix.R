#' Single transferable vote
#'
#' @param votes A list of order-of-preference vote vectors, or a list of ballot
#' objects.
#' @param nseats How many seats to fill.
#' @param verbose Print out intermediate information or not
#'
#' @return An STV object, containing:
#'   \describe{
#'     \item{winners:}{the winning entries in order of preference}
#'     \item{More to come!}{}
#'   }
#' @export
#' @examples
#' votes <- list(
#'   dex = c("Ice Skating", "Unihoc", "Food"),
#'   dean = c("Ice Skating", "Unihoc", "Food"),
#'   paul = c("Whiskey Tasting", "Established"),
#'   james = c("Ice Skating", "Unihoc", "Food")
#' )
#'
#' stv(votes, 2)
#'
#' map <- c("e", "f", "i", "u", "w")
#' votes <- list(
#'   ballot(0, 3, 1, 2, 0, map = map),
#'   ballot(0, 3, 1, 2, 0, map = map),
#'   ballot(2, 0, 0, 0, 1, map = map),
#'   ballot(0, 3, 1, 2, 0, map = map)
#' )
#' stv(votes, 2)
stv_fix <- function(votes, nseats, verbose=FALSE, getMatrix=FALSE, getTable=FALSE) {
  winners <- c()
  out <- c()
  nvotes <- length(votes)
  weights <- rep(1, nvotes)
  quota <- droop_quota(nvotes, nseats)
  if (verbose) message('Quota is ',quota)
  running <- get_all_entries(votes)
  if (getMatrix) {
    transfer_matrix <- data.frame()
  } else {
    transfer_matrix <- NULL
  }
  if (getTable) {
      count_table <- data.frame(Candidate=running)
  } else {
      count_table <- NULL
  }

  round <- 1
  while (length(winners) < nseats) {
    if (verbose) message(sprintf('Round %i\n',round))
    round <- round + 1
    fps <- get_first_preferences(votes)
    through_this_rnd <- get_above_quota(fps, quota, weights)
    stillin <- setdiff(running, union(winners, out))
    if (verbose) print(get_current_table(stillin, fps, weights))
    if (getTable) {
        count_table <- merge(count_table, get_current_table(stillin, fps, weights), 
                             by='Candidate', all.x=T)
        names(count_table)[ncol(count_table)] <- paste0('Round_',round-1)
    }
    
    if (length(fps) == 0) {
      break
    }
    
    if (length(stillin) <= (nseats - length(winners))) {
        # Everyone left is in
        winners <- c(winners, stillin)
        if (verbose) cat('Rest are in\n')
        break
    }

    if (length(through_this_rnd) > 0) {
        
        #At end, may need to just take top few of remaining
        if (length(winners) + length(through_this_rnd) > nseats) {
            howManyIn <- nseats - length(winners)
            votesEach <- c()
            for (winner in through_this_rnd) {
                votesEach <- c(votesEach, get_nvotes(fps, winner, weights))
            }
            if (verbose) print(votesEach)
            if (verbose) print(order(votesEach,decreasing=TRUE)[1:howManyIn])
            #reduce to the number of free seats
            through_this_rnd <- through_this_rnd[order(votesEach,decreasing=TRUE)[1:howManyIn]]
        }
        
      # Move everyone's votes up and discount weights for each winner
      votesForTransfer <- 0
      for (winner in through_this_rnd) {
        nvotes <- get_nvotes(fps, winner, weights)
        excess <- round(nvotes - quota, 3) #avoid precision errors
        transfer_ratio <- excess / nvotes
        weights[fps == winner] <- weights[fps == winner] * transfer_ratio

        if (verbose) {
            cat(sprintf('-> %s elected with %g (%g to transfer)\n\n',
                        winner,nvotes,excess))
        }
        votesForTransfer <- votesForTransfer + excess
        cand_out <- winner
      }
      winners <- c(winners, through_this_rnd)
      votes <- remove_prefs(votes, through_this_rnd)
    } else {
      # Eliminate someone, update votes with no change to weights
      loser <- get_stv_loser(fps, stillin, weights)
      votesForTransfer <- get_nvotes(fps, loser, weights)
      if (verbose) {
        cat(sprintf('-> %s eliminated with %g\n\n',loser,votesForTransfer))
      }
      cand_out <- loser
      votes <- remove_prefs(votes, loser)
      out <- c(out, loser)
    }
    
    if (getMatrix) {
        stillin <- setdiff(running, union(winners,out)) #update early, for output
        prevVotes <- vector('integer', length=length(stillin))
        for (i in seq(length(stillin))) {
            prevVotes[i] <- get_nvotes(fps, stillin[i], weights)
        }
    }
    
    vw <- drop_empty_votes_and_update_weights(votes, weights) #keep their lengths consistent
    votes <- vw[[1]]
    weights <- vw[[2]]
    #quota <- droop_quota(length(votes), nseats)  #quota doesn't change
    
    if (getMatrix) transfer_matrix <- add_row_to_transfer_matrix(votes,weights,running,
                                                                 stillin,cand_out,prevVotes,
                                                                 votesForTransfer,
                                                                 verbose,transfer_matrix)
  }
  if (getTable) count_table <- finalise_count_table(count_table, winners)
  
  structure(
    list(winners = winners,
         transfer_matrix = transfer_matrix,
         count_table = count_table),
    class = "STV"
  )
}
#formattable(stv_fix(read_votes_from_csv('~/Documents/sample_vote.csv'),2)$count_table, 
#            list(R1=normalize_bar('pink'),
#                 elected=formatter("span",style = x ~ style(color = ifelse(x,"green","red")),
#                                   x ~ icontext(ifelse(x,"ok","remove")))))

#' Return the names of the candidates who are over the current quota
get_above_quota <- function(fps, quota, weights) {
  ufps <- unique(fps)
  sum_wts <- sapply(ufps, function(fp) sum(weights[fps == fp]))

  ufps[sum_wts >= quota]
}

#' Get the number of votes for a given candidate
get_nvotes <- function(fps, candidate, weights) {
  sum(weights[fps == candidate])
}

#' Return the lowest-voted candidate by FPs, resolving ties randomly
# get_stv_loser <- function(fps, weights) {
#   ufps <- unique(fps)
#   sum_wts <- sapply(ufps, function(fp) sum(weights[fps == fp]))
#   #losers <- ufps[sum_wts == min(sum_wts)] #DPM: seem to get precision error when transfers were previously involved
#   losers <- ufps[abs(sum_wts - min(sum_wts)) < 1e-9] 
#   if (length(losers) == 1) {
#     return(losers)
#   }
#   sample(losers, 1)
# }
get_stv_loser <- function(fps,stillin,weights) {
    ufps <- unique(fps)
    if (length(stillin) > length(ufps)) {
        with_zero_fps <- c()
        for (party in stillin) {
            if (get_nvotes(fps,party,weights) == 0) 
                with_zero_fps <- c(with_zero_fps, party)
        }
        return(sample(with_zero_fps, 1))
    } else {
        sum_wts <- sapply(ufps, function(fp) sum(weights[fps == fp]))
        #losers <- ufps[sum_wts == min(sum_wts)] #seem to get precision error when transfers were previously involved
        losers <- ufps[abs(sum_wts - min(sum_wts)) < 1e-9] 
        if (length(losers) == 1) {
            return(losers)
        }
        return(sample(losers, 1))
    }
}

#' For verbose output, print the current votes table
get_current_table <- function(stillin, fps, weights) {
    fpframe <- data.frame()
    for (p in stillin) {
        fpframe <- rbind(fpframe, data.frame(Candidate=p, 
                                             votes=round(get_nvotes(fps,p,weights),2)))
    }
    fpframe[order(fpframe$votes, decreasing=TRUE), ]
}

#' Add an entry to the transferMatrix, for cand_out
add_row_to_transfer_matrix <- function(votes,weights,running,stillin,cand_out,
                                       prevVotes,votesForTransfer,verbose,transfer_matrix) {
    fps <- get_first_preferences(votes)
    transfer_fracs <- list()
    for (i in seq_along(stillin)) {
        transfers <- get_nvotes(fps,stillin[i],weights) - prevVotes[i]
        if (transfers > 0 & verbose) {
            cat(sprintf('%g to %s (%.2f)\n',transfers,stillin[i],transfers/votesForTransfer))
        }
        if (votesForTransfer > 0) transfer_fracs[[stillin[i]]] <- transfers/votesForTransfer
    }
    transfer_frame <- data.frame(lapply(running, function(x) ifelse(x %in% names(transfer_fracs),
                                                                    transfer_fracs[[x]], NA)))
    names(transfer_frame) <- running
    row.names(transfer_frame) <- c(cand_out)
    if (votesForTransfer > 0) transfer_matrix <- rbind(transfer_matrix, transfer_frame)
    transfer_matrix
}
    
finalise_count_table <- function(count_table, winners) {
    count_table[,2:ncol(count_table)] <- apply(count_table[,2:ncol(count_table)],2,
        function(c) ifelse(is.na(c), ifelse(count_table$Candidate %in% winners, 'E', ' '), c))
    count_table <- cbind(count_table, data.frame(Elected=count_table$Candidate %in% winners))
    count_table <- count_table[order(count_table$Round_1,decreasing = TRUE), ]
    arrange_cols <- colnames(count_table)[which(!(colnames(count_table) %in% c('Candidate','Elected')))]
    count_table <- arrange_(count_table, .dots=arrange_cols)
    count_table <- count_table[rev(row.names(count_table)),]
    row.names(count_table) <- NULL
    return(count_table)
}

drop_empty_votes_and_update_weights <- function(votes, weights) {
    keepInds <- vector('logical',length=length(votes))
    for (v in seq_along(votes)) {
        if (length(votes[[v]] > 0)) keepInds[v] <- TRUE
    }
    list(votes[keepInds],weights[keepInds])
}


#' @export
print.STV <- function(stv) {
  message("An avr stv object.")
  winners <- stv$winners
  message("Winners:")
  for (i in seq_along(winners)) {
    str <- paste0("Round ", i, ":\t", winners[i])
    message(str)
  }
  invisible()
}
