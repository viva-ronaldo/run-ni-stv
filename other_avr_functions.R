#' Interface for running irv on ballot cards.
#'
#' @param ... Each entry in the ballot. Elements without votes cast can be
#'   noted with 0, -1, NA, or any number below 1.
#' @param map optional character mapping giving names of the voting options
#' @return ballot object
#' @export
ballot <- function(..., map = NULL) {
    votes <- c(...)
    votes[votes < 1] <- NA
    nas <- which(is.na(votes))
    ord <- seq(votes)[order(votes)]
    answer <- setdiff(ord, nas)
    
    if (!is.null(map)) {
        answer <- map[answer]
    }
    
    answer
}

droop_quota <- function(votes_cast, seats_to_fill) {
    floor(votes_cast / (seats_to_fill + 1)) + 1 - .Machine$double.eps
}

get_all_entries <- function(votes) {
    unique(unlist(votes))
}

get_first_preferences <- function(votes) {
    unlist(sapply(votes, function(vote) vote[1]))
}

remove_prefs <- function(votes, exclusions) {
    lapply(votes, function(vote) vote[!vote %in% exclusions])
}

#Read in a csv file of options and ballots in standard format, no header
read_votes_from_csv <- function(fileName) {
    votes <- read.csv(fileName, header=FALSE, stringsAsFactors=FALSE)
    cat(sprintf('%g candidates on ballot\n',nrow(votes)))
    cat(sprintf('%g votes received\n',ncol(votes)-1))
    names(votes) <- c('Candidates', paste0('ballot',seq(ncol(votes)-1)))
    return(lapply(votes[2:ncol(votes)], function(b) ballot(b, map=votes$Candidates)))
}

#Run stv multiple times and return election frequencies,
#  to handle ties being broken randomly.
do_ensemble_stv <- function(votes, nseats, nensemble) {
    elected_counts <- list()
    running <- get_all_entries(votes)
    for (candidate in running) elected_counts[[candidate]] <- 0
    
    for (e in seq(nensemble)) {
        results <- stv_fix(votes, nseats)  #TODO change to stv
        for (candidate in results$winners) {
            elected_counts[[candidate]] <- elected_counts[[candidate]] + 1
        }
    }
    
    winner_freqs <- data.frame()
    for (winner in names(elected_counts[elected_counts > 0])) {
        winner_freqs <- rbind(winner_freqs,
            data.frame(candidate=winner, elected_pct=100*elected_counts[[winner]]/nensemble))
    }
    winner_freqs[order(winner_freqs$elected_pct,decreasing=TRUE),]
}
