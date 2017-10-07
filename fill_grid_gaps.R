#Fill in gaps in nw grids; also combine self and rest

library(reshape2)
require(dplyr)
require(stringr)
require(ggplot2)

#TODO could use glm to combine reverse frac and transferFriendliness better. OK to excl self?
#TODO NIF etc don't look right in self and rest. Default for unseen party
#  should go by first prefs, not be uniform. Decide what default should be.

#2016 Strangford and South Down DUP to UUP are suspiciously high but seem right
#  according to Wiki and electionsni.org. Corrected West Tyrone 2016, was misprint in ENI
#  Possibly a dodgy SF self transfer in NA 2016, count 3.

#------

fill_via_symmetry <- function(grid, transferFriendliness) {
  for (tfrom in unique(grid$transferFrom)) {
    for (tto in unique(grid$transferTo)) {
      if (is.na(subset(grid,transferFrom==tfrom & transferTo==tto)$fraction) &
          tto != 'votes_lost') {
        tf_ratio <- subset(transferFriendliness,transferTo==tto)$avFrac / 
          subset(transferFriendliness,transferTo==tfrom)$avFrac
        if (is.infinite(tf_ratio)) print(paste('PROBLEM!!',tfrom,tto))
        grid[grid$transferFrom==tfrom & grid$transferTo==tto, 'fraction'] <- 
          subset(grid,transferFrom==tto & transferTo==tfrom)$fraction * tf_ratio
      }
    }
  }
  grid
}

get_party_sim_values <- function(raw_rest_grid) {
  sim_values <- data.frame()
  for (p1 in unique(raw_rest_grid$transferFrom)) {
    for (p2 in setdiff(unique(raw_rest_grid$transferFrom),p1)) {
      vals1 <- subset(raw_rest_grid, transferFrom==p1 & 
                        !(transferTo %in% c(p1,p2)))$fraction
      vals2 <- subset(raw_rest_grid, transferFrom==p2 & 
                        !(transferTo %in% c(p1,p2)))$fraction
      if (sum(!is.na(vals1)) >= 3 & sum(!is.na(vals2)) >= 3) {
        sim_values <- rbind(sim_values, data.frame(p1=p1,p2=as.character(p2),
                                                   cor=cor(vals1,vals2,use='complete.obs')))
      }
    }
  }
  return(sim_values)  
}
#PBP very similar to SF, so use SF for gap
#Ind not very similar to anyone, PUP best

fill_via_most_similar <- function(grid, sim_values) {
  for (tfrom in unique(sim_values$p1)) {
    ordered_sim <- arrange(subset(sim_values,p1==tfrom),-cor)
    for (tto in setdiff(unique(grid$transferFrom),tfrom)) {
      if (is.na(subset(grid,transferFrom==tfrom & transferTo==tto)$fraction)) {
        for (o in 1:3) {
          if (ordered_sim[o,'p2'] != tto & 
              !is.na(subset(grid,transferFrom==ordered_sim[o,'p2'] & 
                            transferTo==tto)$fraction)) {
            #print(paste('replacing',tfrom,'to',tto,'with',ordered_sim[o,'p2']))
            grid[grid$transferFrom==tfrom & grid$transferTo==tto, 'fraction'] <-
              grid[grid$transferFrom==ordered_sim[o,'p2'] & grid$transferTo==tto, 'fraction']
            break
          }
        }
      }
    }
  }
  return(grid)
}

fill_self_using_rest <- function(selfgrid, restgrid, model) {
  restgrid$fraction_rest <- restgrid$fraction
  for (tfrom in unique(selfgrid$transferFrom)) {
    for (tto in unique(selfgrid$transferTo)) {
      if (is.na(subset(selfgrid,transferFrom==tfrom & transferTo==tto)$fraction)) {
        selfgrid[selfgrid$transferFrom==tfrom & 
                   selfgrid$transferTo==tto, 'fraction'] <- 
          predict(self_from_rest, restgrid[restgrid$transferFrom==tfrom & 
                                             restgrid$transferTo==tto, ])
      }
    }
  }
  restgrid$fraction_rest <- NULL
  return(selfgrid)
}


convert_long_back_to_grid <- function(long_transfers, party_short_names) {
  long_transfers <- reshape(long_transfers,direction='wide',
                            v.names='fraction',timevar='transferTo',idvar='transferFrom')
  row.names(long_transfers) <- long_transfers$transferFrom
  long_transfers$transferFrom <- NULL
  names(long_transfers) <- unlist(sapply(names(long_transfers), 
                                         function(n) sub('fraction.','',n)))
  row.names(long_transfers) <- as.character(sapply(row.names(long_transfers), 
                                                   function(n) names(party_short_names)
                                                   [which(as.character(party_short_names)==n)]))
  colnames(long_transfers) <- as.character(sapply(colnames(long_transfers), 
                                                  function(n) names(party_short_names)
                                                  [which(as.character(party_short_names)==n)]))
  return(long_transfers)
}

get_grid_with_gaps_filled <- function(year,situation,party_short_names,form='wide',
                                      rest_grid=NULL, self_rest_model=NULL) {
  if (situation == 'self' | situation == 'all') {
    nw_grid <- read.csv(paste0('transferProbs',year,'/transferMatrix_whenSelfAvailable_nationwide.csv'),
                          header=TRUE,check.names=FALSE)
    nw_numPs <- read.csv(paste0('transferProbs',year,'/numPartiesLeft_whenSelfAvailable_nationwide.csv'),
                        header=TRUE,check.names=FALSE)
    if (situation == 'all') {
      nw_counts_self <- read.csv(paste0('transferProbs',year,'/counts_whenSelfAvailable_nationwide.csv'),
                                 header=TRUE,check.names=FALSE)
      nw_counts_rest <- read.csv(paste0('transferProbs',year,'/counts_whenSelfNotAvailable_nationwide.csv'),
                                 header=TRUE,check.names=FALSE)
    }
  } else if (situation == 'rest') {
    nw_grid <- read.csv(paste0('transferProbs',year,'/transferMatrix_whenSelfNotAvailable_nationwide.csv'),
                          header=TRUE,check.names=FALSE)
    nw_numPs <- read.csv(paste0('transferProbs',year,'/numPartiesLeft_whenSelfNotAvailable_nationwide.csv'),
                        header=TRUE,check.names=FALSE)
  } else { return(NULL) }
  
  #Correct transfer fractions to those that would have applied if the target
  #  had been available along with an *average number* of other targets each time
  #GLM from 2007 excluding self transfers showed transfer inflation factor to be 
  #  2.236 - 0.284 * numPartiesLeft + 0.0129 * (numPartiesLeft^2)
  #cat('Mean frac to Workers raw = ',mean(nw_grid[,'Workers Party'],na.rm=T),'\n')
  numPs_corr_factors <- 1 / (2.236 - 0.284 * nw_numPs + 0.0129 * (nw_numPs^2))
  for (i in seq(nrow(nw_grid))) { numPs_corr_factors[i,i] <- 1 }  #don't alter selfs
  #and separate (similar) correction for votes lost
  numPs_corr_factors[,'votes_lost'] <- 1 / (2.915 - 0.485*nw_numPs[,'votes_lost'] + 
                                              0.0254*(nw_numPs[,'votes_lost']^2))
  nw_grid <- nw_grid * numPs_corr_factors
  #cat('after correction for numPs = ',mean(nw_grid[,'Workers Party'],na.rm=T),'\n')
  #normalise?
  for (transferrer in row.names(nw_grid)) {
    nw_grid[transferrer,] <- nw_grid[transferrer,] / 
      sum(nw_grid[transferrer,],na.rm=T)
  }
  
  #put any missing selfs to mean
  if (situation == 'self' | situation == 'all') {
    meanSelfFrac <- mean(sapply(row.names(nw_grid)[row.names(nw_grid) != 'Independent'], 
                                function(p) nw_grid[p,p]),na.rm=T)
    for (transferrer in row.names(nw_grid)) {
      if (is.na(nw_grid[transferrer,transferrer])) {
        nw_grid[transferrer,transferrer] <- meanSelfFrac
        nw_counts_self[transferrer,transferrer] <- 1
      }
    }
  }
  
  if (situation == 'all') {
    #Read in rest grid, correct for numP, then convert to Self_Available fracs
    #  and combine with self grid
    nw_grid_rest <- read.csv(paste0('transferProbs',year,'/transferMatrix_whenSelfNotAvailable_nationwide.csv'),
                        header=TRUE,check.names=FALSE)
    nw_numPs_rest <- read.csv(paste0('transferProbs',year,'/numPartiesLeft_whenSelfNotAvailable_nationwide.csv'),
                         header=TRUE,check.names=FALSE)
    #cat('Mean frac to Workers rest raw = ',mean(nw_grid_rest[,'Workers Party'],na.rm=T),'\n')
    numPs_rest_corr_factors <- 1 / (2.236 - 0.284 * nw_numPs_rest + 0.0129 * (nw_numPs_rest^2))
    for (i in seq(nrow(nw_grid_rest))) { numPs_rest_corr_factors[i,i] <- 1 }  #don't alter selfs
    numPs_rest_corr_factors[,'votes_lost'] <- 1 / (2.915 - 0.485*nw_numPs_rest[,'votes_lost'] + 
                                                0.0254*(nw_numPs_rest[,'votes_lost']^2))
    nw_grid_rest <- nw_grid_rest * numPs_rest_corr_factors
    for (transferrer in row.names(nw_grid)) {
      nw_grid_rest[transferrer,] <- nw_grid_rest[transferrer,] / 
        sum(nw_grid_rest[transferrer,],na.rm=T)
    }
    #cat('after correction for rest numPs = ',mean(nw_grid_rest[,'Workers Party'],na.rm=T),'\n')
    
    print(sum(nw_grid['Progressive Unionist Party',],na.rm=T))
    print(sum(nw_grid_rest['Progressive Unionist Party',],na.rm=T))
    for (transferrer in row.names(nw_grid)) {
      frac_non_self <- 1 - nw_grid[transferrer,transferrer]
      #print(paste(transferrer,frac_non_self,sum(nw_grid_rest[transferrer,],na.rm=T)))
      nw_grid_rest[transferrer,] <- nw_grid_rest[transferrer,] * frac_non_self
      #adjust because transfers to other parties are slightly larger when self not available
      #  than frac_non_self would suggest. 0.7 reduction in votes_lost seems to work well.
      nw_grid_rest[transferrer,'votes_lost'] <- nw_grid_rest[transferrer,'votes_lost'] * 0.7
    }
    print(sum(nw_grid_rest['Progressive Unionist Party',],na.rm=T))
    
    #Can put NaNs to 0 as if both are NaN, will get that back from /0
    assertthat::assert_that(sum(nw_counts_self[!is.na(nw_grid)] == 0) == 0)
    assertthat::assert_that(sum(nw_counts_rest[!is.na(nw_grid_rest)] == 0) == 0)
    nw_grid[is.na(nw_grid)] <- 0
    nw_grid_rest[is.na(nw_grid_rest)] <- 0
    nw_grid <- (nw_grid*nw_counts_self + nw_grid_rest*nw_counts_rest) / 
      (nw_counts_self + nw_counts_rest)
  }

  #anybody not present this year or present but never transferring will be 
  #  included in grid but with uniform default values.
  exclude_from_main <- c()
  for (party in row.names(nw_grid)) {
    if (sum(!is.na(nw_grid[party,])) <= 1) exclude_from_main <- 
        c(exclude_from_main, as.character(party_short_names[as.character(party)]))
  }
  print(exclude_from_main)
    
  nw_grid$transferFrom <- as.character(party_short_names[row.names(nw_grid)])
  nw_grid <- melt(nw_grid,id.vars='transferFrom',variable.name='transferTo',value.name='fraction')
  nw_grid$transferTo <- as.character(party_short_names[as.character(nw_grid$transferTo)])
  
  #This gives av frac received when available: some parties are more transfer friendly in general
  #Now excluding self transfers so as not to penalise smaller parties
  transferFriendliness <- nw_grid %>% filter(transferTo!=transferFrom) %>% 
    group_by(transferTo) %>% summarise(avFrac=mean(fraction,na.rm=T))
  #transferFriendliness$avFrac[transferFriendliness$avFrac==0] <- min(transferFriendliness$avFrac[transferFriendliness$avFrac > 0],na.rm=T)
  #transferFriendliness$avFrac[is.na(transferFriendliness$avFrac)] <- min(transferFriendliness$avFrac,na.rm=T)
  transferFriendliness$avFrac[is.na(transferFriendliness$avFrac) | transferFriendliness$avFrac==0] <- 0.005

  titlebit <- list('self'='self available','rest'='no self available','all'='all transfers')[[situation]]  
  g1 <- ggplot(subset(nw_grid,!(transferTo %in% exclude_from_main) & !(transferFrom %in% exclude_from_main)),
              aes(transferTo,transferFrom)) + 
    geom_tile(aes(fill=fraction)) + theme(axis.text.x=element_text(angle=45,hjust=1)) +
    scale_fill_distiller(palette='Spectral',direction=-1) +
    labs(title=sprintf('Raw transfer fractions, %s',titlebit),
         subtitle=paste(year,'nationwide'))
  print(g1)
  #TODO plot ordered by transfer friendliness on x axis, alphabetical on y axis

  #don't allow any 0s
  nw_grid$fraction[nw_grid$fraction==0] <- 0.001

  cat(sum(is.na(nw_grid$fraction)),'missing\n')
  
  #Find similarity of parties in terms of where they transfer to
  sim_values <- get_party_sim_values(nw_grid)
  #sim_values %>% group_by(p1) %>% arrange(-cor) %>% summarise(first(p2),first(cor))  

  nw_grid_sym <- fill_via_symmetry(nw_grid,transferFriendliness)
  cat('After using symmetry,',sum(is.na(nw_grid_sym$fraction)),'\n')
  print(subset(nw_grid_sym,transferFrom=='NIF' & transferTo=='NIF'))

  if (situation == 'self' & !is.null(self_rest_model)) {
    #TODO for self, fill remaining using rest or use glm below
    nw_grid_sym <- fill_self_using_rest(nw_grid_sym, rest_grid, self_rest_model)
    cat('After using rest->self,',sum(is.na(nw_grid_sym$fraction)),'\n')
    print(subset(nw_grid_sym,transferFrom=='NIF' & transferTo=='NIF'))
  }
  
  nw_grid_sym_sim <- fill_via_most_similar(nw_grid_sym, sim_values)
  #print(sum(is.na(nw_grid_sym_sim$fraction)))
  cat('After using most similar,',sum(is.na(nw_grid_sym_sim$fraction)),'\n')
  print(subset(nw_grid_sym_sim,transferFrom=='NIF' & transferTo=='NIF'))

  #Just leaves small parties for which there is no information
  nw_grid_sym_sim$fraction[is.na(nw_grid_sym_sim$fraction) &
                             nw_grid_sym_sim$transferFrom != 
                             nw_grid_sym_sim$transferTo] <- 0.005
  #print(sum(is.na(nw_grid_sym_sim$fraction)))
  cat('After putting remaining to 0.005,',sum(is.na(nw_grid_sym_sim$fraction)),'\n')
  print(subset(nw_grid_sym_sim,transferFrom=='NIF' & transferTo=='NIF'))
  
  #normalise by row - converts fracs for average numPs to fracs for all parties present (?)
  for (tfrom in unique(nw_grid_sym_sim$transferFrom)) {
    scale_fact <- sum(nw_grid_sym_sim$fraction[nw_grid_sym_sim$transferFrom==tfrom],na.rm=T)
    nw_grid_sym_sim$fraction[nw_grid_sym_sim$transferFrom==tfrom] <-
      nw_grid_sym_sim$fraction[nw_grid_sym_sim$transferFrom==tfrom] / scale_fact
  }
  
  g2 <- ggplot(subset(nw_grid_sym_sim,!(transferTo %in% exclude_from_main) & !(transferFrom %in% exclude_from_main)),
               aes(transferTo,transferFrom)) +
    geom_tile(aes(fill=fraction)) + theme(axis.text.x=element_text(angle=45,hjust=1)) +
    scale_fill_distiller(palette='Spectral',direction=-1) +
    labs(title=sprintf('Inferred transfer fractions, %s',titlebit),
         subtitle=paste(year,'nationwide'))
  print(g2)

  #Should roughly preserve order of tfs
  #head(arrange(transferFriendliness,avFrac))
  #head(nw_grid_sym_sim %>% group_by(transferTo) %>% 
  #       summarise(avFrac=mean(fraction,na.rm=T)) %>% arrange(avFrac))
  #Second one now shows expected frac received if everyone was available all the time
    
  if (form == 'wide') nw_grid_sym_sim <- convert_long_back_to_grid(nw_grid_sym_sim, party_short_names)

  return(nw_grid_sym_sim)
}

#-----------------

if (FALSE) {
  #Check similarity of self and rest transfers
  fullTra2007Joined <- merge(fullTra2007Rest,fullTra2007Self,
                             by=c('transferFrom','transferTo'),
                             suffixes = c('_rest','_self'))
  fullTra2011Joined <- merge(fullTra2011Rest,fullTra2011Self,
                             by=c('transferFrom','transferTo'),
                             suffixes = c('_rest','_self'))
  fullTra2016Joined <- merge(fullTra2016Rest,fullTra2016Self,
                             by=c('transferFrom','transferTo'),
                             suffixes = c('_rest','_self'))
  fullTra2017Joined <- merge(fullTra2017Rest,fullTra2017Self,
                                 by=c('transferFrom','transferTo'),
                                 suffixes = c('_rest','_self'))
  fullTra4yrsJoined <- rbind(data.frame(fullTra2007Joined,year=2007),
                             data.frame(fullTra2011Joined,year=2011),
                             data.frame(fullTra2016Joined,year=2016),
                             data.frame(fullTra2017Joined,year=2017))
  rm(fullTra2007Joined,fullTra2011Joined,fullTra2016Joined,fullTra2017Joined)
  fullTra4yrsJoined$year <- factor(fullTra4yrsJoined$year)
  fullTra4yrsJoined <- subset(fullTra4yrsJoined,!is.na(fraction_rest) & !is.na(fraction_self))
  
  ggplot(fullTra4yrsJoined) + geom_point(aes(fraction_rest,fraction_self,colour=year))
  print(sum(fullTra4yrsJoined$fraction_self)/sum(fullTra4yrsJoined$fraction_rest)) #0.42
  print(cor.test(fullTra4yrsJoined$fraction_self,fullTra4yrsJoined$fraction_rest)) #0.43
  
  self_from_rest_4yrs <- glm(fraction_self ~ fraction_rest, data=fullTra4yrsJoined)
  
  mine <- get_grid_with_gaps_filled(2016,'self',party_short_names,form='long',
                                    rest_grid=fullTra2016Rest, self_rest_model=self_from_rest_4yrs)
  ggplot(mine) + geom_tile(aes(transferTo,transferFrom,fill=fraction)) + 
    scale_fill_distiller(palette='Spectral',direction=-1) +
    theme(axis.text.x=element_text(angle=45,hjust=1))
}
