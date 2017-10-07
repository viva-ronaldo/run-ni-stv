library(avr)
library(stringr)
library(doMC)
registerDoMC(cores=2)
library(dplyr)
library(reshape2)
library(ggplot2)
source('buildList_functions.R')
source('fill_grid_gaps.R')
source('tmpfunctions.R')

#TODO: Use numP correction on local grid when combining. Look at Gr->All in Str16, seems 60%.
#TODO: Modify nw transfer grid by fps? Probably some relation beyond nw fracs vs nw fps.
#  Try using the long list to fit glm of nw grid and local grid transfers and FPs.
#TODO: Need to get fair measure of candidate transfer friendliness, esp so
#  can use last election to nudge cases like Sugden away from generic Ind.
#TODO: check if there has been a downward trend in votes lost and if so handle it.
#TODO: is there any chance of predicting changes in overall transfer fracs
#  based on changes in FPs of each party, trajectory of transfer frac?
#  e.g. PBP->SDLP in 07-17 was 0.006,0.04,0.05,0.05.


#Limits:
#  Looks like D Kelly got about twice as many transfers from Alliance in 2017 as 2016
#  UB16: all->SDLP similarly overestimated in 11 as in 16, so local helps here.
#  SA16, SDLP too many from UUP3. But UUP->SDLP was as expected in 2011.
#  BW16 with 11 probs, in 1 grid mode DUP can get in; seems to be due to bigger
#    underestimate of PBP->SDLP, also slightly more from small parties to DUP.
#  EA16 40% nw not 1grid, or 60% with nSample 6000, or correct full sample, or 60% 3000 comb0.1;
#    with 1grid, seems always wrong.
#-> Problems in 2016 are BW,SA,EA,Str. BW was close so OK; UB close and could
#     be improved by local; Str ??.
#DUP->UUP increased by 1/3 2011->16, which is probably unpredictable so a big problem.
#
#  UB11:
#  ELD11: UUP2 too much from All, and order of elect/elim at end is wrong meaning big SF->UUP2;
#    1grid affected by grouping which gives big UUP self-t in count 1.
#  FST11: 1grid better than self/rest; self/rest UUP->SDLP too high; local doesn't help much
#    because only have a rest UUP->SDLP and need it to apply moreso in self situation (i.e. at start).
#    Even with UUP->DUP bigger than UUP->SDLP prob by 6-10x, when transfer happens it is only ~2x
#    in favour of DUP, can't understand why. Seems to be that DUP do get more at start of lists
#    but some votes pass via eliminated parties to SDLP which closes the gap. But there is only
#    Ind,TUV,All, and overall these look as likely to pass to DUP as to SDLP.


#TODO May need to alter local grid values before combining, to be most accurate.

#2016:
#EA was close between SF, UKIP, UUP2 - accept 30-50%
#BW was close between SDLP and DUP
#UB was close between SF2 and SDLP - accept 40-60%
#Str was close between UUP and SDLP because UUP could have been knocked out by DUP4 or Ind
#2011:
#BN was a bit close between DUP3 and UUP
#BS was a bit close between SF, SDLP, DUP
#UB was a bit close between UUP2, SDLP, SF
#ND was close between Green and All2 (Ind may have chance too)
#ELD was a bit close between DUP3 and UUP, seems tricky due to order - accept 50-60%
#FST was very close between SF3 and SDLP (accept 50%)
#WT was close between UUP and DUP2

party_short_names <- list('Democratic Unionist Party'='DUP',
                          'Ulster Unionist Party'='UUP',
                          'Sinn Fein'='SF',
                          'Social Democratic and Labour Party'='SDLP',
                          'Alliance Party'='Alliance',
                          'Traditional Unionist Voice'='TUV',
                          'Green Party'='Green',
                          'People Before Profit Alliance'='PBP',
                          'Independent'='Ind',
                          'Workers Party'='Workers',
                          'Labour Alternative'='Labour',
                          'Cross-Community Labour Alternative'='CCLA',
                          'South Belfast Unionists'='SBU',
                          'Cannabis Is Safer Than Alcohol'='CISTA',
                          'NI Labour Representation Committee'='NILRC',
                          'NI Conservatives'='NI Cons',
                          'Progressive Unionist Party'='PUP',
                          'Animal Welfare Party'='AWP',
                          'Democracy First'='DF',
                          'Northern Ireland First'='NIF',
                          'UK Independence Party'='UKIP',
                          'Socialist Party'='Soc',
                          'British Nationalist Party'='BNP',
                          'Procapitalism'='Cap',
                          'UK Unionist Party'='UKUP',
                          'Socialist Environmental Alliance'='SEA',
                          'Republican Sinn Fein'='RSF',
                          'Labour Party'='Lab',
                          'Make Politicians History'='MPH',
                          'votes_lost'='votes_lost')

constituencies <- c('Belfast North','Belfast West','Belfast East','Belfast South',
                    'North Antrim','East Antrim','South Antrim','Strangford',
                    'Lagan Valley','Upper Bann','North Down','South Down',
                    'Newry Armagh','East Londonderry','Fermanagh South Tyrone',
                    'Foyle','West Tyrone','Mid Ulster')
constituencies_lowerdash <- as.character(sapply(constituencies, function(c) 
  paste(strsplit(tolower(c),' ')[[1]],collapse='-')))
constituencies_lower <- as.character(sapply(constituencies, function(c) 
  paste(strsplit(tolower(c),' ')[[1]],collapse='')))

#Transfer matrix must take into account availability of each target at each
#  elimination/election. Particularly with regard to self transfers.
#  For now this matrix is a rough but usable one.
#fullTransfers2016 <- read.csv('transferProbs2016/transferMatrix_all_nationwide.csv',header=TRUE,check.names=FALSE)
fullTransfersSelf2016 <- read.csv('transferProbs2016/transferMatrix_whenSelfAvailable_nationwide.csv',header=TRUE,check.names=FALSE)
fullTransfersRest2016 <- read.csv('transferProbs2016/transferMatrix_whenSelfNotAvailable_nationwide.csv',header=TRUE,check.names=FALSE)
fullTransfersSelf2016[!is.na(fullTransfersSelf2016) & fullTransfersSelf2016==0] <- 0.001
fullTransfersRest2016[!is.na(fullTransfersRest2016) & fullTransfersRest2016==0] <- 0.001

fullTransfersSelf2011 <- read.csv('transferProbs2011/transferMatrix_whenSelfAvailable_nationwide.csv',header=TRUE,check.names=FALSE)
fullTransfersRest2011 <- read.csv('transferProbs2011/transferMatrix_whenSelfNotAvailable_nationwide.csv',header=TRUE,check.names=FALSE)
fullTransfersSelf2011[!is.na(fullTransfersSelf2011) & fullTransfersSelf2011==0] <- 0.001
fullTransfersRest2011[!is.na(fullTransfersRest2011) & fullTransfersRest2011==0] <- 0.001

traRest2011Filled <- get_grid_with_gaps_filled(2011,'rest',party_short_names)

get_local_transfers <- function(situation,year,constituencies_lowerdash) {
  list_grids <- list()
  for (constit in constituencies_lowerdash) {
    list_grids[[constit]] <- read.csv(sprintf('transferProbs%d/transferMatrix_%s_%s.csv',
                                              year,situation,constit),
                                      header=TRUE,check.names=FALSE)
  }
  list_grids
}
#localTransfers2016 <- get_local_transfers('all',2016,constituencies_lowerdash)
localTransfersSelf2016 <- get_local_transfers('whenSelfAvailable',2016,constituencies_lowerdash)
localTransfersRest2016 <- get_local_transfers('whenSelfNotAvailable',2016,constituencies_lowerdash)
localTransfersAll2016 <- get_local_transfers('all',2016,constituencies_lowerdash)
localTransfersSelf2011 <- get_local_transfers('whenSelfAvailable',2011,constituencies_lowerdash)
localTransfersRest2011 <- get_local_transfers('whenSelfNotAvailable',2011,constituencies_lowerdash)
localTransfersAll2011 <- get_local_transfers('all',2011,constituencies_lowerdash)
localTransfersSelf2007 <- get_local_transfers('whenSelfAvailable',2007,constituencies_lowerdash)
localTransfersRest2007 <- get_local_transfers('whenSelfNotAvailable',2007,constituencies_lowerdash)
localTransfersAll2007 <- get_local_transfers('all',2007,constituencies_lowerdash)


combine_transfer_grids <- function(fullgrid,localgrid,alpha) {
  goodvals <- (!is.na(fullgrid))*alpha + (!is.na(localgrid))*(1-alpha)
  fullgrid[is.na(fullgrid)] <- 0
  localgrid[is.na(localgrid)] <- 0
  combgrid <- (fullgrid*alpha + localgrid*(1-alpha))/goodvals
  return(combgrid)
}

#e.g. Foyle has DUP->SDLP 0.66 because no unionists left
runConstit('Foyle',year,fullTransfersSelf2016,fullTransfersRest2016,3000)
runConstit('Foyle',year,localTransfersSelf2016[['foyle']],localTransfersRest2016[['foyle']],3000)
runConstit('Foyle',year,
           combine_transfer_grids(fullTransfersSelf2016,localTransfersSelf2016[['foyle']],0.5),
           combine_transfer_grids(fullTransfersRest2016,localTransfersRest2016[['foyle']],0.5),3000)

#Possible problems: 
# - UUP to SDLP when no self available seems high at 0.20, vs 0.30 to DUP (BN doesn't show this at all)
# - should have a PBP self for 2017 BW, check why not

#How to combine grids:
#goodvals <- (!is.na(traFull)) + (!is.na(traIndiv))
#traFull[is.na(traFull)] <- 0
#traIndiv[is.na(traIndiv)] <- 0
#traComb <- (traFull + traIndiv)/goodvals

#Rough transfer-friendliness:
#for (col in names(fullTransfers)) { cat(col,sum(fullTransfers[,col],na.rm=TRUE),'\n') }

get_party_map <- function(all_parties) {
  partyMap <- vector('list')
  for (party in all_parties) {
    partyMap[party] <- party
    for (ind in 1:5) {
      partyMap[paste(party,ind)] <- party
    }
  }
  return(partyMap)
}
partyMap <- get_party_map(names(party_short_names))

replist <- function(arg, times) lapply(seq(times), function(i) arg)

getFirstPrefs <- function(file) {
  nValid <- as.integer(read.csv(file,header=FALSE,nrows=1))
  firstPrefs <- read.csv(file,header=FALSE,skip=1)
  names(firstPrefs) <- c('cand','percent')
  firstPrefs$number <- floor(firstPrefs$percent*nValid/100)
  if (length(firstPrefs$cand) != length(unique(firstPrefs$cand))) {
    print('Duplicates in results file!')
    return()
  } else {
    firstPrefs 
  }
}

getUnionistPercent <- function(firstPrefs) {
  firstPrefs$cand <- as.character(firstPrefs$cand)
  firstPrefs$cand <- as.character(sapply(firstPrefs$cand, function(n) ifelse(grepl('[:alnum:]* [1-5]',n),
                                                      substr(n,1,nchar(n)-2),n)))
  sum(subset(firstPrefs, cand %in% c('Democratic Unionist Party','Ulster Unionist Party',
                                     'Progressive Unionist Party','Traditional Unionist Voice',
                                     'UK Independence Party','NI Conservatives',
                                     'South Belfast Unionists','UK Unionist Party'))$percent)
}
getNationalistPercent <- function(firstPrefs) {
  firstPrefs$cand <- as.character(firstPrefs$cand)
  firstPrefs$cand <- as.character(sapply(firstPrefs$cand, function(n) ifelse(grepl('[:alnum:]* [1-5]',n),
                                                                             substr(n,1,nchar(n)-2),n)))
  sum(subset(firstPrefs, cand %in% c('Sinn Fein','Social Democratic and Labour Party',
                                     'People Before Profit Alliance','Workers Party',
                                     'Republican Sinn Fein','Socialist Party',
                                     'Socialist Environmental Alliance'))$percent)
}
getIndependentPercent <- function(firstPrefs) {
  firstPrefs$cand <- as.character(firstPrefs$cand)
  firstPrefs$cand <- as.character(sapply(firstPrefs$cand, function(n) ifelse(grepl('[:alnum:]* [1-5]',n),
                                                                             substr(n,1,nchar(n)-2),n)))
  sum(subset(firstPrefs, cand == 'Independent')$percent)
}
#TODO update for 2011,2007 parties

for (constit in constituencies_lower) {
  #cat(constit,'\n')
  for (year in c(2011,2016,2017)) {
    prevyear <- list('2011'=2007,'2016'=2011,'2017'=2016)[[as.character(year)]]
    up <- getUnionistPercent(getFirstPrefs(paste0('fp',year,'/alt/',constit,year,'alt.csv')))
    np <- getNationalistPercent(getFirstPrefs(paste0('fp',year,'/alt/',constit,year,'alt.csv')))
    ip <- getIndependentPercent(getFirstPrefs(paste0('fp',year,'/alt/',constit,year,'alt.csv')))
    up_prev <- getUnionistPercent(getFirstPrefs(paste0('fp',prevyear,'/alt/',constit,prevyear,'alt.csv')))
    np_prev <- getNationalistPercent(getFirstPrefs(paste0('fp',prevyear,'/alt/',constit,prevyear,'alt.csv')))
    ip_prev <- getIndependentPercent(getFirstPrefs(paste0('fp',prevyear,'/alt/',constit,prevyear,'alt.csv')))
    uni_down <- FALSE
    nat_down <- FALSE
    if ((up-up_prev) < -2 & (np-np_prev) < -2) {
      if ((up-up_prev) < (np-np_prev)) {
        uni_down <- TRUE
      } else { nat_down <- TRUE }
    } else if ((up-up_prev) < -2) { uni_down <- TRUE 
    } else if ((np-np_prev) < -2) { nat_down <- TRUE }
    if (uni_down & (ip-ip_prev > 2)) {
      cat(sprintf('Unionist Ind %s %s: Uni = %.1f (%+.1f), Nat = %.1f (%+.1f), Ind = %.1f (%+.1f)\n',
                               constit,year,up,up-up_prev,np,np-np_prev,ip,ip-ip_prev))  
    }
    if (nat_down & (ip-ip_prev > 2)) {
      cat(sprintf('Nationalist Ind %s %s: Uni = %.1f (%+.1f), Nat = %.1f (%+.1f), Ind = %.1f (%+.1f)\n',
                  constit,year,up,up-up_prev,np,np-np_prev,ip,ip-ip_prev))  
    }
  }
}
#When Ind goes up by >~2 and u/n down by similar amount, can assume that Ind as 
#  a group has become more u/n.
#Works for BE 07/11, ND 07/11, ELD 07/11, Str 11/16, Str 16/17, WT 11/16, Fy 11/16
#Rule doesn't work for EL 16/17, LV 16/17, misidentifies SD 11/16, NwA 11/16, ELD 16/17
#Maybe need to use prev local for Inds and then update that using the u/n change.


runConstit <- function(constitName, year, transfersSelf, transfersRest, nSample,
                       use_1grid_buildlist=FALSE, use_party_1_for_transfers=FALSE) {
  infer_independent <- TRUE
  
  print(constitName)
  #resultsFile <- paste0('fp',year,'/',str_replace_all(tolower(constitName),' ',''),year,'.csv')
  resultsFile <- paste0('fp',year,'/alt/',str_replace_all(tolower(constitName),' ',''),year,'alt.csv')
  nSeats <- ifelse(year >= 2017, 5, 6)
  
  firstPrefs <- getFirstPrefs(resultsFile)
  if ( sum(firstPrefs$percent) < 98 ) { print('Below 100%!!!')}
  if ( sum(firstPrefs$percent) > 102 ) { print('Over 100%!!!')}
  
  if (infer_independent) {
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
  }
  
  #get factors for distributing transfers among candidates from same party
  parties <- unique(as.character(sapply(firstPrefs$cand, function(p) partyMap[as.character(p)])))
  firstPrefs$adjust <- 1
  for (party in parties) {
      firstPrefs$adjust[grepl(party,firstPrefs$cand)] <- firstPrefs[grepl(party,firstPrefs$cand),]$percent / 
          mean(firstPrefs[grepl(party,firstPrefs$cand),]$percent)
  }
  
  votes <- c()
  for (i in seq(1,nrow(firstPrefs))) {
    votes <- c(votes, replist(as.character(firstPrefs$cand[i]), firstPrefs$number[i]))
  }

  fullvotes <- c()
  #slow to run, so sampling the total ~40k at the moment
  for (vote in sample(votes, nSample, replace=FALSE)) {
    if (use_1grid_buildlist) {
      fullvotes <- c(fullvotes, buildList_1grid(vote, firstPrefs$cand,
                                                firstPrefs[,c('cand','adjust')],
                                                transfersSelf))  #Self OK?  
    } else {
      fullvotes <- c(fullvotes, buildList(vote, firstPrefs$cand, 
                                          firstPrefs[,c('cand','adjust')],
                                          transfersSelf, transfersRest,
                                          use_party_1_for_transfers=use_party_1_for_transfers))
    }
  }
  # fullvotes <- foreach (vote = sample(votes, nSample, replace=FALSE), .combine=c) %dopar% 
  #     buildList(vote, firstPrefs$cand, firstPrefs[,c('cand','adjust')],
  #               transfersSelf, transfersRest)

  #print("Elected using STV:")
  #elected2 <- stv(fullvotes, nSeats)$winners[1:(nSeats+1)]
  elected2 <- my_stv(fullvotes, nSeats, verbose=TRUE)$winners  #testing
  elected3 <- sapply(elected2, function(p) { as.character(partyMap[p]) })
  
  res2 <- data.frame()
  count <- 1
  for (e in seq(length(elected2))) { 
    res2 <- rbind(res2, data.frame(constit=constitName,seat=count,
                                   party=as.character(elected3[e]),
                                   cand=ifelse(grepl('[1-9]',as.character(elected2[e])),
                                               substr(as.character(elected2[e]),nchar(elected2[e]),nchar(elected2[e])),
                                               "1")))
    count <- count + 1
  }
  res2
}

runConstitEnsemble <- function(constitName, year,
                               transfersSelf, transfersRest, 
                               nSample, nEnsemble, 
                               use_1grid_buildlist=FALSE,use_party_1_for_transfers=FALSE,
                               returnByParty=TRUE) {
  res <- list()
  res2 <- data.frame()
  for (e in seq(nEnsemble)) {
    capture.output(result <- runConstit(constitName,year,transfersSelf,transfersRest,nSample,
                                        use_1grid_buildlist=use_1grid_buildlist,
                                        use_party_1_for_transfers=use_party_1_for_transfers))
    
    result$candp <- paste(result$party,result$cand)
    by_party <- arrange(data.frame(party=as.character(party_short_names[names(table(result$party))]),
                                   seats=as.integer(table(result$party))),-seats,party)
    by_party <- paste(paste(by_party$seats,by_party$party),collapse=', ')
    res2 <- rbind(res2,data.frame(result=by_party))
    for (c in result$candp) {
      res[[c]] <- ifelse(c %in% names(res), res[[c]]+1, 1)
    }
  }
  if (returnByParty) {
    table_results <- table(res2$result)/nEnsemble
    arrange(data.frame(result=names(table_results),prob=as.numeric(table_results)),-prob)  
  } else {
    res <- sapply(names(res),function(c) res[[c]]/nEnsemble)
    res <- data.frame(cand=names(res), probElected=as.numeric(res))
    arrange(res,-probElected)      
  }
}

runAllConstitsEnsemble <- function(constituencies, year, transfersSelf, transfersRest,
                                   nSample, nEnsemble, alpha=1, 
                                   use_1grid_buildlist = FALSE,
                                   localTransfersSelf = NULL, localTransfersRest = NULL) {
  brier <- 0
  num_right <- 0
  for (constit in constituencies) {
    #slight simplification to use 'all' for local in both cases
    if (alpha < 1 & !is.null(localTransfersAll)) {
      constit_dash <- as.character(paste(strsplit(tolower(constit),' ')[[1]],collapse='-'))
      selfgrid <- combine_transfer_grids(transfersSelf,localTransfersSelf[[constit_dash]],alpha)
      if (!use_1grid_buildlist) {
        restgrid <- combine_transfer_grids(transfersRest,localTransfersRest[[constit_dash]],alpha)
      } else {
        restgrid <- NULL
      }
    } else {
      selfgrid <- transfersSelf
      restgrid <- transfersRest
    }
    res <- runConstitEnsemble(constit, year, selfgrid, restgrid,
                              nSample, nEnsemble, use_1grid_buildlist = use_1grid_buildlist)
    brier_add <- get_brier_for_constit(res,constit,year)
    cat(constit,(1-brier_add**0.5),'\n')
    brier <- brier + brier_add
    if (brier_add <= 0.04) num_right <- num_right + 1
  }
  brier <- brier / length(constituencies)
  list(brier=brier, num_right=num_right)
}


verifyAssembly <- function(mine,year) {
    if (year == 2016) {
        mine <- as.data.frame(table(subset(mine,seat<=6)$party))
        #actual <- data.frame(party=c('Democratic Unionist Party','Sinn Fein',
        #                             'Ulster Unionist Party','Social Democratic and Labour Party',
        #                             'Alliance Party','Green Party','People Before Profit Alliance',
        #                             'Traditional Unionist Voice','Independent'),
        #                     seats=c(38,28,16,12,8,2,2,1,1))
        indivs <- unlist(sapply(subset(truth,Year==2016)$Elected_string, 
                                function(r) strsplit(as.character(r),',')))
        indivs <- as.character(sapply(indivs, function(w) str_trim(w)))
        allSeats <- data.frame(seats = as.integer(sapply(indivs, function(w) substr(w,1,1))),
                               party = as.character(sapply(indivs, function(w) substr(w,3,nchar(w)))))
        actual <- allSeats %>% group_by(party) %>% summarise(seats=sum(seats))
    } else if (year == 2017) {
        mine <- as.data.frame(table(subset(mine,seat<=5)$party))
        actual <- data.frame(party=c('Democratic Unionist Party','Sinn Fein',
                                     'Ulster Unionist Party','Social Democratic and Labour Party',
                                     'Alliance Party','Green Party','People Before Profit Alliance',
                                     'Traditional Unionist Voice','Independent'),
                             seats=c(28,27,10,12,8,2,1,1,1))
    }
    names(mine) <- c('party','mySeats')
    compare <- merge(actual,mine,by='party',all.x=TRUE)
    #compare$mySeats[is.na(compare$mySeats)] <- 0
    compare$diff <- compare$mySeats - compare$seats
    compare[order(-compare$seats),]
}


#---- 2017

#With votes_lost, s-t 0.7, 10k run: DUP 30 (+2), SF 29 (+2), UUP 10, SDLP 9 (-3), All 8, Gr 2, PBP 1, Ind 1, TUV 0 (-1)
#  Miss on LV (UUP/SDLP), ELD (SF/SDLP), NA (DUP/TUV), UB (SF/SDLP), FST (DUP/UUP): 85/90

#As printed on Friday: 84/90 (missed 5/6 switches and wrong BN)
#From top 5 first prefs: 84/90 (miss 6 switches)


get_brier_for_constit <- function(constit_ens_df,constit,year) {
  truth <- read.csv('results_for_verification.csv')
  truth <- as.character(subset(truth, Year==year & Constituency==constit)$Elected_string)
  if (truth %in% constit_ens_df$result) {
    return((subset(constit_ens_df, result==truth)$prob - 1)**2)  
  } else {
    return(1)
  }
}

fullTra2007All <- get_grid_with_gaps_filled(2007,'all',party_short_names)
fullTra2007Self <- get_grid_with_gaps_filled(2007,'self',party_short_names)
fullTra2007Rest <- get_grid_with_gaps_filled(2007,'rest',party_short_names)
fullTra2011All <- get_grid_with_gaps_filled(2011,'all',party_short_names)
fullTra2011Self <- get_grid_with_gaps_filled(2011,'self',party_short_names)
fullTra2011Rest <- get_grid_with_gaps_filled(2011,'rest',party_short_names)
fullTra2016All <- get_grid_with_gaps_filled(2016,'all',party_short_names)
fullTra2016Self <- get_grid_with_gaps_filled(2016,'self',party_short_names)
fullTra2016Rest <- get_grid_with_gaps_filled(2016,'rest',party_short_names)
fullTra2017All <- get_grid_with_gaps_filled(2017,'all',party_short_names)

for (constit in constituencies) {
  print(constit)
  res <- runConstitEnsemble(constit,2016,fullTra2011Self,fullTra2011Rest,
                            3000,10,use_1grid_buildlist = TRUE)
  print(res)
}
print('Now 2016 for 2016')
for (constit in constituencies) {
  print(constit)
  res <- runConstitEnsemble(constit,2016,fullTra2016All,fullTra2016Rest,
                            3000,10,use_1grid_buildlist = TRUE)
  print(res)
}


#-------- 16w16 - allow UB, BW ~60%, EA ~40%
#Fixed 1grid Fy with Ind group change; fixed selfrest WT due to Ind uni/nat bug;
#  selfrest UB fixed with local; selfrest Str, improved with local;
#  1grid Str,FST, improved with local.
res2016w16_1grid <- runAllConstitsEnsemble(constituencies,2016,fullTra2016All,NULL,
                                           3000,20,use_1grid_buildlist = TRUE)
#13/18, 0.116: miss EA, SA, ELD, FST (too much SF self-t?)
res2016w16_1grid_comb0.5 <- runAllConstitsEnsemble(constituencies,2016,fullTra2016All,NULL,
                                           3000,20,use_1grid_buildlist = TRUE,
                                           alpha=0.5, localTransfersAll = localTransfersAll2016)
#14/18, 0.058: miss EA, SA, FST
res2016w16_1grid_comb0.1 <- runAllConstitsEnsemble(constituencies,2016,fullTra2016All,NULL,
                                           3000,20,use_1grid_buildlist = TRUE,
                                           alpha=0.1, localTransfersAll = localTransfersAll2016)
#15/18, 0.066: miss EA, SA
res2016w16_allall <- runAllConstitsEnsemble(constituencies,2016,fullTra2016All,fullTra2016All,
                                            3000,20,use_1grid_buildlist = FALSE)
#12/18, 0.074: miss BS (65), SA (70), Str (60), UB (15)
res2016w16_selfrest <- runAllConstitsEnsemble(constituencies,2016,fullTra2016Self,fullTra2016Rest,
                                           3000,20,use_1grid_buildlist = FALSE)
#15/18, 0.061: miss UB (10), Str (70)
res2016w16_selfrest_comb0.5 <- runAllConstitsEnsemble(constituencies,2016,fullTra2016Self,fullTra2016Rest,
                                                      3000,20,use_1grid_buildlist = FALSE,
                                                      alpha=0.5, localTransfersAll = localTransfersAll2016)
#16/18, 0.039 (EA 35, UB 55)
res2016w16_selfrest_comb0.1 <- runAllConstitsEnsemble(constituencies,2016,fullTra2016Self,fullTra2016Rest,
                                                      3000,20,use_1grid_buildlist = FALSE,
                                                      alpha=0.1, localTransfersAll = localTransfersAll2016)
#15/18, 0.036 (EA 35, SA 70, Str 75)

#--------16w11
res2016w11_1grid <- runAllConstitsEnsemble(constituencies,2016,fullTra2011All,NULL,
                                              3000,20,use_1grid_buildlist = TRUE)
#8/18, 0.254: miss EA, SA, UB, ELD, FST, Fy
res2016w11_selfrest <- runAllConstitsEnsemble(constituencies,2016,fullTra2011Self,fullTra2011Rest,
                                              3000,20,use_1grid_buildlist = FALSE)
#11/18: 0.208:  miss Str, LV, UB, WT
#12/18, 0.184: miss Str, LV, UB, WT, BW 70%, EA 70%, 
res2016w11_allall <- runAllConstitsEnsemble(constituencies,2016,fullTra2011All,fullTra2011All,
                                              3000,20,use_1grid_buildlist = FALSE)
#10/18, 0.246: miss BS, EA, Str, LV, UB, WT

#--------11w11
res2011w11_1grid <- runAllConstitsEnsemble(constituencies,2011,fullTra2011All,NULL,
                                           3000,20,use_1grid_buildlist = TRUE)
#13/18, 0.113: miss BN (35), UB (35), ELD (5)
res2011w11_1grid_comb0.5 <- runAllConstitsEnsemble(constituencies,2011,fullTra2011All,NULL,
                                                   3000,20,use_1grid_buildlist = TRUE,
                                                   alpha=0.5, localTransfersAll = localTransfersAll2011)
#13/18, 0.042: miss EA (75), NwA (75), ELD (40), FST (75)
res2011w11_1grid_comb0.1 <- runAllConstitsEnsemble(constituencies,2011,fullTra2011All,NULL,
                                                   3000,20,use_1grid_buildlist = TRUE,
                                                   alpha=0.1, localTransfersAll = localTransfersAll2011)
#15/18, 0.033: miss UB (50), ND (70), ELD (65)
res2011w11_selfrest <- runAllConstitsEnsemble(constituencies,2011,fullTra2011Self,fullTra2011Rest,
                                              3000,20,use_1grid_buildlist = FALSE)
#9/18, 0.219: miss BN (55), NA (30), Str (55), UB (15), ELD (25), FST (15)

res2011w11_selfrest_comb0.5 <- runAllConstitsEnsemble(constituencies,2011,fullTra2011Self,fullTra2011Rest,
                                                   3000,20,use_1grid_buildlist = FALSE,
                                                   alpha=0.5, localTransfersAll = localTransfersAll2011)
#12/18, 0.156: miss Str (70), UB (50), NwA (25), ELD (65), FST (15)
res2011w11_selfrest_comb0.1 <- runAllConstitsEnsemble(constituencies,2011,fullTra2011Self,fullTra2011Rest,
                                                      3000,20,use_1grid_buildlist = FALSE,
                                                      alpha=0.1, localTransfersAll = localTransfersAll2011)
#12/18, 0.146: Str (75), UB (40), NwA (50), ELD (60), FST (15)
res2011w11_selfrest_comb0.1sr <- runAllConstitsEnsemble(constituencies,2011,fullTra2011Self,fullTra2011Rest,
                                                      3000,20,use_1grid_buildlist = FALSE,
                                                      alpha=0.1, 
                                                      localTransfersSelf = localTransfersSelf2011,
                                                      localTransfersRest = localTransfersRest2011)


#--------11w07


#--------

tune_nSample <- function(constituencies, year, transfersSelf, transfersRest, 
                       nSamples, nEnsembles,
                       use_1grid_buildlist = FALSE) {
  all_res <- data.frame()
  for (i in seq(length(nSamples))) {
    res <- runAllConstitsEnsemble(constituencies, year, transfersSelf, transfersRest,
                           nSamples[i], nEnsembles[i],
                           use_1grid_buildlist = use_1grid_buildlist)
    all_res <- rbind(all_res, data.frame(nSample=nSamples[i],nEnsemble=nEnsembles[i],res))
  }
  all_res
}
tuneNs_16v16_1grid <- tune_nSample(constituencies,2016,fullTra2016All,fullTra2016Rest,
                                   c(500,1500,3000,5000,8000),
                                   c(50,  50,  20,  20,  20),
                                   use_1grid_buildlist = TRUE)
