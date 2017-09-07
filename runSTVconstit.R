library(avr)
library(stringr)
library(doMC)
registerDoMC(cores=2)
library(dplyr)
source('tmpfunctions.R')

#First try to reproduce all 2016 results, possibly also getting close to 
#  count by count progress. ~10k ballots is usually robust, but may prefer 
#  an ensemble of ~1-3k samples to capture uncertainty in transfer probs.
#Maybe do 10k sample, then if any come down to 1%, redo as ensemble of 10x1000?

#TODO: see if can get more transfer fracs, e.g. foyle 2016 noself only have DUP and SDLP.
#TODO: Need to get fair measure of candidate transfer friendliness, esp so
#  can use last election to nudge cases like Sugden away from generic Ind.
#TODO: use previous year unionist and nationalist total vote to work out
#  if a new Ind is uni or nat.
#  - or for now just set Ind transfers to UUP when no self available, plus DUP->UUP.
#TODO: make visualisation for diffs in transfers year to year and nw vs local.
#TODO: check special self transfer rule working.
#  Probably affects BW16, too much SF->SDLP. May also fix Strangford
#TODO: record all transfers along with FPs and use to adjust transfer probs
#  dependent on FPs. Want to see Sugden increase in EL2016; Menagh increase
#  in Str2016, also fewer transfers to DUP3 in Str2016.
#  Modify nw transfer grid by fps? Probably some relation beyond nw fracs vs nw fps.
#TODO: handle downward trend in mean votes_lost (ballots getting longer). From 2011
#  to 2016 everyone's transfers went up, but SF, SDLP went up least since their
#  FP vote went down most.
#TODO: try including hist local transfers. Look at Gr->All in Str16, seems 60%.


#Limits:
#Looks like D Kelly got about twice as many transfers from Alliance in 2017 as 2016
#  UB16 partly due to self transfers too low, also helped by increased DUP->UUP.
#  SA wrong as All doesn't get enough from Green and SDLP too many from UUP3.   
#Switches in 2016: UB, SA, SD, FST, WT, Foyle, ELD, LV, BW

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


constituencies <- c('Belfast North','Belfast West','Belfast East','Belfast South',
                    'North Antrim','East Antrim','South Antrim','Strangford',
                    'Lagan Valley','Upper Bann','North Down','South Down',
                    'Newry Armagh','East Londonderry','Fermanagh South Tyrone',
                    'Foyle','West Tyrone','Mid Ulster')
constituencies_lowerdash <- as.character(sapply(constituencies, function(c) 
  paste(strsplit(tolower(c),' ')[[1]],collapse='-')))
constituencies_lower <- as.character(sapply(constituencies, function(c) 
  paste(strsplit(tolower(c),' ')[[1]],collapse='')))

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

combine_transfer_grids <- function(fullgrid,localgrid,alpha) {
  goodvals <- (!is.na(fullgrid))*alpha + (!is.na(localgrid))*(1-alpha)
  fullgrid[is.na(fullgrid)] <- 0
  localgrid[is.na(localgrid)] <- 0
  combgrid <- (fullgrid*alpha + localgrid*(1-alpha))/goodvals
  return(combgrid)
}

#e.g. Foyle has DUP->SDLP 0.66 because no unionists left
runConstit('Foyle',year,NULL,fullTransfersSelf2016,fullTransfersRest2016,3000)
runConstit('Foyle',year,NULL,localTransfersSelf2016[['foyle']],localTransfersRest2016[['foyle']],3000)
runConstit('Foyle',year,NULL,
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

get_party_map <- function() {
  partyMap <- vector('list')
  parties_with_multiple_cands <- c('Sinn Fein','Democratic Unionist Party','Ulster Unionist Party',
                                   'Social Democratic and Labour Party','Alliance Party',
                                   'People Before Profit Alliance','Independent',
                                   'Traditional Unionist Voice','NI Conservatives')
  for (party in parties_with_multiple_cands) {
    for (ind in 1:5) {
      partyMap[paste(party,ind)] <- party
    }
  }
  for (party in c('Animal Welfare Party','Cannabis Is Safer Than Alcohol',
                  'Cross-Community Labour Alternative','Democracy First',
                  'Green Party',
                  'Labour Alternative','NI Labour Representation Committee',
                  'Northern Ireland First','Progressive Unionist Party',
                  'South Belfast Unionists','UK Independence Party',
                  'Workers Party','votes_lost',parties_with_multiple_cands)) {
    partyMap[party] <- party
  }
  return(partyMap)
}
partyMap <- get_party_map()
#partyMap['Northern Ireland First'] <- 'Northern Ireland First'  #temporary

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
                          'votes_lost'='votes_lost')

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
                                     'South Belfast Unionists'))$percent)
}
getNationalistPercent <- function(firstPrefs) {
  firstPrefs$cand <- as.character(firstPrefs$cand)
  firstPrefs$cand <- as.character(sapply(firstPrefs$cand, function(n) ifelse(grepl('[:alnum:]* [1-5]',n),
                                                                             substr(n,1,nchar(n)-2),n)))
  sum(subset(firstPrefs, cand %in% c('Sinn Fein','Social Democratic and Labour Party',
                                     'People Before Profit Alliance','Workers Party'))$percent)
}
getIndependentPercent <- function(firstPrefs) {
  firstPrefs$cand <- as.character(firstPrefs$cand)
  firstPrefs$cand <- as.character(sapply(firstPrefs$cand, function(n) ifelse(grepl('[:alnum:]* [1-5]',n),
                                                                             substr(n,1,nchar(n)-2),n)))
  sum(subset(firstPrefs, cand == 'Independent')$percent)
}
#TODO update for 2011,2007 parties

for (constit in c('newryarmagh','northdown','southdown')) {
  cat(constit,'\n')
  for (year in c(2011,2016,2017)) {
    up <- getUnionistPercent(getFirstPrefs(paste0('fp',year,'/alt/',constit,year,'alt.csv')))
    np <- getNationalistPercent(getFirstPrefs(paste0('fp',year,'/alt/',constit,year,'alt.csv')))
    ip <- getIndependentPercent(getFirstPrefs(paste0('fp',year,'/alt/',constit,year,'alt.csv')))
    cat(year,sprintf('Uni = %.1f, Nat = %.1f, Ind = %.1f\n',up,np,ip))
  }
}
#When Ind goes up by >~2 and u/n down by similar amount, can assume that Ind as 
#  a group has become more u/n.
#Rule doesn't work for EL 16/17, LV 16/17, SD 11/16. 
#Best is Str 11/16 (would it work 16/17?), WT 11/16. NwA 11/16 both u/n but could work.
#  Fy 11/16 didn't matter as Ind never transferred.
#Maybe need to use prev local for Inds and then update that using the u/n change.

buildList <- function(vote, candidates, transfers, adjusts,
                      useSplit, transfersSelf, transfersRest) {
    
  while (length(vote) < length(candidates)) {

    stillToUse <- setdiff(candidates,vote)
    stillToUse <- union(stillToUse,'votes_lost')
    
    if (useSplit) {
        #print(partyMap[vote[length(vote)]])
        #print(stillToUse)
        if (partyMap[vote[length(vote)]] %in% sapply(stillToUse, function(p) partyMap[p])) {
            if (as.character(partyMap[vote[length(vote)]]) %in% rownames(transfersSelf)) {
                probs <- unlist(sapply(stillToUse, function(p) { transfersSelf[as.character(partyMap[vote[length(vote)]]),
                                                                           as.character(partyMap[p])]}))
            } else {
                print(paste("Don't know self transfer prob for ",vote[length(vote)]))
                print('Using 0.75 for self and dividing rest evenly')
                numNonSelfCands <- sum(unlist(sapply(stillToUse, function(p) 
                    ifelse(as.character(partyMap[p])==as.character(partyMap[vote[length(vote)]]),0,1))))
                probs <- unlist(sapply(stillToUse, function(p) 
                    ifelse(as.character(partyMap[p])==as.character(partyMap[vote[length(vote)]]),0.75,0.25/numNonSelfCands) ))
            }
            
        } else {
            probs <- unlist(sapply(stillToUse, function(p) { transfersRest[as.character(partyMap[vote[length(vote)]]),
                                                                       as.character(partyMap[p])]}))
        }
    } else {
        probs <- unlist(sapply(stillToUse, function(p) { transfers[as.character(partyMap[vote[length(vote)]]),
                                                                   as.character(partyMap[p])]}))
        #probs <- sapply(stillToUse, function(p) { transfers[as.character(partyMap[vote[1]]),
        #                                                    as.character(partyMap[p])]})
        #Not sure which one of the above is better
    }
    #print(vote[length(vote)])
    #print(stillToUse)
    #print(probs)
    
    #if (vote[length(vote)] == 'Alliance Party') {
    #  print('---1---')
    #  print(probs)
    #}
    #If same party present more than once, probs must be divided
    partiesOnly <- as.character(sapply(stillToUse, function(p) partyMap[as.character(p)]))
    partyCounts <- table(partiesOnly)
    for (m in 2:5) {
        multiples <- names(partyCounts)[partyCounts==m]
        for (p in multiples) {
            probs[grepl(p,stillToUse)] <- probs[grepl(p,stillToUse)] / m
        }
    }
    #but adjust for unequal share among candidates from that party
    for (i in seq(length(probs))) {
        if (stillToUse[i] != 'votes_lost') {
            probs[i] <- probs[i] * adjusts[adjusts$cand==stillToUse[i],'adjust']    
        }
    }
    
    #for now use a low-ish default. TODO: use grids with no gaps to avoid this
    # cat(sum(is.na(probs)),'NAs\n')
    # if (sum(is.na(probs)) > 0) {
    #   print(vote[length(vote)])
    #   print(probs)
    # }
    probs[is.na(probs)] <- 0.05
    
    #votes_lost stays at its true value and rest are normalised around it
    #probs <- probs*1/sum(probs)
    probs[names(probs) != 'votes_lost'] <- probs[names(probs) != 'votes_lost'] * 
      (1-probs[['votes_lost']]) / sum(probs[names(probs) != 'votes_lost'])
    #if (vote[length(vote)] == 'Sinn Fein 2') {
    #  print('---3---')
    #  print(probs)
    #}
    
    nextpref <- sample(stillToUse,1,prob=probs)
    if (nextpref == 'votes_lost') { 
      break
    } else {
      vote <- c(vote,nextpref)
    }
  }
  list(vote)
}

runConstit <- function(constitName, year, transfersAll, transfersSelf, transfersRest, nSample) {
  print(constitName)
  resultsFile <- paste0('fp',year,'/',str_replace_all(tolower(constitName),' ',''),year,'.csv')
  nSeats <- ifelse(year >= 2017, 5, 6)
  
  firstPrefs <- getFirstPrefs(resultsFile)
  if ( sum(firstPrefs$percent) < 98 ) { print('Below 100%!!!')}
  if ( sum(firstPrefs$percent) > 102 ) { print('Over 100%!!!')}
  
  #TODO count nat/uni vote
  # new_unionist_pc <- getUnionistPercent(firstPrefs)
  # new_nationalist_pc <- getNationalistPercent(firstPrefs)
  # prev_year <- list('2017'=2016,'2016'=2011,'2011'=2007)[[as.character(year)]]
  # prev_resultsFile <- paste0('fp',prev_year,'/',
  #                            str_replace_all(tolower(constitName),' ',''),prev_year,'.csv')
  # prev_first_prefs <- getFirstPrefs(prev_resultsFile)
  # prev_unionist_pc <- getUnionistPercent(prev_first_prefs)
  # prev_nationalist_pc <- getNationalistPercent(prev_first_prefs)
  # cat('Change in unionist pc: ',new_unionist_pc-prev_unionist_pc,'\n')
  # cat('Change in nationalist pc: ',new_nationalist_pc-prev_nationalist_pc,'\n')
  
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

  #ts <- proc.time()
  fullvotes <- c()
  #slow to run, so sampling the total ~40k at the moment
  for (vote in sample(votes, nSample, replace=FALSE)) {
    fullvotes <- c(fullvotes, buildList(vote, firstPrefs$cand, transfersAll,
                                        firstPrefs[,c('cand','adjust')],
                                        TRUE, transfersSelf, transfersRest))
  }
  #cat('Building list took ',(proc.time()-ts)[1],'s\n')
  # fullvotes <- foreach (vote = sample(votes, nSample, replace=FALSE), .combine=c) %dopar% 
  #     buildList(vote, firstPrefs$cand, transfersAll, firstPrefs[,c('cand','adjust')],
  #               TRUE, transfersSelf, transfersRest)

  #print("Ballot length summary:")
  #print(summary(sapply(fullvotes,length)))
  #print("Elected using STV:")
  #elected2 <- stv(fullvotes, nSeats)$winners[1:(nSeats+1)]
  elected2 <- my_stv(fullvotes, nSeats, verbose=TRUE)$winners  #testing
  elected3 <- sapply(elected2, function(p) { as.character(partyMap[p]) })
  #print(table(elected3[1:nSeats]))
  
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

#runConstit('South Antrim','fp2016/southAntrim2016.csv', 6, fullTransfers, fullTransfersSelf, fullTransfersRest, 1000)

runAllConstits <- function(year, transfersAll, transfersSelf, transfersRest) {
    constituencies <- c('Belfast North','Belfast West','Belfast East','Belfast South',
                        'North Antrim','East Antrim','South Antrim','Strangford',
                        'Lagan Valley','Upper Bann','North Down','South Down',
                        'Newry Armagh','East Londonderry','Fermanagh South Tyrone',
                        'Foyle','West Tyrone','Mid Ulster')
    
    assembly <- data.frame()
    for (constit in constituencies) {
        assembly <- rbind(assembly, runConstit(constit,year,
                                               transfersAll, transfersSelf, transfersRest,
                                               1000))
        #result <- runConstit(constit,paste0('fp',year,'/',tolower(gsub(' ','',constit)),year,'.csv'),
        #                     ifelse(year <= 2016, 6, 5), fullTransfers, fullTransfersSelf, fullTransfersRest, 500)
        # for (e in 1:4) {
        #     result2 <- runConstit(constit,paste0('fp',year,'/',tolower(gsub(' ','',constit)),year,'.csv'),
        #                          ifelse(year <= 2016, 6, 5), fullTransfers, fullTransfersSelf, fullTransfersRest, 500)
        #     if (!setequal(table(result2$party),table(result$party))) {
        #         print('Need bigger sample')
        #         break
        #     }
        # }
        
        if (nrow(subset(assembly,constit=='East Londonderry' & party=='Ulster Unionist Party'))==1) {
            #temporarily labelled Sugden a UUP for transfer purposes
            assembly$party <- as.character(assembly$party)
            assembly[assembly$constit=='East Londonderry' & assembly$party=='Ulster Unionist Party','party'] <- 'Independent'
            assembly$party <- factor(assembly$party)
        }
    }
    assembly
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


assembly <- runAllConstits(2016, fullTransfers2016, fullTransfersSelf2016, fullTransfersRest2016)
print(table(subset(assembly,seat<=6)$party))

verifyAssembly(assembly,2016)


#---- 2017
assembly2017 <- runAllConstits(2017, fullTransfers2016, fullTransfersSelf2016, fullTransfersRest2016)
print(table(subset(assembly2017, round<=5)$party))

verifyAssembly(assembly,2017)

#Updating Ind, 10k run:
#DUP +1, SF +1, SDLP -3, UUP +2, Gr -1. Miss on BS (Gr), FST (UUP), LV,ELD,UB (SDLP); 85/90

#With votes_lost, s-t 0.7, 10k run: DUP 30 (+2), SF 29 (+2), UUP 10, SDLP 9 (-3), All 8, Gr 2, PBP 1, Ind 1, TUV 0 (-1)
#  Miss on LV (UUP/SDLP), ELD (SF/SDLP), NA (DUP/TUV), UB (SF/SDLP), FST (DUP/UUP): 85/90

#As printed on Friday: 84/90 (missed 5/6 switches and wrong BN)
#From top 5 first prefs: 84/90 (miss 6 switches)

runConstitEnsemble <- function(constitName, year,
                               transfersAll, transfersSelf, transfersRest, 
                               nSample, nEnsemble, returnByParty=TRUE) {
    res <- list()
    res2 <- data.frame()
    for (e in seq(nEnsemble)) {
      capture.output(result <- runConstit(constitName,year,transfersAll,transfersSelf,transfersRest,nSample))
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
get_brier_for_constit <- function(constit_ens_df,constit,year) {
  truth <- read.csv('results_for_verification.csv')
  truth <- as.character(subset(truth, Year==year & Constituency==constit)$Elected_string)
  if (truth %in% constit_ens_df$result) {
    return((subset(constit_ens_df, result==truth)$prob - 1)**2)  
  } else {
    return(1)
  }
}

for (constit in constituencies[11:14]) {
  print(constit)
  res <- runConstitEnsemble(constit,2016,NULL,fullTransfersSelf2016,
                            fullTransfersRest2016,3000,20)
  print(res)
}

#2016 with 2016 self/rest transfers, 3000x20 ('Right' if truth is >80%):
#Right: BN, BE, BS, NA, EA, LV, ND,
#SD, NwA, FST, Fy, WT, MU (13/18)
#Can improve: 
#  BW only 60%? May be OK as it was close between SF and DUP.
#  EA 60%. Could get larger SDLP transfer from 2011. Was basically 3-way tie between UUP2,UKIP,SF.
#  SA only 60%
#  Str always have SDLP over UUP2 (will improve if treat Ind as more unionist;
#    not present last year but figure out by counting uni/nat vote and compare to last year)
#  UB close between SDLP, SF2 and UUP2. Should be split SDLP/SF2. (CISTA, Green, UKIP, UUP to SDLP a bit high; could see
#    UUP to SDLP was small in 2011). 60% (30% SDLP) comb 0.5.
#  EL 75% nw (SDLP, DUP3 sometimes missed), 100% comb 0.1.
#With nw rest,self gaps filled: 

#2016 with 2011 transfers, gaps filled:
#  Without gaps BN,BE,BS,Antrims,LV,ND,SD are good, 
#    BW DUP v SDLP, Str same problem as before, UB SDLP > UUP, 

#2016 with 2011 transfers, gaps filled, comb 0.5:



#Can use this to find best nSample by Brier
runAllConstitsEnsemble <- function(year, transfersAll, transfersSelf, transfersRest, 
                                   nSample, nEnsemble, alpha=1) {
  constituencies <- c('Belfast North','Belfast West','Belfast East','Belfast South',
                      'North Antrim','East Antrim','South Antrim','Strangford',
                      'Lagan Valley','Upper Bann','North Down','South Down',
                      'Newry Armagh','East Londonderry','Fermanagh South Tyrone',
                      'Foyle','West Tyrone','Mid Ulster')
  brier <- 0
  num_right <- 0
  for (constit in constituencies[1:4]) {
    res <- runConstitEnsemble(constit,year,transfersAll,
                              transfersSelf,transfersRest,nSample,nEnsemble)
    brier_add <- get_brier_for_constit(res,constit,year)
    cat(constit,brier_add,'\n')
    brier <- brier + brier_add
    if (brier_add <= 0.04) num_right <- num_right + 1
  }
  list(brier=brier, num_right=num_right)
}


