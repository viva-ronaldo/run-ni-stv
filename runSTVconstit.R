library(avr)
library(stringr)
library(doMC)
registerDoMC(cores=2)
source('tmpfunctions.R')

#First try to reproduce all 2016 results, possibly also getting close to 
#  count by count progress. ~10k ballots is usually robust, but may prefer 
#  an ensemble of ~1-3k samples to capture uncertainty in transfer probs.
#Maybe do 10k sample, then if any come down to 1%, redo as ensemble of 10x1000?

#Looks like D Kelly got about twice as many transfers from Alliance in 2017 as 2016
#TODO: some candidates go down in votes in STV, seems because empty votes need to also be dropped from weights
#TODO: Strangford UUP2 vs SDLP is close - ~50 out of 5000. Strang transfers
#  show less than usual to UUP from DUP,TUV, more from Ind; much less from Ind,DUP, more from SF to SDLP.
#  UB16 partly due to self transfers too low, also helped by increased DUP->UUP.
#  SA wrong as All doesn't get enough from Green and SDLP too many from UUP3.   
#TODO: merge full and local transfers to keep coverage from full
#TODO: check special self transfer rule working.
#  Probably affects BW16, too much SF->SDLP. May also fix Strangford

#Transfer matrix must take into account availability of each target at each
#  elimination/election. Particularly with regard to self transfers.
#  For now this matrix is a rough but usable one.
fullTransfers <- read.csv('transferProbs/transferMatrix_rough_nationwide.csv',header=TRUE,check.names=FALSE)
fullTransfersSelf <- read.csv('transferProbs/transferMatrix_whenSelfAvailable_nationwide.csv',header=TRUE,check.names=FALSE)
fullTransfersRest <- read.csv('transferProbs/transferMatrix_whenSelfNotAvailable_nationwide.csv',header=TRUE,check.names=FALSE)
UBTransfers <- read.csv('transferProbs/transferMatrix_rough_upper-bann.csv',header=TRUE,check.names=FALSE)
StrangTransfers <- read.csv('transferProbs/transferMatrix_rough_strangford.csv',header=TRUE,check.names=FALSE)

#Rough transfer-friendliness:
#for (col in names(fullTransfers)) { cat(col,sum(fullTransfers[,col],na.rm=TRUE),'\n') }


partyMap <- vector('list')
for (party in c('Sinn Fein','Democratic Unionist Party','Ulster Unionist Party',
                'Social Democratic and Labour Party','Alliance Party',
                'People Before Profit Alliance','Independent',
                'Traditional Unionist Voice','NI Conservatives')) {
  for (ind in 1:5) {
    partyMap[paste(party,ind)] <- party
  }
}
for (party in c(names(fullTransfers),'votes_lost')) {
  partyMap[party] <- party
}
partyMap['Northern Ireland First'] <- 'Northern Ireland First'  #temporary

replist <- function(arg, times) lapply(seq(times), function(i) arg)

#TODO: add alternative getFirstPrefs to use electionsni.org files
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

buildList <- function(vote, candidates, transfers, adjusts,
                      transfersSelf, transfersRest) {
  useSplit <- TRUE
    
  while (length(vote) < length(candidates)) {

    stillToUse <- setdiff(candidates,vote)
    if ('votes_lost' %in% names(transfers)) { stillToUse <- union(stillToUse,'votes_lost') } 
    
    #temporary**
    stillToUse <- setdiff(stillToUse,c('Northern Ireland First'))
    
    if (useSplit) {
        if (partyMap[vote[length(vote)]] %in% sapply(stillToUse, function(p) partyMap[p])) {
            if (as.character(partyMap[vote[length(vote)]]) %in% rownames(transfersSelf)) {
                probs <- unlist(sapply(stillToUse, function(p) { transfersSelf[as.character(partyMap[vote[length(vote)]]),
                                                                           as.character(partyMap[p])]}))
            } else {
                print(paste("Don't know self transfer prob for ",vote[length(vote)]))
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
            probs[i] <- probs[i] / adjusts[adjusts$cand==stillToUse[i],'adjust']    
        }
    }
    
    #for now use a low-ish default
    probs[is.na(probs)] <- 0.05
    probs[is.null(probs)] <- 0.05  #temporarily missing NI First
    
    #TODO: possibly votes_lost should stay at its value and normalise the rest around it
    probs <- probs*1/sum(probs)
    #print(probs)
    
    nextpref <- sample(stillToUse,1,prob=probs)
    if (nextpref == 'votes_lost') { 
      break
    } else {
      vote <- c(vote,nextpref)
    }
  }
  list(vote)
}

runConstit <- function(constitName, resultsFile, nSeats, transfers, nSample) {
  print(constitName)
  
  firstPrefs <- getFirstPrefs(resultsFile)
  if ( sum(firstPrefs$percent) < 98 ) { print('Below 100%!!!')}
  if ( sum(firstPrefs$percent) > 102 ) { print('Over 100%!!!')}
  
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
  #for (vote in sample(votes, nSample, replace=FALSE)) {
  #for (vote in votes) {
  #  fullvotes <- c(fullvotes, buildList(vote, firstPrefs$cand, transfers, firstPrefs[,c('cand','adjust')]))
  #}
  fullvotes <- foreach (vote = sample(votes, nSample, replace=FALSE), .combine=c) %dopar% 
      buildList(vote, firstPrefs$cand, transfers, firstPrefs[,c('cand','adjust')],
                fullTransfersSelf, fullTransfersRest)  
  #fullvotes <- foreach (vote = votes, .combine=c) %dopar% 
  #    buildList(vote, firstPrefs$cand, transfers) 
  
  #print("Ballot length summary:")
  #print(summary(sapply(fullvotes,length)))
  #print("Elected using STV:")
  #elected2 <- stv(fullvotes, nSeats)$winners[1:(nSeats+1)]
  elected2 <- my_stv(fullvotes, nSeats, verbose=TRUE)$winners  #testing
  elected3 <- sapply(elected2, function(p) { as.character(partyMap[p]) })
  print(table(elected3[1:nSeats]))
  
  res2 <- data.frame()
  count <- 1
  for (e in seq(length(elected2))) { 
    res2 <- rbind(res2, data.frame(constit=constitName,seat=count,
                                   party=as.character(elected3[e]),
                                   cand=ifelse(grepl('[1-9]',as.character(elected2[e])),
                                               substr(as.character(elected2[e]),nchar(elected2[e]),nchar(elected2[e])),
                                               1)))
    count <- count + 1
  }
  res2
}

#runConstit('South Antrim','fp2016/southAntrim2016.csv', 6, fullTransfers, 1000)

runAllConstits <- function(year, fullTransfers) {
    constituencies <- c('Belfast North','Belfast West','Belfast East','Belfast South',
                        'North Antrim','East Antrim','South Antrim','Strangford',
                        'Lagan Valley','Upper Bann','North Down','South Down',
                        'Newry Armagh','East Londonderry','Fermanagh South Tyrone',
                        'Foyle','West Tyrone','Mid Ulster')
    
    assembly <- data.frame()
    for (constit in constituencies) {
        assembly <- rbind(assembly, runConstit(constit,
                                               paste0('fp',year,'/',tolower(gsub(' ','',constit)),year,'.csv'),
                                               ifelse(year <= 2016, 6, 5),
                                               fullTransfers,
                                               1000))
        #result <- runConstit(constit,paste0('fp',year,'/',tolower(gsub(' ','',constit)),year,'.csv'),
        #                     ifelse(year <= 2016, 6, 5), fullTransfers, 500)
        # for (e in 1:4) {
        #     result2 <- runConstit(constit,paste0('fp',year,'/',tolower(gsub(' ','',constit)),year,'.csv'),
        #                          ifelse(year <= 2016, 6, 5), fullTransfers, 500)
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
        actual <- data.frame(party=c('Democratic Unionist Party','Sinn Fein',
                                     'Ulster Unionist Party','Social Democratic and Labour Party',
                                     'Alliance Party','Green Party','People Before Profit Alliance',
                                     'Traditional Unionist Voice','Independent'),
                             seats=c(38,28,16,12,8,2,2,1,1))
    } else {
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

assembly <- runAllConstits(2016,fullTransfers)
print(table(subset(assembly,seat<=6)$party))

verifyAssembly(assembly,2016)


#Switches in 2016: UB, SA, SD, FST, WT, Foyle, ELD, LV, BW

#Now DUP +1, SF -1, SDLP -1, UUP +2 (maybe lucky on All SA)
#104/108?
#10k run: SF -1 (EA,UB), SDLP +2 (UB,SD,Foyle), UUP +1 (EA,SA,SD), All -1 (SA), PBP -1 (Foyle)
#103/108
#Incl votes_lost and ballot gen from p1, 10k run: DUP -1, SF -1, SDLP +2, UUP +1, Gr -1 
# 102/108: BS SDLP/Gr, EA All/SF, SA SDLP/All, UB SDLP/SF, FST SF/SDLP, NA SDLP/SF
#Incl votes_lost, ballot gen previous, self-t 0.7, 10k run: SDLP, UUP +2, All -1, PBP -1
# 103/108: EA UUP/SF, SA UUP/All, UB SDLP/SF, FST SF/SDLP, Foyle SDLP/PBP

#With transferMatrix_rough, these seem correct: BN, BE, NoA, EA, SA, LV, ND, SD, 
#  ELD, FST, F, WT, MU, NwA
#Harder: BW (DUP vs SF4), Strangford (SDLP vs UUP2, Ind involved), UB (SDLP vs UUP2),
#UB,BW right if use local transfers. Strangford transfers not right yet because need
#  to cover simultaneous eliminations.

#Now:
#Correct and never within 1%: BE, NwA, NA, BS, MU,
#Correct, only 1% between same party: LV, SD, WT, FST, Fo,
#Correct but includes a 1%: BN(?), ND(?), BW (good with self/rest), EL
#Wrong but down to 1%: EA, SA, Str, UB (with self/rest)

#---- 2017
assembly2017 <- runAllConstits(2017,fullTransfers)
print(table(subset(assembly2017, round<=5)$party))

verifyAssembly(assembly,2017)

#Updating Ind, 10k run:
#DUP +1, SF +1, SDLP -3, UUP +2, Gr -1. Miss on BS (Gr), FST (UUP), LV,ELD,UB (SDLP); 85/90

#With votes_lost, s-t 0.7, 10k run: DUP 30 (+2), SF 29 (+2), UUP 10, SDLP 9 (-3), All 8, Gr 2, PBP 1, Ind 1, TUV 0 (-1)
#  Miss on LV (UUP/SDLP), ELD (SF/SDLP), NA (DUP/TUV), UB (SF/SDLP), FST (DUP/UUP): 85/90

#As printed on Friday: 84/90 (missed 5/6 switches and wrong BN)
#From top 5 first prefs: 84/90 (miss 6 switches)

runConstitEnsemble <- function(constitName, resultsFile, nSeats, 
                               transfers, nSample, nEnsemble) {
    ensResults <- list()
    for (e in seq(nEnsemble)) {
        result <- runConstit(constitName,resultsFile,nSeats,transfers,nSample)
        result$candp <- paste(result$party,result$cand)
        for (c in result$candp) {
            if (c %in% names(ensResults)) {
                ensResults[[c]] <- ensResults[[c]] + 1
            } else {
                ensResults[[c]] <- 1
            }
        }
    }
    sapply(names(ensResults),function(c) ensResults[[c]]/nEnsemble)
}

