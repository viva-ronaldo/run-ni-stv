library(avr)
library(stringr)
library(doMC)
registerDoMC(cores=2)

#First try to reproduce all 2016 results, possibly also getting close to 
#  count by count progress. ~10k ballots is usually robust, but may prefer 
#  an ensemble of ~1-3k samples to capture uncertainty in transfer probs.

#Looks like D Kelly got about twice as many transfers from Alliance in 2017 as 2016
#TODO: some candidates go down in votes in STV, seems because empty votes need to also be dropped from weights

#Transfer matrix must take into account availability of each target at each
#  elimination/election. Particularly with regard to self transfers.
#  For now this matrix is a rough but usable one.
fullTransfers <- read.csv('transferMatrix_rough_nationwide.csv',header=TRUE,check.names=FALSE)
UBTransfers <- read.csv('transferMatrix_rough_upper-bann.csv',header=TRUE,check.names=FALSE)

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

buildList <- function(vote, candidates, transfers) {
  while (length(vote) < length(candidates)) {

    stillToUse <- setdiff(candidates,vote)
    if ('votes_lost' %in% names(transfers)) { stillToUse <- union(stillToUse,'votes_lost') } 
    
    #temporary**
    stillToUse <- setdiff(stillToUse,c('Northern Ireland First'))
    probs <- unlist(sapply(stillToUse, function(p) { transfers[as.character(partyMap[vote[length(vote)]]),
                                                        as.character(partyMap[p])]}))
    #probs <- sapply(stillToUse, function(p) { transfers[as.character(partyMap[vote[1]]),
    #                                                    as.character(partyMap[p])]})
    #Not sure which one of the above is better
    
    #If same party present more than once, prob is divided equally
    partiesOnly <- as.character(sapply(stillToUse, function(p) partyMap[p]))
    partyCounts <- table(partiesOnly)
    for (m in 1:5) {
        multiples <- names(partyCounts)[partyCounts==m]
        for (p in multiples) {
            probs[grepl(p,stillToUse)] <- probs[grepl(p,stillToUse)] / m
        }    
    }
    
    #for now use a low-ish default
    probs[is.na(probs)] <- 0.05
    probs[is.null(probs)] <- 0.05  #temporarily missing NI First
    
    #print(stillToUse)
    #print(probs)
    #if (sum(probs) == 0) { probs <- probs + 0.01}
    probs <- probs*1/sum(probs)
    #print(probs)
    
    nextpref <- sample(stillToUse,1,prob=probs)
    #maybe go no further
    if (nextpref == 'votes_lost') { 
      break
    } else {
      vote <- c(vote,nextpref)
    }
  }
  list(vote)
}

runConstit <- function(constitName, resultsFile, nSeats, fullTransfers) {
  print(constitName)
  
  firstPrefs <- getFirstPrefs(resultsFile)
  if ( sum(firstPrefs$percent) < 98 ) { print('Below 100%!!!')}
  if ( sum(firstPrefs$percent) > 102 ) { print('Over 100%!!!')}
  
  partiesOnly <- unique(as.character(sapply(firstPrefs$cand, function(p) partyMap[p])))
  #transfers <- fullTransfers[, names(fullTransfers) %in% c(partiesOnly,'votes_lost')]
  transfers <- fullTransfers
  
  votes <- c()
  for (i in seq(1,nrow(firstPrefs))) {
    votes <- c(votes, replist(as.character(firstPrefs$cand[i]), firstPrefs$number[i]))
  }
  #print("Elected using first preference order:")
  #print(sort(table(unlist(votes)),decreasing=TRUE)[1:nSeats])
  
  fullvotes <- c()
  #slow to run, so sampling the total ~40k at the moment
  #for (vote in sample(votes,500,replace=FALSE)) {
  #for (vote in votes) {
  #  fullvotes <- c(fullvotes, buildList(vote, firstPrefs$cand, transfers))
  #}
  fullvotes <- foreach (vote = sample(votes,10000,replace=FALSE), .combine=c) %dopar% 
      buildList(vote, firstPrefs$cand, transfers) 
  #fullvotes <- foreach (vote = votes, .combine=c) %dopar% 
  #    buildList(vote, firstPrefs$cand, transfers) 
  
  print("Ballot length summary:")
  print(summary(sapply(fullvotes,length)))
  print("Elected using STV:")
  #elected2 <- stv(fullvotes, nSeats)$winners[1:(nSeats+1)]
  elected2 <- my_stv(fullvotes, nSeats)$winners[1:(nSeats+1)]  #testing
  elected2 <- sapply(elected2, function(p) { as.character(partyMap[p]) })
  print(table(elected2[1:nSeats]))
  
  res2 <- data.frame()
  count <- 1
  for (el in elected2) { 
    res2 <- rbind(res2, data.frame(constit=constitName,round=count,party=el))
    count <- count + 1
  }
  res2
}

#runConstit('South Antrim','fp2016/southAntrim2016.csv', 6, fullTransfers)

runAllConstits <- function(nSeats, fullTransfers) {
  assembly <- data.frame()
  assembly <- rbind(assembly, runConstit('Belfast North','fp2016/belfastnorth2016.csv', 6, fullTransfers))
  assembly <- rbind(assembly, runConstit('Belfast West','fp2016/belfastwest2016.csv', 6, fullTransfers))
  assembly <- rbind(assembly, runConstit('Belfast East','fp2016/belfasteast2016.csv', 6, fullTransfers))
  assembly <- rbind(assembly, runConstit('Belfast South','fp2016/belfastsouth2016.csv', 6, fullTransfers))
  assembly <- rbind(assembly, runConstit('North Antrim','fp2016/northAntrim2016.csv', 6, fullTransfers))
  assembly <- rbind(assembly, runConstit('East Antrim','fp2016/eastAntrim2016.csv', 6, fullTransfers))
  assembly <- rbind(assembly, runConstit('South Antrim', 'fp2016/southAntrim2016.csv', 6, fullTransfers))
  assembly <- rbind(assembly, runConstit('Strangford','fp2016/strangford2016.csv', 6, fullTransfers))
  assembly <- rbind(assembly, runConstit('Lagan Valley','fp2016/laganvalley2016.csv', 6, fullTransfers))
  assembly <- rbind(assembly, runConstit('Upper Bann', 'fp2016/upperbann2016.csv', 6, fullTransfers))
  assembly <- rbind(assembly, runConstit('North Down','fp2016/northdown2016.csv', 6, fullTransfers))
  assembly <- rbind(assembly, runConstit('South Down','fp2016/southdown2016.csv', 6, fullTransfers))
  assembly <- rbind(assembly, runConstit('East Londonderry','fp2016/eastLD2016.csv', 6, fullTransfers))
  if (nrow(subset(assembly,constit=='East Londonderry' & party=='Ulster Unionist Party'))) {
    #temporarily labelled Sugden a UUP for transfer purposes
    assembly$party <- as.character(assembly$party)
    assembly[assembly$constit=='East Londonderry' & assembly$party=='Ulster Unionist Party','party'] <- 'Independent'
    assembly$party <- factor(assembly$party)
  }
  assembly <- rbind(assembly, runConstit('Fermanagh South Tyrone','fp2016/fermanaghST2016.csv', 6, fullTransfers))
  assembly <- rbind(assembly, runConstit('Foyle','fp2016/foyle2016.csv', 6, fullTransfers))
  assembly <- rbind(assembly, runConstit('West Tyrone','fp2016/westtyrone2016.csv', 6, fullTransfers))
  assembly <- rbind(assembly, runConstit('Mid Ulster','fp2016/midulster2016.csv', 6, fullTransfers))
  assembly <- rbind(assembly, runConstit('Newry and Armagh','fp2016/newryarmagh2016.csv', 6, fullTransfers))
  assembly
}
assembly <- runAllConstits(6,fullTransfers)
print(table(subset(assembly,round<=6)$party))

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

#---- 2017
runAllConstits2017 <- function(fullTransfers) {
  assembly <- data.frame()
  assembly <- rbind(assembly, runConstit('East Antrim', 'fp2017/eastAntrim2017.csv', 5, fullTransfers))   #2 DUP, 1 SF, 1 UUP, 1 All, =FP
  assembly <- rbind(assembly, runConstit('South Antrim', 'fp2017/southAntrim2017.csv', 5, fullTransfers))  #2 DUP, 1 SF, 1 UUP, 1 All, =FP
  assembly <- rbind(assembly, runConstit('Belfast West', 'fp2017/westB2017.csv', 5, fullTransfers))  #4 SF, 1 PBPA, =FP
  assembly <- rbind(assembly, runConstit('Strangford', 'fp2017/strangford2017.csv', 5, fullTransfers))  #3 DUP, 1 UUP, 1 All, =FP
  assembly <- rbind(assembly, runConstit('Belfast South', 'fp2017/southB2017.csv', 5, fullTransfers))  #1 DUP, 1 SF, 1 All, 1 SDLP, 1 Green, **change from FP
  assembly <- rbind(assembly, runConstit('North Down', 'fp2017/northDown2017.csv', 5, fullTransfers))  #2 DUP, 1 UUP, 1 All, 1 Green, =FP
  assembly <- rbind(assembly, runConstit('Foyle', 'fp2017/foyle2017.csv', 5, fullTransfers))  #2 SF, 2 SDLP, 1 DUP, =FP
  assembly <- rbind(assembly, runConstit('Mid Ulster', 'fp2017/midulster2017.csv', 5, fullTransfers))  #3 SF, 1 SDLP, 1 DUP, =FP
  assembly <- rbind(assembly, runConstit('Belfast East', 'fp2017/eastB2017.csv', 5, fullTransfers))  #2 All, 2 DUP, 1 UUP, =FP
  assembly <- rbind(assembly, runConstit('Lagan Valley', 'fp2017/laganv2017.csv', 5, fullTransfers))  #3 DUP, 1 UUP, 1 All, =FP
  assembly <- rbind(assembly, runConstit('Newry and Armagh', 'fp2017/newrya2017.csv', 5, fullTransfers))  #3 SF, 1 SDLP, 1 DUP, =FP
  assembly <- rbind(assembly, runConstit('Belfast North', 'fp2017/northB2017.csv', 5, fullTransfers))  #2 SF, 2 DUP, 1 All, **change from FP
  assembly <- rbind(assembly, runConstit('East Londonderry', 'fp2017/eastLD2017.csv', 5, fullTransfers))  #2 DUP, 2 SF, 1 Ind, =FP
  if (nrow(subset(assembly,constit=='East Londonderry' & party=='Ulster Unionist Party'))) {
    #temporarily labelled Sugden a UUP for transfer purposes
    assembly$party <- as.character(assembly$party)
    assembly[assembly$constit=='East Londonderry' & assembly$party=='Ulster Unionist Party','party'] <- 'Independent'
    assembly$party <- factor(assembly$party)
  }
  assembly <- rbind(assembly, runConstit('South Down', 'fp2017/southDown2017.csv', 5, fullTransfers))  #2 SF, 2 SDLP, 1 DUP, =FP
  assembly <- rbind(assembly, runConstit('North Antrim', 'fp2017/northAntrim2017.csv', 5, fullTransfers))  #2 DUP, 1 SF, 1 TUV, 1 UUP, =FP
  assembly <- rbind(assembly, runConstit('West Tyrone', 'fp2017/westtyrone2017.csv', 5, fullTransfers))  #3 SF, 1 DUP, 1 SDLP, =FP
  assembly <- rbind(assembly, runConstit('Upper Bann', 'fp2017/upperbann2017.csv', 5, fullTransfers)) #2 DUP, 2 SF, 1 UUP, =FP
  assembly <- rbind(assembly, runConstit('Fermanagh South Tyrone', 'fp2017/fermanagh2017.csv', 5, fullTransfers))  #2 DUP, 3 SF, =FP
  assembly
}
assembly2017 <- runAllConstits2017(fullTransfers)
print(table(subset(assembly2017,round<=5)$party))
#Updating Ind, 10k run:
#DUP +1, SF +1, SDLP -3, UUP +2, Gr -1. Miss on BS (Gr), FST (UUP), LV,ELD,UB (SDLP); 85/90

#With votes_lost, s-t 0.7, 10k run: DUP 30 (+2), SF 29 (+2), UUP 10, SDLP 9 (-3), All 8, Gr 2, PBP 1, Ind 1, TUV 0 (-1)
#  Miss on LV (UUP/SDLP), ELD (SF/SDLP), NA (DUP/TUV), UB (SF/SDLP), FST (DUP/UUP): 85/90

#As printed on Friday: 84/90 (missed 5/6 switches and wrong BN)
#From top 5 first prefs: 84/90 (miss 6 switches)

