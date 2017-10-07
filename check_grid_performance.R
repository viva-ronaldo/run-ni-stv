#Go through long list and use my grid to predict each transfer

#TODO allow grid to be a combined nw+local one and use to test combination.
#TODO votes_lost currently overestimated by 20%, moreso for low count number. Much less of a problem for self/rest.
#  Seems parties transfer a bit more to favourites when no self available than expected, e.g.
#  subset(res,from=='Alliance Party' & Self_Available=='False') %>% group_by(to) %>% summarise(mean(numP),mean(mine),mean(true))

#Mean(abs(mine-true)) for all vs self/rest: 2007 0.046 vs 0.048, 2011 0.046 vs 0.050, 2016 0.050 vs 0.054
#  So conversion of rest to self doesn't work perfectly; though this is all for same year evaluation.

get_grid_residuals <- function(long,grid) {
  long_agg_party <- long %>% group_by(Constituency,Count_Number,Transferrer,Target,Self_Available,Number_Of_Target_Parties_Left) %>% 
    summarise(fracReceived=sum(fracReceived),targetVotes=mean(Target_Total_Votes_So_Far))
  losts <- long_agg_party %>% group_by(Constituency,Count_Number) %>% summarise(votes_lost=1-sum(fracReceived))
  long_agg_party <- merge(long_agg_party,losts,by=c('Constituency','Count_Number'))
    
  unique_counts <- long_agg_party %>% group_by(Constituency,Count_Number) %>% 
    summarise(Transferrer=first(Transferrer),numP=first(Number_Of_Target_Parties_Left),
              Self_Available=first(Self_Available))
  
  res <- data.frame()
  
  for (i in seq(nrow(unique_counts))) {
    parties <- subset(long_agg_party,Constituency==unique_counts[i,]$Constituency &
                        Count_Number==unique_counts[i,]$Count_Number)$Target
    parties <- c(as.character(parties),'votes_lost')
    transferrer <- as.character(unique_counts[i,]$Transferrer)
    myprobs <- grid[transferrer,parties]
    myprobs <- myprobs / sum(myprobs)
    
    constit=as.character(unique_counts[i,]$Constituency)
    count=as.integer(unique_counts[i,]$Count_Number)
    numP=as.integer(unique_counts[i,]$numP)
    Self_Available=as.character(unique_counts[i,]$Self_Available)
    for (party in parties) {
      res <- rbind(res,data.frame(constit=constit, count=count, numP=numP,
                                  targetVotes=ifelse(party!='votes_lost',
                                                     subset(long_agg_party,Constituency==constit &
                                                       Count_Number==count & Target==party)$targetVotes,
                                                     NA),
                                  from=transferrer, to=party, Self_Available=Self_Available,
                                  mine=myprobs[[party]],
                                  true=ifelse(party!='votes_lost',
                                              subset(long_agg_party,Constituency==constit &
                                                       Count_Number==count & Target==party)$fracReceived,
                                              subset(long_agg_party,Constituency==constit &
                                                       Count_Number==count)[1,'votes_lost'])))
    }  
  }
  res$self <- as.character(res$from)==as.character(res$to)
  res
}

get_selfrest_grid_residuals <- function(long,self,rest) {
  long_agg_party <- long %>% group_by(Constituency,Count_Number,Transferrer,Target,Self_Available,Number_Of_Target_Parties_Left) %>% 
    summarise(fracReceived=sum(fracReceived),targetVotes=mean(Target_Total_Votes_So_Far))
  losts <- long_agg_party %>% group_by(Constituency,Count_Number) %>% summarise(votes_lost=1-sum(fracReceived))
  long_agg_party <- merge(long_agg_party,losts,by=c('Constituency','Count_Number'))
  
  unique_counts <- long_agg_party %>% group_by(Constituency,Count_Number) %>% 
    summarise(Transferrer=first(Transferrer),numP=first(Number_Of_Target_Parties_Left),
              Self_Available=first(Self_Available))
  
  res <- data.frame()
  
  for (i in seq(nrow(unique_counts))) {
    parties <- subset(long_agg_party,Constituency==unique_counts[i,]$Constituency &
                        Count_Number==unique_counts[i,]$Count_Number)$Target
    parties <- c(as.character(parties),'votes_lost')
    transferrer <- as.character(unique_counts[i,]$Transferrer)
    
    constit=as.character(unique_counts[i,]$Constituency)
    count=as.integer(unique_counts[i,]$Count_Number)
    numP=as.integer(unique_counts[i,]$numP)
    Self_Available=as.character(unique_counts[i,]$Self_Available)
    for (party in parties) {
      if (Self_Available) {
        myprobs <- self[transferrer,parties]
      } else {
        myprobs <- rest[transferrer,parties]
      }
      myprobs <- myprobs / sum(myprobs)  
      
      res <- rbind(res,data.frame(constit=constit, count=count, numP=numP,
                                  targetVotes=ifelse(party!='votes_lost',
                                                     subset(long_agg_party,Constituency==constit &
                                                              Count_Number==count & Target==party)$targetVotes,
                                                     NA),
                                  from=transferrer, to=party, Self_Available=Self_Available,
                                  mine=myprobs[[party]],
                                  true=ifelse(party!='votes_lost',
                                              subset(long_agg_party,Constituency==constit &
                                                       Count_Number==count & Target==party)$fracReceived,
                                              subset(long_agg_party,Constituency==constit &
                                                       Count_Number==count)[1,'votes_lost'])))
    }  
  }
  res$self <- as.character(res$from)==as.character(res$to)
  res
}

#--- 2007
long <- read.csv('transferProbs2007/transfersByRow_2007_all.csv')
grid <- fullTra2007All
self <- fullTra2007Self
rest <- fullTra2007Rest
res <- get_grid_residuals(long,grid)
res <- get_selfrest_grid_residuals(long,self,rest)

ggplot(res) + geom_point(aes(mine,true,colour=self)) + geom_abline(slope=1,intercept=0)
#Ind are most widely spread
ggplot(subset(res,from=='Independent')) + geom_point(aes(mine,true,colour=self)) + geom_abline(slope=1,intercept=0)
#Others are better
ggplot(subset(res,from=='Ulster Unionist Party')) + geom_point(aes(mine,true,colour=self)) + geom_abline(slope=1,intercept=0)
ggplot(subset(res,to=='votes_lost')) + geom_point(aes(mine,true,colour=Self_Available)) + geom_abline(slope=1,intercept=0)
#votes_lost overestimated on average, 0.171 vs 0.145; OK for Self_Available, high for not available

res %>% group_by(numP) %>% summarise(n=n(),mean(mine-true))
#Big underestimate for a few cases of numP=1, otherwise good. Overall -0.005.
res %>% group_by(self) %>% summarise(n=n(),mean(mine-true))
#Underestimate self by 0.03.
res %>% mutate(resid=mine-true) %>% ggplot() + geom_point(aes(targetVotes,resid)) +
  geom_smooth(aes(targetVotes,resid),method='lm',se=FALSE)
#Slightly underestimate when target has more fps
res$resid <- res$mine-res$true
glm(resid ~ targetVotes, data=res)  #could only alter probs by ~0.03

res %>% group_by(to) %>% summarise(n=n(),mostUnder=round(min(mine-true),3),
                                   meanResid=round(mean(mine-true),3),
                                   mostOver=round(max(mine-true),3)) %>% filter(n>5)
#All party means within 0.01.
#  Big parties can be +/- 0.2-0.3.

#--- 2011
long <- read.csv('transferProbs2011/transfersByRow_2011_all.csv')
grid <- fullTra2011All
self <- fullTra2011Self
rest <- fullTra2011Rest
res <- get_grid_residuals(long,combine_transfer_grids(fullTra2011All,localTransfersAll2011[['fermanagh-south-tyrone']],0.1))
res <- get_selfrest_grid_residuals(long,
                                   combine_transfer_grids(fullTra2011Self,localTransfersSelf2011[['fermanagh-south-tyrone']],0.1),
                                   combine_transfer_grids(fullTra2011Rest,localTransfersRest2011[['fermanagh-south-tyrone']],0.1))

ggplot(res) + geom_point(aes(mine,true,colour=self)) + geom_abline(slope=1,intercept=0)
ggplot(subset(res,to=='votes_lost')) + geom_point(aes(mine,true,colour=self)) + geom_abline(slope=1,intercept=0)
#votes_lost overestimated on average, 0.201 vs 0.176; slightly low Self_Available, high not available

res %>% group_by(numP) %>% summarise(n=n(),mean(mine-true))
#Underestimate numP=1,2. Overall -0.005.
res %>% group_by(self) %>% summarise(n=n(),mean(mine-true))
#Overestimate self by 0.01, underestimate rest by -0.006.
res %>% mutate(resid=mine-true) %>% ggplot() + geom_point(aes(targetVotes,resid)) +
  geom_smooth(aes(targetVotes,resid),method='lm',se=FALSE)
#Slightly underestimate when target has more fps

#--- 2016
long <- read.csv('transferProbs2016/transfersByRow_2016_all.csv')
grid <- fullTra2016All
self <- fullTra2016Self
rest <- fullTra2016Rest
res <- get_grid_residuals(long,grid)
res <- get_selfrest_grid_residuals(long,self,rest)

ggplot(res) + geom_point(aes(mine,true,colour=self)) + geom_abline(slope=1,intercept=0)
ggplot(subset(res,to=='votes_lost')) + geom_point(aes(mine,true)) + geom_abline(slope=1,intercept=0)
#votes_lost overestimated on average, 0.203 vs 0.165; slightly low Self_Available, high not available

res %>% group_by(numP,self) %>% summarise(n=n(),mean(mine-true))
#Underestimate numP=1,2. Overall -0.007.
res %>% group_by(self) %>% summarise(n=n(),mean(mine-true))
#Overestimate self by 0.012, underestimate rest by -0.006.
res %>% group_by(numP,self) %>% summarise(n=n(),resid=mean(mine-true)) %>% 
  ggplot() + geom_point(aes(numP,resid,colour=self,size=n))
#Not much to be done for self
res %>% mutate(resid=mine-true) %>% ggplot() + geom_point(aes(targetVotes,resid)) +
  geom_smooth(aes(targetVotes,resid),method='lm',se=FALSE)
#negligible dependence on target votes

#--------- See if local variations are carried through from previous year
long <- read.csv('transferProbs2007/transfersByRow_2007_all.csv')
grid <- fullTra2007All
res <- get_grid_residuals(long,grid)
res$year <- 2007
long <- read.csv('transferProbs2011/transfersByRow_2011_all.csv')
grid <- fullTra2007All
res2 <- get_grid_residuals(long,grid)
res2$year <- 2011
long <- read.csv('transferProbs2016/transfersByRow_2016_all.csv')
grid <- fullTra2011All
res3 <- get_grid_residuals(long,grid)
res3$year <- 2016
res <- rbind(res,res2,res3)
rm(res2,res3)

res$resid <- res$mine - res$true
res <- subset(res,numP >= 3)
res <- res %>% group_by(constit,from,to,year) %>% summarise(resid=mean(resid))
res <- as.data.table(arrange(res,year))
res[, prev_resid := shift(resid,n=1,type='lag'), by=.(constit,from,to)]
ggplot(res) + geom_point(aes(prev_resid,resid)) + 
  geom_smooth(aes(prev_resid,resid),method='lm') + geom_abline(slope=1,intercept=0)
cor(res$resid,res$prev_resid,use='complete.obs') #0.32 for 2007-2016

#------- Can first prefs tell us anything?

all_fps <- data.frame()
for (year in c(2007,2011,2016)) {
  for (constit in constituencies_lowerdash) {
    resultsFile <- paste0('fp',year,'/alt/',str_replace_all(constit,'-',''),year,'alt.csv')
    fps <- getFirstPrefs(resultsFile)
    fps <- subset(fps)
    fps$cand <- sapply(as.character(fps$cand), function(p) partyMap[[p]])
    fps <- fps %>% group_by(cand) %>% summarise(fps=sum(percent))
    fps$constit <- constit
    fps$year <- year
    all_fps <- rbind(all_fps,fps)
  }
}
all_fps <- data.table(all_fps)
all_fps[, fps_nw := mean(fps), by=.(cand,year)]

res_party <- res %>% group_by(constit,year,to) %>% summarise(true=mean(true),resid=mean(resid),
                                                        self=first(self))

res_party <- merge(res_party,all_fps,by.x=c('constit','to','year'),by.y=c('constit','cand','year'))
#fps and true are strongly positively correlated, but this is mostly party differences
#fps and resid are negatively correlated, i.e. I underestimate transfers more when fps are high.
#  This is mostly at the high fps end, many of which may be self transfers.
ggplot(res_party) + geom_point(aes(fps,resid,colour=self)) + 
  geom_smooth(aes(fps,resid),method='loess')
cor(res_party$resid, res_party$fps)
cor(res_party$resid, res_party$fps_nw)
cor(res_party$resid, res_party$fps-res_party$fps_nw)

res_party <- as.data.table(arrange(res_party,year))
res_party[, delta_fps := fps - shift(fps,n=1,type='lag'), by=.(constit,to)]
res_party[, delta_fps_nw := fps_nw - shift(fps_nw,n=1,type='lag'), by=.(constit,to)]
cor(res_party$delta_fps, res_party$resid, use='complete.obs')
ggplot(res_party) + geom_point(aes(delta_fps,resid))
#very little dependence on change in fps over last time

res_party[, prev_resid := shift(resid,n=1,type='lag'), by=.(constit,to)]
cor.test(res_party$resid, res_party$prev_resid)
ggplot(res_party) + geom_point(aes(prev_resid,resid))

#So have two ways of reducing errors: prev_resid, which is actually using
#  local transfers from last time, and fps, via a glm.
glm(resid ~ prev_resid + fps, data=subset(res_party,year > 2007))
#But note any correction will be different if using previous year's grid
#  to form residuals.
