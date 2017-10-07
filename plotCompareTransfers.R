#Plot transfers and changes

#TODO Deal with votes_lost vs Count, following from nw_numPs.
#TODO line plot(s) to highlight particular transfer changes over time:
#  UUP/SDLP (both up over time), UUP/DUP (UUP did esp well in 2016), 
#    nat/Alliance, uni/Alliance, 
#    SDLP/SF (SDLP not transferring to SF as much lately, going PBP instead)
#TODO separate routine to count up votes on final round of Count.csv
#  to calculate weighted fraction lost. Compare this by constit to number
#  of counts needed, or number of candidates?

plot_transfer_grid_ordered <- function(grid,title,subtitle,party_order=NULL) {
  if (!is.null(party_order)) {
    grid_ordered <- grid
    grid_ordered$transferFrom <- factor(grid_ordered$transferFrom,levels=party_order)
    grid_ordered$transferTo <- factor(grid_ordered$transferTo,levels=c(party_order,'votes_lost'))  
  } else { grid_ordered <- grid}
  ggplot(grid_ordered,aes(transferTo,transferFrom)) + 
    geom_tile(aes(fill=fraction)) + theme(axis.text.x=element_text(angle=45,hjust=1)) +
    scale_fill_distiller(palette='Spectral',direction=-1,limits=c(0,0.6)) + 
    labs(title=title,subtitle=subtitle,y='Transferring party',x='Transfer destination',fill='Fraction')
}

plot_differences_grid <- function(grid,title,subtitle) {
  ggplot(grid,aes(transferTo,transferFrom)) + 
    geom_tile(aes(fill=fraction_change)) + theme(axis.text.x=element_text(angle=45,hjust=1)) +
    scale_fill_distiller(palette='RdBu',direction=-1,
                         limits=c(-max(abs(grid$fraction_change),na.rm=T),
                                  max(abs(grid$fraction_change),na.rm=T))) + 
    labs(title=title,subtitle=subtitle,
         y='Transferring party',x='Transfer destination',fill='Fraction change')
}

read_and_melt_grid <- function(filename) {
  grid <- read.csv(filename,header=TRUE,check.names=FALSE)
  grid$transferFrom <- as.character(party_short_names[row.names(grid)])
  grid <- melt(grid,id.vars='transferFrom',variable.name='transferTo',value.name='fraction')
  grid$transferTo <- as.character(party_short_names[as.character(grid$transferTo)])
  grid
}
read_and_melt_4_grids <- function(filetype,situation) {
  filebit1 <- ifelse(filetype=='counts', 'counts', ifelse(filetype=='numPs', 'numPartiesLeft', 'transferMatrix'))
  filebit2 <- ifelse(situation=='self','whenSelfAvailable',
                     ifelse(situation=='noself','whenSelfNotAvailable','all'))
  g2007 <- read_and_melt_grid(paste0('transferProbs2007/',filebit1,'_',filebit2,'_nationwide.csv'))
  g2011 <- read_and_melt_grid(paste0('transferProbs2011/',filebit1,'_',filebit2,'_nationwide.csv'))
  g2016 <- read_and_melt_grid(paste0('transferProbs2016/',filebit1,'_',filebit2,'_nationwide.csv'))
  g2017 <- read_and_melt_grid(paste0('transferProbs2017/',filebit1,'_',filebit2,'_nationwide.csv'))
  g_all <- rbind(data.frame(g2007,year=2007),data.frame(g2011,year=2011),
                 data.frame(g2016,year=2016),data.frame(g2017,year=2017))
  g_all$year <- factor(g_all$year)
  g_all
}

fullTra2016Self <- read_and_melt_grid('transferProbs2016/transferMatrix_whenSelfAvailable_nationwide.csv')
fullTra2016Rest <- read_and_melt_grid('transferProbs2016/transferMatrix_whenSelfNotAvailable_nationwide.csv')

main_targets <- c('DUP','SF','UUP','SDLP','Alliance','Green','UKIP','PUP','TUV','PBP','Ind','votes_lost')
fullTra2016AllMain <- subset(fullTra2016All,transferTo %in% main_targets & transferFrom %in% main_targets)
fullTra2016RestMain <- subset(fullTra2016Rest,transferTo %in% main_targets & transferFrom %in% main_targets)
fullTra2016SelfMain <- subset(fullTra2016Self,transferTo %in% main_targets & transferFrom != 'Ind')

plot_transfer_grid_ordered(fullTra2016RestMain,
                           'Transfer fractions when no self-transfer available',
                           '2016 nationwide')

plot_transfer_grid_ordered(fullTra2016RestMain,
                           'Transfer fractions when no self-transfer available',
                           '2016 nationwide',
                           c('DUP','UUP','TUV','UKIP','PUP','Ind','Alliance','SDLP','Green','PBP','SF'))

#compare to 2011

fullTra2011Self <- read_and_melt_grid('transferProbs2011/transferMatrix_whenSelfAvailable_nationwide.csv')
fullTra2011Rest <- read_and_melt_grid('transferProbs2011/transferMatrix_whenSelfNotAvailable_nationwide.csv')
fullTra2011SelfMain <- subset(fullTra2011Self,transferTo %in% main_targets & transferFrom != 'Ind')
fullTra2011RestMain <- subset(fullTra2011Rest,transferTo %in% main_targets & transferFrom %in% main_targets)

plot_transfer_grid_ordered(fullTra2011RestMain,
                           'Transfer fractions when no self-transfer available',
                           '2011 nationwide',
                           c('DUP','UUP','TUV','UKIP','PUP','Ind','Alliance','SDLP','Green','PBP','SF'))

#change from 2011 to 2016
fullTra2016minus2011RestMain <- merge(fullTra2011RestMain,fullTra2016RestMain,
                                      by=c('transferFrom','transferTo'),all=FALSE,
                                      suffixes=c('_2011','_2016'))
fullTra2016minus2011RestMain$fraction_change <- fullTra2016minus2011RestMain$fraction_2016 -
  fullTra2016minus2011RestMain$fraction_2011

plot_differences_grid(fullTra2016minus2011RestMain,
                      'Change in transfers, no self-transfer available',
                      '2016 - 2011, nationwide')
#Increases in DUP->UUP, UUP->SDLP, Alliance->Green; decreases SDLP->Alliance
fullTra2016minus2011RestMain %>% group_by(transferTo) %>% 
  summarise(meanfrac11=mean(fraction_2011,na.rm=T),meanfrac16=mean(fraction_2016,na.rm=T)) %>% 
  mutate(meanFracIncr=meanfrac16-meanfrac11) %>% arrange(-meanFracIncr)
#UKIP,PUP were more transferred to; Alliance,SDLP less transferred to (totals don't seem to add up?).
mean(subset(fullTra2016minus2011RestMain,transferTo=='votes_lost')$fraction_change)
#Seems to have been fewer lost votes in 2016 - from 19% to 12% of available transfers

#and compare to 2017
fullTra2017Self <- read_and_melt_grid('transferProbs2017/transferMatrix_whenSelfAvailable_nationwide.csv')
fullTra2017Rest <- read_and_melt_grid('transferProbs2017/transferMatrix_whenSelfNotAvailable_nationwide.csv')
fullTra2017SelfMain <- subset(fullTra2017Self,transferTo %in% main_targets & transferFrom != 'Ind')
fullTra2017RestMain <- subset(fullTra2017Rest,transferTo %in% main_targets & transferFrom %in% main_targets)

plot_transfer_grid_ordered(fullTra2017RestMain,
                           'Transfer fractions when no self-transfer available',
                           '2017 nationwide',
                           c('DUP','UUP','TUV','UKIP','PUP','Ind','Alliance','SDLP','Green','PBP','SF'))

fullTra2017minus2016RestMain <- merge(fullTra2016RestMain,fullTra2017RestMain,
                                      by=c('transferFrom','transferTo'),all=FALSE,
                                      suffixes=c('_2016','_2017'))
fullTra2017minus2016RestMain$fraction_change <- fullTra2017minus2016RestMain$fraction_2017 -
  fullTra2017minus2016RestMain$fraction_2016

plot_differences_grid(fullTra2017minus2016RestMain,
                      'Change in transfer, no self-transfer available',
                      '2017 - 2016, nationwide')

#----- other plots moved to rmd doc
#Change in nat/uni transfer over time
collected_transfers_nu <- rbind(data.frame(fullTra2007All,year=2007),
                              data.frame(fullTra2011All,year=2011),
                              data.frame(fullTra2016All,year=2016),
                              data.frame(fullTra2017All,year=2017))
collected_transfers_nu <- subset(collected_transfers_nu, !is.na(fraction))
uni_parties <- c('DUP','NI Cons','PUP','SBU','TUV','UUP','UKUP')
nat_parties <- c('SF','SDLP','RSF','PBP','Workers')
collected_transfers_nu$fromUni <- collected_transfers_nu$transferFrom %in% uni_parties
collected_transfers_nu$fromNat <- collected_transfers_nu$transferFrom %in% nat_parties
collected_transfers_nu$toUni <- collected_transfers_nu$transferTo %in% uni_parties
collected_transfers_nu$toNat <- collected_transfers_nu$transferTo %in% nat_parties
uni_fracs <- collected_transfers_nu %>% group_by(transferFrom,year) %>% filter(fromUni) %>%
  summarise(uniSelf=sum(ifelse(toUni,fraction,0))) %>% 
  group_by(year) %>% summarise(Unionist=mean(uniSelf))
nat_fracs <- collected_transfers_nu %>% group_by(transferFrom,year) %>% filter(fromNat) %>%
  summarise(natSelf=sum(ifelse(toNat,fraction,0))) %>% 
  group_by(year) %>% summarise(Nationalist=mean(natSelf))
uni_nat_fracs <- melt(merge(uni_fracs,nat_fracs,by='year'),id.vars='year')
#uni_nat_fracs <- melt(rbind(uni_fracs,nat_fracs),id.vars=c('year','bloc'))

ggplot(uni_nat_fracs) + geom_point(aes(factor(year),value,colour=variable),size=2.5) +
  geom_line(aes(factor(year),value,colour=variable,group=variable),size=1) + 
  ylim(0.5,0.7) +
  labs(y='Fraction transferred',x='Year',colour='Bloc',
       title='Votes transferred within blocs (all counts)') +
  scale_colour_manual(values = c('Unionist'='orange','Nationalist'='green3'))
#ggplot(subset(uni_nat_fracs,bloc=='Uni')) + geom_col(aes(year,value,fill=variable))

uni_fracs_2 <- collected_transfers_nu %>% group_by(transferFrom,year) %>% filter(fromUni) %>%
  summarise(uniSelf=sum(ifelse(toUni,fraction,0)),
            uniLost=sum(ifelse(transferTo=='votes_lost',fraction,0)),
            uniNat=sum(ifelse(toNat,fraction,0))) %>%
  group_by(year) %>% summarise(bloc='Uni',within=mean(uniSelf),lost=mean(uniLost),across=mean(uniNat))
nat_fracs_2 <- collected_transfers_nu %>% group_by(transferFrom,year) %>% filter(fromNat) %>%
  summarise(natSelf=sum(ifelse(toNat,fraction,0)),
            natLost=sum(ifelse(transferTo=='votes_lost',fraction,0)),
            natUni=sum(ifelse(toUni,fraction,0))) %>%
  group_by(year) %>% summarise(bloc='Nat',within=mean(natSelf),lost=mean(natLost),across=mean(natUni))

#-----
#votes lost by constituency
vlconstit2007 <- read.csv('transferProbs2007/transfersByRow_2007_all.csv') %>% group_by(Constituency,Count_Number) %>% 
  summarise(votes_lost=1-sum(fracReceived)) %>% summarise(votes_lost=mean(votes_lost))
vlconstit2011 <- read.csv('transferProbs2011/transfersByRow_2011_all.csv') %>% group_by(Constituency,Count_Number) %>% 
  summarise(votes_lost=1-sum(fracReceived)) %>% summarise(votes_lost=mean(votes_lost))
vlconstit2016 <- read.csv('transferProbs2016/transfersByRow_2016_all.csv') %>% group_by(Constituency,Count_Number) %>% 
  summarise(votes_lost=1-sum(fracReceived)) %>% summarise(votes_lost=mean(votes_lost))
vlconstit2017 <- read.csv('transferProbs2017/transfersByRow_2017_all.csv') %>% group_by(Constituency,Count_Number) %>% 
  summarise(votes_lost=1-sum(fracReceived)) %>% summarise(votes_lost=mean(votes_lost))
votes_lost_constit_2007_2017 <- rbind(data.frame(vlconstit2007,year=2007),
                                      data.frame(vlconstit2011,year=2011),
                                      data.frame(vlconstit2016,year=2016),
                                      data.frame(vlconstit2017,year=2017))
rm(vlconstit2007,vlconstit2011,vlconstit2016,vlconstit2017)

#not a good plot but there is a range; NwA seems worst at 30% av, but may depend on count details.

#--------------
#Interesting individual changes
# fullTra2007Rest <- read_and_melt_grid('transferProbs2007/transferMatrix_whenSelfNotAvailable_nationwide.csv')
# fullTra2011Rest <- read_and_melt_grid('transferProbs2011/transferMatrix_whenSelfNotAvailable_nationwide.csv')
# fullTra2016Rest <- read_and_melt_grid('transferProbs2016/transferMatrix_whenSelfNotAvailable_nationwide.csv')
# fullTra2017Rest <- read_and_melt_grid('transferProbs2017/transferMatrix_whenSelfNotAvailable_nationwide.csv')
# collected_transfers_all <- rbind(data.frame(fullTra2007Rest,year=2007),
#                                  data.frame(fullTra2011Rest,year=2011),
#                                  data.frame(fullTra2016Rest,year=2016),
#                                  data.frame(fullTra2017Rest,year=2017))
# collected_transfers_all$year <- factor(collected_transfers_all$year)
collected_transfers_rest <- read_and_melt_4_grids('matrix','noself')
collected_transfers_rest$fromTo <- paste(collected_transfers_rest$transferFrom,
                                        collected_transfers_rest$transferTo,sep='-')
#TODO weight by FPs?

plot_transfer_pair_over_time <- function(collected_transfers_rest,p1,p2) {
  ggplot(subset(collected_transfers_rest,transferFrom %in% c(p1,p2) &
                  transferTo %in% c(p1,p2) &
                  transferTo != transferFrom)) + 
    geom_point(aes(year,fraction,colour=fromTo)) +
    geom_line(aes(year,fraction,colour=fromTo,group=fromTo))
}


#---------------
#also can do interesting local - national.

#most votes lost
data.frame(party=row.names(fullTra2016Rest),fracLost=fullTra2016Rest[,transferTo],stringsAsFactors=FALSE) %>%
    arrange(fracLost) %>% ggplot() + geom_bar(aes(party,fracLost),stat='identity') +
    labs(x='',y='Fraction of votes not transferred')

