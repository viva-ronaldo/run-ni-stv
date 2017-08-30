#Plot transfers and changes

#TODO calculate countrywide all,self,rest matrices for 2007,11,16,17
#TODO basic grid plots for each
#TODO difference plots 16-11, 17-16
#  - votes lost, self and rest x 3 years
#  - self transfer by party x 3 years
#  - unionist/nationalist transfer x 3 years
#TODO compare cw to local and plot differences where significant

fullTra2016All <- read.csv('transferProbs2016/transferMatrix_all_nationwide.csv',header=TRUE,check.names=FALSE)
fullTra2016All$transferFrom <- as.character(party_short_names[row.names(fullTra2016All)])
fullTra2016All <- melt(fullTra2016All,id.vars='transferFrom',variable.name='transferTo',value.name='fraction')
fullTra2016All$transferTo <- as.character(party_short_names[as.character(fullTra2016All$transferTo)])
fullTra2016Self <- read.csv('transferProbs2016/transferMatrix_whenSelfAvailable_nationwide.csv',header=TRUE,check.names=FALSE)
fullTra2016Self$transferFrom <- as.character(party_short_names[row.names(fullTra2016Self)])
fullTra2016Self <- melt(fullTra2016Self,id.vars='transferFrom',variable.name='transferTo',value.name='fraction')
fullTra2016Self$transferTo <- as.character(party_short_names[as.character(fullTra2016Self$transferTo)])
fullTra2016Rest <- read.csv('transferProbs2016/transferMatrix_whenSelfNotAvailable_nationwide.csv',header=TRUE,check.names=FALSE)
fullTra2016Rest$transferFrom <- as.character(party_short_names[row.names(fullTra2016Rest)])
fullTra2016Rest <- melt(fullTra2016Rest,id.vars='transferFrom',variable.name='transferTo',value.name='fraction')
fullTra2016Rest$transferTo <- as.character(party_short_names[as.character(fullTra2016Rest$transferTo)])

main_targets <- c('DUP','SF','UUP','SDLP','Alliance','Green','UKIP','PUP','TUV','PBP','Ind','votes_lost')
fullTra2016AllMain <- subset(fullTra2016All,transferTo %in% main_targets & transferFrom %in% main_targets)
fullTra2016RestMain <- subset(fullTra2016Rest,transferTo %in% main_targets & transferFrom %in% main_targets)
fullTra2016SelfMain <- subset(fullTra2016Self,transferTo %in% main_targets & transferFrom != 'Ind')

ggplot(fullTra2016AllMain,aes(transferTo,transferFrom)) + 
  geom_tile(aes(fill=fraction)) + theme(axis.text.x=element_text(angle=45,hjust=1)) +
  scale_fill_distiller(palette='YlGn',direction=1)
ggplot(fullTra2016RestMain,aes(transferTo,transferFrom)) + 
  geom_tile(aes(fill=fraction)) + theme(axis.text.x=element_text(angle=45,hjust=1)) +
  scale_fill_distiller(palette='Spectral',direction=-1) +
  labs('Transfer fractions when no self-transfer available',subtitle='2016 nationwide')

#compare to 2011

fullTra2011Self <- read.csv('transferProbs2011/transferMatrix_whenSelfAvailable_nationwide.csv',header=TRUE,check.names=FALSE)
fullTra2011Self$transferFrom <- as.character(party_short_names[row.names(fullTra2011Self)])
fullTra2011Self <- melt(fullTra2011Self,id.vars='transferFrom',variable.name='transferTo',value.name='fraction')
fullTra2011Self$transferTo <- as.character(party_short_names[as.character(fullTra2011Self$transferTo)])
fullTra2011Rest <- read.csv('transferProbs2011/transferMatrix_whenSelfNotAvailable_nationwide.csv',header=TRUE,check.names=FALSE)
fullTra2011Rest$transferFrom <- as.character(party_short_names[row.names(fullTra2011Rest)])
fullTra2011Rest <- melt(fullTra2011Rest,id.vars='transferFrom',variable.name='transferTo',value.name='fraction')
fullTra2011Rest$transferTo <- as.character(party_short_names[as.character(fullTra2011Rest$transferTo)])
fullTra2011SelfMain <- subset(fullTra2011Self,transferTo %in% main_targets & transferFrom != 'Ind')
fullTra2011RestMain <- subset(fullTra2011Rest,transferTo %in% main_targets & transferFrom %in% main_targets)


ggplot(fullTra2011RestMain,aes(transferTo,transferFrom)) + 
  geom_tile(aes(fill=fraction)) + theme(axis.text.x=element_text(angle=45,hjust=1)) +
  scale_fill_distiller(palette='Spectral',direction=-1) + 
  labs(title='Transfer fractions when no self-transfer available',subtitle='2011 nationwide')

#change from 2011 to 2016
fullTra2016minus2011RestMain <- merge(fullTra2011RestMain,fullTra2016RestMain,
                                      by=c('transferFrom','transferTo'),all=FALSE,
                                      suffixes=c('_2011','_2016'))
fullTra2016minus2011RestMain$fraction_change <- fullTra2016minus2011RestMain$fraction_2016 -
  fullTra2016minus2011RestMain$fraction_2011
ggplot(fullTra2016minus2011RestMain,aes(transferTo,transferFrom)) + 
  geom_tile(aes(fill=fraction_change)) + theme(axis.text.x=element_text(angle=45,hjust=1)) +
  scale_fill_distiller(palette='RdBu',direction=-1) + 
  labs(title='Transfer fractions when no self-transfer available',subtitle='2016 - 2011, nationwide')
#Increases in DUP->UUP, UUP->SDLP, Alliance->Green; decreases SDLP->Alliance
fullTra2016minus2011RestMain %>% group_by(transferTo) %>% 
  summarise(meanfrac11=mean(fraction_2011,na.rm=T),meanfrac16=mean(fraction_2016,na.rm=T)) %>% 
  mutate(meanFracIncr=meanfrac16-meanfrac11) %>% arrange(-meanFracIncr)
#UKIP,PUP were more transferred to; Alliance,SDLP less transferred to (totals don't seem to add up?).
mean(subset(fullTra2016minus2011RestMain,transferTo=='votes_lost')$fraction_change)
#Seems to have been fewer lost votes in 2016 - from 19% to 12% of available transfers

#and compare to 2017
fullTra2017Self <- read.csv('transferProbs2017/transferMatrix_whenSelfAvailable_nationwide.csv',header=TRUE,check.names=FALSE)
fullTra2017Self$transferFrom <- as.character(party_short_names[row.names(fullTra2017Self)])
fullTra2017Self <- melt(fullTra2017Self,id.vars='transferFrom',variable.name='transferTo',value.name='fraction')
fullTra2017Self$transferTo <- as.character(party_short_names[as.character(fullTra2017Self$transferTo)])
fullTra2017Rest <- read.csv('transferProbs2017/transferMatrix_whenSelfNotAvailable_nationwide.csv',header=TRUE,check.names=FALSE)
fullTra2017Rest$transferFrom <- as.character(party_short_names[row.names(fullTra2017Rest)])
fullTra2017Rest <- melt(fullTra2017Rest,id.vars='transferFrom',variable.name='transferTo',value.name='fraction')
fullTra2017Rest$transferTo <- as.character(party_short_names[as.character(fullTra2017Rest$transferTo)])
fullTra2017SelfMain <- subset(fullTra2017Self,transferTo %in% main_targets & transferFrom != 'Ind')
fullTra2017RestMain <- subset(fullTra2017Rest,transferTo %in% main_targets & transferFrom %in% main_targets)

ggplot(fullTra2017RestMain,aes(transferTo,transferFrom)) + 
  geom_tile(aes(fill=fraction)) + theme(axis.text.x=element_text(angle=45,hjust=1)) +
  scale_fill_distiller(palette='Spectral',direction=-1) + 
  labs(title='Transfer fractions when no self-transfer available',subtitle='2017 nationwide')

fullTra2017minus2016RestMain <- merge(fullTra2016RestMain,fullTra2017RestMain,
                                      by=c('transferFrom','transferTo'),all=FALSE,
                                      suffixes=c('_2016','_2017'))
fullTra2017minus2016RestMain$fraction_change <- fullTra2017minus2016RestMain$fraction_2017 -
  fullTra2017minus2016RestMain$fraction_2016
ggplot(fullTra2017minus2016RestMain,aes(transferTo,transferFrom)) + 
  geom_tile(aes(fill=fraction_change)) + theme(axis.text.x=element_text(angle=45,hjust=1)) +
  scale_fill_distiller(palette='RdBu',direction=-1) + 
  labs(title='Transfer fractions when no self-transfer available',subtitle='2017 - 2016, nationwide')


#--------------
#also can do interesting local - national.

#most votes lost
data.frame(party=row.names(fullTra2016Rest),fracLost=fullTra2016Rest[,'votes_lost'],stringsAsFactors=FALSE) %>%
    arrange(fracLost) %>% ggplot() + geom_bar(aes(party,fracLost),stat='identity') +
    labs(x='',y='Fraction of votes not transferred')

#do stacked bar for transfersSelf, 3 categories: self, other, lost.
#9% SDLP votes lost when self transfer available.