#Findings:
# - self transfers aren't affected by number of parties available (numP)
# - non-self transfers are inflated when numP is small and can be corrected with a numP+numP^2 glm
#   - can't get clean signal for differences between Self_Available and not, so treat as one.
# - votes_lost has a similar dependence on numP, but fit a different glm
# - non-self transfers when Self_Available are divided similarly to not S_A among the fraction
#     that doesn't go to self, but the big parties below get slightly less than expected, so
#     need to divide by another 0.93 to get from S_A to not S_A.
#     In check_grid_performance it seems that a reduction of votes_lost
#     by 0.6 in the rest grid fixes it for 'all', but problem seems to be more in the self grid.
# - self/rest can't be too much better than 1grid, because often I will calculate the 
#     first transfer using self, but the real transfer will later take place in a rest situation.
#     So a lot of the rest transfers recorded are really self after other parties are removed.
#     Therefore all/all should work almost as well as self/rest.
# - also means nw+local with self/rest is limited because I will often be unable to alter
#     the self frac which is the one that matters

#----

long <- read.csv('transferProbs2007/transfersByRow_2007_all.csv')

long_agg_party <- long %>% group_by(Constituency,Count_Number,Transferrer,Target,Self_Available,Number_Of_Target_Parties_Left) %>% 
  summarise(fracReceived=sum(fracReceived))
long_agg_party <- subset(long_agg_party, !(Transferrer=='Independent' & Target=='Independent'))

#---- Work out the correction for different numP
meanFracReceiveds <- long_agg_party %>% 
  group_by(Target,Self_Available,is_self=(as.character(Transferrer)==as.character(Target))) %>% 
  summarise(meanFrac=mean(fracReceived),meanNumPs=mean(Number_Of_Target_Parties_Left),n=n())
long_agg_party$is_self <- as.character(long_agg_party$Transferrer)==as.character(long_agg_party$Target)
long_agg_party <- merge(long_agg_party,meanFracReceiveds,by=c('Self_Available','is_self','Target'))
long_agg_party$relFracReceived <- long_agg_party$fracReceived / long_agg_party$meanFrac

forglm <- long_agg_party %>% group_by(Number_Of_Target_Parties_Left,Self_Available,is_self) %>% summarise(transferInflationFactor=mean(relFracReceived),n=n())
forglm %>% mutate(self_avail_is_self=paste(Self_Available,as.character(is_self))) %>% 
  ggplot() + geom_point(aes(Number_Of_Target_Parties_Left,transferInflationFactor,colour=self_avail_is_self)) +
  geom_smooth(aes(Number_Of_Target_Parties_Left,transferInflationFactor,colour=self_avail_is_self),method='loess',se=FALSE)
#no correction needed for self; non-self when self available is noisy, so better to group by
#  is_self only. Then meanNumPs is ~6 for both cases so correction to average number of
#  targets should be fair.
forglm <- long_agg_party %>% group_by(Number_Of_Target_Parties_Left,is_self) %>% summarise(transferInflationFactor=mean(relFracReceived))
forglm %>% ggplot() + geom_point(aes(Number_Of_Target_Parties_Left,transferInflationFactor,colour=is_self))

inflat_glm_rest <- glm(transferInflationFactor ~ Number_Of_Target_Parties_Left + I(Number_Of_Target_Parties_Left^2), 
                       data=subset(forglm,!is_self))
#2.237 - 0.284*numP + 0.0129*numP^2
#So the following inflations applied:
long_agg_party %>% filter(!is_self) %>% 
  mutate(inflationFactor=predict(inflat_glm_rest,.),1) %>% group_by(Target) %>%  
  summarise(n(),Number_Of_Target_Parties_Left=mean(Number_Of_Target_Parties_Left),
            inflationFactor=mean(inflationFactor)) %>% arrange(-inflationFactor)
#SEA,SDLP,DUP non-self transfers were inflated, smaller parties deflated.

#----
#Use correction for numP and test self->rest conversion
#2.237 - 0.284*numP + 0.0129*numP^2 for non-selfs.
long_agg_party$fracReceived_corr <- ifelse(long_agg_party$is_self,
                                           long_agg_party$fracReceived,
                                           long_agg_party$fracReceived /
                                             (2.237 - 0.284*long_agg_party$Number_Of_Target_Parties_Left + 
                                                0.0129*(long_agg_party$Number_Of_Target_Parties_Left^2)))
long_agg_party$fracReceived_corr <- pmin(long_agg_party$fracReceived_corr, 0.95)
long_agg_party_selfs <- long_agg_party %>% filter(is_self) %>% 
  group_by(Transferrer,Constituency,Count_Number) %>% summarise(selffrac=mean(fracReceived_corr))
long_agg_party_selfs <- merge(long_agg_party,long_agg_party_selfs,
                              by=c('Transferrer','Constituency','Count_Number'),all=T)
long_agg_party_selfs <- subset(long_agg_party_selfs,Transferrer!='Independent')
#long_losts <- long %>% group_by(Transferrer,Constituency,Count_Number) %>% 
#  summarise(lost=1-sum(fracReceived))
#long_wself <- merge(long_wself,long_losts,by=c('Transferrer','Constituency','Count_Number'),all=T)

tra_summ <- long_agg_party_selfs %>% group_by(Transferrer,Target,Self_Available) %>% 
  summarise(n=n(),frac=mean(fracReceived_corr),
            selffrac=mean(selffrac),numP=mean(Number_Of_Target_Parties_Left))
tra_summ <- subset(tra_summ,as.character(Transferrer)!=as.character(Target))
tra_summ$frac_left_scale_by <- ifelse(tra_summ$Self_Available=='True',(1-tra_summ$selffrac)*0.93,1)
tra_summ$adj_frac <- tra_summ$frac/tra_summ$frac_left_scale_by

#subset to from-tos that are present for both Self_Available and not, to evaluate the conversion
boths <- tra_summ %>% group_by(Transferrer,Target) %>% summarise(haveboth=n()>1)
tra_summ_wboth <- merge(tra_summ,boths,by=c('Transferrer','Target'),all=T)
tra_summ_wboth %>% filter(haveboth) %>% group_by(Self_Available) %>% summarise(mean(adj_frac))
#Now 0.143 vs 0.143, i.e. adjustment works correctly, with the *0.93 above. So at least these
#  big parties get a bit less of the remaining transfers when Self_Available than would
#  be expected from no self available.

tra_summ_wide <- reshape(subset(select(tra_summ_wboth,-c(frac,selffrac,frac_left_scale_by)),haveboth),
                         direction='wide',v.names=c('n','numP','adj_frac'),
                         timevar='Self_Available',idvar=c('Transferrer','Target'))
#ggplot(subset(tra_summ_wide,n.True>2 & n.False>2)) + 
ggplot(tra_summ_wide) +
  geom_point(aes(adj_frac.False,adj_frac.True,size=n.True+n.False)) + geom_abline(slope=1,intercept=0)

#--- 

long_losts <- long_agg_party %>% group_by(Transferrer,Constituency,Count_Number) %>% 
  summarise(lost=1-sum(fracReceived))
long_agg_party <- merge(long_agg_party,long_losts,by=c('Transferrer','Constituency','Count_Number'),all=T)

meanLosts <- long_agg_party %>% 
  group_by(Transferrer,Self_Available) %>% 
  summarise(meanLost=mean(lost))
long_agg_party <- merge(long_agg_party,meanLosts,by=c('Transferrer','Self_Available'))
long_agg_party$relLost <- long_agg_party$lost / long_agg_party$meanLost
forglmLost <- long_agg_party %>% group_by(Constituency,Count_Number) %>% 
  summarise(Number_Of_Target_Parties_Left=first(Number_Of_Target_Parties_Left),
            Self_Available=first(Self_Available),relLost=first(relLost)) %>% 
  group_by(Number_Of_Target_Parties_Left,Self_Available) %>% summarise(lostInflationFactor=mean(relLost),n=n())
forglmLost %>% ggplot() + geom_point(aes(Number_Of_Target_Parties_Left,lostInflationFactor,size=n))
inflat_glm_lost <- glm(lostInflationFactor ~ Number_Of_Target_Parties_Left + I(Number_Of_Target_Parties_Left^2), 
                       data=subset(forglmLost,n>3))
#2.915 - 0.485*numP + 0.0254*numP^2
forglmLost$est <- predict(inflat_glm_lost,forglmLost)
forglmLost %>% ggplot() + geom_point(aes(Number_Of_Target_Parties_Left,lostInflationFactor,size=n)) +
  geom_point(aes(Number_Of_Target_Parties_Left,est),colour='blue')
#**currently this corrects only for numP effect on votes_lost, not Self_Available effect**
#The following lost inflations applied:
long_agg_party %>% 
  mutate(lostInflationFactor=predict(inflat_glm_lost,.),1) %>% group_by(Transferrer) %>%  
  summarise(n(),Number_Of_Target_Parties_Left=mean(Number_Of_Target_Parties_Left),
            lostInflationFactor=mean(lostInflationFactor)) %>% arrange(-lostInflationFactor)
#SEA,SDLP,UUP,Green had smaller numP so lost frac was inflated, small parties and
#  DUP,SF lost fracs were deflated
long_agg_party %>% group_by(Transferrer) %>% 
  summarise(lost=mean(lost),numP=mean(Number_Of_Target_Parties_Left)) %>% 
  mutate(corrLost=lost/(2.915 - 0.485*numP + 0.0254*(numP^2))) %>% arrange(corrLost)
#After correcting, see that UKIP, Alliance lost fewest, SDLP, Green, Ind lost most (and RSF)

  