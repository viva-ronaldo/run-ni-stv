#Look at filling in gaps in nw grids
library(reshape2)
require(dplyr)
require(stringr)
require(ggplot2)

#TODO functionalise this and call for 2016,2011
#TODO decide if votes_lost should get special treatment here or in buildlist, or
#  is it being preserved well enough already.

#2016 Strangford and South Down DUP to UUP are suspiciously high but seem right
#  according to Wiki and electionsni.org. Corrected West Tyrone 2016, was misprint in eni.org.
#  Possibly a dodgy SF self transfer in NA 2016, count 3.

fullTra2016Rest <- read.csv('transferProbs2016/transferMatrix_whenSelfNotAvailable_nationwide.csv',header=TRUE,check.names=FALSE)
fullTra2016Rest$transferFrom <- as.character(party_short_names[row.names(fullTra2016Rest)])
fullTra2016Rest <- melt(fullTra2016Rest,id.vars='transferFrom',variable.name='transferTo',value.name='fraction')
fullTra2016Rest$transferTo <- as.character(party_short_names[as.character(fullTra2016Rest$transferTo)])

#This gives av frac received when available: some parties are more transfer friendly in general
transferFriendliness2016 <- fullTra2016Rest %>% group_by(transferTo) %>% summarise(avFrac=mean(fraction,na.rm=T))
transferFriendliness2016$avFrac[is.na(transferFriendliness2016$avFrac)] <- 0.01

ggplot(fullTra2016Rest,aes(transferTo,transferFrom)) + 
  geom_tile(aes(fill=fraction)) + theme(axis.text.x=element_text(angle=45,hjust=1)) +
  scale_fill_distiller(palette='Spectral',direction=-1) +
  labs(title='Raw transfer fractions, no self available',
       subtitle='2016 nationwide')
#TODO plot ordered by transfer friendliness on x axis, alphabetical on y axis

#don't allow any 0s
fullTra2016Rest$fraction[fullTra2016Rest$fraction==0] <- 0.001

fill_via_symmetry <- function(grid, transferFriendliness) {
  for (tfrom in unique(grid$transferFrom)) {
    for (tto in unique(grid$transferTo)) {
      if (is.na(subset(grid,transferFrom==tfrom & transferTo==tto)$fraction) &
          tto != 'votes_lost') {
        tf_ratio <- subset(transferFriendliness,transferTo==tto)$avFrac / 
          subset(transferFriendliness,transferTo==tfrom)$avFrac
        grid[grid$transferFrom==tfrom & grid$transferTo==tto, 'fraction'] <- 
          subset(grid,transferFrom==tto & transferTo==tfrom)$fraction * tf_ratio
      }
    }
  }
  grid
}
#TODO could use glm to combine reverse frac and transferFriendliness better

#Find similarity of parties in terms of where they transfer to
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
sim_values_2016 <- get_party_sim_values(fullTra2016Rest)
sim_values_2016 %>% group_by(p1) %>% arrange(-cor) %>% summarise(first(p2),first(cor))
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
            print(paste('replacing',tfrom,'to',tto,'with',ordered_sim[o,'p2']))
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

fullTra2016Rest_sym <- fill_via_symmetry(fullTra2016Rest,transferFriendliness2016)
fullTra2016Rest_sym_sim <- fill_via_most_similar(fullTra2016Rest_sym, sim_values_2016)
#Just leaves DF,NIF,AWP for which there is no information
fullTra2016Rest_sym_sim$fraction[is.na(fullTra2016Rest_sym_sim$fraction) &
                                   fullTra2016Rest_sym_sim$transferFrom != 
                                   fullTra2016Rest_sym_sim$transferTo] <- 0.01
#normalise by row
for (tfrom in unique(fullTra2016Rest_sym_sim$transferFrom)) {
  scale_fact <- sum(fullTra2016Rest_sym_sim$fraction[fullTra2016Rest_sym_sim$transferFrom==tfrom],na.rm=T)
  fullTra2016Rest_sym_sim$fraction[fullTra2016Rest_sym_sim$transferFrom==tfrom] <-
    fullTra2016Rest_sym_sim$fraction[fullTra2016Rest_sym_sim$transferFrom==tfrom] / scale_fact
}

ggplot(fullTra2016Rest_sym_sim,aes(transferTo,transferFrom)) + 
  geom_tile(aes(fill=fraction)) + theme(axis.text.x=element_text(angle=45,hjust=1)) +
  scale_fill_distiller(palette='Spectral',direction=-1) +
  labs(title='Inferred transfer fractions, no self available',subtitle='2016 nationwide')

#Should roughly preserve order of tfs
head(arrange(transferFriendliness,avFrac))
head(fullTra2016Rest_sym_sim %>% group_by(transferTo) %>% 
       summarise(avFrac=mean(fraction,na.rm=T)) %>% arrange(avFrac))
#Second one now shows expected frac received if everyone was available all the time

#Turn back to wide
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
fullTra2016Rest_sym_sim <- convert_long_back_to_grid(fullTra2016Rest_sym_sim, party_short_names)

#------ do similar for self

fullTra2016Self <- read.csv('transferProbs2016/transferMatrix_whenSelfAvailable_nationwide.csv',header=TRUE,check.names=FALSE)
fullTra2016Self$transferFrom <- as.character(party_short_names[row.names(fullTra2016Self)])
fullTra2016Self <- melt(fullTra2016Self,id.vars='transferFrom',variable.name='transferTo',value.name='fraction')
fullTra2016Self$transferTo <- as.character(party_short_names[as.character(fullTra2016Self$transferTo)])
fullTra2016Self$fraction[fullTra2016Self$fraction==0] <- 0.001

ggplot(fullTra2016Self,aes(transferTo,transferFrom)) + 
  geom_tile(aes(fill=fraction)) + theme(axis.text.x=element_text(angle=45,hjust=1)) +
  scale_fill_distiller(palette='Spectral',direction=-1) +
  labs('Transfer fractions when no self-transfer available',subtitle='2016 nationwide')

meanSelfFrac <- mean(subset(fullTra2016Self,transferTo==transferFrom &
                              transferFrom != 'Ind')$fraction,na.rm=T)
for (tfrom in unique(fullTra2016Self$transferFrom)) {
  for (tto in unique(fullTra2016Self$transferTo)) {
    if (is.na(subset(fullTra2016Self,transferFrom==tfrom &
                     transferTo==tto)$fraction) & tto == tfrom) {
      fullTra2016Self[fullTra2016Self$transferFrom==tfrom & 
                        fullTra2016Self$transferTo==tto, 'fraction'] <- meanSelfFrac
    }
  }
}

fullTra2016Self_sym <- fill_via_symmetry(fullTra2016Self,transferFriendliness2016)
fullTra2016Self_sym_sim <- fill_via_most_similar(fullTra2016Self_sym, sim_values_2016)
fullTra2016Self_sym_sim$fraction[is.na(fullTra2016Self_sym_sim$fraction) &
                                   fullTra2016Self_sym_sim$transferFrom != 
                                   fullTra2016Self_sym_sim$transferTo] <- 0.01
#TODO fill remaining with rest values, or use glm below

#normalise by row
for (tfrom in unique(fullTra2016Self_sym_sim$transferFrom)) {
  scale_fact <- sum(fullTra2016Self_sym_sim$fraction[fullTra2016Self_sym_sim$transferFrom==tfrom],na.rm=T)
  fullTra2016Self_sym_sim$fraction[fullTra2016Self_sym_sim$transferFrom==tfrom] <-
    fullTra2016Self_sym_sim$fraction[fullTra2016Self_sym_sim$transferFrom==tfrom] / scale_fact
}

ggplot(fullTra2016Self_sym_sim,aes(transferTo,transferFrom)) + 
  geom_tile(aes(fill=fraction)) + theme(axis.text.x=element_text(angle=45,hjust=1)) +
  scale_fill_distiller(palette='Spectral',direction=-1) +
  labs(title='Inferred transfer fractions when self available',subtitle='2016 nationwide')




#Check similarity of self and rest transfers
fullTra2016Joined <- merge(fullTra2016Rest,fullTra2016Self,
                               by=c('transferFrom','transferTo'),
                               suffixes = c('_rest','_self'))
fullTra2016Joined <- subset(fullTra2016Joined,!is.na(fraction_rest) &
                                  !is.na(fraction_self))
ggplot(fullTra2016Joined) + geom_point(aes(fraction_rest,fraction_self))
print(sum(fullTra2016Joined$fraction_self)/sum(fullTra2016Joined$fraction_rest)) #0.40

self_from_rest <- glm(fraction_self ~ fraction_rest, data=fullTra2016Joined)
fullTra2016Rest$fraction_rest <- fullTra2016Rest$fraction 

for (tfrom in unique(fullTra2016Self$transferFrom)) {
  for (tto in unique(fullTra2016Self$transferTo)) {
    if (is.na(subset(fullTra2016Self,transferFrom==tfrom &
                     transferTo==tto)$fraction)) {
      fullTra2016Self[fullTra2016Self$transferFrom==tfrom & 
                            fullTra2016Self$transferTo==tto, 'fraction'] <- 
        predict(self_from_rest, fullTra2016Rest[fullTra2016Rest$transferFrom==tfrom & 
                                                      fullTra2016Rest$transferTo==tto, ])
      #print(paste(tfrom,tto,subset(fullTra2016Self,transferFrom==tto & transferTo==tfrom)$fraction))
    }
  }
}
mean(is.na(fullTra2016Self$fraction))
for (tfrom in unique(fullTra2016Self$transferFrom)) {
  scale_fact <- sum(subset(fullTra2016Self, transferFrom==tfrom)$fraction)
  fullTra2016Self[fullTra2016Self$transferFrom==tfrom,'fraction'] <- 
    fullTra2016Self[fullTra2016Self$transferFrom==tfrom,'fraction'] / scale_fact
}

