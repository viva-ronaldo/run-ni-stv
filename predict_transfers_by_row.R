library(reshape2)
library(ModelMetrics)

#TODO merge on local transfers too

byrow <- read.csv('transfersByRow_2016_all.csv', stringsAsFactors = FALSE)

noSelf <- subset(byrow, Self_Available=='False')
noSelf <- subset(noSelf, Transferred_Votes >= 200)  #get decent fraction estimates

fullTransfersRest2016 <- read.csv('transferProbs2016/transferMatrix_whenSelfNotAvailable_nationwide.csv',
                                  header=TRUE,check.names=FALSE)
fullTransfersRest2016$transferFrom <- as.character(row.names(fullTransfersRest2016))
fullTransfersRest2016 <- melt(fullTransfersRest2016,id.vars='transferFrom',variable.name='transferTo',value.name='fraction')
names(fullTransfersRest2016)[3] <- 'nw_avg_fraction'

noSelf <- merge(noSelf, fullTransfersRest2016,
                by.x=c('Transferrer','Target'),by.y=c('transferFrom','transferTo'),all.x=TRUE)

rmse(noSelf$fracReceived, noSelf$nw_avg_fraction) #0.093
model1 <- glm(fracReceived ~ nw_avg_fraction + Number_Of_Targets_Left + Count_Number,
              data=noSelf)
summary(model1)
rmse(noSelf$fracReceived, predict(model1, noSelf)) #0.088

localTransfersRest2016 <- data.frame()
for (local_transfer_file in Sys.glob('transferProbs2016/transferMatrix_whenSelfNotAvailable_*')) {
  if (!grepl('nationwide',local_transfer_file)) {
    tmp <- read.csv(local_transfer_file,header=TRUE,check.names=FALSE)
    tmp$transferFrom <- as.character(row.names(tmp))
    tmp <- melt(tmp,id.vars='transferFrom',variable.name='transferTo',value.name='fraction')
    names(tmp)[3] <- 'local_avg_fraction'
    constitName <- strsplit(local_transfer_file,'_')[[1]][3]
    constitName <- strsplit(constitName,'\\.')[[1]][1]
    tmp$Constituency <- constitName
    localTransfersRest2016 <- rbind(localTransfersRest2016, tmp)
  }
}
localTransfersRest2016 <- subset(localTransfersRest2016, !is.na(local_avg_fraction))
noSelf <- merge(noSelf, localTransfersRest2016, by.x=c('Transferrer','Target','Constituency'),
                by.y=c('transferFrom','transferTo','Constituency'),all.x=TRUE)
#Because I started with byrow, this has no missing local_avg_fraction

model2 <- glm(fracReceived ~ nw_avg_fraction + Number_Of_Targets_Left + Count_Number + local_avg_fraction,
              data=noSelf)
summary(model2)
#Takes more from local than nw, but that's expected for same year
rmse(noSelf$fracReceived, predict(model2, noSelf)) #0.078

#Now test on 2011
byrow2011 <- read.csv('transfersByRow_2011_all.csv', stringsAsFactors = FALSE)
rmse(byrow2011$fracReceived, predict(model1, byrow2011))
rmse(byrow2011$fracReceived, predict(model2, byrow2011))