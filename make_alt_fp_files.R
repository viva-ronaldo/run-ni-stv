#Make alternative first pref files from electionsni.org data
library(dplyr)

year <- '2007'
for (constit_file in Sys.glob(paste0('data/',year,'_archive_datapackage/constituency/*/Count.csv'))) {
  constit <- strsplit(constit_file,'/')[[1]][4]
  print(constit)
  full_count <- read.csv(constit_file)
  full_count <- subset(full_count,Count_Number==1)[,c('Candidate_First_Pref_Votes','Party_Name')]
  total_votes <- sum(full_count$Candidate_First_Pref_Votes)
  full_count$fp_percent <- round(100*full_count$Candidate_First_Pref_Votes / total_votes, 1)
  full_count <- arrange(full_count,-fp_percent)
  parties_so_far <- c()
  for (i in seq(nrow(full_count))) {
    parties_so_far <- c(parties_so_far, as.character(full_count$Party_Name[i]))
    p_count <- sum(parties_so_far == full_count$Party_Name[i])
    full_count$Party_Numbered[i] <- paste(full_count$Party_Name[i],p_count)
  }
  write(total_votes,file=paste0('fp',year,'/alt/',gsub('-','',constit),year,'alt.csv'))
  write.table(full_count[,c('Party_Numbered','fp_percent')],
            file=paste0('fp',year,'/alt/',gsub('-','',constit),year,'alt.csv'),append=TRUE,
            sep=',',quote=FALSE,row.names=FALSE,col.names=FALSE)
}
