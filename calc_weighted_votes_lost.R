
all_res <- data.frame()
for (year in c(2007,2011,2016,2017)) {
  all_constit_files <- Sys.glob(paste0('data/',year,'_archive_datapackage/constituency/*/Count.csv'))
  for (file in all_constit_files) {
    constit <- strsplit(file,'/')[[1]][4]
    res <- read.csv(file)
    res <- res %>% group_by(Count_Number) %>% 
      summarise(totalfps=sum(Candidate_First_Pref_Votes),
                totalv=sum(Total_Votes),frac_kept=totalv/totalfps)
    num_counts <- max(res$Count_Number)
    frac_kept <- subset(res,Count_Number==num_counts)$frac_kept
    assertthat::are_equal(res$totalfps,subset(res,Count_Number==num_counts)$totalv)
    all_res <- rbind(all_res,data.frame(year=year,constit=constit,
                                        num_counts=num_counts,frac_kept=frac_kept))
  }
}
ggplot(all_res) + geom_point(aes(num_counts,frac_kept,colour=factor(year))) + 
  geom_smooth(aes(num_counts,frac_kept,colour=factor(year),group=factor(year)),method='lm',se=FALSE)
