#Find the best alpha for 2016, 2016.

fullTransfers2016 <- read.csv('transferProbs2016/transferMatrix_all_nationwide.csv',header=TRUE,check.names=FALSE)
fullTransfersSelf2016 <- read.csv('transferProbs2016/transferMatrix_whenSelfAvailable_nationwide.csv',header=TRUE,check.names=FALSE)
fullTransfersRest2016 <- read.csv('transferProbs2016/transferMatrix_whenSelfNotAvailable_nationwide.csv',header=TRUE,check.names=FALSE)

localTransfers2016 <- get_local_transfers('all',2016)
localTransfersSelf2016 <- get_local_transfers('whenSelfAvailable',2016)
localTransfersRest2016 <- get_local_transfers('whenSelfNotAvailable',2016)

runAllConstitsEnsemble_alpha <- function(year, fullTransfersSelf, fullTransfersRest, 
                                         localTransfersSelf, localTransfersRest,
                                         nSample, nEnsemble, alpha) {
  constituencies <- c('Belfast North','Belfast West','Belfast East','Belfast South',
                      'North Antrim','East Antrim','South Antrim','Strangford',
                      'Lagan Valley','Upper Bann','North Down','South Down',
                      'Newry Armagh','East Londonderry','Fermanagh South Tyrone',
                      'Foyle','West Tyrone','Mid Ulster')

  brier <- 0
  num_right <- 0
  for (constit in constituencies) {
    constit_lower <- paste(strsplit(tolower(constit),' ')[[1]],collapse='-')
    combined_self <- combine_transfer_grids(fullTransfersSelf,
                                            localTransfersSelf[[constit_lower]],alpha)
    combined_rest <- combine_transfer_grids(fullTransfersRest,
                                            localTransfersRest[[constit_lower]],alpha)

    res <- runConstitEnsemble(constit,year,NULL,
                              combined_self,combined_rest,nSample,nEnsemble)
    brier_add <- get_brier_for_constit(res,constit,year)
    cat(constit,brier_add,'\n')
    brier <- brier + brier_add
    if (brier_add <= 0.04) num_right <- num_right + 1
  }
  list(brier=brier, num_right=num_right)
}
alpha_results <- data.frame()
for (alpha in seq(0,1,0.2)) {
  this_alpha <- runAllConstitsEnsemble_alpha(2016,fullTransfersSelf2016,fullTransfersRest2016,
                                             localTransfersSelf2016,localTransfersRest2016,
                                             3000,10,alpha)
  alpha_results <- rbind(alpha_results, 
                         data.frame(this_alpha,alpha=alpha))
}
