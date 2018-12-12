#' Calculating confidence intervals around bootstrap estimates from boot_rank
#' @param boots output from boot_res of bootstrapped values
#' @param CIMethod confidence interval calculation method. "quantile" for using tails of 
#'   empirical distribution for CI and getting estimate from median value; "SD" for using standard deviation of distribution;
#'   "qvcalc" for using quasivariance (and this getting interval around reference
#'   level as well)
#' @param level confidence level for CI calculations
#' @keywords bootstrap rank bradley terry
#' @export
#' @examples
CIboot<-function(boots,CIMethod,level){
  if(CIMethod=="quantile"){
    estimates<-boots %>% group_by(option) %>% dplyr::summarise(est=median(coef,na.rm=T),lowerCI=quantile(coef,(1-level)/2,na.rm=T),
                                                               upperCI=quantile(coef,1-((1-level)/2),na.rm=T))
  }
  if(CIMethod=="SD"){
    estimates<-boots %>% group_by(option) %>% dplyr::summarise(est=mean(coef,na.rm=T),se=sd(coef,na.rm=T),
                                                               lowerCI=est-se*qnorm(1-((1-level)/2)),
                                                               upperCI=est+se*qnorm(1-((1-level)/2)))
  }
  if(CIMethod=="qvcalc"){
    estimates<-boots %>% group_by(option) %>% dplyr::summarise(est=mean(coef,na.rm=T))
    
    wide_coef<-boots %>% select(-ref_flag) %>% spread(option,coef)
    
    covariancematrix<-cov(wide_coef[,-1],use = "complete.obs")
    quasises<-qvcalc(covariancematrix)
    quasises<-data.frame(quasises[[2]])
    quasises$option<-rownames(quasises)
    quasises<-select(quasises,-estimate)
    estimates<-merge(quasises,estimates)
    
    estimates$lowerCI<-estimates$est-qnorm(1-((1-level)/2))*estimates$quasiSE
    estimates$upperCI<-estimates$est+qnorm(1-((1-level)/2))*estimates$quasiSE
    
  }
  estimates$option<-reorder(estimates$option,estimates$est,mean,na.rm=TRUE)
  return(estimates)
}
