#' Calculating bootstrapped model coefficients for a bradley terry rank model. Bootstrap based on ID column rather than individual rows.
#' @param data original data frame
#' @param IDcol ID column within data frame
#' @param formula model formula
#' @param nboot number of bootstraps to run
#' @param diagnostics TRUE/FALSE. Produce diagnostic violin plots of coefficients
#' @param CIplot TRUE/FALSE. Produce CI plots of coefficients.
#' @param level confidence level for CI calculations
#' @keywords bootstrap rank bradley terry
#' @export
#' @examples
boot_rank<-function(data,IDcol,formula,nboot=100,diagnostics=TRUE,CIplot=TRUE,level=0.95){

  require(dplyr)
  require(ggplot2)
  require(brglm)

  bootres<-NULL
  if(length(IDcol)==nrow(data)){
    data$ID<-IDcol
  }
  if(length(IDcol)==1){
    data$ID<-data[,IDcol]
  }
  selections<-unique(data$ID)
  pb <- txtProgressBar(min = 0, max = nboot, style = 3)
  for(i in 1:nboot){
  sample<-sample(selections, replace=TRUE)
  sel<- sample %>% map_df(~filter(data,ID==.))

  ####temporary hack - if not all selections present then cheat and restart
  if(any(colSums(abs(model.frame(f,data=modeldata2)))==0)){
    i<-i-1
  }
  else{
    m0<-suppressWarnings(brglm(data=sel, f, family=binomial))
    fulldata<-data.frame(coef=coefficients(m0),option=names(coefficients(m0)),boot=i)
    fulldata$coef[is.na(fulldata$coef)]<-0
    bootres<-rbind(bootres,fulldata)
    setTxtProgressBar(pb, i)
    }

  }
  bootres$option<-reorder(bootres$option,bootres$coef,mean)
  estimates<-bootres %>% group_by(option) %>% summarise(mean=mean(coef),lowerCI=quantile(coef,(1-level)/2),upperCI=quantile(coef,1-((1-level)/2)))
  output<-list(estimates=estimates,boots=bootres)
  if(diagnostics==TRUE){

    p1<- ggplot(data=bootres,aes(y=coef,x=option))+
      geom_violin()+
      geom_jitter(alpha=0.2,width = 0.2,height=0)+
      stat_summary(geom="point",col="red",fun.y = "mean",size=4,shape=4)

    plot(p1)
    output$diagnostics<-p1
  }
  if(CIplot==TRUE){

    p2<- ggplot(data=estimates,aes(y=mean,x=option,ymax=upperCI,ymin=lowerCI))+
      geom_point()+
      geom_errorbar()

    plot(p2)
    output$CIplot<-p2
  }
  close(pb)
  return(output)
}


