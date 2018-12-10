#' Take long format rank data and calculate pairwise wide data frame with one row per comparison and one column per option
#' @param options vector of options
#' @param ranks vector of ranks
#' @param IDs vector of IDs
#' @param ties method for solving ties. does nothing at the moment but might do later.
#' @param fixnames make names of options syntactically valid in case of spaces or punctuations in the name
#' @keywords ranks matrix pairwise comparisons
#' @export
#' @examples
#'
#' #write function to take data and make pairwise comparison matrix
#allow two options for ties - random winner or 0.5 value. Come back to random option later
paircomp<-function(options,ranks,IDs,ties="half",fixnames=TRUE){
  require(dplyr)
  require(tidyr)
  #combine data together
  data1<-data.frame(options=options,ranks=ranks,IDs=IDs)

  #check for orphans
  if(min(tapply(data1$options,data1$IDs,length) )<2){
    warning("ID = ",names(tapply(data1$options,data1$IDs,length)[tapply(data1$options,data1$IDs,length)<2])," removed. Only 1 option ranked")
    data1<-filter(data1,!IDs%in%names(tapply(data1$options,data1$IDs,length)[tapply(data1$options,data1$IDs,length)<2]))
  }

 #check for duplicates
  if(anyDuplicated(paste(data1$IDs,data1$options))){
    stop("Duplicated options in ID(s) : ",paste(unique(data1$IDs[duplicated(paste(data1$IDs,data1$options))]),collapse=" ; "))
  }

  #build parwise data frame
  data2<-full_join(data1,data1,by="IDs") %>%
    #remove self comparisons and select only "upper" triangle comparisons
      filter((options.x!=options.y)&ranks.x<=ranks.y) %>%
    #create flag for ties.
        mutate(tie=ranks.x==ranks.y) %>%
    #Ties will currently appear twice so need to only select on of the two instances. Almost certainly a better way of doing this?
          filter(tie==FALSE|(as.numeric(as.factor(options.x))<as.numeric(as.factor(options.y))))

  #winners are options.x; lsoers are options.y. Shift data to long format to then shift it wide

  data3<-data2 %>%
    #create newID variable for each "contest"
    mutate(newID=1:nrow(data2)) %>%
    #remove ranking columns for reshape
    select(-ranks.x,-ranks.y) %>%
    #reshape into long
    gather("column","option",options.x,options.y) %>%
    #x is winners so give 1; y is losers so give -1
    mutate(x=ifelse(column=="options.x",1,-1)) %>%
    #remove column column as now obselete with new x column
            select(-column) %>%
    #reshpae to wide fill in zeroes
            spread(option,x,fill=0) %>%
    #if it's a tie add in a 0.5 otherwise add in a 1
              mutate(y=ifelse(tie==FALSE,1,0.5)) %>%
    #remove competition ID from output
                select(-newID)
if(fixnames==TRUE){
  colnames(data3)<-make.names(colnames(data3))
}

attributes(data3)$formula <-paste(colnames(data3 %>% select(-IDs,-tie,-y)),collapse="+")

 return(data3)

}


