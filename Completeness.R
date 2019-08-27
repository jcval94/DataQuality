#Completeness
#Completeness is defined as expected comprehensiveness. Data can be complete even if optional data is missing.

#It depends of the problem
library(tidyverse)

Completeness<-function(df,list_na){
  df<-as_tibble(df)
  if(ncol(df)!=length(list_na)){
    warning("Rule lost")
    return(invisible())
  }
  df_lgl<-is.na(list_na) | is.null(list_values)
  
  dfe<-df[,!df_lgl]
  
  rules<-list_values[!df_lgl]
  
  NAs<-purrr::map(df,~is.na(.x) | is.null(.x))
  NAS<-map_lgl(NAs,any)
  NA_sum<-1-map_df(NAs,sum)/nrow(df)
  NA_L<-do.call(c,list_na)
  
  Completeness_ratio<-as.data.frame(ifelse(NA_L,1,NA_sum))
  names(Completeness_ratio)<-names(df)
  Completeness_ratio<-rbind(Completeness_ratio,NA_sum)
  row.names(Completeness_ratio)<-c("After Rule","Real")
  
  conflict<-c()
  for (i in 1:length(NAS)) {
    if(NAS[i]==T & NA_L[i]==F){
      conflict<-c(conflict,T)
    }else{
      conflict<-c(conflict,F)
    }
  }
  
  return(list(Conflicts=names(df)[conflict],Completeness_ratio=Completeness_ratio))
  
}

#Are NA allowed? TRUE="Yes"
Rule<-list(T,T,F,T,T)
Completeness(df,Rule)

