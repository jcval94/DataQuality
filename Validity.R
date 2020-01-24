#' Title
#'
#' @param df 
#' @param list_values 
#'
#' @return
#' @export fact_as_string from DataQuality
#'
#' @examples
#' 
#' library(tidyverse)
#' 
#' fact_as_string<-function(df){
#' f_a_s<-function(X){
#'   if(class(X) %in% c("factor")){
#'       X<-as.character(X)
#'         }
#'           X
#'         }
#'       purrr::map_df(df,~f_a_s(.x))
#'      }
#' 
#' #Numeric: c(1,10) closed range 
#' Rule<-list(c(1,10),NA,NA,NA,NA)
#' Validity(df = iris,list_values = Rule)
#' 
#' #String/factor: c("setosa", "virginica", "magarita")
#' #Possible
#' Rule<-list(NA,NA,NA,NA,c("setosa","virginica","magarita"))
#' Validity(df,Rule)
#' 
#' Rule<-list(NA,NA,NA,NA,c("setosa","virginica","versicolor","magarita"))
#' Validity(df,Rule)
#' #Multiple
#' 
#' Rule<-list(c(1,10),c(1,3),NA,NA,c("setosa","virginica","versicolor","magarita"))
#' Validity(df = df,list_values = Rule)
#' 
#' 
Validity<-function(df,list_values){
  df<-as_tibble(df)
  if(ncol(df)!=length(list_values)){
    warning("Rule lost")
    return(invisible())
  }
  df_lgl<-is.na(list_values) | is.null(list_values)
  
  dfe<-df[,!df_lgl]
  
  dfe<-fact_as_string(dfe)
  rules<-list_values[!df_lgl]
  
  
  Diagn<-list()
  for(r in 1:length(rules)){
    cls<-class(dfe[[r]])
    rl<-rules[[r]]
    if(cls=="character"){
      cls<-unique(dfe[[r]])
      Diagn[[r]]<-list(New=cls[!cls %in% rl],
                       Extra=rl[!rl %in% cls],
                       Compl=(length(cls)-length(cls[!cls %in% rl]))/length(cls))
    }else{
      nbs<-dfe[[r]]
      cnd<-nbs >=rl[1] & nbs <=rl[2]
      Diagn[[r]]<-list(In=nbs[cnd],
                       Out=nbs[!cnd],
                       Compl=sum(cnd)/length(cnd))
      
    }
  }
  return(Diagn)
  
}
