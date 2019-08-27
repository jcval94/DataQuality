library(tidyverse)

#check<-c("row","col","all")

fact_as_string<-function(df){
  f_a_s<-function(X){
    if(class(X) %in% c("factor")){
      X<-as.character(X)
    }
    X
  }
  purrr::map_df(df,~f_a_s(.x))
}

Uniquenes<-function(df,check="all"){
  
  if(!"data.frame" %in% class(df))
    df<-as_tibble(df)
  
  
  if(check=="col"){
    dft<-as.data.frame(t(df))
    names(dft)<-row.names(df)
    df<-dft
  }
  #Rows duplicated
  Dup1<-duplicated(df)
  Dup2<-duplicated(df,fromLast = TRUE)
  Dup<-Dup1 | Dup2
  
  row_dup<-row.names(df)[Dup]
  
  Dup_reg<-df[Dup,]
  
  Dup_Count<-as.data.frame(table(fact_as_string(Dup_reg)))
  
  Clean_df<-df[!Dup1,]
  
  rt<-list(Row_Dup=row_dup,
           Clean_df=Clean_df,
           Dup_Count=Dup_Count)
  
  if(check=="all"){
    dft<-as.data.frame(t(df))
    names(dft)<-row.names(df)
    rt2<-Uniquenes(dft,check = "row")
    return(list(rt,rt2))
  }
  return(rt)
}

#Example
data(iris)
Uniquenes(iris,check = "all")
