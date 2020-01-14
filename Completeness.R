#' Completeness is defined as expected comprehensiveness. Data can be complete even if optional data is missing.
#'
#' @param df data frame object
#' @param list_na A list of boolean values which indicate if are NA are allowed for each column, where TRUE="Yes" and else FALSE ="No".
#'
#' @return
#' @export
#'
#' @examples
#' 
#' df<-iris
#' Rule<-list(T,T,F,T,T)
#' Completeness(df,Rule)
#' 
Completeness<-function(df,list_na){
  if (!"data.frame" %in% class (df)){
    warning("df must be a data frame");return(invisible())
  }
  df<-tibble::as_tibble(df)
  if(ncol(df)!=length(list_na)){
    warning("Rule lost")
    return(invisible())
  }
  df_lgl<-is.na(list_na) | is.null(list_na)
  dfe<-df[,!df_lgl]
  rules<-list_na[!df_lgl]
  NAs<-purrr::map(df,~is.na(.x) | is.null(.x))
  NAS<-purrr::map_lgl(NAs,any)
  NA_sum<-1-purrr::map_df(NAs,sum)/nrow(df)
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
