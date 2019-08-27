#Accuracy
#Degree to which data correctly reflects the real world object OR an event being described
#It depends of the problem

#To generalize this functions, it is necesacy to compare our information with a new dataset which we know is real

getRealData<-function(Search,Open=TRUE){
  #Search in google with the next additions
  URL_1<-"https://www.google.com/search?q="
  URL_2<-"jje%2B2&oq=jje%2B2"
  URL_3<-"&aqs=chrome..69i57j0l5.1518j0j4"
  URL_4<-"&sourceid=chrome&ie=UTF-8"
  
  URL<-paste0(URL_1,URL_2,URL_3,URL_4)
  
  browseURL(URL)
  #If is a financial data go to yahoo finance
  if("Finance" %in% "Search"){
    
  }
  
}
