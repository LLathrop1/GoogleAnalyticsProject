
# The GA Function for reports ---------------------------------------------

ga_report_stats <- function(view_id_number, 
                            records = 25000,
                            start_date = Sys.Date()-30,
                            end_date = Sys.Date(),
                            page_url){
   
    #this is the stats for page views
          ga_stats_main<-google_analytics(
            viewId = view_id_number,
            max = records,
            date_range = c(as.Date(start_date),as.Date(end_date)),
            metrics = c("pageviews",
                        "uniquePageviews",
                        "timeOnPage",
                        "exits",
                        "entrances",
                        "bounceRate",
                        "exitRate"),
            dimensions = c("date","pagePath","pageTitle"),
            filters= paste0('ga:pagePath=~',as.character(page_url), sep="") 
          )
            
      #this is getting the events of the page
          ga_stats_events<-google_analytics(
            viewId = view_id_number,
            max = records,
            date_range = c(as.Date(start_date),as.Date(end_date)),
            metrics = c("totalEvents",
                        "uniqueEvents"),
            dimensions = c("date","pagePath","pageTitle","eventCategory","eventAction"),
            filters=  paste0('ga:pagePath=~',as.character(page_url), sep="")
      )
          
       #this removes the email address and replaces it with email
          ga_stats_events$eventAction[grepl("mailto:", ga_stats_events$eventAction, ignore.case=TRUE)] <- "Email Click"
        
       #this removes the email address and replaces it with email
          ga_stats_events$eventAction[grepl("youtube", ga_stats_events$eventAction, ignore.case=TRUE)] <- "YouTube View"
      
       #Pivoting the events for a join
          ga_stats_pivot<- ga_stats_events%>%
            pivot_wider(names_from = c(eventCategory,eventAction),
                        names_sep = " - ",
                        values_from = c(totalEvents,uniqueEvents))
          
       #Combing the data to a signal data frame
          ga_full<-ga_stats_main%>%
            full_join(ga_stats_pivot)
          
 
       #printing the results head of data.frame
          print(head(ga_full))
      
          
       #returning the data frame
          return(as.data.frame(ga_full))
}
