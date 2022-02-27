
# The GA Function for whole page report ---------------------------------------------

ga_report_stats_all <- function(view_id_number, 
                            records = 25000,
                            start_date = Sys.Date()-30,
                            end_date = Sys.Date()){
  
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
    dimensions = c("date")
  )
  
  #this is getting the events of the page
  ga_stats_events<-google_analytics(
    viewId = view_id_number,
    max = records,
    date_range = c(as.Date(start_date),as.Date(end_date)),
    metrics = c("totalEvents",
                "uniqueEvents"),
    dimensions = c("date")
  )
 
  
  #Combing the data to a signal data frame
  ga_full<-ga_stats_main%>%
    full_join(ga_stats_events)
  
  
  #printing the results head of data.frame
  print(head(ga_full))
  
  
  #returning the data frame
  return(as.data.frame(ga_full))
  
  
  
}
