interval_spd = function(spd.bins){
  df <- data.frame(age=spd.bins$grid$calBP, PrDens=spd.bins$grid$PrDens)
  df$calBP = floor(df$age/100)*100
  df <- df %>% group_by(calBP) %>%
    summarise(SPD_med = median(PrDens)) #take the median. they also use the median in the rcarbon package.
  df <- arrange(df, -row_number())
  return(df)
}