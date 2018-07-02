## make up some data
# dates = as.Date(40000:40600, origin="1899-12-30")
# df = data.frame(Date=sort(rep(dates,1,2000)), val=runif(2000))


GlobalAttributionMulti = function(df, ...) {}


rollingGA = function(df, rollWin=21, ...) {
  
  dates = sort(unique(df$Date))
  
  RollGAResults = list()
  
  for (i in rollWin:length(dates)) {
    # you may need to add start Date and end Date in your output of GlobalAttributionMulti
    RollGAResults[[i+1-rollWin]] = GlobalAttributionMulti(df[which(df$Date %in% dates[(i+1-rollWin):i])], ... )
  }
  # next just use as.data.frame(data.table::rbindlist(...))
  return(RollGAResults)
}


rollingGA = function(df, rollWin=21, ...) {
  
  dates = sort(unique(df$Date))
  
  #  take the index of your subsetted data, 
    # if you have one row for each day, then subSet_idx = 1,2,3,..,21
    # if you have one 2 row for each day, then subSet_idx = 1,2,3,..,42
  subSet_idx = lapply(rollWin:length(dates),
                      function(x){ return(which(df$Date %in% dates[(x+1-rollWin):x]))})
  
  # use subSet_idx to subset your original df by df[x,] where x is element of subSet_idx
    # notice that subSet_idx should be list
    # and you may need to use the following line to verify that x is the correct object you need
    # if you have 2 rows for each day, than the following line should return 42 : 2*rollWin
  lapply(subSet_idx, function(x) length(x))
  
  RollGAResults = lapply(subSet_idx, function(x) GlobalAttributionMulti(df[x,], ... ))
  
  # next just use as.data.frame(data.table::rbindlist(...))
  
  return(RollGAResults)
}


