library(dplyr); library(zoo)

# create a base data table
d1 = data.frame(id = rep(paste0("stk_",1:10),1,30))
d1$date = sort(rep(as.Date(c(40000:40002), origin="1899-12-30"),1,30))
d1$wt = c(runif(10), runif(10), runif(10))
d1$wt = unlist(lapply(1:3, function(x) 
  d1$wt[(10*x-9):(10*x)]/sum(d1$wt[(10*x-9):(10*x)])))

# create a factor table
df_factor = d1[c(1,5,15,29,30),c("id","date")]
df_factor$alpha = round(runif(nrow(df_factor))*10,4)

# merge
df_agg = merge(d1, df_factor, by=c("id","date"), all=T)

# fill
df_agg = df_agg %>% dplyr::arrange(id, date) %>% 
  dplyr::group_by(id) %>% 
  dplyr::mutate(alpha_fill = zoo::na.locf(alpha, na.rm=F)) 

# sort by date
df_agg = df_agg %>% dplyr::ungroup() %>% 
  dplyr::arrange(date,id)

