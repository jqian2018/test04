# test04

# split d2 to list by country
dList = split(d2[,c("date","country","ret_loc","wpRescale","wbRescale","sector")] %>% 
                mutate(cntry2=country), f=d$country)

print(head(dList[[1]]))

# for each country, call BrinsonSgl
a = list()
for (i in 1:length(dList)) {
    
  x  = dList[[i]]

  ai = BrinsonSgl(x, ret="ret_loc", 
                 benchwt="wbRescale", portwt="wpRescale")
  
  ai = data.frame(country=x$cntry2[[1]], as.data.frame(ai))
  
  a[[i]] = ai; 
  
  rm(ai); print(i)
}

# convert to data.frame
a2 = as.data.frame(rbindlist(a))

# merge with wpc, and adjust for wpc
# refer to cooper's slides page-5 for each country local effect = wpc*(XXX - XXX)
a2 = a2 %>% 
  merge(dcalc[,c("country","wpc")], by="country") %>% 
  mutate(Allocation.Effect=Allocation.Effectwpc, 
         StkSelect.Effect=StkSelect.Effectwpc, 
         Interatction=Interatctionwpc, 
         Active.Ret=Active.Retwpc, date = d$date[1]) %>% 
  select(-wpc)
