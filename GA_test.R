rm(list=ls())
library(dplyr); library(readstata13)

portSize = 300; 

load("sample.rdata")

# randomly select portSize in tot
ranSel = function(portSize, tot) {
  flag = rep(0,1,tot)
  flag[sample(tot, portSize)] = 1
  flag
}

# The data takes largest 2000 stocks (mktcap in USD) from developed country
  # and the benchmark is USD cap weighted for 12 months in 2017.
# Every month randomly select 300 stocks,
  # and one additional stock would be added to the portfolio 
  # if the corresponding country has no investment at all.
# The portfolio is also cap weighted.
# meL_USD : last month end mkt cap in USD
# me_USD : current month end mkt cap in USD
# fx : current month exchange rate to USD
# fxL : last month exchange rate to USD

df = sample.data  %>% rename(country=excntry, ret=ret_loc) %>% 
  arrange(date, country) %>% group_by(date) %>% 
  mutate(benchwt=meL_USD/sum(meL_USD), fx_ret = fx/fxL-1) %>% 
  mutate(flag=ranSel(portSize, n())) %>% 
  group_by(date, country) %>% 
  mutate(tot=sum(flag), 
         flag2=ifelse(tot>0, flag,ranSel(1,n()))) %>% 
  group_by(date) %>% 
  mutate(portwt=meL_USD*flag2/sum(meL_USD*flag2)) %>% 
  select(-me_USD, -prc_loc, -meL_USD, -flag, -fx, -fxL, -flag2)

# take unique dates
dates = sort(unique(df$date))

# take one month from df

k = 3;

d = df %>% filter(date==dates[k]) %>% ungroup()

head(d)

# his GA model aggregated; slides Page-5 Line-4 Formula
(d %>% arrange(country) %>% group_by(country) %>% 
  mutate(ret_USD=(1+ret)*(1+fx_ret)-1) %>% 
  summarise(re=fx_ret[1], wp=sum(portwt), wb=sum(benchwt),
            rp=sum(portwt*ret)/wp,
            rb=sum(benchwt*ret)/wb) %>% 
  ungroup() %>% 
  summarise(val=sum(wp*(rp+re+rp*re)-wb*(rb+re+rb*re))))$val

# actual active return  
  sum(((1+d$ret)*(1+d$fx_ret)-1)*(d$portwt-d$benchwt))

# ~~~~ notice that model may need to be modified if 
  # there is a country include in the benchmark, 
  # but portfolio has no investment in this country


# # --------------------------------------------------------------
# 
# # 
# # GlobalAttribution <- function(d) {
# #   
# #   # ------ rename ------
# # 
# #   # d = data.frame(d)
# #   # d$date = d[,date.var]; d$country = d[,ctry.var]
# #   # d$sector = d[,cat.var]; d$benchwt = d[,bench.weight]
# #   # d$ret = d[,ret.var]; d$fx_ret = d[,fx.var]
# #   # 
# #   # ------ check input ------
# #   
# #   # if (class(d[,date.var]!="Date")) {}
# #   
# #   if (length(unique(d$date))!=1) {
# #     warning("Multiple dates not allowed");# stop()
# #   }
# #   
# #   if (abs(sum(d$benchwt)-1) > 0.000001) {
# #     warning("Invalid bench weights"); stop()
# #   }
# #   
# #   if (abs(sum(d$portwt)-1) > 0.000001) {
# #     warning("Invalid port weights"); stop()
# #   }
# #   
# #   # ------ calculation ------
# #   
# #   d = d[d$date==min(d$date),]
# #   # s = d;
# #   d = d %>% dplyr::arrange(country) %>% group_by(country)
# #   
# #   d$ret_USD <- (1+d$ret)*(1+d$fx_ret) - 1
# #   wpc = data.frame(d %>% dplyr::summarise(val=sum(portwt)))[,"val"]
# #   wbc = data.frame(d %>% dplyr::summarise(val=sum(benchwt)))[,"val"]
# #   rplc = data.frame(d %>% dplyr::summarise(val=sum(portwt*ret)/sum(portwt)))[,"val"]
# #   rblc = data.frame(d %>% dplyr::summarise(val=sum(benchwt*ret)/sum(benchwt)))[,"val"]
# #   rpuc = data.frame(d %>% dplyr::summarise(val=sum(portwt*ret_USD)/sum(portwt)))[,"val"]
# #   rbuc = data.frame(d %>% dplyr::summarise(val=sum(benchwt*ret_USD)/sum(benchwt)))[,"val"]
# #   fx.var = data.frame(d %>% dplyr::summarise(val=mean(fx_ret,na.rm=T)))[,"val"]
# #   
# #   rblg = sum(d$benchwt*d$ret); rplg = sum(d$portwt*d$ret)
# #   
# #   rbug = sum(d$benchwt*d$ret_USD); rpug = sum(d$portwt*d$ret_USD)
# #   
# #   eff_1_country = (wpc-wbc)*(rblc-rblg)
# #   eff_2_Local_Port = wpc*(rplc-rblc)
# #   eff_3_FX_Hedgeable = (wpc-wbc)*fx.var
# #   eff_4_FX_Local = fx.var*(rplc*wpc - rblc*wbc)
# #   
# #   active_return = (d$portwt - d$benchwt) %*% d$ret_USD
# #   active_return2 = sum(eff_1_country,eff_2_Local_Port,eff_3_FX_Hedgeable,eff_4_FX_Local)
# #   
# #   print(active_return - active_return2)
# #   # print(active_return2)
# #   
# # }
# # 
# # GlobalAttribution(d)


# d = data.frame(date=as.Date("2017-12-31"), country=c("A","B","C","D","E"),
#                sector=NA, portwt=runif(5), benchwt=runif(5),
#                ret=runif(5)/25, fx_ret=runif(5)/100)
# d[,c("portwt","benchwt")] = sapply(d[,c("portwt","benchwt")], function(x) x/sum(x))
# rns = sample(5,2); d$ret[rns] = -d$ret[rns]
# rns = sample(5,1); d$fx_ret[rns] = 0
# d

# date.var="date"; ctry.var="country"; cat.var="sector";
# portfolio.weight="portwt"; bench.weight="benchwt";
# ret.var="ret"; fx.var="fx_ret"
# 
# , date.var="date", ctry.var="country", cat.var="sector",
# portfolio.weight="portwt", bench.weight="benchwt",
# ret.var="ret", fx.var="fx_ret"
