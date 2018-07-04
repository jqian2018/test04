rm(list=ls()); 
library(ggplot2); library(dplyr)
library(scales); library(reshape2)

MGA = data.frame(DATE=sort(rep(as.Date(42001:42200, origin="1899-12-30"),1,200*4)),
                 COUNTRY=rep(c("ABC","DEF","GHI","JKL"),1,200*4))

colSet = c("PORT_WT","BENCH_WT","ACT_BET","PORT_RET_LOC","BENCH_RET_LOC",
           "ACT_RET_LOC","FX_RET","PORT_RET_USD","BENCH_RET_USD","ACT_RET_USD",
           "CTRY_EFF","LOC_PORT_EFF","HEDG_CUR_EFF","LOC_CUR_EFF")

for (i in 3:16) { MGA[,i] = runif(nrow(MGA)) }
colnames(MGA)[-c(1,2)] = colSet

MGA = dcast(melt(MGA, id.vars=c("DATE","COUNTRY")) %>% 
        filter(variable == "PORT_WT" | variable == "BENCH_WT") %>% 
        group_by(DATE, variable) %>% mutate(value=value/sum(value)),
        DATE+COUNTRY~variable, value.var="value") %>%
     merge(MGA[,c("DATE","COUNTRY",colSet[-c(1,2)])], by=c("DATE","COUNTRY"))
MGA$ACT_BET = MGA$PORT_WT - MGA$BENCH_WT
MGA$ACT_RET_USD = MGA$PORT_RET_USD - MGA$BENCH_RET_USD

MGA$TOTAL_EFF = rowSums(MGA[,c("CTRY_EFF","LOC_PORT_EFF","HEDG_CUR_EFF","LOC_CUR_EFF")])

MGA = rbind((dcast(melt(MGA, id.vars=c("DATE","COUNTRY")) %>% 
          group_by(DATE,variable) %>% summarise(TOTAL=sum(value)),
          DATE~variable, value.var="TOTAL") %>% mutate(COUNTRY="TOTAL"))[,colnames(MGA)],MGA) %>% 
  arrange(DATE, COUNTRY)

MGAL = melt(MGA, id.vars=c("DATE","COUNTRY"))

sglDf = MGAL %>% filter(DATE==DATE[1])

MGA_total = MGAL %>% filter(COUNTRY=="TOTAL")


# ------- Single period attribution -------

textSize = 3

sets = list(set1 = c("PORT_WT","BENCH_WT","ACT_BET"),
            set2 = c("PORT_RET_LOC","BENCH_RET_LOC","ACT_RET_LOC",
                     "PORT_RET_USD","BENCH_RET_USD","ACT_RET_USD"),
            set3 = c("CTRY_EFF","LOC_PORT_EFF","HEDG_CUR_EFF","LOC_CUR_EFF"))

defineTags = function(setTag) {
  if (setTag=="set1") {
    return(list(ylabs="Weights in %",
                titles="Portfolio, Benchmark and Active Weights by Country on "))
  } else if (setTag=="set2") {
    return(list(ylabs="Returns in %",
                titles="Portfolio, Benchmark and Active Returns by Country on "))
  } else if (setTag=="set3") {
    return(list(ylabs="Attributed Effects in %",
                titles="Decompose Active Return on "))
  }
}

sglBarGen = function(dfx, fillvar="variable",textSize=3, xVar="COUNTRY") {
  
  dfx$fillvar = dfx[,fillvar];
  dfx$xVar = dfx[,xVar]
  ggplot(dfx, aes(x=xVar, y=value, fill=fillvar)) +
    geom_bar(stat="identity", position=position_dodge()) +
    scale_y_continuous(labels=scales::percent) +
    xlab(xVar)+
    guides(fill=guide_legend(title="")) +
    geom_text(size=textSize, position = position_dodge(width = 1),
              aes(x=xVar, y=value, label=sprintf("%1.2f%%", 100*value))) +
    geom_hline(aes(yintercept=0))
    
}

plotGA_byCountry = function(sglDf, sets, textSize=3) {
  
  ggList = list()
  
  for (i in 1:length(sets)) {
    
    dfx = sglDf %>% filter(variable %in% sets[[i]], COUNTRY!="TOTAL")
    texts = defineTags(names(sets)[i])
    
    if (i==3) { dfx$value = dfx$value / sum(dfx$value) } 
    
    if (i==2) {
      
      dfx$port = "Portfolio"
      dfx$port[which(dfx$variable %in% c("BENCH_RET_USD","BENCH_RET_LOC"))] = "Benchmark"
      dfx$port[which(dfx$variable %in% c("ACT_RET_USD","ACT_RET_LOC"))] = "Active"
      dfx$CURCY = "LOCAL"
      dfx$CURCY[which(dfx$variable %in% sets[[i]][4:6])] = "USD"
      dfx$port = factor(dfx$port, levels=c("Portfolio","Benchmark","Active"))
      
      ggList = append(ggList,
                      list(sglBarGen(dfx,textSize=textSize,fillvar="port") +
                                    facet_wrap(~CURCY) +
                                    ylab(texts$ylabs) +
                                    ggtitle(paste0(texts$titles,dfx$DATE[1]))))
      ggList = append(ggList,
                      list(sglBarGen(dfx,textSize=textSize,fillvar="CURCY") +
                             facet_wrap(~port) +
                             ylab(texts$ylabs) +
                             ggtitle(paste0(texts$titles,dfx$DATE[1])))
                      )
      
    } else {
      ggList = append(ggList,list(sglBarGen(dfx,textSize=textSize) +
                               ylab(texts$ylabs) +
                               ggtitle(paste0(texts$titles,dfx$DATE[1]))))
    }; rm(dfx)
  }
  return(ggList)
}

plotGA_Total = function(sglDf, textSize=3) {
  
  dfx = sglDf %>% filter(COUNTRY=="TOTAL", variable!="FX_RET")
  dfxb = dfx[grep("EFF",dfx$variable),] %>% filter(variable!="TOTAL_EFF") %>% 
    mutate(value=value/sum(value))
  dfx = dfx[-grep("EFF",dfx$variable),]
  dfx$port = "Portfolio"
  dfx$port[grep("BENCH",dfx$variable)] = "Benchmark"
  dfx$port[grep("ACT",dfx$variable)] = "Active"
  dfx$port = factor(dfx$port, levels=c("Portfolio","Benchmark","Active"))
  dfx$cat = "Weights"
  dfx$cat[grep("LOC",dfx$variable)] = "Local Return"
  dfx$cat[grep("USD",dfx$variable)] = "USD Return"
  dfx$cat = factor(dfx$cat, levels=c("Weights","Local Return","USD Return"))
  
  return(list(
    sglBarGen(dfx, fillvar="port", textSize=textSize, xVar="port") +
      facet_wrap(~cat, scale="free_y") + xlab("") +
      ylab("Weights / Returns in %") + 
      ggtitle(paste0("Portfolio, Benchmark and Active Weights and Returns on ",dfx$DATE[1])) +
      theme(legend.position="none"),
    
    sglBarGen(dfxb, fillvar="variable", textSize=textSize, xVar="variable") +
      ylab("Attributed Effects in %") + 
      ggtitle(paste0("Decompose Active Return on ",dfx$DATE[1])) +
      theme(legend.position="none")))
  
  
  
}

plotGA_Wrap = function(sglDf, sets, textSize=3) {
  
  return(c(plotGA_byCountry(sglDf, sets, textSize),
           plotGA_Total(sglDf, textSize)))
  
}

# usage
plots_ga = plotGA_Wrap(sglDf, sets, textSize)

# ------- Plot overall -------

# COUNTRY = TOTAL
# barchart AR TE IR Contribution
# barchart IR Risk weights, Component IR, IR Contribution

# COUNTRY = ...........
# barchart AR TE IR Contribution
# barchart IR Risk weights, Component IR, IR Contribution


# ------- Plot time-series -------

# COUNTRY = TOTAL
# plot time-series active weigths and returns
# plot time-series % Effs


