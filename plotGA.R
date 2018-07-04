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


# plot GA

#  single day
  # 1 - bar chart weights
  # 2 - bar chart ret_loc & ret_USD
  # 3 - Effs
  # 4 - Effs in %

textSize = 3

sets = list(set1 = c("PORT_WT","BENCH_WT","ACT_BET"),
            set2 = c("PORT_RET_LOC","BENCH_RET_LOC","ACT_RET_LOC",
                     "PORT_RET_USD","BENCH_RET_USD","ACT_RET_USD"),
            set3 = c("CTRY_EFF","LOC_PORT_EFF","HEDG_CUR_EFF","LOC_CUR_EFF"))

sglBarGen = function(dfx, fillvar="variable",textSize=3) {
  dfx$fillvar = dfx[,fillvar]
  ggplot(dfx, aes(x=COUNTRY, y=value, fill=fillvar)) +
    geom_bar(stat="identity", position=position_dodge()) +
    scale_y_continuous(labels=scales::percent) +
    guides(fill=guide_legend(title="")) +
    geom_text(size=textSize, position = position_dodge(width = 1),
              aes(x=COUNTRY, y=value, label=sprintf("%1.2f%%", 100*value))) +
    geom_hline(aes(yintercept=0))
    
}

defineTags = function(setTag) {
  if (setTag=="set1") {
    return(list(ylabs="Weights in %",
                titles="Portfolio, Benchmark and Active Weights by Country on "))
  } else if (setTag=="set2") {
    return(list(ylabs="Returns in %",
                titles="Portfolio, Benchmark and Active Returns by Country on "))
  } else if (setTag=="set3") {
    return(list(ylabs="Effects in %",
                titles="Decompose Active Return on "))
  }
}

plotGA = function(sglDf, sets, textSize=3) {
  
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
                                    ggtitle(paste0(texts$titles,dfx$DATE[1]))),
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




# 
# 
# # savePng=T, pngName=NULL, width=NULL, height=NULL
# 
# 
# 
# defaultDIM = function(width=NULL, height=NULL) {
#   if (is.null(width) & is.null(height)) {
#     width = 10; height = 5
#   } else if (is.null(width) & !is.null(height)) {
#     width = 2*height
#   } else if (!is.null(width) & is.null(height)) {
#     height = .5*width
#   }
#   return(list(width=width, height=height))
# }
# 
# savePngF = function(p, pngName=NULL, width=NULL, height=NULL) {
#   DIM = defaultDIM(width, height)
#   if (is.null(pngName)) {
#     pngName = paste0(getwd(),"/plot_",format(Sys.Date(), "%Y%m%d"),
#                      "_", round(runif(1),2),".png")
#   }
#   ggsave(plot=p, filename=pngName, width=width, height=height)
#   shell.exec(pngName)
# }
# 
# plotGen = function(df, xvar, yvar, grpvar=NULL, 
#                    Type = "bar",
#                     titleName=NULL, xlabs=NULL, ylabs=NULL,
#                    ) {
#   
#   if (!(Type %in% c("bar","line"))) {
#     warning("only support bar / line"); stop()
#   }
#   
#   df$xvar = df[,xvar]; df$yvar = df[,yvar]
# 
#   if (is.null(xlabs)) { xlabs=xvar }
#   if (is.null(ylabs)) { ylabs=yvar }
#   
#   if (!is.null(grpvar)) {
#     df$grpvar = df[,grpvar]
#     p = ggplot(df, aes(x=xvar, y=yvar, group=grpvar, fill=grpvar, color=grpvar))  
#   } else {
#     p = ggplot(df, aes(x=xvar, y=yvar)) 
#   }
#   
#   if (!is.null(facetVar)) {
#     df$facetVar = df[, facetVar]
#     p = p + facet_wrap(~facetVar, ncol=facetCol)
#   }
#   
#   if (Type=="line") {
#     p = p + geom_line()
#   } else if (Type=="bar") {
#     p = p + geom_bar(stat="identity")
#   }
#   
#   p = p + xlab(xlabs) + ylab(ylabs)
#   
#   if (!is.null(titleName)) {
#     p = p + ggtitle(titleName)
#   }
#   
#   if (savePng) {
#     savePngF(p, pngName, width, height)
#   } else {
#     return(p)
#   }
# }
# 
# # plot MGA
#   
# 
# 
