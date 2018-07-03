
library(reshape2); library(ggplot2); library(scales)

df_plot = data.frame(start.date="2017-12-31", end.date="2018-06-30",
                     eff1=c(-0.2,0.3,.4),
                     eff2=runif(3),
                     eff3=runif(3),
                     eff4=runif(3),
                     Measure=c("Active Return","Tracking Error","Info Raio"))
df_plot[,3:6] = apply(df_plot[,3:6], 1, function(x) x/sum(x))

df_plot = reshape2::melt(df_plot, id.vars=c("start.date","end.date","Measure"))

# levels determine the displaying order in ggplot
df_plot$variable = factor(df_plot$variable, levels=c("eff1","eff4","eff3","eff2"))
df_plot$Measure = factor(df_plot$Measure, levels=c("Active Return","Tracking Error","Info Raio"))

p = ggplot(data=df_plot, aes(x=variable, y=value)) +
  geom_bar(stat="identity") +
	# "free_y" scales of y-axis in each panel are different
        # ncol = 1: three panels in one column
        # ncol = 3: three panels in one row
  facet_wrap(~Measure, scales="free_y", ncol=1) +
  xlab("Effects") +
  ylab("Measures") +
  scale_y_continuous(labels=scales::percent) +
  ggtitle("Enter your plot title")

p = ggplot(data=df_plot, aes(x=variable, y=value, fill=Measure)) +
  geom_bar(stat="identity", position=position_dodge()) +
  xlab("Effects") +
  ylab("Measures") +
  scale_y_continuous(labels=scales::percent) +
  ggtitle("Enter your plot title")

fileName = "xxx.png"
# may need to modify height
ggsave(plot=p, file=paste0(getwd(),"/",fileName), width=10, height=6)