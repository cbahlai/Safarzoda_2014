##################################
#
#  Analysis of aphid population data 
#  Collected in 20012-13 by Shahlo Safarzoda
#
#  Expt1 is a predator exclusion experiment
#  where the closed, open and sham cages 
#  were used to exclude natural enemies 
#  sham cages were use to quantify cage effects on 
#  aphid population growth
#
#  Expt2 is a predator exclusion experiment
#  designed to parse out effects of ground dwelling vs 
#  foliar foraging predators. Top cages excluded foliar preds
#  and bottom cages excluded ground dwelling predators
#  closed cages excluded all predators and 
#  open cages allowed access to all predators
#
#  Datasets: aphid counts over time, expt 1
#           aphid counts over time, expt 2
#           natural enemy counts over time, expt 2
####################################
library(MASS)
library(plyr)
library (ggplot2)
library(wesanderson)
library(gridExtra)


pal1<-c((wes.palette(5, "Zissou"))[c(1, 4, 5)],(wes.palette(4, "Rushmore"))[4])
shapepal<-c(15,17,19,8)
pd <- position_dodge(.5)

# bring in data
expt1<-read.table(file="https://raw.githubusercontent.com/cbahlai/Safarzoda_2014/master/safarzoda_pred_exclusion.txt", header=TRUE)
expt2<-read.table(file="https://raw.githubusercontent.com/cbahlai/Safarzoda_2014/master/safarzoda_pred_guilds.txt", header=TRUE)
enemies<-read.table(file="https://raw.githubusercontent.com/cbahlai/Safarzoda_2014/master/safarzoda_predators.txt", header=TRUE, na.strings="NA")

#################
# expt 1
#################

#compute numbers of aphids per tiller
expt1$BCHO.tiller<-expt1$BCHO/expt1$tillers
expt1$EG.tiller<-expt1$EG/expt1$tillers

#break data into two sets, one for each year
expt1.2012<-expt1[which(expt1$year==2012),]
expt1.2013<-expt1[which(expt1$year==2013),]


#analyze expt1 population data- BCHO first
expt1.BCHO.model.12<-glm.nb(BCHO~treatment+as.factor(day)+site, offset(tillers), data=expt1.2012)
anova(expt1.BCHO.model.12, test="Rao")
with(expt1.2012, pairwise.t.test(BCHO, treatment, p.adjust.method="holm", paired=TRUE))
#compute summary stats for plotting
expt.bcho.summary.12<-ddply(expt1.2012, .(year, day, treatment), summarize, 
                         N=length(BCHO.tiller),
                         mean=mean(BCHO.tiller),
                         sd   = sd(BCHO.tiller),
                         se   = sd / sqrt(N) )
#create plot
expt1.bcho.2012<-ggplot(expt.bcho.summary.12, aes(day, mean, colour=treatment, shape=treatment))+geom_errorbar(aes(ymin=mean-se, ymax=mean+se), colour="black", width=.2, position=pd)+geom_point(size=6, position=pd)+scale_color_manual(values = pal1, breaks=c("closed","bottom", "top", "open"))+xlab(NULL)+ylab("2012")+theme_bw()+theme(text=element_text(size=18),plot.title=element_text(face="italic"), legend.position="none", plot.margin=unit(c(1,1,2,1), "lines"))+geom_line(size=1, position=pd)+scale_shape_manual(values = shapepal, breaks=c("closed","bottom", "top", "open"))+ggtitle("R. padi")


expt1.BCHO.model.13<-glm.nb(BCHO~treatment+as.factor(day)+site, offset(tillers), data=expt1.2013)
anova(expt1.BCHO.model.13, test="Rao")
with(expt1.2013, pairwise.t.test(BCHO, treatment, p.adjust.method="holm", paired=TRUE))
#compute summary stats for plotting
expt.bcho.summary.13<-ddply(expt1.2013, .(year, day, treatment), summarize, 
                            N=length(BCHO.tiller),
                            mean=mean(BCHO.tiller),
                            sd   = sd(BCHO.tiller),
                            se   = sd / sqrt(N) )
#create plot
expt1.bcho.2013<-ggplot(expt.bcho.summary.13, aes(day, mean, colour=treatment, shape=treatment))+geom_errorbar(aes(ymin=mean-se, ymax=mean+se), colour="black", width=.2, position=pd)+geom_point(size=6, position=pd)+scale_color_manual(values = pal1, breaks=c("closed","bottom", "top", "open"))+xlab(NULL)+ylab("2013")+theme_bw()+theme(text=element_text(size=18),plot.title=element_text(face="italic"), legend.position="none", plot.margin=unit(c(1,1,2,1), "lines"))+geom_line(size=1, position=pd)+scale_shape_manual(values = shapepal, breaks=c("closed","bottom", "top", "open"))+ggtitle(NULL)



#analyze expt1 population data- EG next
expt1.EG.model.12<-glm.nb(EG~treatment+as.factor(day)+site, offset(tillers), data=expt1.2012)
anova(expt1.EG.model.12, test="Rao")
with(expt1.2012, pairwise.t.test(EG, treatment, p.adjust.method="holm", paired=TRUE))
#compute summary stats for plotting
expt1.eg.summary.12<-ddply(expt1.2012, .(day, treatment), summarize, 
                       N=length(EG.tiller),
                       mean=mean(EG.tiller), 
                       sd   = sd(EG.tiller),
                       se   = sd / sqrt(N) )
#create plot
expt1.eg.2012<-ggplot(expt1.eg.summary.12, aes(day, mean, colour=treatment, shape=treatment))+geom_errorbar(aes(ymin=mean-se, ymax=mean+se), colour="black", width=.2, position=pd)+geom_point(size=6, position=pd)+scale_color_manual(values = pal1, breaks=c("closed","bottom", "top", "open"))+xlab(NULL)+ylab(NULL)+theme_bw()+theme(text=element_text(size=18),plot.title=element_text(face="italic"), legend.position="none", plot.margin=unit(c(1,1,2,1), "lines"))+geom_line(size=1, position=pd)+scale_shape_manual(values = shapepal, breaks=c("closed","bottom", "top", "open"))+ggtitle("S. avenae")


expt1.EG.model.13<-glm.nb(EG~treatment+as.factor(day)+site, offset(tillers), data=expt1.2013)
anova(expt1.EG.model.13, test="Rao")
with(expt1.2013, pairwise.t.test(EG, treatment, p.adjust.method="holm", paired=TRUE))
#compute summary stats for plotting
expt1.eg.summary.13<-ddply(expt1.2013, .(day, treatment), summarize, 
                           N=length(EG.tiller),
                           mean=mean(EG.tiller), 
                           sd   = sd(EG.tiller),
                           se   = sd / sqrt(N) )
#create plot
expt1.eg.2013<-ggplot(expt1.eg.summary.13, aes(day, mean, colour=treatment, shape=treatment))+geom_errorbar(aes(ymin=mean-se, ymax=mean+se), colour="black", width=.2, position=pd)+geom_point(size=6, position=pd)+scale_color_manual(values = pal1, breaks=c("closed","bottom", "top", "open"))+xlab(NULL)+ylab(NULL)+theme_bw()+theme(text=element_text(size=18),plot.title=element_text(face="italic"), legend.position="none", plot.margin=unit(c(1,1,2,1), "lines"))+geom_line(size=1, position=pd)+scale_shape_manual(values = shapepal, breaks=c("closed","bottom", "top", "open"))+ggtitle(NULL)




#create legend
expt1.base<-ggplot(expt1.eg.summary.12, aes(day, mean, colour=treatment, shape=treatment))+geom_point(size=6, position=pd)+scale_color_manual(values = pal1, name="Treatment", breaks=c("closed","sham", "open"))+theme_bw()+theme(text=element_text(size=18),plot.title=element_text(face="italic"), legend.key = element_blank())+scale_shape_manual(values=shapepal, name="Treatment", breaks=c("closed","sham", "open"))
#pull legend out of plot
g_legend <- function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

leg.1<-g_legend(expt1.base) 

#stick plots together with a legend on the right
grid.arrange(arrangeGrob(arrangeGrob(arrangeGrob(expt1.bcho.2012, expt1.eg.2012, ncol=2),arrangeGrob(expt1.bcho.2013, expt1.eg.2013, ncol=2), ncol=1),leg.1 ,ncol=2,widths=c(5/6,1/6), heights=c(0.5, 0)), sub=textGrob("Days since infestation", gp=gpar(font=2), vjust=-2), left=textGrob("Aphids per tiller", gp=gpar(font=2), vjust=1, rot=90))




#################
# expt 2
#################



#compute numbers of aphids per tiller
expt2$BCHO.tiller<-expt2$BCHO/expt2$tillers
expt2$EG.tiller<-expt2$EG/expt2$tillers


#analyze expt2 population data- BCHO first
expt2.BCHO.model<-glm(BCHO~treatment+site+as.factor(day), data=expt2)
anova(expt2.BCHO.model, test="Rao")
with(expt2, pairwise.t.test(BCHO, treatment, p.adjust.method="holm", paired=TRUE))
#compute summary stats for plotting
expt2.bcho.summary<-ddply(expt2, .(day, treatment), summarize, 
                         N=length(BCHO.tiller),
                         mean=mean(BCHO.tiller), 
                         sd   = sd(BCHO.tiller),
                         se   = sd / sqrt(N) )
#create plot
expt2.bcho<-ggplot(expt2.bcho.summary, aes(day, mean, colour=treatment, shape=treatment))+geom_errorbar(aes(ymin=mean-se, ymax=mean+se), colour="black", width=.2, position=pd)+geom_point(size=6, position=pd)+scale_color_manual(values = pal1, breaks=c("closed","bottom", "top", "open"))+xlab(NULL)+ylab("Aphids per tiller")+theme_bw()+theme(text=element_text(size=18),plot.title=element_text(face="italic"), legend.position="none", plot.margin=unit(c(1,1,2,1), "lines"))+geom_line(size=1, position=pd)+scale_shape_manual(values = shapepal, breaks=c("closed","bottom", "top", "open"))+ggtitle("R. padi")
expt2.bcho

#analyze expt2 population data- EG next
expt2.EG.model<-glm.nb(EG~treatment+as.factor(day)+site, offset(tillers), data=expt2)
anova(expt2.EG.model, test="Rao")
with(expt2, pairwise.t.test(EG, treatment, p.adjust.method="holm", paired=TRUE))
#compute summary stats for plotting
expt2.eg.summary<-ddply(expt2, .(day, treatment), summarize, 
                         N=length(EG.tiller),
                         mean=mean(EG.tiller), 
                         sd   = sd(EG.tiller),
                         se   = sd / sqrt(N) )
#create plot
expt2.eg<-ggplot(expt2.eg.summary, aes(day, mean, colour=treatment, shape=treatment))+geom_errorbar(aes(ymin=mean-se, ymax=mean+se), colour="black", width=.2, position=pd)+geom_point(size=6, position=pd)+scale_color_manual(values = pal1, breaks=c("closed","bottom", "top", "open"))+xlab(NULL)+ylab(NULL)+theme_bw()+theme(text=element_text(size=18),plot.title=element_text(face="italic"), legend.position="none", plot.margin=unit(c(1,1,2,1), "lines"))+geom_line(size=1, position=pd)+scale_shape_manual(values = shapepal, breaks=c("closed","bottom", "top", "open"))+ggtitle("S. avenae")
expt2.eg

#create legend
expt2.base<-ggplot(expt2.eg.summary, aes(day, mean, colour=treatment, shape=treatment))+geom_point(size=6, position=pd)+scale_color_manual(values = pal1, name="Treatment", breaks=c("closed","bottom", "top", "open"), labels=c("-G-F","-G", "-F", "O"))+theme_bw()+theme(text=element_text(size=18),plot.title=element_text(face="italic"), legend.key = element_blank())+scale_shape_manual(values=shapepal, name="Treatment", breaks=c("closed","bottom", "top", "open"), labels=c("-G-F","-G", "-F", "O"))
#pull legend out of plot


leg.2<-g_legend(expt2.base) 

#stick plots together with a legend on the right
grid.arrange(arrangeGrob(arrangeGrob(expt2.bcho, expt2.eg, ncol=2),leg.2 ,ncol=2,widths=c(5/6,1/6), heights=c(0.5, 0)), sub=textGrob("Days since infestation", gp=gpar(font=2), vjust=-2))

#################
# expt 2 natural enemies
#################



#analyze coccinellid data
coccinellid.model<-glm.nb(Coccinellids~treatment+as.factor(day)+site,  data=enemies)
anova(coccinellid.model, test="Rao")
with(enemies, pairwise.t.test(Coccinellids, treatment, p.adjust.method="holm", paired=TRUE))
#compute summary stats for plotting
coccinellid.summary<-ddply(enemies, .(day, treatment), summarize, 
                          N=length(Coccinellids),
                          mean=mean(Coccinellids), 
                          sd   = sd(Coccinellids),
                          se   = sd / sqrt(N) )
#create plotexpt2.BCHO.model<-glm.nb(BCHO~treatment*as.factor(day)+site+as.factor(row), offset(tillers), data=expt2)
expt2.coccinellid<-ggplot(coccinellid.summary, aes(day, mean, colour=treatment, shape=treatment))+geom_errorbar(aes(ymin=mean-se, ymax=mean+se), colour="black", width=.2, position=pd)+geom_point(size=6, position=pd)+scale_color_manual(values = pal1, breaks=c("closed","bottom", "top", "open"))+xlab(NULL)+ylab("individuals per plot")+theme_bw()+theme(text=element_text(size=18), legend.position="none", plot.margin=unit(c(1,1,2,1), "lines"))+geom_line(size=1, position=pd)+scale_shape_manual(values = shapepal, breaks=c("closed","bottom", "top", "open"))+ggtitle("Coccinellidae")
expt2.coccinellid

#analyze expt2 population data- EG next
carabid.model<-glm.nb(Carabids~treatment+as.factor(day)+site, data=enemies)
anova(carabid.model, test="Rao")
with(enemies, pairwise.t.test(Carabids, treatment, p.adjust.method="holm", paired=TRUE))
#compute summary stats for plotting
carabid.summary<-ddply(enemies, .(day, treatment), summarize, 
                        N=length(Carabids),
                        mean=mean(Carabids, na.rm=TRUE ), 
                        sd   = sd(Carabids, na.rm=TRUE),
                        se   = sd / sqrt(N) )
#create plot
expt2.carabid<-ggplot(carabid.summary, aes(day, mean, colour=treatment, shape=treatment))+geom_errorbar(aes(ymin=mean-se, ymax=mean+se), colour="black", width=.2, position=pd)+geom_point(size=6, position=pd)+scale_color_manual(values = pal1, breaks=c("closed","bottom", "top", "open"))+xlab(NULL)+ylab(NULL)+theme_bw()+theme(text=element_text(size=18), legend.position="none", plot.margin=unit(c(1,1,2,1), "lines"))+geom_line(size=1, position=pd)+scale_shape_manual(values = shapepal, breaks=c("closed","bottom", "top", "open"))+ggtitle("Carabidae")
expt2.carabid

#create legend
enemies.base<-ggplot(carabid.summary, aes(day, mean, colour=treatment, shape=treatment))+geom_point(size=6, position=pd)+scale_color_manual(values = pal1, name="Treatment", breaks=c("closed","bottom", "top", "open"), labels=c("-G-F","-G", "-F", "O"))+theme_bw()+theme(text=element_text(size=18),plot.title=element_text(face="italic"), legend.key = element_blank())+scale_shape_manual(values=shapepal, name="Treatment", breaks=c("closed","bottom", "top", "open"), labels=c("-G-F","-G", "-F", "O"))
#pull legend out of plot


leg.3<-g_legend(enemies.base) 

#stick plots together with a legend on the right
grid.arrange(arrangeGrob(arrangeGrob(expt2.coccinellid, expt2.carabid, ncol=2),leg.3 ,ncol=2,widths=c(5/6,1/6), heights=c(0.5, 0)), sub=textGrob("Days since infestation", gp=gpar(font=2), vjust=-2))
