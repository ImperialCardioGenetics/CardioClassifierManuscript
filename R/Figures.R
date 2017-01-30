#FIGURE 2

data<-read.table("../data/InternalDataProportions_core.txt", header=TRUE, sep="\t")
data$Class <- factor(data$Class, c("Pathogenic", "Likely Pathogenic", "VUS", "Likely Benign", "Benign"))

library(ggplot2)
library(gridExtra)

png("../figures/Figure2.png",width=1400,height=600,res=150)

hcm_plot<-ggplot(subset(data, (Test=='HCM' & (Class %in% c('Pathogenic', 'Likely Pathogenic', 'VUS')))), aes(Class, Proportion)) +
  geom_bar(aes(fill=Cohort), position="dodge", stat="identity") +
  scale_fill_manual(values=c("#a2c4c4","#2f7a7a","grey"))

dcm_plot<-ggplot(subset(data, (Test=='DCM' & (Class %in% c('Pathogenic', 'Likely Pathogenic', 'VUS')))), aes(Class, Proportion)) +
  geom_bar(aes(fill=Cohort), position="dodge", stat="identity") +
  scale_fill_manual(values=c("#a2c4c4","#2f7a7a","grey"))

grid.arrange(hcm_plot, dcm_plot, ncol=2)

dev.off()

# Try pie charts

data2<-read.table("../data/InternalDataProportions_core_cumulative.txt", header=TRUE, sep="\t")
data2$Class <- factor(data2$Class, c("Pathogenic", "Likely Pathogenic", "VUS", "Likely Benign", "Benign"))

col_func<-colorRampPalette(c("red","grey","deepskyblue4"))

hcm_hcm<-ggplot(subset(data2, (Test=='HCM' & Cohort=='HCM')), aes(Cohort, Proportion)) +
  geom_bar(aes(fill=Class), stat="identity", width=1, color='black') +
  scale_fill_manual(values=col_func(5)) +
  coord_polar(theta="y") +
  theme_void() + 
  theme(legend.position="bottom") +
  ggtitle("HCM")

hcm_dcm<-ggplot(subset(data2, (Test=='HCM' & Cohort=='DCM')), aes(Cohort, Proportion)) +
  geom_bar(aes(fill=Class), stat="identity", width=1, color='black') +
  scale_fill_manual(values=col_func(5)) +
  coord_polar(theta="y") +
  theme_void() + 
  theme(legend.position="none") +
  ggtitle("DCM")
  
hcm_hvol<-ggplot(subset(data2, (Test=='HCM' & Cohort=='HVOL')), aes(Cohort, Proportion)) +
  geom_bar(aes(fill=Class), stat="identity", width=1, color='black') +
  scale_fill_manual(values=col_func(5)) +
  coord_polar(theta="y") +
  theme_void() + 
  theme(legend.position="none") +
  ggtitle("HVOL")

dcm_hcm<-ggplot(subset(data2, (Test=='DCM' & Cohort=='HCM')), aes(Cohort, Proportion)) +
  geom_bar(aes(fill=Class), stat="identity", width=1, color='black') +
  scale_fill_manual(values=col_func(5)) +
  coord_polar(theta="y") +
  theme_void() + 
  theme(legend.position="none") +
  ggtitle("HCM")

dcm_dcm<-ggplot(subset(data2, (Test=='DCM' & Cohort=='DCM')), aes(Cohort, Proportion)) +
  geom_bar(aes(fill=Class), stat="identity", width=1, color='black') +
  scale_fill_manual(values=col_func(5)) +
  coord_polar(theta="y") +
  theme_void() + 
  theme(legend.position="none") +
  ggtitle("DCM")

dcm_hvol<-ggplot(subset(data2, (Test=='DCM' & Cohort=='HVOL')), aes(Cohort, Proportion)) +
  geom_bar(aes(fill=Class), stat="identity", width=1, color='black') +
  scale_fill_manual(values=col_func(5)) +
  coord_polar(theta="y") +
  theme_void() + 
  theme(legend.position="none") +
  ggtitle("HVOL")

g_legend<-function(a.gplot)
{
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}

legend <- g_legend(hcm_hcm)
hcm_hcm_no_leg<-hcm_hcm + theme(legend.position="none")

hcm_text<-ggplot(data2) + 
  annotate("text", x = 4, y = 25, label = "HCM test", size=6) + 
  theme_void()

dcm_text<-ggplot(data2) + 
  annotate("text", x = 4, y = 25, label = "DCM test", size=6) + 
  theme_void()

png("../figures/PieCharts.png",width=1400,height=800,res=150)
grid.arrange(hcm_text, hcm_hcm_no_leg, hcm_dcm, hcm_hvol, dcm_text, dcm_hcm, dcm_dcm, dcm_hvol, legend, 
             ncol=4, nrow=3, 
             layout_matrix=rbind(c(1,2,3,4), c(5,6,7,8), c(9,9,9,9)), 
             heights=c(10, 10, 1), widths=c(4,10,10,10))
dev.off()

# Try stacked bars

data2<-read.table("../data/InternalDataProportions_core_cumulative.txt", header=TRUE, sep="\t")
data2$Class <- factor(data2$Class, c("Pathogenic", "Likely Pathogenic", "VUS", "Likely Benign", "Benign"))
data2$Cohort <- factor(data2$Cohort, c("HVOL", "DCM", "HCM"))

col_func<-colorRampPalette(c("red","grey","deepskyblue4"))

hcm<-ggplot(subset(data2, (Test=='HCM')), aes(Cohort, Proportion)) +
  geom_bar(aes(fill=Class), stat="identity", width=.75, color='black') +
  scale_fill_manual(values=col_func(5)) +
  theme_classic() +
  theme(legend.position="bottom", axis.title.x=element_blank(), axis.title.y=element_blank()) +
  ggtitle("HCM test") +
  coord_flip()

dcm<-ggplot(subset(data2, (Test=='DCM')), aes(Cohort, Proportion)) +
  geom_bar(aes(fill=Class), stat="identity", width=.75, color='black') +
  scale_fill_manual(values=col_func(5)) +
  theme_classic() +
  ggtitle("DCM test") +
  theme(axis.title.x=element_blank(), axis.title.y=element_blank(), legend.position="none") +
  coord_flip()

legend <- g_legend(hcm)
hcm_no_leg<-hcm + theme(legend.position="none")

png("../figures/StackedBars.png",width=1000,height=1000,res=150)
grid.arrange(hcm_no_leg, dcm, legend, 
             ncol=1, nrow=3, 
             layout_matrix=rbind(c(1), c(2), c(3)), 
             heights=c(10, 10, 1))
dev.off()
