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

