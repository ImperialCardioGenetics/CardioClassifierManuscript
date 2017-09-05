library(ggplot2)
library(grid)
library(gtable)
library(gridExtra)

#FIGURE 3

#InterVar figure (3a)

counts<-read.table("../data/ActivatedRules.txt", header=TRUE, sep="\t")

counts$Rule <- factor(counts$Rule, c("PVS1", "PVS1_strong", "PS4", "PM1", "PM2", "PM4", "PM5", "PS1_moderate", "PP2", "PP3", "PP5", "PM5_supporting", "BA1", "BS1", "BS2", "BP1", "BP4", "BP6"))

fig3a <- ggplot(subset(counts, (Rule %in% c("PVS1", "PVS1_strong", "PS4", "PM1", "PM2", "PM4", "PM5", "PS1_moderate", "PP2", "PP3", "PP5", "PM5_supporting"))), aes(Rule, Count)) +
  geom_bar(aes(fill=Tool), position="dodge",stat="identity",width=0.6)+
  scale_fill_manual(values=c("#2f7a7a","grey")) +
  theme(axis.text=element_text(size=12),axis.title.x=element_blank(),axis.title=element_text(size=12),legend.title=element_blank(),axis.text.x = element_text(angle = 45, hjust = 1)) +
  ylab("Number of variants")

#png("../figures/Figure3a.png",width=1400,height=800,res=150)
#fig3a
#dev.off()

# Create multipanel (Figure 3)

flow <- readPNG("../figures/ClinGenComparisonFlowDiagram.png")
g <- rasterGrob(flow, interpolate=TRUE)
lay <- rbind(c(1,1),c(2,2))

png(file="../figures/Figure3.png",width=1400, height=1400,res=120)
grid.arrange(g,fig3a, layout_matrix = lay)
dev.off()

#SUPPLEMENTARY FIGURE 3

data<-read.table("../data/PerBaseCoverageClassifierGenesHVOLsAndExAC.txt", header=TRUE, sep="\t")

GeneList<-c('LMNA','TNNT2','SCN5A','TTN','TCAP','MYH7','VCL','TPM1','TNNC1','RBM20','DSP','BAG3','MYBPC3','PRKAG2','TNNI3','MYL3','MYL2','ACTC1','CSRP3','PLN','GLA','FHL1','LAMP2','GAA','TTR','PKP2','DSG2','DSC2','JUP','RAF1','SOS1','PTPN11','KRAS','KCNQ1','KCNH2','KCNE1','KCNE2','RYR2','FBN1','LDLR')
NumGenes<-length(GeneList)

plot_list=list()
second_plot_list=list()
third_plot_list=list()

for(i in 1:NumGenes)
{
  GeneName<-GeneList[i]
  p<-ggplot() +
    geom_point(data=subset(data, (Gene==GeneName)), aes(GenePosition, ExACProp20, col='ExAC')) +
    geom_point(data=subset(data, (Gene==GeneName)), aes(GenePosition, HVOLProp20, col='TruSight Cardio')) +
    theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), legend.title = element_blank()) +
    xlab(GeneName) +
    ylab("Proportion of Samples >= 20x coverage") +
    scale_color_manual(values=c("grey","#2f7a7a")) +
    scale_y_continuous(limits = c(0,1), expand = c(0, 0)) +
    scale_x_continuous(expand = c(0, 0)) +
    theme(legend.position="none",axis.text=element_text(size=14),axis.title=element_text(size=14))
  
  if (i<16)
  {
    plot_list[[i]]=p
  }
  else if (i<31)
  {
    j=i-15;
    second_plot_list[[j]]=p
  }
  else
  {
    k=i-30;
    third_plot_list[[k]]=p
  }
}

png(file="../figures/SupplementaryFigure3a.png",width=3000,height=4000,res=150)
do.call(grid.arrange, c(plot_list,ncol=3))
dev.off()

png(file="../figures/SupplementaryFigure3b.png",width=3000,height=4000,res=150)
do.call(grid.arrange, c(second_plot_list,ncol=3))
dev.off()

png(file="../figures/SupplementaryFigure3c.png",width=3000,height=3330,res=150)
do.call(grid.arrange, c(third_plot_list,ncol=3))
dev.off()

#SUPPLEMENTARY FIGURE 4

data<-read.table("../data/InternalDataProportions_core.txt", header=TRUE, sep="\t")

data$Class<-factor(data$Class, levels=c('Pathogenic','Likely Pathogenic','VUS'))

vus.prop<-data$Proportion[data$Class=='VUS' & data$Cohort=='HVOL' & data$Curated=='N']
vus.hcm<-data$Proportion[data$Class=='VUS' & data$Cohort=='HCM' & data$Curated=='N']

not_curated<-ggplot(subset(data, (Cohort=='HCM' & Curated=='N')), aes(Cohort, Proportion, fill=Class)) +
  geom_bar(stat="identity", colour="black",width=0.57) +
  scale_fill_manual(values=c('#8D1F18','#BC6157','grey')) +
  ylim(0,0.45) +
  coord_flip() +
  geom_hline(yintercept=vus.prop,linetype = "longdash") +
  geom_segment(aes(x=1.36,y=vus.hcm,xend=1.36,yend=(vus.prop+0.001))) +
  geom_segment(aes(x=1.36,y=0,xend=1.36,yend=(vus.prop-0.001))) +
  geom_segment(aes(x=1.33,y=(vus.prop-0.001),xend=1.39,yend=(vus.prop-0.001))) +
  geom_segment(aes(x=1.33,y=(vus.prop+0.001),xend=1.39,yend=(vus.prop+0.001))) +
  geom_segment(aes(x=1.33,y=vus.hcm,xend=1.39,yend=vus.hcm)) +
  geom_segment(aes(x=1.33,y=0,xend=1.39,yend=0)) +
  annotate("text",x=1.41,y=(vus.prop/2),label="Background VUS rate",size=3.5) +
  annotate("text",x=1.41,y=(((vus.hcm-vus.prop)/2)+vus.prop),label="Case VUS excess",size=3.5) +
  annotate("text",x=1.47,y=0,label="(a)",size=4) +
  theme(legend.title=element_blank(),axis.title.y=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank(),
        axis.title.x=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank(),panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),panel.border = element_blank(),panel.background = element_blank(),axis.text=element_text(size=12),
        axis.title=element_text(size=12),legend.text=element_text(size=12))

vus.prop2<-data$Proportion[data$Class=='VUS' & data$Cohort=='HVOL' & data$Curated=='Y']
vus.hcm2<-data$Proportion[data$Class=='VUS' & data$Cohort=='HCM' & data$Curated=='Y']

curated<-ggplot(subset(data, (Cohort=='HCM' & Curated=='Y')), aes(Cohort, Proportion, fill=Class)) +
  geom_bar(stat="identity", colour="black",width=0.74) +
  scale_fill_manual(values=c('#8D1F18','#BC6157','grey')) +
  ylab('Proportion of samples') +
  ylim(0,0.45) +
  coord_flip() +
  geom_hline(yintercept=vus.prop2,linetype = "longdash") +
  #	geom_segment(aes(x=1.40,y=vus.hcm2,xend=1.40,yend=(vus.prop2+0.001))) +
  #	geom_segment(aes(x=1.40,y=0,xend=1.40,yend=(vus.prop2-0.001))) +
  #	geom_segment(aes(x=1.38,y=(vus.prop2-0.001),xend=1.42,yend=(vus.prop2-0.001))) +
  #	geom_segment(aes(x=1.38,y=(vus.prop2+0.001),xend=1.42,yend=(vus.prop2+0.001))) +
  #	geom_segment(aes(x=1.38,y=vus.hcm2,xend=1.42,yend=vus.hcm2)) +
  #	geom_segment(aes(x=1.38,y=0,xend=1.42,yend=0)) +
  #	annotate("text",x=1.45,y=(vus.prop2/2),label="Background VUS rate",size=3) +
  #	annotate("text",x=1.45,y=(((vus.hcm2-vus.prop2)/2)+vus.prop2),label="Case VUS excess",size=3) +
  annotate("text",x=1.47,y=0,label="(b)",size=4) +
  theme(legend.title=element_blank(),axis.title.y=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank(),panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),panel.border = element_blank(),panel.background = element_blank(),axis.text=element_text(size=12),
        axis.title=element_text(size=12),legend.text=element_text(size=12))

legend <- gtable_filter(ggplot_gtable(ggplot_build(curated + theme(legend.position="bottom"))), "guide-box")
curated_print<-curated + theme(legend.position="none")
not_curated_print<-not_curated + theme(legend.position="none")

png(file="../figures/SupplementaryFigure4.png",width=1000, height=500,res=120)
grid.arrange(not_curated_print, curated_print, legend, ncol=1, heights=c(5,5,1))
dev.off()

supfig4 <- grid.arrange(not_curated_print, curated_print, legend, ncol=1, heights=c(5,5,1))

