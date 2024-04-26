### Panel plots ###
library(data.table)
library(tidyverse)
library(jtools)
library(ggplot2)
library(betareg)
library(FactoMineR)
library(factoextra)
library(patchwork) ; library(svglite) ; library(scales)

gc()

metadata <- fread("Data/metadata_elevation.csv")
colnames(metadata) <- c("v", "ID", "lineage", "lat", "lon", "elevation")
mydata_habitat <- read.csv2("Data/mydata_habitat.csv" ) %>% filter(!duplicated(ID))

#mydataROH <- fread("All_ROH_filtered_different_refs.csv")
mydataROH <- fread("Data/Full_results_02_August.csv")
colnames(mydataROH) <- c("NA", "ID", "FRoh")
mydata <- merge(mydataROH, metadata, by="ID") ## Add metadata 
### genetic data (pca)
scree_plot <- readRDS(file = "Genetic_PCA/Scree_plot.rds") ## scree plot
scores <- read.csv2(file = "Genetic_PCA/PCA_data_filtered.csv") ## pca data 
summary(scores)
PEV <- readRDS(file = "Genetic_PCA/PEV.rds") # PEV

##########################################################################################
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ First Panel ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Violin plot + PCA
# Violin
violin <- ggplot(mydata, aes(x=lineage, y=FRoh, fill=lineage))+
  geom_violin() +
  scale_color_brewer(aesthetics = "fill", palette="Set2")+
  geom_point(alpha = 4/10, cex=2.5)+
  xlab("Lineage")+
  ylim(0,0.21)+
  #scale_x_discrete(labels=c("Alaska", "British Columbia","Colorado", 
   #                         "East Coast", "Europe", "Guatemala", "West Coast"))+
  ylab(expression("F"[ROH]))+
  theme_apa()+
  ggtitle("")+
  #theme(aspect.ratio = 1)+
  theme(legend.position = "none")+
  theme(axis.title.y.left = element_text(size=24),
        axis.title.x.bottom = element_text(size=22, vjust = -1),
        axis.text.y.left = element_text(size=20), 
        axis.text.x.bottom = element_text(size=20, vjust = 0) ) ; violin

###### PCA ######
p_pca1 <- ggplot(data = scores, aes(x=scores$X1,y=scores$X2, color=scores$pop)) + theme_apa() +
  #ggtitle(i:i+1) +
  geom_jitter(cex=2.5)+
  stat_ellipse(type="norm")+
  scale_color_brewer(palette = "Dark2")+
  #geom_jitter(fill=scores$pop,size=0.4) +
  xlab(paste("PC ","1" ," (" ,PEV[1],")",sep="")) +
  ylab(paste("PC ","2"," (" ,PEV[2],")",sep="")) +
  scale_x_continuous(n.breaks =4)+
  scale_y_continuous(n.breaks =4)+
  #labs(color="Lineages")+
  #theme(aspect.ratio = 1.25)+
  theme(legend.position="none") + geom_vline(xintercept = 0, lty = 3 , alpha=0.6) +
  theme( axis.title.y.left = element_text(size=22),
        axis.text.y.left = element_text(size=20),
        axis.title.x.bottom = element_text(size=22, vjust = -1),
        axis.text.x.bottom = element_text(size = 20))+
  geom_hline(yintercept = 0,lty = 3,alpha=0.6 ) ; p_pca1

p_pca2 <- ggplot(data = scores, aes(x=scores$X2,y=scores$X3, color=scores$pop)) + theme_apa() +
  #ggtitle(i:i+1) +
  geom_jitter(cex=2.5)+
  stat_ellipse(type="norm")+
  scale_x_continuous(n.breaks =4)+
  scale_y_continuous(n.breaks =4)+
  scale_color_brewer(palette = "Dark2")+
  #geom_jitter(fill=scores$pop,size=0.4) +
  xlab(paste("PC ","2" ," (",PEV[2],")",sep="")) +
  ylab(paste("PC ","3"," (",PEV[3],")",sep="")) +
  #labs(color="Lineages")+
  #theme(aspect.ratio = 1.25)+
  theme(legend.position="none") + geom_vline(xintercept = 0, lty = 3 , alpha=0.6) +
  theme(axis.title.y.left = element_text(size=22),
        axis.text.y.left = element_text(size=20),
        axis.title.x.bottom = element_text(size=22, vjust = -1),
        axis.text.x.bottom = element_text(size = 20))+
  geom_hline(yintercept = 0,lty = 3,alpha=0.6 ) +
  geom_hline(yintercept = 0,lty = 3,alpha=0.6 ) ; p_pca2

### Panel patchwork
#pdf("panel1.pdf", width = 10, height = 12)
#ggsave("panel1_02.svg", plot=panel1,width = 10, height = 12 )
panel1 <-  (p_pca1 | p_pca2) / violin + 
  plot_layout(heights = c(0.4,0.6)) ; panel1#+

#ggsave("panel_left_pca_parent.svg", plot = panel_alternative, width = 19, height = 10)
panel_alternative <- p_pca1 + p_pca2 +violin +plot_layout(design= "AC 
  BC", widths =c(3, 10) , heights = c(5,5)) +
  plot_annotation(tag_levels = list(c("(b)", "(c)", "(d)"))) & theme(plot.tag = element_text(size=20)) ; panel_alternative

#panel_alternative <- (violin) +p_pca1/p_pca2 + 
#  plot_layout(widths = c(10,3), heights = c(5,5)) +
#  plot_annotation(tag_levels = 'A' ) #& theme(aspect.ratio = 1)
###ggsave("panel_alternative.svg", plot = panel_alternative, width = 15, height = 10)



##########################################################################################################################
### Screeplot as supplementary?
library(stringr) 
Scree_plot<- PEV %>%
  as_data_frame() %>% 
  mutate(PC =as.numeric(rownames(.))) %>%
  mutate(value= str_sub(value , end = -2)) %>%
  mutate(value=as.numeric(value)) %>%
  ggplot()+
  geom_col(aes(x=PC, y=value), fill="darkgray", color="black", size=0.7)+
  theme_classic() +
  ggtitle("Genomic PCA")+
  scale_x_continuous(n.breaks = 10)+
  ylab("Explained variance (%)") +
  xlab("Principal components")+
  theme(axis.title.y.left = element_text(size=16),
        axis.text.y.left = element_text(size=12),
        axis.text.x.bottom = element_text(size = 12, face = "bold"),
        title=element_text(size=16)) ; Scree_plot

#pdf("scree_plot_genet.pdf", width=10, height=10)
#ggsave("scree_plot_genet.svg", plot=Scree_plot, width = 10, height=8)
Scree_plot
#dev.off()




##################################################################################
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Figure 3 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# need to add the * for significance, and put better axis
Palette_pca <- c("#66C2A5", "#FC8D62" ,"#8DA0CB" ,"#E78AC3", "#A6D854" , "#E5C494" )
Palette2 <- c("#66C2A5" ,"#FC8D62", "#8DA0CB", "#E78AC3", "#A6D854" ,"#E5C494")

HS <- mydata %>% filter(!lineage=="GU") %>%
  ggplot(data=., aes(x=HS, y=FRoh, color=lineage, fill=lineage))+ 
  geom_point(cex=4,shape = 21, colour = "black")+
  scale_fill_manual(values = Palette2,labels=c("AK", "BC", "CO *", "EC", "EU",  "WC"))+
  scale_color_manual(values=Palette2, guide=F)+
  xlab("LGM Habitat suitability")+
  guides(fill = guide_legend(override.aes = list(linetype = 0)))+
  #guides(color  = NULL)+
  ylab(expression("F"[ROH]))+
  geom_smooth(method="glm",
              method.args = list(family =binomial(link="logit")), 
              se = F,  cex=1.2)+
  theme_apa()+
  theme(aspect.ratio = 1, axis.title.y.left = element_text(size=24),
        axis.title.x.bottom = element_text(size=20, vjust = -2),
        axis.text.y.left = element_text(size=15, color="black"), 
        axis.text.x.bottom = element_text(size=15, color="black", vjust = 0) ) ; HS

LAT <- ggplot(mydata, aes(x=lat, y=FRoh, color=lineage, fill=lineage))+ 
  geom_point(cex=4,shape = 21, colour = "black")+
  scale_fill_manual(values = Palette,labels=c("AK *", "BC", "CO", "EC", "EU", "GU *", "WC"))+
  scale_color_manual(values=Palette, guide=F)+
  guides(fill = guide_legend(override.aes = list(linetype = 0)))+
  xlab("Latitude")+
  ylab(expression("F"[ROH]))+
  geom_smooth(method="glm",
              method.args = list(family =binomial(link="logit")), 
              se = F,  cex=1.2)+
  theme_apa()+
        theme(aspect.ratio = 1, axis.title.y.left = element_text(size=24),
              axis.title.x.bottom = element_text(size=20, vjust = -1),
              axis.text.y.left = element_text(size=15, color="black"), 
              axis.text.x.bottom = element_text(size=15, color="black", vjust = 0) ) ; LAT

EL <- ggplot(mydata, aes(x=elevation, y=FRoh, color=lineage, fill=lineage))+ 
  geom_point(cex=4,shape = 21, colour = "black")+
  scale_fill_manual(values = Palette,labels=c("AK", "BC", "CO", "EC", "EU *", "GU *", "WC"))+
  scale_color_manual(values=Palette, guide=F)+
  guides(fill = guide_legend(override.aes = list(linetype = 0)))+
  geom_smooth(method="glm",
              method.args = list(family =binomial(link="logit")), 
              se = F,  cex=1.2)+
  xlab("Elevation (m)")+
  ylab(expression("F"[ROH]))+
  theme_apa() +
  theme(aspect.ratio = 1, axis.title.y.left = element_text(size=24),
        axis.title.x.bottom = element_text(size=20, vjust = -1),
        axis.text.y.left = element_text(size=15, color="black"), 
        axis.text.x.bottom = element_text(size=15, color="black", vjust = 0) ); EL

library(patchwork)
panel3 <- EL / LAT / HS +plot_annotation(tag_levels = list(c("(a)",
  "(b)", "(c)"))) & theme(plot.tag = element_text(size=20)) ;panel3


##################################################################################
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Figure 2 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

###############################################################
## Removing yellow (Guatemala which is absent of this analysis)
######### ADDING PCA ##############
# Good palette NO GU (orange)
#palette.colors(palette = "Set2")
Palette_pca <- c("#66C2A5", "#FC8D62" ,"#8DA0CB" ,"#E78AC3", "#A6D854" , "#E5C494" )
full_for_pca <- mydata_habitat %>%
  dplyr::select(lineage, lat, elevation,  FRoh, HS)

colnames(full_for_pca) <- c("Lineage", "Latitude" , "Elevation", 
                            "FROH" ,"LGM Habitat Suitability")
res.pca = PCA(full_for_pca, scale.unit=TRUE, quali.sup = 1, graph=F) 

### Scree plot ###
#ggsave("Scree_plot_env_data.svg", plot = scree_data_p, width = 10, height = 9)
scree_data <- as.data.frame(res.pca$eig) %>%
  mutate(comp= rownames(res.pca$eig))
scree_data_p <- ggplot(scree_data, aes(x=comp, y=scree_data$`percentage of variance`))+
  geom_col(fill="darkgray", color="black", size=0.7, width=0.7)+
  theme_classic() +
  ggtitle("PCA with our LGM proxies")+
  xlab("Principal components")+
  ylab("Variance explained (%)") +
  scale_x_discrete(labels=c("1st", "2nd", "3rd", "4th"))+
  theme(title=element_text(size=22), axis.title.y = element_text(vjust = 2, size=18), 
        axis.title.x = element_text( size=18))


### PCA ###

p3 <- fviz_pca_biplot(res.pca, title = NULL, axes = c(1,2),
                # Fill individuals by groups
                geom.ind = "point",
                pointshape = 21, labelsize=6, 
                addEllipses = F,
                ellipse.alpha = 0,
                #ellips=28,
                #mean.point = FALSE,
                mean.point.size = 10,
                mean.point.color="red",
                pointsize = 4,
                fill.ind = full_for_pca$Lineage,
                col.ind = "black",
                col.var = "black",
                legend.title = list(fill = "Lineage"),
                repel = TRUE )+     # Avoid label overplotting)+
  theme_apa()+
  theme(plot.margin = margin(r = -1))+
  theme(strip.text = element_text(face="bold"), 
        axis.title.x = element_text( size=22),
        axis.title.y = element_text( size=22))+
  xlab("PC 1 (42.1%)")+
  ylab("PC 2 (28.1%)")+
  theme(legend.position="none", aspect.ratio = 1)+
  theme(axis.text.y.left = element_text(size=20),  
        axis.text.x.bottom  = element_text(size=20))+
  scale_fill_manual(values = Palette_pca)+
  theme(legend.position="right", legend.title = element_text("Lineages", size=18), 
        legend.text = element_text(size=15)) ;p3
  #ggpubr::fill_palette(Palette_pca) + ggpubr::color_palette(Palette_pca) ;p3 # Indiviual fill color

#ggsave("PCA_facto_v2.svg", plot=p3, width = 11, height = 10 )

ppp <- p3 + p1/p2  +
  plot_layout(widths = c(1,0.5)) & theme(aspect.ratio = 1); ppp

ggsave("panel2.svg", plot = ppp, width =12, height = 8)

# scree plot 
ggplot(res.pca)
###### Other dimensions supplementary Figures ##########

# 2:3
sup_pca2 <- fviz_pca_biplot(res.pca, title = NULL, axes = c(2,3),
                      # Fill individuals by groups
                      geom.ind = "point",
                      pointshape = 21, labelsize=6, 
                      pointsize = 4,
                      fill.ind = full_for_pca$Lineage,
                      col.ind = "black",
                      col.var = "black",
                      legend.title = list(fill = "Lineage"),
                      repel = TRUE      # Avoid label overplotting
)+
  theme_apa()+
  theme(plot.margin = margin(r = -1))+
  theme(strip.text = element_text(face="bold"), 
        axis.title.x = element_text( size=18, vjust = -1),
        axis.title.y = element_text( size=18, vjust = +1.5))+
  theme(legend.position="none", aspect.ratio = 1)+
  ggpubr::fill_palette(Palette_pca)  # Indiviual fill color

## 3:4
sup_pca3 <- fviz_pca_biplot(res.pca, title = NULL, axes = c(3,4),
                      # Fill individuals by groups
                      geom.ind = "point",
                      pointshape = 21, labelsize=6, 
                      pointsize = 4,
                      fill.ind = full_for_pca$Lineage,
                      col.ind = "black",
                      col.var = "black",
                      legend.title = list(fill = "Lineage"),
                      repel = TRUE      # Avoid label overplotting
)+
  theme_apa()+
  theme(plot.margin = margin(r = -1))+
  theme(strip.text = element_text(face="bold"), 
        axis.title.x = element_text( size=18, vjust = -1),
        axis.title.y = element_text( size=18, vjust = 0))+
  theme(legend.position="none", aspect.ratio = 1)+
  ggpubr::fill_palette(Palette_pca)  # Indiviual fill color

sup_pca2 + sup_pca3
#

metadata <- fread("metadata_elevation.csv")
metadata %>%
  group_by(lineage) %>%
  count()


#ggsave("Correlation_graph.svg", plot=sup1, width = 5, height = 4)
sup1 <- fviz_pca_var(res.pca, col.var = "black", col.circle = "blue") +
  xlab('Correlation coefficient to PC1')+
  ylab('Correlation coefficient to PC2')+
  ggtitle("Correlation between PC and variables")+
  theme(title = element_text(size=14))+
  theme_apa();sup1

cor1 <- res.pca$var$cor %>%
  as.data.frame %>%
  mutate(Variable =rownames(.)) %>%
  ggplot(aes(Variable, Dim.1))+
  geom_col(fill="darkgray", color="black", size=0.7, width = 0.8)+
  theme_classic() +
  theme(axis.title.x = element_text( size=16, vjust = -1),
  axis.title.y = element_text( size=12, vjust = 0))+
  scale_y_continuous(n.breaks = 8, labels=label_number(accuracy = 0.1))+
  geom_abline(slope = 0, intercept = 0, color="black")+
  ylab("Correlation to PC1")+
  xlab("")
cor2 <- res.pca$var$cor %>%
  as.data.frame %>%
  mutate(Variable =rownames(.)) %>%
  ggplot(aes(Variable, Dim.2))+
  geom_col(fill="darkgray", color="black", size=0.7, width = 0.8)+
  geom_abline(slope = 0, intercept = 0, color="black")+
  theme_classic() +
  theme(axis.title.x = element_text( size=16, vjust = -1),
        axis.title.y = element_text( size=12, vjust = 0))+
  ylab("Correlation to PC2")+
  xlab("")
cor1 + cor2
pq <- cor1 + cor2

#ggsave("correlation_bar.svg", plot = pq, width=10, height = 6)


##################################################################################
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Figure 4 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#


mydata <- read.csv2(file = "FROH_per_category_10_April.csv") %>% 
  pivot_longer(cols=c("FRoh_600_1700g", "FRoh_600_300g", "FRoh_200_50g",
                      "FRoh_300_100g", "FRoh_100_20g",
                      "FRoh_20_9g", "FRoh_1700_500g",
                      "FRoh_500_100g", "FRoh_500_50g",  "FRoh_50_20g", "FRoh_500_20g",
                      "FRoh_50_9g", "FRoh_1700_200g",
                      "FRoh_200_20g"), 
               names_to = "Category", values_to = "FROH") %>% as.data.frame()


new_plot <- mydata %>%
  filter(Category=="FRoh_1700_200g" |
           Category=="FRoh_200_50g"|
           Category=="FRoh_50_9g") %>%
  mutate(prop_geno = FROH*100) %>%
  ggplot()+
  #geom_violin(mapping=aes(x=factor(Category,
   #                                 levels = c("FRoh_1700_200g", "FRoh_200_50g", "FRoh_50_9g")),
    #                       y=prop_geno, fill=pop), alpha=0.4) +
  geom_boxplot(mapping=aes(x=factor(Category,
                                    levels = c("FRoh_1700_200g", "FRoh_200_50g", "FRoh_50_9g")),
                           y=prop_geno, fill=pop), outliers = FALSE) +
  geom_point(mapping=aes(x=factor(Category,
                                         levels = c("FRoh_1700_200g", "FRoh_200_50g", "FRoh_50_9g")),
                                y=prop_geno, fill=pop), shape=21, alpha=0.5) +
  facet_wrap(~pop, ncol=3, scales = "free", strip.position = "top")+
  #  theme_simple(axis_lines = TRUE, grid_lines = FALSE, base_size = 13,
  #              base_family = "Helvetica") +
  ylab("% of genome IBD") +
    scale_x_discrete(guide = guide_axis(n.dodge = 1), labels=c("[1700:200g]",                        
                            "[200:50g]",
                             "[50:9g]")) +
  scale_fill_brewer(palette = "Set2") +
  xlab("ROH length class [~ generations to common ancestor]") +
  theme_apa() +
  theme(legend.position = "bottom",panel.grid.minor.x = element_line(color="black", linewidth=0.4),
        #panel.grid.major.y = element_line(color="black", linewidth=0.4),
        #axis.line = element_line(color="black", linewidth=0.2),
        axis.line.x.top =element_line(color="black", linewidth=0.4),
        axis.line.y.right = element_line(color="black", linewidth=0.2),
        axis.text.x.bottom = element_text(color="black"),
        axis.text.y.left= element_text(color="black")); new_plot




##################################################################################
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Figure S3 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
df <- read_csv2("Valid_ROH_for_freq_10_April.csv") 


hist1 <- ggplot(df ,aes(x=Length/1000,fill=pop)) +
  geom_histogram(col="black")+
  scale_color_brewer(aesthetics = "fill", palette="Set2")+
  #xlab("ROH length (bp)")+
  #facet_wrap(~ Threshold, ncol=4)+ #scales = "free", )+
  labs(fill="Lineage", y="Count")+
  ylab("")+
  ggtitle("a)")+
  facet_wrap(.~ pop ,ncol=7, scales="free_y")+ #scales = "free", )+
  #xlim(1000,200000)+
  theme_bw() +
  theme(strip.text.y = element_text(),legend.position = "none", axis.title.x.bottom = element_blank()) ; hist1

hist2 <- df %>% filter(Length>9999) %>% # 10kb
  ggplot(. ,aes(x=Length/1000,fill=pop)) +
  geom_histogram(col="black")+
  ggtitle("b)")+
  scale_color_brewer(aesthetics = "fill", palette="Set2")+
  xlab("ROH length (bp)")+
  #facet_wrap(~ Threshold, ncol=4)+ #scales = "free", )+
  labs(fill="Lineage", y="ROH count")+
  facet_wrap(.~ pop ,ncol=7, scales="free_y")+ #scales = "free", )+
  #xlim(1000,200000)+
  theme_bw() +
  theme(strip.text.y = element_text(), axis.title.x.bottom = element_blank()) ; hist2

hist3 <- df %>% filter(Length>19999) %>% # 20kb
  ggplot(. ,aes(x=Length/1000,fill=pop)) +
  geom_histogram(col="black")+
  ggtitle("c)")+
  scale_color_brewer(aesthetics = "fill", palette="Set2")+
  xlab("ROH length (kb)")+
  ylab("")+
  #facet_wrap(~ Threshold, ncol=4)+ #scales = "free", )+
  labs(fill="Lineage")+
  facet_wrap(.~ pop ,ncol=7, scales="free_y")+ #scales = "free", )+
  #xlim(1000,200000)+
  theme_bw() +
  theme(strip.text.y = element_text(), legend.position = "none",
        axis.title.x=element_text(vjust=-1)) ; hist3

library(patchwork)

full <- hist1/hist2/hist3 ; full

ggsave(plot = full, filename = "Histogram_panel.png", width = 16, height = 8, dpi = 300)  

##################################################################################
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Figure S4 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

for_FIS_comp <- read.csv2(file = "FROH_per_category_10_April.csv") %>% 
  pivot_longer(cols=c("FRoh_600_1700g", "FRoh_600_300g", "FRoh_200_50g",
                      "FRoh_300_100g", "FRoh_100_20g",
                      "FRoh_20_9g", "FRoh_1700_500g",
                      "FRoh_500_100g", "FRoh_500_50g",  "FRoh_50_20g", "FRoh_500_20g",
                      "FRoh_50_9g", "FRoh_1700_200g",
                      "FRoh_200_20g"), 
               names_to = "Category", values_to = "FROH") %>% 
  filter(Category=="FRoh_1700_200g"| Category=="FRoh_200_50g"|
           Category=="FRoh_50_9g") %>%
  group_by(ID,pop) %>%
  mutate(FROH_tot=sum(FROH)) %>%
  summarize(FROH_tot=sum(FROH), FIS=mean(as.numeric(F),na.rm = T)) %>%
  mutate(FROH=FROH_tot) %>%
  select(-FROH_tot) %>%
  rename(lineage=pop) %>%
  ggplot()+
  ylab(expression("F"[ROH]))+
  xlab(expression("F"[IS]))+
  geom_point(mapping=aes(x =FIS, y=FROH, fill=lineage),cex=4,shape = 21, colour = "black")+
  scale_fill_brewer(palette = "Set2")+ theme_apa()+theme(aspect.ratio = 1,
        axis.title.y.left = element_text(size=18),
        axis.title.x.bottom = element_text(size=18));for_FIS_comp

ggsave("Supplentary_Figure_FIS.png", for_FIS_comp, width = 8, height = 8)

##################################################################################
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Figure S5 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
refs <- read.csv2("all_fai.csv")
sorted_refs <- refs %>%
  group_by(Lineages) %>%
  arrange(Lineages, desc(length)) %>%
  mutate(
    Cumulative_Sum = cumsum(length),
    Total_length = sum(length),
    Percent_cumu = Cumulative_Sum / Total_length * 100
  )

# Plotting this
p <- ggplot(sorted_refs) +
  geom_col(mapping=aes(x=reorder(name, -length, FUN = sum), y=length, fill = Lineages)) +
  geom_line(mapping=aes(x=reorder(name, -length, FUN = sum), y =Percent_cumu*40000, group=Lineages),
            color = "red") +
  ylim(0, 3e+06)+
  facet_wrap(~Lineages, scales = "free_x", ncol=1) +
  scale_y_continuous(sec.axis = sec_axis(~.*1/40000, 
  breaks = c(0, 20, 40, 60, 80, 100),
  labels = c("0", "20", "40", "60", "80", "100"), name = "Percentage of Genome"))+
  scale_fill_brewer(palette="Set2") +
  theme_classic()+
  theme(axis.text.x.bottom = element_blank(),
        axis.ticks.x.bottom = element_blank())+
  xlab("Scaffolds")+ylab("Length bp")+
  geom_vline(xintercept = which(sorted_refs$length < 50000)[1],
             color = "blue", linetype = "dashed");p

ggsave(file="Scaffold_plots.png", p, width = 5, height = 18)


