library(tidyverse)
library(ggpubr)
library(xtable)
library(grid)
library(gridExtra)
library(RColorBrewer)


# Read in data
within.pred <- read_csv("../within_predictions.csv")
across.pred <- read_csv("../test_predictions.csv")
strat.pred <- read_csv("../strat_predictions.csv")

# Make a named vector for changing labels
h <- c("a*", "b*", "L*","°Bx", "%DM",
       "FrtCt", "TotalWt", "TotalDM",
       "Len", "Wd", "Wt", "Shp",
        "C0", "C2", "T1", "Prog", "Test")
names(h) <- c("a", "b", "L","Brix", "DM",
              "TotalFrtCt", "TotalWtKg", "Y3",
              "Length", "Width", "Weight", "Shape",
              "C0_uni", "C2_uni", "T1_uni", "Prog", "Test")
              
# Look at within set predictive abilities
 within.pred <- rbind(within.pred)
sets <- c("C0_uni", "C2_uni", "T1_uni")
res.within <- within.pred %>%
			  filter(Set %in% sets) %>%
			  group_by(Rep, Set, Trait) %>%
			  summarise(r = cor(Pred, Val))

res.within$Trait <- sapply(as.character(res.within$Trait), function(x) h[x], USE.NAMES=F)  # Change Trait names
res.within$Set <- sapply(as.character(res.within$Set), function(x) h[x], USE.NAMES=F)  # Change Set names

# Add trait class info
qual <- c("a*", "b*", "L*","°Bx", "%DM")
morph <- c("Len", "Wd", "Wt", "Shp")
yield <- c("FrtCt", "TotalWt", "TotalDM")
res.within$type <- ifelse(res.within$Trait %in% qual, "Quality", 
                         ifelse(res.within$Trait %in% morph, "Morphology", "Yield"))
res.within$Set <- factor(res.within$Set,levels=c("C0", "C2","T1"))
qual.plt <- ggplot(filter(res.within, type == "Quality"),
                  aes(x=Set, y=r, fill=Set)) +
                  geom_boxplot() +
                  xlab("") + ylab("") + ylim(-0.3,0.9) +
                  scale_fill_brewer(palette="Dark2") +
                  facet_grid(type ~ Trait, scales="free") +
                  theme_bw(base_size=18) + 
                  theme(panel.grid=element_blank(),
                        axis.text.x=element_blank(),
                        axis.ticks.x=element_blank(),
                        legend.position="none")
                  
morph.plt <- ggplot(filter(res.within, type == "Morphology"),
                  aes(x=Set, y=r, fill=Set)) +
                  geom_boxplot() +
                  xlab("") + ylab("") + ylim(-0.3,0.9) +
                  scale_fill_brewer(palette="Dark2") +
                  facet_grid(type ~ Trait, scales="free") +
                  theme_bw(base_size=18) + 
                  theme(panel.grid=element_blank(),
                        axis.text.x=element_blank(),
                        axis.ticks.x=element_blank(),
                        legend.position="none")  

yld.plt <- ggplot(filter(res.within, type == "Yield"),
                  aes(x=Set, y=r, fill=Set)) +
                  geom_boxplot() +
                  xlab("") + ylab("") + ylim(-0.3,0.9) +
                  scale_fill_brewer(palette="Dark2") +
                  facet_grid(type ~ Trait, scales="free") +
                  theme_bw(base_size=18) + 
                  theme(panel.grid=element_blank(),
                        axis.text.x=element_blank(),
                        axis.ticks.x=element_blank(),
                        legend.position="none")

lg <- get_legend(ggplot(res.within, aes(x=Set, y=r, fill=Set)) + 
                        geom_boxplot() +
                        scale_fill_brewer(palette="Dark2") +
                        labs(fill="CV Set") +
                        theme_bw(base_size=18)) 

uni_cv.plt <- arrangeGrob(qual.plt, morph.plt, yld.plt, lg,
                          layout_matrix=rbind(c(1,1,1,1,1),
                                              c(2,2,2,2,4),
                                              c(3,3,3,NA,NA)),
                           left=textGrob("Predictive Ability", 
                                         rot=90, hjust=0.5,
                                         gp=gpar(cex=1.5)))
ggsave("./uni_CV.png", uni_cv.plt, "png", width=10)

#  Make a table summarizing across set predictive abilities
across.table <- across.pred %>%
                 group_by(Set, Trait) %>%
                 summarize(r=cor(Pred, Val))

across.table <- spread(across.table, Set, r)
across.table$Trait <- sapply(as.character(across.table$Trait), function(x) h[x], USE.NAMES=F)
print(xtable(across.table), type="latex", file="across_CV.tex",
      include.rownames=F)

# Make Line plot showing predictive ability for each trait for each pop size
res.strat <- strat.pred  %>%
        group_by(Rep, Set, Trait) %>%
        summarise(r = cor(Pred, Val))
res.strat$Trait <- sapply(as.character(res.strat$Trait), function(x) h[x], USE.NAMES=F)  # Change Trait names
res.strat$Type <- ifelse(res.strat$Trait %in% qual, "Quality",
	                      ifelse(res.strat$Trait %in% morph, "Morphology",
	                      ifelse(res.strat$Trait %in% yield, "Yield", NA)))
res.strat$Trait<- as.factor(res.strat$Trait)
res.strat$Trait <- factor(res.strat$Trait,
                         levels=c("L*", "b*", "°Bx", "%DM", "a*"))
qual_strat.plt <- ggplot(filter(res.strat, Type=="Quality"),
              aes(x=Set, y=r, color=Trait)) +
		      geom_smooth(method="loess", se=F) +
		      ylab("Predictive Ability") + xlab("Population Size") +
		      scale_x_continuous(breaks=c(48, 99, 150, 201, 249, 300, 348, 402)) +
		      scale_color_brewer(palette="Dark2") +
			  theme_bw(base_size=18) +
			  theme(legend.position="right", axis.text.x = element_text(angle = 90),
			        panel.grid.major=element_blank(), panel.grid.minor=element_blank())
ggsave("./pop_CV.png", qual_strat.plt, "png")

# Summarize results from applying different weights
# res.weights <- test.weights %>%
        # group_by(Rep, Set, Trait) %>%
        # summarise(r = cor(Pred, Val, use="complete.obs")) %>%
        # group_by( Set, Trait) %>%
        # summarize(mean= signif(mean(r),2), sd=signif(sd(r),2))
#write.csv(res.test, "test_summary.csv", quote=F, row.names=F)
