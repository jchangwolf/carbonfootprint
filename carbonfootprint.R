## Carbon Footprint
# Script by Jennifer Chang-Wolf
# for modeling and plotting carbon footprint of reviewed articles

#install.packages("xlsx")
#install.packages("directlabels")
library("xlsx")
library(ggplot2)
library("dplyr")
library(directlabels)
library(ggrepel)

## Import data (Carbon Footprint Review Paper Data Points)
data <- read.xlsx(file.choose(), 1)
#data <- read.xlsx("C://Users//Jenny//OneDrive - UMC Utrecht//Shared Folder", 1)
cf <- as.data.frame(data[1:25, 1:18])

## Check normal distributions
# boxplot(cf$"Mean.roundtrip.km.s")
# skewness(cf$"Mean.roundtrip.km.s")
# 
# boxplot(cf$"Population")
# skewness(cf$"Population")

## Set color scheme
require(gridExtra)
fill_col <- c("#ADD8E6", "#808080", "#CC79A7")
outline_col <-  c("#0072B2", "#000000")

## Plot by analysis type
cf$Analysis <- factor(cf$Life.Cycle.Analysis..LCA.,
                      levels = c("LCA Excluded", "LCA Included"))
data <- subset(cf, Analysis %in% c("LCA Excluded", "LCA Included"))
tag <- ggplot(data %>% arrange(Analysis), aes(x = Mean.roundtrip.km.s,
                   y = Mean.roundtrip.emissions.in.kgCO2.e.,
                   color = Analysis)) +
  geom_point(aes(color = Analysis, fill = Analysis,
                  shape = 21, size = 4, stroke = 1.5), show.legend = FALSE) +
  scale_shape_identity() +
  geom_smooth(data = subset(cf, Analysis %in% c("LCA Excluded")),
              aes(x = Mean.roundtrip.km.s,
                  y = Mean.roundtrip.emissions.in.kgCO2.e.),
              color = outline_col[1],
              method = "lm", formula = y~x, fill = NA, show.legend = FALSE) +
  geom_smooth(data = subset(cf, Analysis %in% c("LCA Included")),
              aes(x = Mean.roundtrip.km.s,
                  y = Mean.roundtrip.emissions.in.kgCO2.e.),
              color = outline_col[2],
              method = "lm", formula = y~x-1, fill = NA, show.legend = FALSE) +
  geom_dl(aes(label = Analysis), method = list(dl.trans(x = x - 0.2,
                                                        y = y - 0.5), cex = 1,
                                               "last.points", fontface='bold')) +
#  guides(color=guide_legend(override.aes=list(fill = NA))) +
  theme_classic() +
  theme(legend.title=element_blank()) +
  scale_fill_manual(values = fill_col) +
  scale_color_manual(values = outline_col) +
  xlim(0, 1600) + ylim(0, 400) +
  geom_vline(xintercept = 12.2, size = 0.3, linetype="dashed") +
  geom_text(inherit.aes = FALSE,
            hjust = 0,
            mapping = aes(x = 30, y = 250,
                          label = "Telemedicine becomes \ncarbon cost-effective \naccording to LCA")) +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=12,face="bold"),
        axis.title.y=element_text(angle=0, vjust = 0.5)) +
  labs(x = "Mean Roundtrip Distance (km)", y = "Mean Roundtrip \nEmissions Savings \n(kgCO2)")
tag

## Obtain linear regression equations
summary(lm(data = subset(cf, Analysis %in% c("LCA Included")), Mean.roundtrip.emissions.in.kgCO2.e. ~ Mean.roundtrip.km.s))
# y = 0.249696 x - 3.037337
# LCA Included = 97.5% LCA Excluded
summary(lm(data = subset(cf, Analysis %in% c("LCA Excluded")), Mean.roundtrip.emissions.in.kgCO2.e. ~ Mean.roundtrip.km.s-1))
# y = 0.256239 x
summary(lm(data = cf, Mean.roundtrip.emissions.in.kgCO2.e. ~ Mean.roundtrip.km.s))