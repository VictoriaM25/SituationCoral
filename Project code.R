#Load packages

#cleaning data
library(janitor)

#data manipulation
library(tidyr)
library(dplyr)

#create plots
library(ggplot2)

#modelling
library(glmmTMB)

#conduct likelihood ratio tests 
library(lmtest)

#calculate predicted model means
library(emmeans)

#Deal with the data

#import data
csd <- read.csv("Coralspawning.csv")

#clean relevant columns
csd$Situation <- gsub("[^A-Za-z]", "", csd$Situation)
csd$Genus <- gsub("[^A-Za-z]", "", csd$Genus)
csd$Species <- gsub("[^A-Za-z]", "", csd$Species)
csd$Ecoregion <- gsub("[^A-Za-z]", "", csd$Ecoregion)

#ensure spawning values are numeric
csd$DoSRtNFM <- as.numeric(as.character(csd$DoSRtNFM))

#how many ex situ and in situ observations in total (for intro)
s_result <- csd %>%
  group_by(Situation) %>%
  summarise(count = n()) %>%
  spread(key = Situation, value = count, fill = 0)

#Effect of situation on DoSRtNFM at the genus level

#create summary table for genus/ situation
g_result <- csd %>%
  group_by(Genus, Situation) %>%
  summarise(count = n()) %>%
  spread(key = Situation, value = count, fill = 0)

#remove 0s
g_result <- g_result[apply(g_result[, c("Exsitu", "Insitu")], 1, function(x) all(x != 0)), ]

#how many observations is enough?
six <- g_result[g_result$Exsitu >= 6 & g_result$Insitu >= 6, ]

ten <- g_result[g_result$Exsitu >= 10 & g_result$Insitu >= 10, ]

twenty <- g_result[g_result$Exsitu >= 20 & g_result$Insitu >= 20, ]

thirty <- g_result[g_result$Exsitu >= 30 & g_result$Insitu >= 30, ]

#use genera with at least 30 observations per situation
ug <- g_result[g_result$Exsitu >= 30 & g_result$Insitu >= 30, ]

#filter csd based on ug
Gcsd <- csd %>%
  semi_join(ug, by = "Genus")

#count spawning values per situation per genus and convert into proportion
Ghist <- Gcsd %>%
  group_by(Genus, Situation, DoSRtNFM) %>%
  summarise(Count = n(), .groups = "drop") %>%
  group_by(Genus, Situation) %>%
  mutate(Proportion = Count / sum(Count))

#check both contain the same genera to confirm success
unique(Ghist$Genus)
unique(ug$Genus)

#genus plot
Gplot <- ggplot(Ghist, aes(x = DoSRtNFM, y = Proportion, fill = Situation)) +
  geom_bar(stat = "identity", position = "identity", colour = "black", linewidth = 0.4) +
  scale_fill_manual(values = c("Exsitu" = "lightskyblue3", "Insitu" = alpha("wheat2", 0.5)),
                    labels = c("Ex situ", "In situ")) +
  scale_x_continuous(
    breaks = c(-15, -10, -5, 0, 5, 10, 15),  
    labels = c(-15, -10, -5, 0, 5, 10, 15),
  ) +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.line.y = element_line(color = "black", linewidth = 0.3),
    axis.ticks.y = element_line(color = "black", linewidth = 0.3),
    axis.text.y = element_text(color = "black"),
    axis.ticks.x = element_line(color = "black", linewidth = 0.3),
    axis.title.x = element_text(color = "black"),
    axis.title.y = element_text(color = "black"),
    legend.position = "bottom", legend.justification = "centre", legend.direction = "horizontal", legend.key.width = unit(0.65, "cm"), legend.key.height = unit(0.35, "cm")
  ) +
  geom_segment(aes(x = -15, xend = 15, y = 0, yend = 0), color = "black", linewidth = 0.3) +
  scale_y_continuous(expand = c(0, 0)) +
  labs(x = "DoSRtNFM", y = "Proportion of observations", title = "") +
  guides(fill = guide_legend(title = NULL)) +
  facet_wrap(~ Genus, labeller = label_bquote(italic(.(Genus)) ~ "spp."), scales = "free")

print(Gplot)

#GLMM for genus
Gmodel <- glmmTMB(DoSRtNFM ~ Situation + Genus + (1 | Ecoregion), data = Gcsd, family = gaussian(link = "identity"))

summary(Gmodel)

#validate choice of model
#fit the simpler models
rmodel <- glmmTMB(DoSRtNFM ~ Situation + Genus, data = Gcsd, family = gaussian(link = "identity"))
rmodel2 <- glmmTMB(DoSRtNFM ~ Situation + (1 | Ecoregion), data = Gcsd, family = gaussian(link = "identity"))

#fit the full model
fmodel <- glmmTMB(DoSRtNFM ~ Situation + Genus + (1 | Ecoregion), data = Gcsd, family = gaussian(link = "identity"))

#perform the likelihood ratio tests
lrt_result <- lrtest(rmodel, fmodel)
lrt_result2 <- lrtest(rmodel2, fmodel)

#display the test results
print(lrt_result)
print(lrt_result2)

#assess reliability of p value 
#extract confidence interval for situation estimate for genus
Gconf <- confint(Gmodel)
Gconf <- Gconf[2, ]
print(Gconf)

#create graph for predicted model means ± 95% confidence intervals for genus
#obtain these values
Gpredicted_means <- emmeans(Gmodel, ~ Situation + Genus, type = "response")
Gci <- confint(Gpredicted_means)

#create plot
Gciplot <- ggplot(Gci, aes(x = Genus, y = emmean, color = Situation)) +
  geom_point(position = position_dodge(width = 0.8), size = 1.5) +
  geom_errorbar(
    aes(ymin = lower.CL, ymax = upper.CL),
    width = 0.2,
    position = position_dodge(width = 0.8)
  ) +
  labs(
    x = NULL,
    y = "Predicted model mean DoSRtNFM ± 95% C.I."
  ) +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(color = "black", linewidth = 0.3),
    axis.ticks = element_line(color = "black", linewidth = 0.3),
  axis.text.x = element_text(angle = 45, hjust = 1),
    axis.title.x = element_text(color = "black"),
    axis.title.y = element_text(color = "black"),
    legend.title = element_blank(),
  legend.position = "bottom", legend.justification = "centre", legend.direction = "horizontal",
  ) +
  scale_y_continuous(breaks = seq(0, max(Gci$upper.CL), by = 1)) +
  scale_color_manual(values = c("Exsitu" = "lightskyblue3", "Insitu" = "wheat3"),
                     labels = c("Exsitu" = "Ex situ", "Insitu" = "In situ")) +
  scale_x_discrete(labels = function(x) parse(text = paste0("italic('", x, "') ~ 'spp.'")))

print(Gciplot)

#Effect of situation on DoSRtNFM at the species level

#create summary table for genus, species/ situation
gs_result <- csd %>%
  group_by(Genus, Species, Situation) %>%
  summarise(count = n()) %>%
  spread(key = Situation, value = count, fill = 0)

#extract Acropora as genus with the most observations
fgs <- gs_result %>%
  filter(Genus == "Acropora")

#in order of descending Exsitu and Insitu values to visualise the data better
fgs <- fgs[order(fgs$Insitu, decreasing = TRUE), ]
fgs <- fgs[order(fgs$Exsitu, decreasing = TRUE), ]

#extract species with sufficient observations
ufgs <- fgs[c(1, 2, 4), ]

#filter csd based on ufgs
Scsd <- csd %>%
  semi_join(ufgs, by = c("Species"))

#count spawning values per situation per species per genus and convert into proportion
Shist <- Scsd %>%
  group_by(Species, Situation, DoSRtNFM) %>%
  summarise(Count = n(), .groups = "drop") %>%
  group_by(Species, Situation) %>%
  mutate(Proportion = Count / sum(Count))

#check both contain the same species to confirm success
unique(Shist$Species)
unique(ufgs$Species)

#species plot
Splot <- ggplot(Shist, aes(x = DoSRtNFM, y = Proportion, fill = Situation)) +
    geom_bar(stat = "identity", position = "identity", colour = "black", linewidth = 0.4) +
    scale_fill_manual(values = c("Exsitu" = "lightskyblue3", "Insitu" = alpha("wheat2", 0.5)),
                      labels = c("Ex situ", "In situ")) +
    scale_x_continuous(
      breaks = c(-15, -10, -5, 0, 5, 10, 15),  
      labels = c(-15, -10, -5, 0, 5, 10, 15),
    ) +
    theme_minimal() +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.background = element_blank(),
      axis.line.y = element_line(color = "black", linewidth = 0.3),
      axis.ticks.y = element_line(color = "black", linewidth = 0.3),
      axis.text.y = element_text(color = "black"),
      axis.ticks.x = element_line(color = "black", linewidth = 0.3),
      axis.title.x = element_text(color = "black"),
      axis.title.y = element_text(color = "black"),
      legend.position = "bottom", legend.justification = "centre", legend.direction = "horizontal", legend.key.width = unit(0.65, "cm"), legend.key.height = unit(0.35, "cm")
    ) +
    geom_segment(aes(x = -15, xend = 15, y = 0, yend = 0), color = "black", linewidth = 0.3) +
    scale_y_continuous(expand = c(0, 0)) +
    labs(x = "DoSRtNFM", y = "Proportion of observations", title = "") +
    guides(fill = guide_legend(title = NULL)) +
  facet_wrap(~Species, scales = "free", labeller = label_bquote(rows = italic("A."~.(Species))))

print(Splot)

#GLMM for species
Smodel <- glmmTMB(DoSRtNFM ~ Situation + Species + (1 | Ecoregion), data = Scsd, family = gaussian(link = "identity"))

summary(Smodel)

#assess reliability of p value
#extract confidence interval for situation estimate for species
Sconf <- confint(Smodel)
Sconf <- Sconf[2, ]
print(Sconf)

#create graph for predicted model means ± 95% confidence intervals for species
#obtain these values
Spredicted_means <- emmeans(Smodel, ~ Situation + Species, type = "response")
Sci <- confint(Spredicted_means)

#create plot
Sciplot <- ggplot(Sci, aes(x = Species, y = emmean, color = Situation)) +
  geom_point(position = position_dodge(width = 0.8), size = 2) +
  geom_errorbar(
    aes(ymin = lower.CL, ymax = upper.CL),
    width = 0.2,
    position = position_dodge(width = 0.8)
  ) +
  labs(
    x = NULL,
    y = "Predicted model mean DoSRtNFM ± 95% C.I."
  ) +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(color = "black", linewidth = 0.3),
    axis.ticks = element_line(color = "black", linewidth = 0.3),
    axis.text = element_text(color = "black"),
    axis.text.x = element_text(face = "italic"),
    axis.title.x = element_text(color = "black"),
    axis.title.y = element_text(color = "black"),
    legend.title = element_blank(),
    legend.position = "bottom", legend.justification = "centre", legend.direction = "horizontal",
  ) +
  scale_color_manual(values = c("Exsitu" = "lightskyblue3", "Insitu" = "wheat3"),
                     labels = c("Exsitu" = "Ex situ", "Insitu" = "In situ")) +
  scale_x_discrete(labels = function(x) paste("A.", x))

print(Sciplot)
