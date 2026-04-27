library(tidyverse)
library(ggpubr)
library(lme4)
library(lmerTest)
library(gridExtra)
library(emmeans)

WFF <- read_csv("wffdata.csv")

ditch <- WFF %>%
          filter(SWBType == "Ditch")

pond <- WFF %>%
  filter(SWBType == "Pond")

stream <- WFF %>%
  filter(SWBType == "Stream")

WFF <- WFF %>% 
  mutate(SWBType = fct_relevel(SWBType, c("Pond", "Ditch", "Stream")))
WFF$SWBType <- factor(WFF$SWBType, levels = c("Pond", "Ditch", "Stream"))
colors <- c("Pond" = "#E41A1C", "Ditch" = "#4DAF4A", "Stream" = "#377EB8")


##############################


# OECD species Oct25 ------------------------------------------------------
oecdcolorsindiv <- c("Lemna_minor" = "#ff7f0e", "Glyceria_maxima" = "#42d4f4")


# glyceria maxima
glyceriagraphdata <- WFF %>%  pivot_longer(cols = c(Glyceria_maxima),
                                                  names_to = "Species",
                                                  values_to = "Richness")

glyceriagraph <- ggplot(glyceriagraphdata, aes(x = PollutionRisk, y = Richness, color = Species)) +
  geom_point(size = 1.2) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 0.5) +
  theme_bw() + 
  ylab("Cover (%)") +
  annotate("text", x = 0.4, y= 0.095, color = "black",
           label = "A", size = 5) +
  xlab("Pollution Risk") +
  scale_color_manual(values = oecdcolorsindiv) +
  theme(axis.title.x = element_text(size = 9),
        axis.title.y = element_text(size = 9))  +
  theme(legend.position="none")

#lemna minor
lemnagraphdata <- WFF %>%  pivot_longer(cols = c(Lemna_minor),
                                       names_to = "Species",
                                       values_to = "Richness")

lemnagraph <- ggplot(lemnagraphdata, aes(x = PollutionRisk, y = Richness, color = Species)) +
  geom_point(size = 1.2) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 0.5) +
  scale_color_manual(values = oecdcolorsindiv) +
  theme_bw() + 
  ylab(NULL) +
  xlab("Pollution Risk") +
  annotate("text", x = 0.4, y= 70, color = "black",
           label = "B", size = 5) +
  theme(axis.title.x = element_text(size = 9),
        axis.title.y = element_text(size = 9))  +
  theme(legend.position="none")

# legend
pivotedforoecdlegend <- WFF %>%  pivot_longer(cols = c(Lemna_minor, Glyceria_maxima),
                                                    names_to = "Species",
                                                    values_to = "Richness")

graphoecdlegend <- ggplot(pivotedforoecdlegend, aes(x = PollutionRisk, y = Richness, color = Species)) +
  geom_point(size = 1.2, alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 0.5) +
  theme_bw() + 
  scale_color_manual(values = oecdcolorsindiv) +
  ylab("% Cover") +
  theme(axis.title.x = element_text(size = 9),
        axis.title.y = element_text(size = 9))+
  guides(color = guide_legend(nrow = 6, byrow = TRUE))

oecdlegend <- get_legend(graphoecdlegend)

# put together
ggarrange(glyceriagraph, lemnagraph, oecdlegend, 
          ncol = 3, widths = c(3, 3, 1))


# comparing SWB types against eachother -----------------------------------
## nitro
swbnitro <- lm(data = WFF, Nitrogen ~ SWBType)
summary(swbnitro)
emmeans(swbnitro, ~ SWBType) |> pairs()

swbnitrograph <- ggplot(WFF, aes(x= SWBType, y = Nitrogen, fill = SWBType)) + 
  geom_boxplot() +
  ylab("Nitrogen Concentration (mg/L)") +
  ylim(0, 30) +
  annotate(geom="text", x= 2, y= 17.1, size = 6, label="***") +
  annotate("segment", x = 1, xend = 3, 
           y = 17, yend = 17,
           colour = "black") +  
  annotate(geom="text", x= 1, y= 29, size = 3.5, label="n = 60") +
  annotate(geom="text", x= 2, y= 29, size = 3.5, label="n = 60") +
  annotate(geom="text", x= 3, y= 29, size = 3.5, label="n = 59") +
  xlab(NULL) +
  theme_bw() + 
  theme(axis.title.y = element_text(size = 9),
        axis.text.x = element_blank()) +
  theme(legend.position = "none")



# poll risk
swbpollrisk <- lm(data = WFF, PollutionRisk ~ SWBType)
summary(swbnitro)
emmeans(swbpollrisk, ~ SWBType) |> pairs()

swbpollgraph <- ggplot(WFF, aes(x= SWBType, y = PollutionRisk, fill = SWBType)) + 
  geom_boxplot() +
  ylab("Pollution Risk") +
  xlab("SWB Type") +
  ylim(0, 0.53) +
   annotate(geom="text", x= 2, y= 0.41, size = 6, label="*") +
  annotate("segment", x = 1, xend = 3, 
           y = 0.4, yend = 0.4) +
  annotate(geom="text", x= 1.5, y= 0.44, size = 6, label="***") +
  annotate("segment", x = 1, xend = 2, 
           y = 0.43, yend = 0.43) +  
  annotate(geom="text", x= 2.5, y= 0.47, size = 6, label="***") +
  annotate("segment", x = 2, xend = 3, 
           y = 0.45, yend = 0.45) +  
  annotate(geom="text", x= 1, y= 0.51, size = 3.5, label="n = 60") +
  annotate(geom="text", x= 2, y= 0.51, size = 3.5, label="n = 60") +
  annotate(geom="text", x= 3, y= 0.51, size = 3.5, label="n = 60") +
  theme_bw() + 
  theme(axis.title.x = element_text(size = 9),
        axis.title.y = element_text(size = 9)) +
  theme(legend.position = "none")

# phos
swbphos <- lm(data = WFF, Phosphorus ~ SWBType)
summary(swbphos)
emmeans(swbphos, ~ SWBType) |> pairs()

swbphosgraph <- ggplot(WFF, aes(x= SWBType, y = Phosphorus, fill = SWBType)) + 
  geom_boxplot() +
  ylab("Phosphorus Concentration (mg/L)") +
  ylim(0, 1.55) +
  annotate(geom="text", x= 2, y= 1.3, size = 6, label="**") +
  annotate("segment", x = 1, xend = 3, 
           y = 1.25, yend = 1.25) +
  annotate(geom="text", x= 1.5, y= 1.4, size = 6, label="*") +
  annotate("segment", x = 1, xend = 2, 
           y = 1.35, yend = 1.35) +  
  annotate(geom="text", x= 1, y= 1.5, size = 3.5, label="n = 59") +
  annotate(geom="text", x= 2, y= 1.5, size = 3.5, label="n = 60") +
  annotate(geom="text", x= 3, y= 1.5, size = 3.5, label="n = 59") +
  xlab(NULL) +
  theme_bw() + 
  theme(axis.title.y = element_text(size = 9),
        axis.text.x = element_blank()) +
  theme(legend.position = "none")

# shade
swbshade <- lm(data = WFF, Shade ~ SWBType)
summary(swbshade)
emmeans(swbshade, ~ SWBType) |> pairs()

swbshadegraph <- ggplot(WFF, aes(x= SWBType, y = Shade, fill = SWBType)) + 
  geom_boxplot() +
  ylab("Shade (%)") +
  xlab("SWB Type") +
  ylim(0, 120) +
  annotate(geom="text", x= 2, y= 106, size = 6, label="***") +
  annotate("segment", x = 1, xend = 3, 
           y = 105, yend = 105) +
  annotate(geom="text", x= 1.5, y= 101, size = 6, label="**") +
  annotate("segment", x = 1, xend = 2, 
           y = 100, yend = 100) +  
  annotate(geom="text", x= 2.5, y= 111, size = 6, label="*") +
  annotate("segment", x = 2, xend = 3, 
           y = 110, yend = 110) +  
  annotate(geom="text", x= 1, y= 118, size = 3.5, label="n = 60") +
  annotate(geom="text", x= 2, y= 118, size = 3.5, label="n = 60") +
  annotate(geom="text", x= 3, y= 118, size = 3.5, label="n = 60") +
  theme_bw() + 
  theme(axis.title.x = element_text(size = 9),
        axis.title.y = element_text(size = 9)) +
  theme(legend.position = "none")

# pH
swbpH <- lm(data = WFF, pH ~ SWBType)
summary(swbpH)
emmeans(swbpH, ~ SWBType) |> pairs()

swbphgraph <- ggplot(WFF, aes(x= SWBType, y = pH, fill = SWBType)) + 
  geom_boxplot() +
  ylab("pH") +
  ylim(7.25, 9.8) +
  annotate(geom="text", x= 2, y= 9.1, size = 6, label="***") +
  annotate("segment", x = 1, xend = 3, 
           y = 9, yend = 9) +
  annotate(geom="text", x= 1.5, y= 9.3, size = 6, label="*") +
  annotate("segment", x = 1, xend = 2, 
           y = 9.2, yend = 9.2) +  
  annotate(geom="text", x= 2.5, y= 9.5, size = 6, label="**") +
  annotate("segment", x = 2, xend = 3, 
           y = 9.4, yend = 9.4) +  
  annotate(geom="text", x= 1, y= 9.7, size = 3.5, label="n = 56") +
  annotate(geom="text", x= 2, y= 9.7, size = 3.5, label="n = 57") +
  annotate(geom="text", x= 3, y= 9.7, size = 3.5, label="n = 58") +
  xlab(NULL) +
  theme_bw() + 
  theme(axis.title.y = element_text(size = 9),
        axis.text.x = element_blank()) +
  theme(legend.position = "none")

# conductivity
swbcond <- lm(data = WFF, Conductivity ~ SWBType)
summary(swbcond)
emmeans(swbcond, ~ SWBType) |> pairs()

swbcondgraph <- ggplot(WFF, aes(x= SWBType, y = Conductivity, fill = SWBType)) + 
  geom_boxplot() +
  ylab("Conductivity (µS/cm)") +
  annotate(geom="text", x= 1, y= 1550, size = 3.5, label="n = 60") +
  annotate(geom="text", x= 2, y= 1550, size = 3.5, label="n = 57") +
  annotate(geom="text", x= 3, y= 1550, size = 3.5, label="n = 60") +
  xlab(NULL) +
  theme_bw() + 
  theme(axis.title.y = element_text(size = 9),
        axis.text.x = element_blank()) +
  theme(legend.position = "none") 

## Put together
ggarrange(swbnitrograph, swbphosgraph, swbphgraph, 
          swbcondgraph, swbshadegraph, swbpollgraph,
          ncol = 2, nrow = 3)


# checking catchments -----------------------------------------------------

# nitro sig between B and S
catchnitro <- lm(data = WFF, Nitrogen ~ Catchment)
summary(catchnitro)
emmeans(catchnitro, ~ Catchment) |> pairs()

# phos no sig 
catchphos <- lm(data = WFF, Phosphorus ~ Catchment)
summary(catchphos)
emmeans(catchphos, ~ Catchment) |> pairs()

# pH no sig 
catchph <- lm(data = WFF, pH ~ Catchment)
summary(catchph)
emmeans(catchph, ~ Catchment) |> pairs()

# cond no sig 
catchcond <- lm(data = WFF, Conductivity ~ Catchment)
summary(catchcond)
emmeans(catchcond, ~ Catchment) |> pairs()

# shade no sig 
catchshade <- lm(data = WFF, Shade ~ Catchment)
summary(catchshade)
emmeans(catchshade, ~ Catchment) |> pairs()

# pollrisk B and E
catchpollrisk <- lm(data = WFF, PollutionRisk ~ Catchment)
summary(catchpollrisk)
emmeans(catchpollrisk, ~ Catchment) |> pairs()

# Section 2.1 - abiotic factors vs sp rich --------------------------------

## STATS
# ditch marg: poll risk **
ditchmargrich <- lm(Marginalplantsp ~ Phosphorus + Nitrogen + pH + Conductivity + PollutionRisk + Shade, data = ditch)
summary(ditchmargrich)

# ditch float: there aren't any
ditchfloatrich <- lm(Floatingplantsp ~ Phosphorus + Nitrogen + pH + Conductivity + PollutionRisk + Shade, data = ditch)
summary(ditchfloatrich)

# ditch sub: there aren't any
ditchsubrich <- lm(Submergedplantspp ~ Phosphorus + Nitrogen + pH + Conductivity + PollutionRisk + Shade, data = ditch)
summary(ditchsubrich)

# pond marg: shade *
pondmargrich <- lm(Marginalplantsp ~ Phosphorus + Nitrogen + pH + Conductivity + PollutionRisk + Shade, data = pond)
summary(pondmargrich)

# pond float: no sig
pondfloatrich <- lm(Floatingplantsp ~ Phosphorus + Nitrogen + pH + Conductivity + PollutionRisk + Shade, data = pond)
summary(pondfloatrich)

# pond sub: no sig
pondsubrich <- lm(Submergedplantspp ~ Phosphorus + Nitrogen + pH + Conductivity + PollutionRisk + Shade, data = pond)
summary(pondsubrich)

# stream marg: poll risk, * shade ***
streammargrich <- lm(Marginalplantsp ~ Phosphorus + Nitrogen + pH + Conductivity + PollutionRisk + Shade, data = stream)
summary(streammargrich)

# stream float: there aren't any
streamfloatrich <- lm(Floatingplantsp ~ Phosphorus + Nitrogen + pH + Conductivity + PollutionRisk + Shade, data = stream)
summary(streamfloatrich)

# stream sub: no sig
streamsubrich <- lm(Submergedplantspp ~ Phosphorus + Nitrogen + pH + Conductivity + PollutionRisk + Shade, data = stream)
summary(streamsubrich)


## GRAPHS
pivotedforabioticvssprich <- WFF %>%   pivot_longer(cols = c(Marginalplantsp, Submergedplantspp, Floatingplantsp),
                                                    names_to = "PlantType",
                                                    values_to = "SepRichness")

## Phos
# graph for phos - no sig for any plant type for any
phosrichgraph <- ggplot(pivotedforabioticvssprich, aes(x = Phosphorus, y = SepRichness, color = SWBType)) +
  geom_point(size = 1.2, alpha = 0.6) +
  facet_wrap(~PlantType, 
             labeller = labeller(PlantType = 
                                   c("Marginalplantsp" = "Emergent",
                                     "Floatingplantsp" = "Floating",
                                     "Submergedplantspp" = "Submerged"))) +
  geom_smooth(aes(group = interaction(SWBType, PlantType)), 
              method = "lm", se = FALSE, linewidth = 0.5) +
  scale_color_manual(values = colors) +
  labs(y = NULL, x = "Phosphorus Concentration (mg/L)") +
  theme_bw() + 
  theme(axis.title.x = element_text(size = 9)) +
  theme(strip.background = element_rect(fill = "white", color = NA),
        strip.text = element_text(size = 10, face = "bold"))

## nitro - no sig
nitrorichgraph <- ggplot(pivotedforabioticvssprich, aes(x = Nitrogen, y = SepRichness, color = SWBType)) +
  geom_point(size = 1.2, alpha = 0.6) +
  facet_wrap(~PlantType, 
             labeller = labeller(PlantType = 
                                   c("Marginalplantsp" = "Emergent",
                                     "Floatingplantsp" = "Floating",
                                     "Submergedplantspp" = "Submerged"))) +
  geom_smooth(aes(group = interaction(SWBType, PlantType)), 
              method = "lm", se = FALSE, linewidth = 0.5) +
  scale_color_manual(values = colors) +
  labs(y = "Species Richness", x = "Nitrogen Concentration (mg/L)") +
  theme_bw() + 
  theme(axis.title.x = element_text(size = 9),
    axis.title.y = element_text(size = 9)) +
  theme(strip.background = element_rect(fill = "white", color = NA),
        strip.text = element_text(size = 10, face = "bold"))

## pollrisk: ditch **, stream *
pollriskrichgraph <- ggplot(pivotedforabioticvssprich, aes(x = PollutionRisk, y = SepRichness, color = SWBType)) +
  geom_point(size = 1.2, alpha = 0.6) +
  facet_wrap(~PlantType, 
             labeller = labeller(PlantType = 
                                   c("Marginalplantsp" = "Emergent",
                                     "Floatingplantsp" = "Floating",
                                     "Submergedplantspp" = "Submerged"))) +
  geom_smooth(aes(group = interaction(SWBType, PlantType)), 
              method = "lm", se = FALSE, linewidth = 0.5) +
  scale_color_manual(values = colors) +
  labs(y = NULL, x = "Pollution Risk") +
  theme_bw() + 
  theme(axis.title.x = element_text(size = 9)) +
  theme(strip.background = element_rect(fill = "white", color = NA),
        strip.text = element_text(size = 10, face = "bold"))


## pH
pHrichgraph <- ggplot(pivotedforabioticvssprich, aes(x = pH, y = SepRichness, color = SWBType)) +
  geom_point(size = 1.2, alpha = 0.6) +
  facet_wrap(~PlantType, 
             labeller = labeller(PlantType = 
                                   c("Marginalplantsp" = "Emergent",
                                     "Floatingplantsp" = "Floating",
                                     "Submergedplantspp" = "Submerged"))) +  
  geom_smooth(aes(group = interaction(SWBType, PlantType)), 
              method = "lm", se = FALSE, linewidth = 0.5) +
  scale_color_manual(values = colors) +
  labs(y = "Species Richness", x = "pH") +
  theme_bw() + 
  theme(axis.title.x = element_text(size = 9),
    axis.title.y = element_text(size = 9)) +
  theme(strip.background = element_rect(fill = "white", color = NA),
        strip.text = element_text(size = 10, face = "bold"))

## conductivity - no sig
condrichgraph <- ggplot(pivotedforabioticvssprich, aes(x = Conductivity, y = SepRichness, color = SWBType)) +
  geom_point(size = 1.2, alpha = 0.6) +
  facet_wrap(~PlantType, 
      labeller = labeller(PlantType = 
      c("Marginalplantsp" = "Emergent",
      "Floatingplantsp" = "Floating",
      "Submergedplantspp" = "Submerged"))) +
  geom_smooth(aes(group = interaction(SWBType, PlantType)), 
              method = "lm", se = FALSE, linewidth = 0.5) +
  scale_color_manual(values = colors) +
  labs(y = NULL, x = "Conductivity (µS/cm)") +
  theme_bw() + 
  theme(axis.title.x = element_text(size = 9)) +
  theme(strip.background = element_rect(fill = "white", color = NA),
    strip.text = element_text(size = 10, face = "bold"))
 
## Shade: stream ***, pond *
shaderichgraph <- ggplot(pivotedforabioticvssprich, aes(x = Shade, y = SepRichness, color = SWBType)) +
  geom_point(size = 1.2, alpha = 0.6) +
  facet_wrap(~PlantType, 
             labeller = labeller(PlantType = 
                                   c("Marginalplantsp" = "Emergent",
                                     "Floatingplantsp" = "Floating",
                                     "Submergedplantspp" = "Submerged"))) +
  geom_smooth(aes(group = interaction(SWBType, PlantType)), 
              method = "lm", se = FALSE, linewidth = 0.5) +
  scale_color_manual(values = colors) +
  labs(y = "Species Richness", x = "Shade (%)") +
  theme_bw() + 
  theme(axis.title.x = element_text(size = 9),
    axis.title.y = element_text(size = 9)) +
  theme(strip.background = element_rect(fill = "white", color = NA),
        strip.text = element_text(size = 10, face = "bold"))


## Put together
ggarrange(nitrorichgraph, phosrichgraph, pHrichgraph, 
             condrichgraph, shaderichgraph, pollriskrichgraph,
             common.legend = TRUE,
             ncol = 2, nrow = 3,
             legend = "top")

# Section 2.2 - abiotic factors vs individual sp --------------------------------

# graph for individual species that were impacted by pH
colorsindiv <- c("Agrostis_stolonifera" = "#800000", "Alisma_plantago_aquatica" = "#9A6324", "Angelica_sylvestris" = "#808000", 
                 "Apium_nodiflorum" = "#469990", "Cardamine_pratensis" = "#000075", "Epilobium_hirsutum" = "#e6194B", 
                  "Eleocharis_palustris" = "#f58231", 
                 "Eupatorium_cannabinum" = "#ffe119", "Equisetum_palustre" = "#3cb44b", "Glyceria_fluitans" = "#bfef45",
                 "Glyceria_maxima" = "#42d4f4", "Gnaphalium_uliginosum" = "#FFF333", "Juncus_bufonius" = "#377EB8", 
                 "Juncus_inflexus" = "#4363d8", "Juncus_effusus" = "#911eb4","Lotus_pedunculatus" = "#f032e6", 
                 "Luzula_sylvatica" = "#a9a9a9", "Myosotis_laxa" = "#CC0033", "Myosotis_scorpioides" = "#4DAF4A",
                 "Ranunculus_sceleratus" = "#fabed4", "Rorippa_aquaticum" = "#ffd8b1", 
                 "Rorippa_microphylla" = "#fffac8", "Sparganium_erectum" = "#aaffc3",
                 "Stellaria_uliginosa" = "#dcbeff", "Typha_latifolia" = "#000000", "Veronica_beccabunga" = "#E41A1C")


pivotedforindividualpH <- WFF %>%  pivot_longer(cols = c(Agrostis_stolonifera, Alisma_plantago_aquatica, Angelica_sylvestris, Glyceria_fluitans,
                                                         Glyceria_maxima, Gnaphalium_uliginosum, Juncus_bufonius, Juncus_inflexus, Lotus_pedunculatus,
                                                         Ranunculus_sceleratus, Rorippa_microphylla, Sparganium_erectum),
                                                names_to = "Species",
                                                values_to = "Richness")

graphindividualph <- ggplot(pivotedforindividualpH, aes(x = pH, y = Richness, color = Species)) +
  geom_point(size = 1.2) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 0.5) +
  theme_bw() + 
  annotate("text", x = 8.9, y= 60, color = "black",
           label = "C", size = 5) +
  scale_color_manual(values = colorsindiv) +
  ylab("% Cover") +
  theme(axis.title.x = element_text(size = 9),
        axis.title.y = element_text(size = 9)) +
  theme(legend.position="none")

# graph for individual species that were impacted by poll risk (its just Epilobium_hirsutum)

pivotedforindividualpollrisk <- WFF %>%  pivot_longer(cols = c(Epilobium_hirsutum),
                                                      names_to = "Species",
                                                      values_to = "Richness")

graphindividualpollrisk <- ggplot(pivotedforindividualpollrisk, aes(x = PollutionRisk, y = Richness, color = Species)) +
  geom_point(size = 1.2) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 0.5) +
  theme_bw() + 
  scale_color_manual(values = colorsindiv) +
  annotate("text", x = 0.375, y= 25, color = "black",
           label = "F", size = 5) +
  guides(color = guide_legend(override.aes = list(alpha = 1))) +
  ylab(NULL) +
  xlab("Pollution Risk") +
  theme(axis.title.x = element_text(size = 9),
        axis.title.y = element_text(size = 9))  +
  theme(legend.position="none")

# graph for individual species that were impacted by nitro (its just Juncus_inflexus)

pivotedforindividualnitro <- WFF %>%  pivot_longer(cols = c(Juncus_inflexus),
                                                      names_to = "Species",
                                                      values_to = "Richness")

graphindividualnitro <- ggplot(pivotedforindividualnitro, aes(x = Nitrogen, y = Richness, color = Species)) +
  geom_point(size = 1.2) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 0.5) +
  theme_bw() + 
  ylab("% Cover") +
  annotate("text", x = 25, y= 19, color = "black",
           label = "A", size = 5) +
  scale_color_manual(values = colorsindiv) +
  xlab("Nitrogen (mg/L)") +
  guides(color = guide_legend(override.aes = list(alpha = 1))) +
  theme(axis.title.x = element_text(size = 9),
        axis.title.y = element_text(size = 9))  +
  theme(legend.position="none")

# graph for individual species that were impacted by phos (its just Eupatorium_cannabinum)

pivotedforindividualphos <- WFF %>%  pivot_longer(cols = c(Eupatorium_cannabinum),
                                                  names_to = "Species",
                                                  values_to = "Richness")

graphindividualphos <- ggplot(pivotedforindividualphos, aes(x = Phosphorus, y = Richness, color = Species)) +
  geom_point(size = 1.2) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 0.5) +
  theme_bw() + 
  ylab(NULL) +
  annotate("text", x = 1.125, y= 0.875, color = "black",
           label = "B", size = 5) +
  xlab("Phosphorus (mg/L)") +
  scale_color_manual(values = colorsindiv) +
  theme(axis.title.x = element_text(size = 9),
        axis.title.y = element_text(size = 9))  +
  theme(legend.position="none")

# graph for individual species that were impacted by conduct

pivotedforindividualconduct <- WFF %>%  pivot_longer(cols = c(Equisetum_palustre, Juncus_effusus, Luzula_sylvatica,
                                                            Myosotis_laxa, Myosotis_scorpioides),
                                                     names_to = "Species",
                                                     values_to = "Richness")

graphindividualconduct <- ggplot(pivotedforindividualconduct, aes(x = Conductivity, y = Richness, colour = Species)) +
  geom_point(size = 1.2) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 0.5) +
  theme_bw() + 
  scale_color_manual(values = colorsindiv) +
  ylab(NULL) +
  annotate("text", x = 1450, y= 18, color = "black",
           label = "D", size = 5) +
  xlab("Conductivity (µs/cm)") +
  theme(axis.title.x = element_text(size = 9),
        axis.title.y = element_text(size = 9))  +
  theme(legend.position="none")

# graph for individual species that were impacted by shade

pivotedforindividualshade <- WFF %>%  pivot_longer(cols = c(Agrostis_stolonifera, Apium_nodiflorum, Cardamine_pratensis,
                                                            Eleocharis_palustris, Epilobium_hirsutum, Glyceria_fluitans,
                                                            Glyceria_maxima, Gnaphalium_uliginosum, Juncus_articulatus,
                                                            Juncus_bufonius, Juncus_effusus, Juncus_inflexus, Rorippa_aquaticum,
                                                            Stellaria_uliginosa, Typha_latifolia, Veronica_beccabunga),
                                                   names_to = "Species",
                                                   values_to = "Richness")

graphindividualshade <- ggplot(pivotedforindividualshade, aes(x = Shade, y = Richness, colour = Species)) +
  geom_point(size = 1.2) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 0.5) +
  theme_bw() + 
  annotate("text", x = 96, y= 60, color = "black",
           label = "E", size = 5) +
  scale_color_manual(values = colorsindiv) +
  ylab("% Cover") +
  xlab("Shade (%)") +
  theme(axis.title.x = element_text(size = 9),
        axis.title.y = element_text(size = 9)) +
  theme(legend.position="none")


# this is just a graph so I can get a legend
colorsindiv <- c("Agrostis_stolonifera" = "#800000", "Alisma_plantago_aquatica" = "#9A6324", "Angelica_sylvestris" = "#808000", 
                 "Apium_nodiflorum" = "#469990", "Cardamine_pratensis" = "#000075", "Epilobium_hirsutum" = "#e6194B", 
                 "Eleocharis_palustris" = "#f58231", 
                 "Eupatorium_cannabinum" = "#ffe119", "Equisetum_palustre" = "#3cb44b", "Glyceria_fluitans" = "#bfef45",
                 "Glyceria_maxima" = "#42d4f4", "Gnaphalium_uliginosum" = "#FFF333", "Juncus_bufonius" = "#377EB8", 
                 "Juncus_inflexus" = "#4363d8", "Juncus_effusus" = "#911eb4","Lotus_pedunculatus" = "#f032e6", 
                 "Luzula_sylvatica" = "#a9a9a9", "Myosotis_laxa" = "#CC0033", "Myosotis_scorpioides" = "#4DAF4A",
                 "Ranunculus_sceleratus" = "#fabed4", "Rorippa_aquaticum" = "#ffd8b1", 
                 "Rorippa_microphylla" = "#fffac8", "Sparganium_erectum" = "#aaffc3",
                 "Stellaria_uliginosa" = "#dcbeff", "Typha_latifolia" = "#000000", "Veronica_beccabunga" = "#E41A1C")


pivotedforindividuallegend <- WFF %>%  pivot_longer(cols = c(Agrostis_stolonifera, Alisma_plantago_aquatica, Angelica_sylvestris, 
                                                             Apium_nodiflorum, Cardamine_pratensis, Epilobium_hirsutum, 
                                                             Eleocharis_palustris, 
                                                             Eupatorium_cannabinum, Equisetum_palustre, Glyceria_fluitans,
                                                             Glyceria_maxima, Gnaphalium_uliginosum, Juncus_bufonius, 
                                                             Juncus_inflexus, Juncus_effusus, Lotus_pedunculatus, 
                                                             Luzula_sylvatica, Myosotis_laxa, Myosotis_scorpioides,
                                                             Ranunculus_sceleratus, Rorippa_aquaticum, 
                                                             Rorippa_microphylla, Sparganium_erectum,
                                                             Stellaria_uliginosa, Typha_latifolia, Veronica_beccabunga),
                                                names_to = "Species",
                                                values_to = "Richness")

graphindividuallegend <- ggplot(pivotedforindividuallegend, aes(x = pH, y = Richness, color = Species)) +
  geom_point(size = 1.2, alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 0.5) +
  theme_bw() + 
  scale_color_manual(values = colorsindiv) +
  ylab("% Cover") +
  theme(axis.title.x = element_text(size = 9),
        axis.title.y = element_text(size = 9))+
  guides(color = guide_legend(nrow = 6, byrow = TRUE))

legend <- get_legend(graphindividuallegend)

combinedindivgraph <- grid.arrange(graphindividualnitro, graphindividualphos, graphindividualph,
             graphindividualconduct, graphindividualshade, graphindividualpollrisk,
             ncol=2)

grid.arrange(legend, combinedindivgraph,
             ncol = 1,
             heights = c(0.2, 1))

# Agrostis_stolonifera pH ***, shade **
model <- lmer(Agrostis_stolonifera ~ Phosphorus + Nitrogen + pH + Conductivity + PollutionRisk + Shade + (1|SWBType), data = WFF)
summary(model)

# Alisma_plantago_aquatica pH *
model <- lmer(Alisma_plantago_aquatica ~ Phosphorus + Nitrogen + pH + Conductivity + PollutionRisk + Shade + (1|SWBType), data = WFF)
summary(model)

# Angelica_sylvestris pH *
model <- lmer(Angelica_sylvestris ~ Phosphorus + Nitrogen + pH + Conductivity + PollutionRisk + Shade + (1|SWBType), data = WFF)
summary(model)

# Apium_nodiflorum shade *
model <- lmer(Apium_nodiflorum ~ Phosphorus + Nitrogen + pH + Conductivity + PollutionRisk + Shade + (1|SWBType), data = WFF)
summary(model)

# Cardamine_pratensis shade *
model <- lmer(Cardamine_pratensis ~ Phosphorus + Nitrogen + pH + Conductivity + PollutionRisk + Shade + (1|SWBType), data = WFF)
summary(model)

# Eleocharis_palustris shade *
model <- lmer(Eleocharis_palustris ~ Phosphorus + Nitrogen + pH + Conductivity + PollutionRisk + Shade + (1|SWBType), data = WFF)
summary(model)

# Epilobium_hirsutum pollrisk * shade ***
model <- lmer(Epilobium_hirsutum ~ Phosphorus + Nitrogen + pH + Conductivity + PollutionRisk + Shade + (1|SWBType), data = WFF)
summary(model)

# Equisetum_palustre conductivity *
model <- lmer(Equisetum_palustre ~ Phosphorus + Nitrogen + pH + Conductivity + PollutionRisk + Shade + (1|SWBType), data = WFF)
summary(model)

# Eupatorium_cannabinum phos *
model <- lmer(Eupatorium_cannabinum ~ Phosphorus + Nitrogen + pH + Conductivity + PollutionRisk + Shade + (1|SWBType), data = WFF)
summary(model)

# Glyceria_fluitans pH *, shade **
model <- lmer(Glyceria_fluitans ~ Phosphorus + Nitrogen + pH + Conductivity + PollutionRisk + Shade + (1|SWBType), data = WFF)
summary(model)

# Glyceria_maxima pH *, shade **
model <- lmer(Glyceria_maxima ~ Phosphorus + Nitrogen + pH + Conductivity + PollutionRisk + Shade + (1|SWBType), data = WFF)
summary(model)

# Gnaphalium_uliginosum pH **, shade *
model <- lmer(Gnaphalium_uliginosum ~ Phosphorus + Nitrogen + pH + Conductivity + PollutionRisk + Shade + (1|SWBType), data = WFF)
summary(model)

# Juncus_articulatus shade **
model <- lmer(Juncus_articulatus ~ Phosphorus + Nitrogen + pH + Conductivity + PollutionRisk + Shade + (1|SWBType), data = WFF)
summary(model)

# Juncus_bufonius pH *, shade *
model <- lmer(Juncus_bufonius ~ Phosphorus + Nitrogen + pH + Conductivity + PollutionRisk + Shade + (1|SWBType), data = WFF)
summary(model)

# Juncus_effusus, conduct ***, shade *
model <- lmer(Juncus_effusus ~ Phosphorus + Nitrogen + pH + Conductivity + PollutionRisk + Shade + (1|SWBType), data = WFF)
summary(model)

# Juncus_inflexus nitro **, pH *, shade ***
model <- lmer(Juncus_inflexus ~ Phosphorus + Nitrogen + pH + Conductivity + PollutionRisk + Shade + (1|SWBType), data = WFF)
summary(model)

# Lotus_pedunculatus pH *
model <- lmer(Lotus_pedunculatus ~ Phosphorus + Nitrogen + pH + Conductivity + PollutionRisk + Shade + (1|SWBType), data = WFF)
summary(model)

# Luzula_sylvatica conduct *
model <- lmer(Luzula_sylvatica ~ Phosphorus + Nitrogen + pH + Conductivity + PollutionRisk + Shade + (1|SWBType), data = WFF)
summary(model)

# Myosotis_laxa conduct *
model <- lmer(Myosotis_laxa ~ Phosphorus + Nitrogen + pH + Conductivity + PollutionRisk + Shade + (1|SWBType), data = WFF)
summary(model)

# Myosotis_scorpioides conduct * 
model <- lmer(Myosotis_scorpioides ~ Phosphorus + Nitrogen + pH + Conductivity + PollutionRisk + Shade + (1|SWBType), data = WFF)
summary(model)

# Ranunculus_sceleratus pH **
model <- lmer(Ranunculus_sceleratus ~ Phosphorus + Nitrogen + pH + Conductivity + PollutionRisk + Shade + (1|SWBType), data = WFF)
summary(model)

# Rorippa_microphylla pH *
model <- lmer(Rorippa_microphylla ~ Phosphorus + Nitrogen + pH + Conductivity + PollutionRisk + Shade + (1|SWBType), data = WFF)
summary(model)

# Rorippa_aquaticum shade ***
model <- lmer(Rorippa_aquaticum ~ Phosphorus + Nitrogen + pH + Conductivity + PollutionRisk + Shade + (1|SWBType), data = WFF)
summary(model)

# Sparganium_erectum pH *
model <- lmer(Sparganium_erectum ~ Phosphorus + Nitrogen + pH + Conductivity + PollutionRisk + Shade + (1|SWBType), data = WFF)
summary(model)

# Stellaria_uliginosa shade *
model <- lmer(Stellaria_uliginosa ~ Phosphorus + Nitrogen + pH + Conductivity + PollutionRisk + Shade + (1|SWBType), data = WFF)
summary(model)

# Typha_latifolia shade *
model <- lmer(Typha_latifolia ~ Phosphorus + Nitrogen + pH + Conductivity + PollutionRisk + Shade + (1|SWBType), data = WFF)
summary(model)

# Veronica_beccabunga shade *
model <- lmer(Veronica_beccabunga ~ Phosphorus + Nitrogen + pH + Conductivity + PollutionRisk + Shade + (1|SWBType), data = WFF)
summary(model)

# Section 3.1 - abiotic factors vs macro div --------------------------------

## Shannon

# STATS: combined lms for shannon (subdivided by SWBtype)

# ditch: poll risk ***
ditchshannon <- lm(Shannon ~ Phosphorus + Nitrogen + pH + Conductivity + PollutionRisk + Shade, data = ditch)
summary(ditchshannon)

# pond: no sig
pondshannon <- lm(Shannon ~ Phosphorus + Nitrogen + pH + Conductivity + PollutionRisk + Shade, data = pond)
summary(pondshannon)

# stream: poll risk *
streamshannon <- lm(Shannon ~ Phosphorus + Nitrogen + pH + Conductivity + PollutionRisk + Shade, data = stream)
summary(streamshannon)

colors <- c("Pond" = "#E41A1C", "Ditch" = "#4DAF4A", "Stream" = "#377EB8")

# Phosphorus
phosshangraph <- ggplot(WFF, aes(x= Phosphorus, y = Shannon, color = SWBType)) + 
  geom_point(size = 1.2, alpha = 0.6) +
  geom_smooth(method = lm, 
              se = FALSE) +
  xlab("Phosphorus Concentration (mg/L)") +
  ylab(NULL) +
  annotate("text", x = 1.12, y= 2, color = "black",
           label = "B", size = 5) +
  theme_bw() + 
  theme(axis.title.x = element_text(size = 9))


# Nitrogen
nitroshangraph <- ggplot(WFF, aes(x= Nitrogen, y = Shannon, color = SWBType)) + 
  geom_point(size = 1.2, alpha = 0.6) +
  geom_smooth(method = lm, 
              se = FALSE) +
  xlab("Nitrogen Concentration (mg/L)") +
  ylab("Shannon's Diversity Index") +
  annotate("text", x = 25, y= 2, color = "black",
           label = "A", size = 5) +
  theme_bw() + 
  theme(axis.title.x = element_text(size = 9),
        axis.title.y = element_text(size = 9))


# Pollution Risk
pollriskshangraph <- ggplot(WFF, aes(x= PollutionRisk, y = Shannon, color = SWBType)) + 
  geom_point(size = 1.2, alpha = 0.6) +
  geom_smooth(method = lm, 
              se = FALSE) +
  annotate(geom="text", x= 0.35, y= 1.7, size = 6, label="***", color="darkgreen") +
  annotate(geom="text", x= 0.35, y= 1.5, size = 6, label="*", color="darkblue") +
  xlab("Pollution Risk") +
  ylab(NULL) +
  annotate("text", x = 0.35, y= 2, color = "black",
           label = "F", size = 5) +
  theme_bw() + 
  theme(axis.title.x = element_text(size = 9))

# pH
phshangraph <- ggplot(WFF, aes(x= pH, y = Shannon, color = SWBType)) + 
  geom_point(size = 1.2, alpha = 0.6) +
  geom_smooth(method = lm, 
              se = FALSE) +
  xlab("pH") +
  ylab("Shannon's Diversity Index") +
  annotate("text", x = 8.75, y= 2, color = "black",
           label = "C", size = 5) +
  theme_bw() + 
  theme(axis.title.x = element_text(size = 9),
        axis.title.y = element_text(size = 9))

# Conductivity
condshangraph <- ggplot(WFF, aes(x= Conductivity, y = Shannon, color = SWBType)) + 
  geom_point(size = 1.2, alpha = 0.6) +
  geom_smooth(method = lm, 
              se = FALSE) +
  xlab("Conductivity (µS/cm)") +
  ylab(NULL) +
  annotate("text", x = 1300, y= 2, color = "black",
           label = "D", size = 5) +
  theme_bw() + 
  theme(axis.title.x = element_text(size = 9))

# Shade
shadeshangraph <- ggplot(WFF, aes(x= Shade, y = Shannon, color = SWBType)) + 
  geom_point(size = 1.2, alpha = 0.6) +
  geom_smooth(method = lm, 
              se = FALSE) +
  xlab("Shade (%)") +
  ylab("Shannon's Diversity Index") +
  theme_bw() + 
  annotate("text", x = 91, y= 2, color = "black",
           label = "E", size = 5) +
  theme(axis.title.x = element_text(size = 9),
        axis.title.y = element_text(size = 9))

## Put together
ggarrange(nitroshangraph, phosshangraph, phshangraph, 
          condshangraph, shadeshangraph, pollriskshangraph,
          common.legend = TRUE,
          ncol = 2, nrow = 3,
          legend = "top")

## Evenness

# ditch: poll risk **
ditchEvenness <- lm(Evenness ~ Phosphorus + Nitrogen + pH + Conductivity + PollutionRisk + Shade, data = ditch)
summary(ditchEvenness)

# pond: no sig
pondEvenness <- lm(Evenness ~ Phosphorus + Nitrogen + pH + Conductivity + PollutionRisk + Shade, data = pond)
summary(pondEvenness)

# stream: shade ***
streamEvenness <- lm(Evenness ~ Phosphorus + Nitrogen + pH + Conductivity + PollutionRisk + Shade, data = stream)
summary(streamEvenness)

# Phosphorus
phosevengraph <- ggplot(WFF, aes(x= Phosphorus, y = Evenness, color = SWBType)) + 
  geom_point(size = 1.2, alpha = 0.6) +
  geom_smooth(method = lm, 
              se = FALSE) +
  xlab("Phosphorus Concentration (mg/L)") +
  ylab(NULL) +
  annotate("text", x = 1.125, y= 1.2, color = "black",
           label = "B", size = 5) +
  theme_bw() + 
  theme(axis.title.x = element_text(size = 9))


# Nitrogen
nitroevengraph <- ggplot(WFF, aes(x= Nitrogen, y = Evenness, color = SWBType)) + 
  geom_point(size = 1.2, alpha = 0.6) +
  geom_smooth(method = lm, 
              se = FALSE) +
  xlab("Nitrogen Concentration (mg/L)") +
  ylab("Species Evenness") +
  annotate("text", x = 25, y= 1, color = "black",
           label = "A", size = 5) +
  theme_bw() + 
  theme(axis.title.x = element_text(size = 9),
        axis.title.y = element_text(size = 9))

# Pollution Risk
pollriskevengraph <- ggplot(WFF, aes(x= PollutionRisk, y = Evenness, color = SWBType)) + 
  geom_point(size = 1.2, alpha = 0.6) +
  geom_smooth(method = lm, 
              se = FALSE) +
  xlab("Pollution Risk") +
  ylab(NULL) +
  annotate(geom="text", x= 0.35, y= 0.875, size = 6, label="**", color="darkgreen") +
  annotate("text", x = 0.35, y= 1, color = "black",
           label = "F", size = 5) +
  theme_bw() + 
  theme(axis.title.x = element_text(size = 9))


# pH
phevengraph <- ggplot(WFF, aes(x= pH, y = Evenness, color = SWBType)) + 
  geom_point(size = 1.2, alpha = 0.6) +
  geom_smooth(method = lm, 
              se = FALSE) +
  xlab("pH") +
  ylab("Species Evenness") +
  annotate("text", x = 8.75, y= 1, color = "black",
           label = "C", size = 5) +
  theme_bw() + 
  theme(axis.title.x = element_text(size = 9),
        axis.title.y = element_text(size = 9))

# Conductivity
condevengraph <- ggplot(WFF, aes(x= Conductivity, y = Evenness, color = SWBType)) + 
  geom_point(size = 1.2, alpha = 0.6) +
  geom_smooth(method = lm, 
              se = FALSE) +
  xlab("Conductivity (µS/cm)") +
  ylab(NULL) +
  annotate("text", x = 1300, y= 1, color = "black",
           label = "D", size = 5) +
  theme_bw() + 
  theme(axis.title.x = element_text(size = 9))


# Shade
shadeevengraph <- ggplot(WFF, aes(x= Shade, y = Evenness, color = SWBType)) + 
  geom_point(size = 1.2, alpha = 0.6) +
  geom_smooth(method = lm, 
              se = FALSE) +
  xlab("Shade (%)") +
  ylab("Species Evenness") +
  annotate(geom="text", x= 75, y= 0.825, size = 6, label="*", color="darkblue") +
  theme_bw() + 
  annotate("text", x = 91, y= 0.96, color = "black",
           label = "E", size = 5) +
  theme(axis.title.x = element_text(size = 9),
        axis.title.y = element_text(size = 9))


## Put together
ggarrange(nitroevengraph, phosevengraph, phevengraph, 
          condevengraph, shadeevengraph, pollriskevengraph,
          common.legend = TRUE,
          ncol = 2, nrow = 3,
          legend = "top")

# Section 4.1 - abiotic factors vs func div --------------------------------
## FDDivergence

# ditch: shade *
ditchfddiv <- lm(FDDivergence ~ Phosphorus + Nitrogen + pH + Conductivity + PollutionRisk + Shade, data = ditch)
summary(ditchfddiv)

# pond: no sig
pondfddiv <- lm(FDDivergence ~ Phosphorus + Nitrogen + pH + Conductivity + PollutionRisk + Shade, data = pond)
summary(pondfddiv)

# stream: no sig
streamfddiv <- lm(FDDivergence ~ Phosphorus + Nitrogen + pH + Conductivity + PollutionRisk + Shade, data = stream)
summary(streamfddiv)

# Phosphorus
phosfddivgraph <- ggplot(WFF, aes(x= Phosphorus, y = FDDivergence, color = SWBType)) + 
  geom_point(size = 1.2, alpha = 0.6) +
  geom_smooth(method = lm, 
              se = FALSE) +
  xlab("Phosphorus Concentration (mg/L)") +
  ylab(NULL) +
  xlim(0, 0.7) +
  annotate("text", x = 0.7, y= 1, color = "black",
           label = "B", size = 5) +
  theme_bw() + 
  theme(axis.title.x = element_text(size = 9))


# Nitrogen
nitrofddivgraph <- ggplot(WFF, aes(x= Nitrogen, y = FDDivergence, color = SWBType)) + 
  geom_point(size = 1.2, alpha = 0.6) +
  geom_smooth(method = lm, 
              se = FALSE) +
  xlab("Nitrogen Concentration (mg/L)") +
  ylab("Functional Divergence") +
  xlim(0, 15) +
  annotate("text", x = 15, y= 1, color = "black",
           label = "A", size = 5) +
  theme_bw() + 
  theme(axis.title.x = element_text(size = 9),
        axis.title.y = element_text(size = 9))

# Pollution Risk
pollriskfddivgraph <- ggplot(WFF, aes(x= PollutionRisk, y = FDDivergence, color = SWBType)) + 
  geom_point(size = 1.2, alpha = 0.6) +
  geom_smooth(method = lm, 
              se = FALSE) +
  xlab("Pollution Risk") +
  ylab(NULL) +
  annotate("text", x = 0.35, y= 1, color = "black",
           label = "F", size = 5) +
  theme_bw() + 
  theme(axis.title.x = element_text(size = 9))


# pH
phfddivgraph <- ggplot(WFF, aes(x= pH, y = FDDivergence, color = SWBType)) + 
  geom_point(size = 1.2, alpha = 0.6) +
  geom_smooth(method = lm, 
              se = FALSE) +
  xlab("pH") +
  ylab("Functional Divergence") +
  annotate("text", x = 9, y= 1, color = "black",
           label = "C", size = 5) +
  theme_bw() + 
  theme(axis.title.x = element_text(size = 9),
        axis.title.y = element_text(size = 9))


# Conductivity
condfddivgraph <- ggplot(WFF, aes(x= Conductivity, y = FDDivergence, color = SWBType)) + 
  geom_point(size = 1.2, alpha = 0.6) +
  geom_smooth(method = lm, 
              se = FALSE) +
  xlab("Conductivity (µS/cm)") +
  ylab(NULL) +
  xlim(400, 1125) +
  annotate("text", x = 1111, y= 1, color = "black",
           label = "D", size = 5) +
  theme_bw() + 
  theme(axis.title.x = element_text(size = 9))


# Shade
shadefddivgraph <- ggplot(WFF, aes(x= Shade, y = FDDivergence, color = SWBType)) + 
  geom_point(size = 1.2, alpha = 0.6) +
  geom_smooth(method = lm, 
              se = FALSE) +
  xlab("Shade (%)") +
  ylab("Functional Divergence") +
  theme_bw() + 
  annotate(geom="text", x= 91, y= 0.3, size = 6, label="*", color="darkgreen") +
  annotate("text", x = 91, y= 1, color = "black",
           label = "E", size = 5) +
  theme(axis.title.x = element_text(size = 9),
        axis.title.y = element_text(size = 9))


## Put together
ggarrange(nitrofddivgraph, phosfddivgraph, phfddivgraph, 
          condfddivgraph, shadefddivgraph, pollriskfddivgraph,
          common.legend = TRUE,
          ncol = 2, nrow = 3,
          legend = "top")

# FDEvenness

# ditch: no sig
ditchfdeve <- lm(FDEvenness ~ Phosphorus + Nitrogen + pH + Conductivity + PollutionRisk + Shade, data = ditch)
summary(ditchfdeve)

# pond: nitro **, conduct **, shade ***
pondfdeve <- lm(FDEvenness ~ Phosphorus + Nitrogen + pH + Conductivity + PollutionRisk + Shade, data = pond)
summary(pondfdeve)

# stream: conduct *, shade ***
streamfdeve <- lm(FDEvenness ~ Phosphorus + Nitrogen + pH + Conductivity + PollutionRisk + Shade, data = stream)
summary(streamfdeve)

# Phosphorus
phosfdevengraph <- ggplot(WFF, aes(x= Phosphorus, y = FDEvenness, color = SWBType)) + 
  geom_point(size = 1.2, alpha = 0.6) +
  geom_smooth(method = lm, 
              se = FALSE) +
  xlab("Phosphorus Concentration (mg/L)") +
  ylab(NULL) +
  xlim(0, 0.85) +
  annotate("text", x = 0.75, y= 1, color = "black",
           label = "B", size = 5) +
  theme_bw() + 
  theme(axis.title.x = element_text(size = 9))


# Nitrogen
nitrofdevengraph <- ggplot(WFF, aes(x= Nitrogen, y = FDEvenness, color = SWBType)) + 
  geom_point(size = 1.2, alpha = 0.6) +
  geom_smooth(method = lm, 
              se = FALSE) +
  xlab("Nitrogen Concentration (mg/L)") +
  ylab("Functional Evenness") +
  xlim(0, 17) +
  annotate(geom="text", x= 15, y= 0.875, size = 6, label="**", color="#E41A1C") +
  annotate("text", x = 15, y= 1, color = "black",
           label = "A", size = 5) +
  theme_bw() + 
  theme(axis.title.x = element_text(size = 9),
        axis.title.y = element_text(size = 9))


# Pollution Risk
pollriskfdevengraph <- ggplot(WFF, aes(x= PollutionRisk, y = FDEvenness, color = SWBType)) + 
  geom_point(size = 1.2, alpha = 0.6) +
  geom_smooth(method = lm, 
              se = FALSE) +
  xlab("Pollution Risk") +
  ylab(NULL) +
  annotate("text", x = 0.35, y= 1, color = "black",
           label = "F", size = 5) +
  theme_bw() + 
  theme(axis.title.x = element_text(size = 9))


# pH
phfdevengraph <- ggplot(WFF, aes(x= pH, y = FDEvenness, color = SWBType)) + 
  geom_point(size = 1.2, alpha = 0.6) +
  geom_smooth(method = lm, 
              se = FALSE) +
  xlab("pH") +
  ylab("Functional Evenness") +
  annotate("text", x = 8.75, y= 1, color = "black",
           label = "C", size = 5) +
  theme_bw() + 
  theme(axis.title.x = element_text(size = 9),
        axis.title.y = element_text(size = 9))

# Conductivity
condfdevengraph <- ggplot(WFF, aes(x= Conductivity, y = FDEvenness, color = SWBType)) + 
  geom_point(size = 1.2, alpha = 0.6) +
  geom_smooth(method = lm, 
              se = FALSE) +
  xlab("Conductivity (µS/cm)") +
  ylab(NULL) +
  annotate(geom="text", x= 1300, y= 0.875, size = 6, label="**", color="#E41A1C") +
  annotate("text", x = 1300, y= 1, color = "black",
           label = "D", size = 5) +
  annotate(geom="text", x= 1300, y= 0.825, size = 6, label="*", color="#377EB8") +
  theme_bw() + 
  theme(axis.title.x = element_text(size = 9))


# Shade
shadefdevengraph <- ggplot(WFF, aes(x= Shade, y = FDEvenness, color = SWBType)) + 
  geom_point(size = 1.2, alpha = 0.6) +
  geom_smooth(method = lm, 
              se = FALSE) +
  xlab("Shade (%)") +
  ylab("Functional Evenness") +
  annotate(geom="text", x= 95, y= 0.875, size = 6, label="*", color="#E41A1C") +
  theme_bw() + 
  annotate("text", x = 95, y= 1, color = "black",
           label = "E", size = 5) +
  annotate(geom="text", x= 95, y= 0.825, size = 6, label="***", color="#377EB8") +
  theme(axis.title.x = element_text(size = 9),
        axis.title.y = element_text(size = 9))


## Put together
ggarrange(nitrofdevengraph, phosfdevengraph, phfdevengraph, 
          condfdevengraph, shadefdevengraph, pollriskfdevengraph,
          common.legend = TRUE,
          ncol = 2, nrow = 3,
          legend = "top")

# FDRichness

# ditch: no sig
ditchfdrich <- lm(FDRichness ~ Phosphorus + Nitrogen + pH + Conductivity + PollutionRisk + Shade, data = ditch)
summary(ditchfdrich)

# pond: no sig
pondfdrich <- lm(FDRichness ~ Phosphorus + Nitrogen + pH + Conductivity + PollutionRisk + Shade, data = pond)
summary(pondfdrich)

# stream: no sig
streamfdrich <- lm(FDRichness ~ Phosphorus + Nitrogen + pH + Conductivity + PollutionRisk + Shade, data = stream)
summary(streamfdrich)

# Phosphorus
phosfdrichgraph <- ggplot(WFF, aes(x= Phosphorus, y = FDRichness, color = SWBType)) + 
  geom_point(size = 1.2, alpha = 0.6) +
  geom_smooth(method = lm, 
              se = FALSE) +
  xlab("Phosphorus Concentration (mg/L)") +
  ylab(NULL) +
  xlim(0, 0.85) +
  annotate("text", x = 0.75, y= 0.2, color = "black",
           label = "B", size = 5) +
  theme_bw() + 
  theme(axis.title.x = element_text(size = 9))


# Nitrogen
nitrofdrichgraph <- ggplot(WFF, aes(x= Nitrogen, y = FDRichness, color = SWBType)) + 
  geom_point(size = 1.2, alpha = 0.6) +
  geom_smooth(method = lm, 
              se = FALSE) +
  xlab("Nitrogen Concentration (mg/L)") +
  ylab("Functional Richness") +
  xlim(0, 17) +
  annotate("text", x = 15, y= 0.2, color = "black",
           label = "A", size = 5) +
  theme_bw() + 
  theme(axis.title.x = element_text(size = 9),
        axis.title.y = element_text(size = 9))


# Pollution Risk
pollriskfdrichgraph <- ggplot(WFF, aes(x= PollutionRisk, y = FDRichness, color = SWBType)) + 
  geom_point(size = 1.2, alpha = 0.6) +
  geom_smooth(method = lm, 
              se = FALSE) +
  xlab("Pollution Risk") +
  ylab(NULL) +
  annotate("text", x = 0.35, y= 0.2, color = "black",
           label = "F", size = 5) +
  theme_bw() + 
  theme(axis.title.x = element_text(size = 9))

# pH
phfdrichgraph <- ggplot(WFF, aes(x= pH, y = FDRichness, color = SWBType)) + 
  geom_point(size = 1.2, alpha = 0.6) +
  geom_smooth(method = lm, 
              se = FALSE) +
  xlab("pH") +
  ylab("Functional Richness") +
  annotate("text", x = 8.75, y= 0.2, color = "black",
           label = "C", size = 5) +
  theme_bw() + 
  theme(axis.title.x = element_text(size = 9),
        axis.title.y = element_text(size = 9))

# Conductivity
condfdrichgraph <- ggplot(WFF, aes(x= Conductivity, y = FDRichness, color = SWBType)) + 
  geom_point(size = 1.2, alpha = 0.6) +
  geom_smooth(method = lm, 
              se = FALSE) +
  xlab("Conductivity (µS/cm)") +
  ylab(NULL) +
  annotate("text", x = 1300, y= 0.2, color = "black",
           label = "D", size = 5) +
  theme_bw() + 
  theme(axis.title.x = element_text(size = 9))

# Shade
shadefdrichgraph <- ggplot(WFF, aes(x= Shade, y = FDRichness, color = SWBType)) + 
  geom_point(size = 1.2, alpha = 0.6) +
  geom_smooth(method = lm, 
              se = FALSE) +
  xlab("Shade (%)") +
  ylab("Functional Richness") +
  theme_bw() + 
  annotate("text", x = 91, y= 0.2, color = "black",
           label = "E", size = 5) +
  theme(axis.title.x = element_text(size = 9),
        axis.title.y = element_text(size = 9))


## Put together
ggarrange(nitrofdrichgraph, phosfdrichgraph, phfdrichgraph, 
          condfdrichgraph, shadefdrichgraph, pollriskfdrichgraph,
          common.legend = TRUE,
          ncol = 2, nrow = 3,
          legend = "top")

