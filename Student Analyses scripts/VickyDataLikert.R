rm(list = ls())

if (require("readxl")==FALSE){
  install.packages("readxl")
  library(readxl)}
if (require("likert")==FALSE){
  install.packages("likert")
  library(likert)}
if (require("dplyr")==FALSE){
  install.packages("dplyr")
  library(dplyr)}


df = read.csv2('E:/Downloads/Versie Jelle Data DMII_1.csv')

df <- df[c("Tevr_Huid_beh", "Tevr_Makkelijk", "Tevr_Flexibel", "Tevr_Kennis", "Tevr_Aanbevelen", "Tevr_Continueren", "Ervaren_Hyper", "Ervaren_Hypo")]


response_scale <- c("Helemaal niet",
                    "Weinig", "Bijna nooit", "Niet veel of weinig", 
                    "Een beetje vaak", "Vaak", "Zeer vaak")

df <- df %>%
  mutate_all(as.numeric)


df <- df %>%
  mutate(Tevr_Huid_beh = factor(Tevr_Huid_beh, levels = 0:6, labels = response_scale),
         Tevr_Makkelijk = factor(Tevr_Makkelijk, levels = 0:6, labels = response_scale),
         Tevr_Flexibel = factor(Tevr_Flexibel, levels = 0:6, labels = response_scale),
         Tevr_Kennis = factor(Tevr_Kennis, levels = 0:6, labels = response_scale),
         Tevr_Aanbevelen = factor(Tevr_Aanbevelen, levels = 0:6, labels = response_scale),
         Tevr_Continueren = factor(Tevr_Continueren, levels = 0:6, labels = response_scale),
         Ervaren_Hyper = factor(Ervaren_Hyper, levels = 0:6, labels = response_scale),
         Ervaren_Hypo = factor(Ervaren_Hypo, levels = 0:6, labels = response_scale))

colnames(df)[1] <- "Tevreden met huidige behandeling?"
colnames(df)[2] <- "Gemakkelijke/handige behandeling?"
colnames(df)[3] <- "Flexibiliteit behandeling?"
colnames(df)[4] <- "Tevreden over kennis van DMII"
colnames(df)[5] <- "Behandeling aanbevelen?"
colnames(df)[6] <- "Tevredenheid voortzetten behandeling?"
colnames(df)[7] <- "Ervaren hyperglykieën?"
colnames(df)[8] <- "Ervaren hypoglykieën?"

likertSummary = likert(df)
LikertPlot = plot(likertSummary)
LikertPlot <- plot(likertSummary,
                   legend.position = "top",
                   legend = "DTSQ questionnaire",
                   text.size = 4,
                   low.color = "lightblue",
                   high.col = "lightgreen")
LikertPlot
