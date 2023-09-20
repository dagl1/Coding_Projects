# Clear vars
rm(list = ls())

# Set libraries
if (require("readxl")==FALSE){
  install.packages("readxl")
  library(readxl)}

if (require("tidyverse")==FALSE){
  install.packages("tidyverse")
  library(tidyverse)}

if (require("ggplot2")==FALSE){
  install.packages("ggplot2")
  library(ggplot2)}

if (require("likert")==FALSE){
  install.packages("likert")
  library(likert)}

if (require("dplyr")==FALSE){
  install.packages("dplyr")
  library(dplyr)}

if (require("dunn.test")==FALSE){
  install.packages("dunn.test")
  library(dunn.test)}

# Import Data
file = 'E:/Downloads/newest pre test.csv'
file = read.csv(file)

# remove check row, prune data
file = file[, -which(names(file) == 'cont_check')]
useFile = file[3:nrow(file),19:30]

# SelectFiles and put together
Em_Ad = useFile[,1:3]
Rat_Ad = useFile[,4:6]
Com_Ad = useFile[,7:9]
Neutral_Ad = useFile[,10:12]

Em_scale <- cbind(Em_Ad[, 2], Rat_Ad[, 1], Com_Ad[, 1], Neutral_Ad[, 1])
colnames(Em_scale) <- c(colnames(Em_Ad)[2], colnames(Rat_Ad)[1], colnames(Com_Ad)[1], colnames(Neutral_Ad)[1])
Em_scale <- data.frame(Em_scale)
#Em_scale <- apply(Em_scale, 2, as.numeric)

Rat_scale <- cbind(Em_Ad[, 1], Rat_Ad[, 2], Com_Ad[, 2], Neutral_Ad[, 2])
colnames(Rat_scale) <- c(colnames(Em_Ad)[1], colnames(Rat_Ad)[2], colnames(Com_Ad)[2], colnames(Neutral_Ad)[2])
Rat_scale <- data.frame(Rat_scale)
#Rat_scale <- apply(Rat_scale, 2, as.numeric)

Pos_Neg_scale <- cbind(Em_Ad[, 3], Rat_Ad[, 3], Com_Ad[, 3], Neutral_Ad[, 3])
colnames(Pos_Neg_scale) <- c(colnames(Em_Ad)[3], colnames(Rat_Ad)[3], colnames(Com_Ad)[3], colnames(Neutral_Ad)[3])
Pos_Neg_scale <- data.frame(Pos_Neg_scale)
#Pos_Neg_scale <- apply(Pos_Neg_scale, 2, as.numeric)

#### Visualize data 
# Define the response scale labels
response_scale <- c("Strongly Disagree",
                    "Disagree", "Neutral", 
                    "Agree", "Strongly Agree")

# Reshape the data frame into long format
df_long <- Em_scale %>%
  pivot_longer(everything(), names_to = "variable", values_to = "values")

# Plot the values of each point
Plot_Emotion = ggplot(df_long, aes(x = variable, y = values)) +
  geom_jitter(width = 0.2, height = 00.2,size = 1) +
  scale_y_discrete(breaks = 1:5, labels = response_scale) +
  xlab("") +
  ylab("") +
  ggtitle("How Emotional is the advertisement") + 
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1), # rotate x-axis labels
    plot.title = element_text(size = 18, hjust = 0.5), # set plot title font size
  )
Plot_Emotion

# Plot Likert data
colnames(Em_scale) <- c('emotional_ad', 'rational_ad', 'combined_ad', 'neutral_ad')

df <- Em_scale
df <- df %>%
  mutate(emotional_ad = factor(emotional_ad, levels = 1:5, labels = response_scale),
         rational_ad = factor(rational_ad, levels = 1:5, labels = response_scale),
         combined_ad = factor(combined_ad, levels = 1:5, labels = response_scale),
         neutral_ad = factor(neutral_ad, levels = 1:5, labels = response_scale))
likertSummary = likert(df)
LikertPlot = plot(likertSummary)
LikertPlot <- plot(likertSummary,
          legend.position = "top",
          legend = "How emotional is advertisement",
          text.size = 4,
          low.color = "lightblue",
          high.col = "lightgreen")
LikertPlot

# load in EM_scale, set to numeric
df <- Em_scale
df <- apply(df, 2, as.numeric)

### Perform T-test for emScore
EMttestEM <- t.test(df[,1], mu = 3)
EMttestEM$p.value <- EMttestEM$p.value*4 # Do some "type" of correction for multiple testing

EMttestRat <- t.test(df[,2], mu = 3)
EMttestRat$p.value <- EMttestRat$p.value*4 # Do some "type" of correction for multiple testing

EMttestCom <- t.test(df[,3], mu = 3)
EMttestCom$p.value <- EMttestCom$p.value*4# Do some "type" of correction for multiple testing

EMttestControl <- t.test(df[,4], mu = 3)
EMttestControl$p.value <- EMttestControl$p.value*4# Do some "type" of correction for multiple testing

### Perform Wilcoxon signed-rank test
EMWilCoxEM = wilcox.test(df[,1], mu = 3, alternative = "greater")
EMWilCoxEM$p.value <- EMWilCoxEM$p.value*4 # Do some "type" of correction for multiple testing

EMWilCoxRat = wilcox.test(df[,2], mu = 3, alternative = "greater")
EMWilCoxRat$p.value <- EMWilCoxRat$p.value*4 # Do some "type" of correction for multiple testing

EMWilCoxCom = wilcox.test(df[,2], mu = 3, alternative = "greater")
EMWilCoxCom$p.value <- EMWilCoxCom$p.value*4 # Do some "type" of correction for multiple testing

EMWilCoxControl = wilcox.test(df[,3], mu = 3, alternative = "greater")
EMWilCoxControl$p.value <- EMWilCoxControl$p.value*4 # Do some "type" of correction for multiple testing


### Oridnal cluster > Visualize cluster (2d)
# calculate mean for each advertisement in emotional scale
Em_scale <- apply(Em_scale,2, as.numeric)
Em_scale <- data.frame(Em_scale)
em_means <- Em_scale %>%
  summarize_all(mean)

# calculate mean for each advertisement in rational scale
Rat_scale <- apply(Rat_scale,2, as.numeric)
Rat_scale <- data.frame(Rat_scale)
rat_means <- Rat_scale %>%
  summarize_all(mean)

# Extract common text before the underscore
em_vars <- sub("^([a-zA-Z]+)_.*", "\\1", names(em_means))
rat_vars <- sub("^([a-zA-Z]+)_.*", "\\1", names(rat_means))

# Add the common text as a column to each data frame
colnames(em_means) <- em_vars
colnames(rat_means) <- rat_vars

# Set column names
means_df <- rbind(rat_means, em_means)
rownames(means_df) <- c('Rational', 'Emotional')

# Transpose data
means_df_transposed <- data.frame(
  Emotional = t(means_df["Emotional", ]),
  Rational = t(means_df["Rational", ]),
  Advertisement = colnames(means_df)
)

# create on cluster
ggplot(means_df_transposed, aes(x = Rational, y = Emotional, label = Advertisement)) +
  geom_point(size = 3) +
  geom_text(hjust = 0, vjust = 0) +
  labs(x = "Rational Response", y = "Emotional Response", title = "Advertisement Response") +
  theme_classic()


