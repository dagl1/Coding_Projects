### Code  voor het testen van linearity > alles ziet er goed uit
library(olsrr)
rm(list = ls())
setwd("E:/Downloads")
Final_survey = read.csv("Final survey thesis .csv")
#data prep van final survey csv. (dus open csv first and waar die staat set as working directory) 
# Delete the first two rows
Final_survey <- Final_survey[-c(1, 2), ]

# Delete columns 76 to 100
Final_survey <- dplyr::select(Final_survey, -c(76:200))

#Install the neccesary [package]
#install.packages("MASS")  # for fitting ordinal logistic regression
library(MASS)
library(dplyr)

#get alll data needed
PI_new <- rowMeans(sapply(Final_survey[, c("PI_1", "PI_2", "PI_3", "PI_5", "PI_6")], as.numeric))
ATA_new <- rowMeans(sapply(Final_survey[, c("ATA_1", "ATA_2", "ATA_3", "ATA_4", "ATA_5", "ATA_6", "ATA_7", "ATA_8")], as.numeric))
WORRY_new <- rowMeans(sapply(Final_survey[, c("WORRY_1", "WORRY_2", "WORRY_3", "WORRY__4", "WORRY_5", "WORRY_6", "WORRY_7")], as.numeric))
SHAME_new <- rowMeans(sapply(Final_survey[, c("SHAME_4", "SHAME_5", "SHAME_6", "SHAME_7", "SHAME_8")], as.numeric))
HOPE_new <- rowMeans(sapply(Final_survey[, c("HOPE_1", "HOPE_2", "HOPE_3", "HOPE_4", "HOPE_5", "HOPE_6", "HOPE_7", "HOPE_8")], as.numeric))

#make a new data frame with all the data needed 
df2 <- data.frame(ATA_new, PI_new, WORRY_new, SHAME_new, HOPE_new, Final_survey$Gender, Final_survey$Age, 
                  Final_survey$Education, Final_survey$Nationality, Final_survey$Employment, 
                  Final_survey$involvement_1, Final_survey$involvement_2 )

#add condition from final survey representing the codnitions into the new data frame
df2$Condition <- Final_survey$Condition

#get ad effectiveness
df2$ad_effect <- (df2$PI_new + df2$ATA_new) / 2
df2$ad_effect <- factor(df2$ad_effect)

#make type of appeal 
df2$Type_Appeal <- factor(df2$Condition,
                          levels = c("controlad", "Rationalcondition", "emotionalad", "combinedad"),
                          labels = c("controlad", "Rationalcondition", "emotionalad", "combinedad"))
df2$Type_Appeal <- factor(df2$Type_Appeal)


#LINEAR REGRESSION

df1 <- df2 %>%
  mutate(Rational = ifelse(Condition %in% c("Rationalcondition", "combinedad"), 1, 0),
         Emotional = ifelse(Condition %in% c("emotionalad", "combinedad"), 1, 0)) %>%
  dplyr::select(-c('Condition', 'Type_Appeal', 'Final_survey.Gender','Final_survey.Age','Final_survey.Education','Final_survey.Nationality', "Final_survey.involvement_1", "Final_survey.involvement_2", "Final_survey.Employment"))

df1$ad_effect <- as.numeric(df1$ad_effect)


model <- lm(ad_effect ~ Rational + Emotional + HOPE_new + SHAME_new + WORRY_new , data = df1)
ols_plot_resid_qq(model)
ols_test_normality(model)
ols_plot_resid_fit(model)
ols_plot_resid_hist(model)
ols_test_correlation(model)
summary(model)


library(lme4)

mixed_model <- lmer(ad_effect ~ Rational + Emotional + HOPE_new + SHAME_new + WORRY_new + Final_survey.Gender
                    + Final_survey.Employment + Final_survey.involvement_1 + Final_survey.involvement_2
                    + (1|Final_survey.Age) + (1|Final_survey.Education) +  (1|Final_survey.Nationality)
                    , data = df2)

summary(mixed_model)


#ANCOVA (hier moeten de mediators niet in?)
model_ANCOVA <- lm(ad_effect ~ Rational + Emotional + HOPE_new + SHAME_new + WORRY_new, data = df1)
model_ancova1 <- lm(ad_effect ~ Type_Appeal + Final_survey.Age + Final_survey.Education + Final_survey.Gender + Final_survey.Nationality + Final_survey.Employment + Final_survey.involvement_1 + Final_survey.involvement_2, data = df2)
summary(model_ancova1)

library(sm)
TwoColums = cbind(df2$Final_survey.Age,df2$Final_survey.Nationality)
sm.ancova(TwoColums, df2$ad_effect,df2$Type_Appeal)
#ANOVA 


#--------------------------------------------------------------------------------
# MEDIATION USING PROCESS MODEL 4 
source('process.R')


##mediation shame and rational 
# Step 1: Subset the data
df_condition <- subset(df1, select = c("ad_effect", "SHAME_new", "Rational"))

# Step 2: Perform mediation analysis using PROCESS of shame mediation between rational advertisment and ad effectivness 
results_mediation_shame <- process(data = df_condition, y = "ad_effect", x = "Rational", m = "SHAME_new", 
                                   model = 4, boot = 5000, seed = 12345)
summary(results_mediation_shame)



##mediation of worry and emotional 
df_condition2 <- subset(df1, select = c("ad_effect", "WORRY_new", "Emotional"))

# Step 3: Perform mediation analysis using PROCESS
results_mediation_worry <- process(data = df_condition2, y = "ad_effect", x = "Emotional", m = "WORRY_new", 
                                   model = 4, boot = 5000, seed = 12345)
summary(results_mediation_worry)



##mediation of hope and emotional
df_condition3 <- subset(df1, select = c("ad_effect", "HOPE_new", "Emotional"))

#Perform mediation analysis using PROCESS
results_mediation_hope_em <- process(data = df_condition3, y = "ad_effect", x = "Emotional", m = "HOPE_new", 
                                     model = 4, boot = 5000, seed = 12345)


##mediation of hope and rational 
df_condition4 <- subset(df1, select = c("ad_effect", "HOPE_new", "Rational"))

# : Perform mediation analysis using PROCESS between rational appeal and ad effectiveness 
results_mediation_hope_rat <- process(data = df_condition4, y = "ad_effect", x = "Rational", m = "HOPE_new", 
                                      model = 4, boot = 5000, seed = 12345)

-----------------------------
#extra to see myself if shame and worry also mediate for the other appeals 
df_condition5 <- subset(df1, select = c("ad_effect", "SHAME_new", "Emotional"))

# Step 2: Perform mediation analysis using PROCESS of shame mediation between rational advertisment and ad effectivness 
results_mediation_shame1 <- process(data = df_condition5, y = "ad_effect", x = "Emotional", m = "SHAME_new", 
                                   model = 4, boot = 5000, seed = 12345)
summary(results_mediation_shame1) # shame did not in the emotional appeal 



df_condition6 <- subset(df1, select = c("ad_effect", "WORRY_new", "Rational"))

# Step 2: Perform mediation analysis using PROCESS of shame mediation between rational advertisment and ad effectivness 
results_mediation_worry1 <- process(data = df_condition6, y = "ad_effect", x = "Rational", m = "WORRY_new", 
                                    model = 4, boot = 5000, seed = 12345)
summary(results_mediation_worry1) # worry did not in rational 



--------------------------------------------------------------------------------
  #HOMOGENEITY OF VARIANCE 
  
  # Using the leveneTest function from the car package
  library(car)
leveneTest(ad_effect ~ df2$Condition, data = df1) #p value = .1959> .05 so dont reject null = homogeneity met 

# Using the bartlett.test function
bartlett.test(ad_effect ~ df2$Condition, data = df1) #p value = .2802> .05 dont reject null = homogeneity met 


#LINEARITY 
shapiro.test(df1$ad_effect)


#NORMALITY 
#normality hist(df1$ad_effect, main = "Normality Histogram of Dependent Variable", xlab = "Ad Effectiveness") #how do i add a normality curve line on here? 

df2$Type_Appeal <- factor(df2$Condition,
                          levels = c("controlad", "Rationalcondition", "emotionalad", "combinedad"),
                          labels = c(1, 2, 3, 4))
# Convert ad_effect to numeric
df2$ad_effect <- as.numeric(as.character(df2$ad_effect))

# Convert other covariates to numeric
df2$Type_Appeal <- as.numeric(as.character(df2$Type_Appeal))
df2$Final_survey.Age <- as.numeric(as.character(df2$Final_survey.Age))
df2$Final_survey.Education <- as.numeric(as.character(df2$Final_survey.Education))
df2$Final_survey.Gender <- as.numeric(as.character(df2$Final_survey.Gender))
df2$Final_survey.Nationality <- as.numeric(as.character(df2$Final_survey.Nationality))
df2$Final_survey.Employment <- as.numeric(as.character(df2$Final_survey.Employment))
df2$Final_survey.involvement_1 <- as.numeric(as.character(df2$Final_survey.involvement_1))
df2$Final_survey.involvement_2 <- as.numeric(as.character(df2$Final_survey.involvement_2))

# Fit the ANCOVA model
model_ancova1 <- lm(ad_effect ~ Type_Appeal + Final_survey.Age + Final_survey.Education + Final_survey.Gender + Final_survey.Nationality + Final_survey.Employment + Final_survey.involvement_1 + Final_survey.involvement_2, data = df2)

residuals <- residuals(model_ancova1)

# Perform Shapiro-Wilk test for normality
shapiro.test(residuals)   #p value .0002506 < .05 dus eig niet normally dsitirbuted 

ols_plot_resid_qq(model_ancova1)
ols_test_normality(model_ancova1)
ols_plot_resid_fit(model_ancova1) # compares the model residuals (vertical axis) to the fitted values or predicted values from the model (horizontal axis). This plot helps in assessing the assumptions of linearity and homoscedasticity
ols_plot_resid_hist(model_ancova1)
skewness_value <- skewness(residuals)
print(skewness_value)

#LINEARITY 

# seperate DV 
model_plot <- lm(ATA_new ~ PI_new, data = df1)
plot(model_plot, which = 1)

# Linearity tussen catagorical variables is wat moeilijk, maar je kan zeker checken of HOPE en PI/ATA linearily correlated zijn
model_plot1 <- lm(ATA_new ~ PI_new, data = df1)
plot(model_plot, which = 1)

model_plot2 <- lm(WORRY_new ~ PI_new, data = df1)
plot(model_plot2, which = 1)

model_plot3 <- lm(SHAME_new ~ PI_new, data = df1)
plot(model_plot3, which = 1)

model_plot4 <- lm(HOPE_new ~ PI_new, data = df1) # Deze is misschien niet volledig linear, dus bij de interpretatie van hope moet je daar wat van zeggen in de discussie
plot(model_plot4, which = 1)

dev.off()

model_plot5 <- lm(WORRY_new ~ ATA_new, data = df1)
plot(model_plot5, which = 1)

model_plot6 <- lm(SHAME_new ~ ATA_new, data = df1)
plot(model_plot6, which = 1)

model_plot7 <- lm(HOPE_new ~ ATA_new, data = df1) # Deze is wat meer okay dan de HOPE<> PI
plot(model_plot7, which = 1)






#APA TABLES niet gelukt... 
library(apaTables)

# Create a data frame with the relevant statistics
model_summary <- data.frame(
  Variable = c("ad_effect", "SHAME_new"),
  R = c(0.4231, 0.1186),
  R_squared = c(0.1790, 0.0141),
  MSE = c(1532.7524, 1.5209),
  F = c(23.8789, 3.1404),
  df1 = c(2, 1),
  df2 = c(219, 220),
  p_value = c(0.0000, 0.0778)
)

model_coefficients <- data.frame(
  Variable = c("constant", "Rational", "SHAME_new"),
  coeff = c(64.5375, 36.4627, -0.5304),
  se = c(7.1154, 5.2945, 2.1403),
  t_value = c(9.0701, 6.8869, -0.2478))
  

model_coefficients <- data.frame(
  Variable = c("constant", "Rational", "SHAME_new"),
  coeff = c(64.5375, 36.4627, -0.5304),
  se = c(7.1154, 5.2945, 2.1403),
  t_value = c(9.0701, 6.8869, -0.2478),
  p_value = c(0.0000, 0.0000, 0.8045),
  LLCI = c(50.5140, 26.0280, -4.7486),
  ULCI = c(78.5610, 46.8974, 3.6877)
)

indirect_effects <- data.frame(
  Variable = "SHAME_new",
  Effect = -0.1557,
  BootSE = 0.7300,
  BootLLCI = -1.7575,
  BootULCI = 1.4074)
  
# Generate APA table
apa_table <- apa_table(model_summary, model_coefficients, indirect_effects,
                       table.number = 1,
                       title = "Summary of Results",
                       caption = "Summary of results for mediation model.",
                       digits = 4,
                       row.names = FALSE)

# Print the APA table
print(apa_table)


