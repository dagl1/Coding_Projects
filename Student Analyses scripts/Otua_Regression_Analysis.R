#load required packages, and install them if they're not yet installed
if(!require(dplyr)) { install.packages("dplyr") }
if(!require(ggplot2)) { install.packages("ggplot2") }
if(!require(gtsummary)) { install.packages("gtsummary") }
if(!require(haven)) { install.packages("haven") }
if(!require(readxl)) { install.packages("readxl") }
if(!require(tidyverse)) { install.packages("tidyverse") }
if(!require(WRS2)) { install.packages("WRS2") }
if(!require(olsrr)) { install.packages("olsrr") }
if(!require(MASS)) { install.packages("MASS") }
if(!require(corrplot)) { install.packages("corrplot") }
if(!require(car)) { install.packages("car") }
if(!require(robustbase)) { install.packages("robustbase") }
if(!require(stargazer)) { install.packages("stargazer") }


setwd("E:/Downloads")

df1 <- read_excel("File Attachment_ ChatGPT Adoption in Higher Education_August 12, 2023_22.48.xlsx")

Hyp1 <- lm(Mean_Intention_To_Use ~ Mean_Performance_Expectancy + Mean_Effort_Excpectancy + Mean_Social_Influence + Mean_Facilitating_Conditions + Mean_Hedonic_Motivation + Mean_Learning_Value + Mean_Habit + Mean_Personal_Innovativeness + Mean_Information_Accuracy     , data = df1)

ols_plot_resid_qq(Hyp1)
ols_test_correlation(Hyp1)
ols_plot_resid_fit(Hyp1)
ols_plot_resid_hist(Hyp1)
shapiro.test(Hyp1$residuals)

summHyp1 = summary(Hyp1)

#remove outliers
outliers <- boxplot(df1$Mean_Intention_To_Use, plot = FALSE)$out
outliers_II <- boxplot(df1$Mean_Intention_To_Use, plot = FALSE)$out
df1_nools <- df1
df1_nools <- df1[-which(df1$Mean_Intention_To_Use %in% outliers),]

Hyp1Outlier <- lm(Mean_Intention_To_Use ~ Mean_Performance_Expectancy + Mean_Effort_Excpectancy + Mean_Social_Influence + Mean_Facilitating_Conditions + Mean_Hedonic_Motivation + Mean_Learning_Value + Mean_Habit + Mean_Personal_Innovativeness + Mean_Information_Accuracy     , data = df1_nools)

ols_plot_resid_qq(Hyp1Outlier)
ols_test_correlation(Hyp1Outlier)
ols_plot_resid_fit(Hyp1Outlier)
ols_plot_resid_hist(Hyp1Outlier)
shapiro.test(Hyp1Outlier$residuals)

summHyp1Outlier = summary(Hyp1Outlier)

# Remove outliers based on cooks distance
cooksd <- cooks.distance(Hyp1Outlier)
threshold <- 4/nrow(df1_nools)  # Adjust the threshold as needed
outliers <- which(cooksd > threshold)
df1_nools2 <- df1_nools[-outliers, ]

Hyp1Outlier2 <- lm(Mean_Intention_To_Use ~ Mean_Performance_Expectancy + Mean_Effort_Excpectancy + Mean_Social_Influence + Mean_Facilitating_Conditions + Mean_Hedonic_Motivation + Mean_Learning_Value + Mean_Habit + Mean_Personal_Innovativeness + Mean_Information_Accuracy     , data = df1_nools2)
summHyp1Outlier2 = summary(Hyp1Outlier2)


ols_plot_resid_qq(Hyp1Outlier2)
ols_test_correlation(Hyp1Outlier2)
ols_plot_resid_fit(Hyp1Outlier2)
ols_plot_resid_hist(Hyp1Outlier2)
shapiro.test(Hyp1Outlier2$residuals)


plot(df1_nools2$Mean_Effort_Excpectancy, df1_nools2$Mean_Intention_To_Use, 
     xlab = "Effort Expectancy", ylab = "Intention To Use", 
     main = "Scatter plot - Effort Expectancy vs. Intention To Use")

plot(df1_nools2$Mean_Social_Influence, df1_nools2$Mean_Intention_To_Use, 
     xlab = "Social Influence", ylab = "Intention To Use", 
     main = "Scatter plot - Social Influence vs. Intention To Use")

plot(df1_nools2$Mean_Facilitating_Conditions, df1_nools2$Mean_Intention_To_Use, 
     xlab = "Facilitating Conditions", ylab = "Intention To Use", 
     main = "Scatter plot - Facilitating Conditions vs. Intention To Use")

plot(df1_nools2$Mean_Hedonic_Motivation, df1_nools2$Mean_Intention_To_Use, 
     xlab = "Hedonic Motivation", ylab = "Intention To Use", 
     main = "Scatter plot - Hedonic Motivation vs. Intention To Use")

plot(df1_nools2$Mean_Learning_Value, df1_nools2$Mean_Intention_To_Use, 
     xlab = "Learning Value", ylab = "Intention To Use", 
     main = "Scatter plot - Learning Value vs. Intention To Use")

plot(df1_nools2$Mean_Habit, df1_nools2$Mean_Intention_To_Use, 
     xlab = "Habit", ylab = "Intention To Use", 
     main = "Scatter plot - Habit vs. Intention To Use")

plot(df1_nools2$Mean_Personal_Innovativeness, df1_nools2$Mean_Intention_To_Use, 
     xlab = "Personal Innovativeness", ylab = "Intention To Use", 
     main = "Scatter plot - Personal Innovativeness vs. Intention To Use")

plot(df1_nools2$Mean_Information_Accuracy, df1_nools2$Mean_Intention_To_Use, 
     xlab = "Information Accuracy", ylab = "Intention To Use", 
     main = "Scatter plot - Information Accuracy vs. Intention To Use")

plot(df1_nools2$Mean_Performance_Expectancy, df1_nools2$Mean_Intention_To_Use, 
     xlab = "Performance Expectancy", ylab = "Intention To Use", 
     main = "Scatter plot - Performance Expectancy vs. Intention To Use")

Hyp1Outlier2_Filtered <- lm(Mean_Intention_To_Use ~  Mean_Effort_Excpectancy + Mean_Social_Influence + Mean_Facilitating_Conditions + Mean_Hedonic_Motivation  + Mean_Habit + Mean_Personal_Innovativeness + Mean_Information_Accuracy     , data = df1_nools2)
summHyp1Outlier2_Filtered = summary(Hyp1Outlier2_Filtered)


ols_plot_resid_qq(Hyp1Outlier2_Filtered)
ols_test_correlation(Hyp1Outlier2_Filtered)
ols_plot_resid_fit(Hyp1Outlier2_Filtered)
ols_plot_resid_hist(Hyp1Outlier2_Filtered)
shapiro.test(Hyp1Outlier2_Filtered$residuals)


Hyp1Outlier2_Filtered_Age <- lm(Mean_Intention_To_Use ~ Q31 + Mean_Effort_Excpectancy + Mean_Social_Influence + Mean_Facilitating_Conditions + Mean_Hedonic_Motivation  + Mean_Habit + Mean_Personal_Innovativeness + Mean_Information_Accuracy     , data = df1_nools2)
summHyp1Outlier2_Filtered_Age = summary(Hyp1Outlier2_Filtered_Age)

ols_plot_resid_qq(Hyp1Outlier2_Filtered_Age)
ols_test_correlation(Hyp1Outlier2_Filtered_Age)
ols_plot_resid_fit(Hyp1Outlier2_Filtered_Age)
ols_plot_resid_hist(Hyp1Outlier2_Filtered_Age)
shapiro.test(Hyp1Outlier2_Filtered_Age$residuals)



stargazer( Hyp1Outlier2, Hyp1Outlier2_Filtered, Hyp1Outlier2_Filtered_Age,   type = "html", out="outputTablesRegression.doc")


