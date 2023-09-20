rm(list=ls())

#Tell R what the working directory is
setwd("E:/Downloads")

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

#load your dataset
df1 <- read_excel("Preqin_funds_export-04-07-2022-1408.xlsx", sheet = "Preqin_Export")
#create new numeric variables for the relevant original variables
df1$DPI <- as.numeric(df1$`DPI (%)`)

df <- df1 %>% dplyr::select('FUND MANAGER', 'FUND NUMBER (OVERALL)', 'CORE INDUSTRIES', 'REGION' ,'VINTAGE / INCEPTION YEAR', "FINAL CLOSE SIZE (EUR MN)", "GEOGRAPHIC FOCUS", "STRATEGY", "DPI", "NET IRR (%)", "STATUS")

# Remove & and replace with and, necessary for later
for (col in names(df)) {
  if (is.character(df[[col]])) {
    df[[col]] <- gsub("&", "and", df[[col]])
  }
}

#Set Factors
df$STRATEGY <- as.factor(ifelse(df$STRATEGY == 'Distressed Debt', 'Distress', 'Buyout'))
df$`GEOGRAPHIC FOCUS` <- as.factor(df$"GEOGRAPHIC FOCUS")
df$`REGION` <- as.factor(df$"REGION")

#Vintage year bracketing and set as factor
VintageYearBrackets = c("1977-1986", "1987-1996","1997-2006","2007-2016","2017-2021")
df$VintageYear_Brackets <- cut(df$'VINTAGE / INCEPTION YEAR', breaks = c(1976, 1986, 1996, 2006, 2016, 2021), labels = VintageYearBrackets, include.lowest = TRUE)
df$VintageYear_Brackets <- as.factor(df$VintageYear_Brackets)

#renaming core variables
colnames(df) <- c("Fund_Manager", "Fund_Number", "Core_Industries", "Region",
                  "Vintage_Year", "Final_Close_Size","Geographic_Focus", 
                  "Strategy", "DPI", "Net_IRR", "Status",'VintageYear_Brackets' )



# setting to BICS industry classification
consumer_industries <- c("Consumer Discretionary", "Consumer Discretionary, Raw Materials & Natural Resources",
                         "Consumer Discretionary, Healthcare", "Consumer Discretionary, Information Technology",
                         "Consumer Discretionary, Financial & Insurance Services", "Business Services, Consumer Discretionary")
df$Core_Industries[df$Core_Industries %in% consumer_industries] <- "Consumer Industries"

information_technology <- c("Industrials, Information Technology",  "Information Technology, Telecoms & Media" , "Financial & Insurance Services, Information Technology", "Healthcare, Information Technology", "Business Services, Information Technology"  )
df$Core_Industries[df$Core_Industries %in% information_technology] <- "Information Technology"

Energy_Utilities <- c("Energy & Utilities","Energy & Utilities, Real Estate")
df$Core_Industries[df$Core_Industries %in% Energy_Utilities] <- "Energy & Utilities"

business_services <- c("Business Services","Business Services, Healthcare")
df$Core_Industries[df$Core_Industries %in% business_services] <- "Business Services"

rawMats <- c("Raw Materials & Natural Resources" ,"Energy & Utilities, Raw Materials & Natural Resources")
df$Core_Industries[df$Core_Industries %in% rawMats] <- "Raw Materials & Natural Resources" 

#Set as factor
df$Core_Industries <- as.factor(df$Core_Industries)

df$Core_Industries <- as.numeric(ifelse(df$Core_Industries == 'Diversified', 1,0 ))

#Set numeric
df$Net_IRR <- as.numeric(df$Net_IRR)

### Data cleaning
df<- df[df$Status == "Liquidated", ]
df <- df[complete.cases(df$Net_IRR, df$DPI), ]

# Check for NA's
length(which(is.na(df$Fund_Manager)))
length(which(is.na(df$Fund_Number)))
length(which(is.na(df$Core_Industries)))
length(which(is.na(df$Region)))
length(which(is.na(df$Vintage_Year)))
length(which(is.na(df$Final_Close_Size)))
length(which(is.na(df$Geographic_Focus)))
length(which(is.na(df$Strategy)))
length(which(is.na(df$DPI)))
length(which(is.na(df$Net_IRR)))

# Optional! Some things are unknown (NA), we can remove these
df <- df[!is.na(df$Final_Close_Size),]
df <- df[!is.na(df$Fund_Number),]
df <- df[!is.na(df$Region),]
df <- df[!is.na(df$Geographic_Focus),]

#Optional, should we filter out any failed ventures (so where DPI and net irr = 0 and -100% respectively (n=2 i think))
df <- df[!df$DPI == 0,]

#Optional, should we filter out any very small groups (vintage year 1977-1986 has 0 in de "distress" group)
categories <- unique(df$VintageYear_Brackets)
for (category in categories) {
  
  # Subset the dataframe for the current category and "distress" group
  subset_df <- subset(df, VintageYear_Brackets == category & Strategy == "Distress")
  
  # Check if the subset dataframe is empty
  if (nrow(subset_df) == 0) {
    # Remove values in the whole dataframe for the current category
    df <- df[df$VintageYear_Brackets != category, ]
  }
}
# Identify columns with factor variables in the dataframe
factor_cols <- sapply(df, is.factor)
df[factor_cols] <- lapply(df[factor_cols], droplevels)


### transforming net_IRR to go from 0 to 418%, with 100% meaning no profit nor loss
df$Net_IRR <- df$Net_IRR+100

df %>% group_by(Core_Industries) %>% summarise('Industry#'= n())

#CREATE SUMMARY STATISTICS
summarysStatTable <- df %>% dplyr::select( "Fund_Number", "Core_Industries", "Region",
                                   "Final_Close_Size", 
                                   "Strategy", "DPI", "Net_IRR", 'VintageYear_Brackets' )
#create summary table
tbl_summary(summarysStatTable, 
            by = Strategy
)



##$df_Buyout = df[df$Strategy== "Buyout",]
#df_Distress = df[df$Strategy=="Distress",]
# mean(df_Buyout$DPI)
# sd(df_Buyout$DPI)
# mean(df_Buyout$Net_IRR)
# sd(df_Buyout$Net_IRR)
# mean(df_Buyout$Final_Close_Size)
# sd(df_Buyout$Final_Close_Size)
# 
# mean(df_Distress$DPI)
# sd(df_Distress$DPI)
# mean(df_Distress$Net_IRR)
# sd(df_Distress$Net_IRR)
# mean(df_Distress$Final_Close_Size)
# sd(df_Distress$Final_Close_Size)
# 
# median(df_Buyout$DPI)
# median(df_Distress$DPI)


#hist(df_Buyout$Final_Close_Size)

### Log transforming the necessary data
hist(df$Net_IRR)
shapiro.test(df$Net_IRR)
hist(log1p(df$Net_IRR))
shapiro.test(log1p(df$Net_IRR)) # We do this as W goes up

df$Net_IRR = log1p(df$Net_IRR)

#create histograms for the performance variables
hist(df$DPI)
shapiro.test(df$DPI)
hist(log1p(df$DPI))
shapiro.test(log1p(df$DPI)) # We do this as W goes up

df$DPI = log1p(df$DPI)

hist(df$Final_Close_Size)
shapiro.test(df$Final_Close_Size)
hist(log1p(df$Final_Close_Size))
shapiro.test(log1p(df$Final_Close_Size)) # We do this as W goes up

df$Final_Close_Size <- log1p(df$Final_Close_Size)

#create scatterplot for size by dpi
plot(df$DPI, df$Final_Close_Size)

#create boxplots variables per fund type
#boxplot
myColors <- c("yellow", "blue" )

#normal boxplot
boxplot(DPI~Strategy,
        col = myColors,
        data=df)

#boxplot without outliers
boxplot(DPI~Strategy,
        col = myColors,
        data=df, 
        outline = FALSE)

#normal boxplot net IRR
boxplot(Net_IRR~Strategy,
        col = myColors,
        data=df)

#boxplot without outliers net IRR
boxplot(Net_IRR~Strategy,
        col = myColors,
        data=df, 
        outline = FALSE)

# Creating dataset without outliers
iqr <- IQR(df$DPI)
lower_limit <- quantile(df$DPI, 0.25) - 1.5 * iqr
upper_limit <- quantile(df$DPI, 0.75) + 1.5 * iqr
df_noOutliers <- df[df$DPI >= lower_limit & df$DPI <= upper_limit, ]

# Creating dataset without outliers
iqr <- IQR(df_noOutliers$Net_IRR)
lower_limit <- quantile(df_noOutliers$Net_IRR, 0.25) - 1.5 * iqr
upper_limit <- quantile(df_noOutliers$Net_IRR, 0.75) + 1.5 * iqr
df_noOutliers <- df_noOutliers[df_noOutliers$Net_IRR >= lower_limit & df_noOutliers$Net_IRR <= upper_limit, ]

# Perform MannWhitney U test (non parametric t-test to assess differences)
wilcox.test(df$DPI ~ df$Strategy, data = df) # We find a difference in DPI, but not in Net_IRR
wilcox.test(df_noOutliers$DPI ~ df_noOutliers$Strategy, data = df)

wilcox.test(df$Net_IRR ~ df$Strategy, data = df)
wilcox.test(df_noOutliers$Net_IRR ~ df_noOutliers$Strategy, data = df)

#robust t-test (as proposed by yuen, see https://link.springer.com/article/10.3758/s13428-019-01246-w)
yuen(df$DPI ~ df$Strategy, data = df) # We find a difference in DPI, but not in Net_IRR
yuen(df_noOutliers$DPI ~ df_noOutliers$Strategy, data = df)

yuen(df$Net_IRR ~ df$Strategy, data = df)
yuen(df_noOutliers$Net_IRR ~ df_noOutliers$Strategy, data = df)

#create histograms for years by fund type
hist(df[df$Strategy == "Buyout",]$Vintage_Year)
hist(df[df$Strategy == "Distress",]$Vintage_Year)

#define quantiles of interest
q = c(.25, .5, .75)

#calculate quantiles by grouping variable
df %>%
  group_by(Strategy) %>%
  summarize(quant25 = quantile(DPI, probs = q[1], na.rm = TRUE), 
            quant50 = quantile(DPI, probs = q[2], na.rm = TRUE),
            quant75 = quantile(DPI, probs = q[3], na.rm = TRUE))

df %>%
  group_by(Strategy) %>%
  summarize(quant25 = quantile(Net_IRR, probs = q[1], na.rm = TRUE), 
            quant50 = quantile(Net_IRR, probs = q[2], na.rm = TRUE),
            quant75 = quantile(Net_IRR, probs = q[3], na.rm = TRUE))


#robust effect sizes
akp.effect(DPI ~ Strategy, data = df)
akp.effect(Net_IRR ~ Strategy, data = df)

#robust regression with one additional covariate: fund size
#reference: Venables, W. N. andRipley, B. D.(2002) Modern Applied 
#Statistics with S. Fourth edition. Springer

#renaming core variables
# colnames(df) <- c("Fund_Manager", "Fund_Number", "Core_Industries", "Region",
#                   "Vintage_Year", "Final_Close_Size","Geographic_Focus", 
#                   "Strategy", "DPI", "Net_IRR", "Status",'VintageYear_Brackets' )

# encode dummy vars
df$Strategy <- as.numeric(ifelse(df$Strategy == 'Distress', 1,0 ))


### Create separate dataframes
df_Buyout = df[df$Strategy==0,]
df_Distress = df[df$Strategy==1,]

factor_cols <- sapply(df_Buyout, is.factor)
df_Buyout[factor_cols] <- lapply(df_Buyout[factor_cols], droplevels)

factor_cols <- sapply(df_Distress, is.factor)
df_Distress[factor_cols] <- lapply(df_Distress[factor_cols], droplevels)





############ DPI analysis
robreg <- rlm(DPI ~ Strategy + Final_Close_Size + Fund_Number + Core_Industries  , data = df)
summary(robreg)
reg1 <- lm(DPI ~ Strategy + Final_Close_Size + Fund_Number + Core_Industries, data = df)


## Normal QQ Plot
ols_plot_resid_qq(reg1)

# Semi QQ plot
n <- length(robreg$residuals)
p <- (1:n - 0.5)/n
qsem <- quantile(robreg$residuals, probs = p, na.rm = TRUE)
plot(qsem, qnorm(p), xlab = "Semi-quantiles", ylab = "Theoretical Quantiles",  main="Semi QQ plot")
abline(0, 1, col = "red")

## Partial QQ plots
std_residuals <- rstandard(reg1)
independent_vars <- c("Final_Close_Size", "Fund_Number")

for (var in independent_vars) {
  # Obtain the coefficients for the specified independent variable
  var_coef <- coef(robreg)[var]
  
  # Remove the effect of the current variable from the residuals
  partial_residuals <- std_residuals - var_coef * model.frame(robreg)[[var]]
  
  # Create the partial QQ plot
  qqnorm(partial_residuals, main = paste("Partial QQ Plot -", var))
  qqline(partial_residuals)
}
# Scatter plots
plot(df$Strategy, df$DPI, xlab = "Strategy", ylab = "DPI", main = "Scatter plot - Strategy vs. DPI")
plot(df$Final_Close_Size, df$DPI, xlab = "Final_Close_Size", ylab = "DPI", main = "Scatter plot - Final_Close_Size vs. DPI")
plot(df$Fund_Number, df$DPI, xlab = "Fund_Number", ylab = "DPI", main = "Scatter plot - Fund_Number vs. DPI")
plot(df$Core_Industries, df$DPI, xlab = "Core_Industries", ylab = "DPI", main = "Scatter plot - Core_Industries vs. DPI")
plot(df$Region, df$DPI, xlab = "Region", ylab = "DPI", main = "Scatter plot - Region vs. DPI")
plot(df$VintageYear_Brackets, df$DPI, xlab = "Strategy * VintageYear_Brackets", ylab = "DPI", main = "Scatter plot - Strategy * VintageYear_Brackets vs. DPI")

# Homoscedasticity
plot(robreg$residuals, fitted(robreg), xlab = "Residuals", ylab = "Fitted Values", main = "Residuals vs. Fitted Values")
abline(h = 0, col = "red")

ols_plot_resid_fit(reg1) # Homoscedasticity of lm model 
bptest(reg1)

# Multicollinearity
cor_matrix <- cor(df[, c("Strategy", "Final_Close_Size", "Fund_Number")])
corrplot(cor_matrix, method = "number")

# Perform the autocorrelation test
residuals <- robreg$residuals
pacf(residuals)

# NOrmality of residuals
residuals <- residuals(robreg)
shapiro.test(residuals)
ols_test_normality(reg1) # of lm model
hist(residuals, freq = FALSE)

#create a table of the summary of the regression results
sum <- summary(robreg)
sumtab <- as.data.frame(sum$coefficients)
dof <- sum$df[2]-1      # degrees of freedom

#calculate p-value
sumtab$p.value <- pt(abs(sumtab[,3]), df=dof, lower.tail=FALSE)
round(sumtab,3)



### DPI BUYOUT
robreg_DPI_BUYOUT <- rlm(DPI ~ Final_Close_Size + Fund_Number + Core_Industries , data = df_Buyout)
summary(robreg_DPI_BUYOUT)

reg_DPI_BUYOUT <- lm(DPI ~ Final_Close_Size, data = df_Buyout)
summary(reg_DPI_BUYOUT)

### DPI Distress
robreg_DPI_Distress<- rlm(DPI ~ Final_Close_Size + Fund_Number + Core_Industries  , data = df_Distress)
summary(robreg_DPI_Distress)

reg_DPI_Distress <- lm(DPI ~  Final_Close_Size + Fund_Number + Core_Industries , data = df_Distress)
summary(reg_DPI_Distress)

### Save to file
stargazer(robreg, robreg_DPI_Distress, robreg_DPI_BUYOUT,  type = "html", out="RobustRegressions DPI.doc")







########### SAME ANALYSIS FOR NET IRR
robreg_NetIRR <- rlm(Net_IRR ~ Strategy + Final_Close_Size + Fund_Number + Core_Industries , data = df)
summary(robreg_NetIRR)
reg_NetIRR <- lm(Net_IRR ~ Strategy + Final_Close_Size + Fund_Number + Core_Industries, data = df)

## Normal QQ Plot
ols_plot_resid_qq(reg_NetIRR)

# Semi QQ plot
n <- length(robreg_NetIRR$residuals)
p <- (1:n - 0.5)/n
qsem <- quantile(robreg_NetIRR$residuals, probs = p, na.rm = TRUE)
plot(qsem, qnorm(p), xlab = "Semi-quantiles", ylab = "Theoretical Quantiles",  main="Semi QQ plot")
abline(0, 1, col = "red")

## Partial QQ plots
std_residuals <- rstandard(reg_NetIRR)
independent_vars <- c("Final_Close_Size", "Fund_Number")

for (var in independent_vars) {
  # Obtain the coefficients for the specified independent variable
  var_coef <- coef(robreg_NetIRR)[var]
  
  # Remove the effect of the current variable from the residuals
  partial_residuals <- std_residuals - var_coef * model.frame(robreg_NetIRR)[[var]]
  
  # Create the partial QQ plot
  qqnorm(partial_residuals, main = paste("Partial QQ Plot -", var))
  qqline(partial_residuals)
}
# Scatter plots
plot(df$Strategy, df$Net_IRR, xlab = "Strategy", ylab = "Net_IRR", main = "Scatter plot - Strategy vs. Net_IRR")
plot(df$Final_Close_Size, df$Net_IRR, xlab = "Final_Close_Size", ylab = "Net_IRR", main = "Scatter plot - Final_Close_Size vs. Net_IRR")
plot(df$Fund_Number, df$Net_IRR, xlab = "Fund_Number", ylab = "Net_IRR", main = "Scatter plot - Fund_Number vs. Net_IRR")
plot(df$Core_Industries, df$Net_IRR, xlab = "Core_Industries", ylab = "Net_IRR", main = "Scatter plot - Core_Industries vs. Net_IRR")
#plot(df$Region, df$Net_IRR, xlab = "Region", ylab = "Net_IRR", main = "Scatter plot - Region vs. Net_IRR")
#plot(df$VintageYear_Brackets, df$Net_IRR, xlab = "Strategy * VintageYear_Brackets", ylab = "Net_IRR", main = "Scatter plot - Strategy * VintageYear_Brackets vs. Net_IRR")

# Homoscedasticity
plot(robreg_NetIRR$residuals, fitted(robreg_NetIRR), xlab = "Residuals", ylab = "Fitted Values", main = "Residuals vs. Fitted Values")
abline(h = 0, col = "red")

ols_plot_resid_fit(reg_NetIRR) # Homoscedasticity of lm model 

# Multicollinearity
cor_matrix <- cor(df[, c("Strategy", "Final_Close_Size", "Fund_Number")])
corrplot(cor_matrix)

# Perform the autocorrelation test
residuals <- robreg_NetIRR$residuals
pacf(residuals)

# NOrmality of residuals
residuals <- residuals(robreg_NetIRR)
shapiro.test(residuals)
ols_test_normality(reg_NetIRR) # of lm model
hist(residuals, freq = FALSE)

#create a table of the summary of the regression results
sum <- summary(robreg_NetIRR)
sumtab <- as.data.frame(sum$coefficients)
dof <- sum$df[2]-1      # degrees of freedom

#calculate p-value
sumtab$p.value <- pt(abs(sumtab[,3]), df=dof, lower.tail=FALSE)
round(sumtab,3)


### Net_IRR BUYOUT
robreg_NetIRR_BUYOUT <- rlm(Net_IRR ~ Final_Close_Size + Fund_Number + Core_Industries  , data = df_Buyout)
summary(robreg_NetIRR_BUYOUT)

reg_NetIRR_BUYOUT <- lm(Net_IRR ~  Final_Close_Size, data = df_Buyout)
summary(reg_NetIRR_BUYOUT)

### Net_IRR Distress
robreg_NetIRR_Distress<- rlm(Net_IRR ~  Final_Close_Size + Fund_Number + Core_Industries  , data = df_Distress)
summary(robreg_NetIRR_Distress)

reg_NetIRR_Distress <- lm(Net_IRR ~  Final_Close_Size + Fund_Number + Core_Industries , data = df_Distress)
summary(reg_NetIRR_Distress)

### Save to file
stargazer(robreg_NetIRR, robreg_NetIRR_Distress, robreg_NetIRR_BUYOUT,  type = "html", out="RobustRegressions Net_IRR.doc")










