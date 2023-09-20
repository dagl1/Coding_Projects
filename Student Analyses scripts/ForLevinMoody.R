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
dfMoody <- read_excel("Moody's Historical Default Rates of All-Corporate & Speculative-Grade Bonds.xlsx")


dfMoody <- dfMoody[,c(1,4)]
dfMoodyUse <- dfMoody[3:nrow(dfMoody), ]


#create new numeric variables for the relevant original variables
df1$DPI <- as.numeric(df1$`DPI (%)`)
#df1$DPI <- df1$DPI/100

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




#### Moody for Levin variables

EarliestYearBound = min(df$Vintage_Year)
LatestYearBound = max(df$Vintage_Year)

LowerPercentageBound = 0.03
HigherPercentageBound = 0.06


####

colnames(dfMoodyUse) <- c("Year", "MoodyPercentage")

selected_values <- dfMoodyUse[dfMoodyUse$Year >= EarliestYearBound & dfMoodyUse$Year <= LatestYearBound, c("Year","MoodyPercentage")]

selected_values$MoodyPercentage <- as.numeric(selected_values$MoodyPercentage)
meanMoodyPercentage = mean(selected_values$MoodyPercentage, na.rm = TRUE)

UpDownTurnList1 = ifelse(selected_values$MoodyPercentage < meanMoodyPercentage, "up", "down")
  
UpDownTurnList2 = ifelse(selected_values$MoodyPercentage < LowerPercentageBound, "up",
                          ifelse(selected_values$MoodyPercentage > HigherPercentageBound, "down", "base"))

selected_values <- cbind(selected_values, UpDownTurnList1, UpDownTurnList2)

selected_values <- na.omit(selected_values)

for (i in 1:length(df$Vintage_Year)) {
  for (j in 1:length(selected_values$MoodyPercentage)) {
    if (df$Vintage_Year[i] == selected_values$Year[j]) {
      df[i, 'UpDown1'] = selected_values$UpDownTurnList1[j]
      df[i, 'UpDown2'] = selected_values$UpDownTurnList2[j]
      df[i, 'DFI'] = selected_values$MoodyPercentage[j]
    }
  }
}

buyout_df <- df[df$Strategy == "Buyout", ]
distressed_df <- df[df$Strategy == "Distress", ]
buyout_df$UpDown1 <- factor(buyout_df$UpDown1)
buyout_df$UpDown2 <- factor(buyout_df$UpDown2)



summary_stats_Buyout_DPI_U1 <- buyout_df %>%
  group_by(UpDown1) %>%
  summarise(
    N = n(),
    Average = mean(DPI, na.rm = TRUE),
    Median = median(DPI, na.rm = TRUE),
    Percentile_75 = quantile(DPI, 0.75, na.rm = TRUE),
    Percentile_25 = quantile(DPI, 0.25, na.rm = TRUE),
    SD = sd(DPI, na.rm = TRUE)
  )

summary_stats_Buyout_DPI_U2 <- buyout_df %>%
  group_by(UpDown2) %>%
  summarise(
    N = n(),
    Average = mean(DPI, na.rm = TRUE),
    Median = median(DPI, na.rm = TRUE),
    Percentile_75 = quantile(DPI, 0.75, na.rm = TRUE),
    Percentile_25 = quantile(DPI, 0.25, na.rm = TRUE),
    SD = sd(DPI, na.rm = TRUE)
  )

summary_stats_Buyout_IRR_U1 <- buyout_df %>%
  group_by(UpDown1) %>%
  summarise(
    N = n(),
    Average = mean(Net_IRR, na.rm = TRUE),
    Median = median(Net_IRR, na.rm = TRUE),
    Percentile_75 = quantile(Net_IRR, 0.75, na.rm = TRUE),
    Percentile_25 = quantile(Net_IRR, 0.25, na.rm = TRUE),
    SD = sd(Net_IRR, na.rm = TRUE)
  )

summary_stats_Buyout_IRR_U2 <- buyout_df %>%
  group_by(UpDown2) %>%
  summarise(
    N = n(),
    Average = mean(Net_IRR, na.rm = TRUE),
    Median = median(Net_IRR, na.rm = TRUE),
    Percentile_75 = quantile(Net_IRR, 0.75, na.rm = TRUE),
    Percentile_25 = quantile(Net_IRR, 0.25, na.rm = TRUE),
    SD = sd(Net_IRR, na.rm = TRUE)
  )


#### DPI
levene_dpi_u1_d1_Buyout <- leveneTest(DPI ~ UpDown1, data = buyout_df, subset = UpDown1 %in% c("up", "down"))
levene_dpi_u2_b2_Buyout <- leveneTest(DPI ~ UpDown2, data = buyout_df, subset = UpDown2 %in% c("up", "base"))
levene_dpi_u2_d2_Buyout <- leveneTest(DPI ~ UpDown2, data = buyout_df, subset = UpDown2 %in% c("up", "down"))
levene_dpi_b2_d2_Buyout <- leveneTest(DPI ~ UpDown2, data = buyout_df, subset = UpDown2 %in% c("base", "down"))

t_equal_dpi_u1_d1_Buyout <- t.test(DPI ~ UpDown1, data = buyout_df, subset = UpDown1 %in% c("up", "down"))
t_equal_dpi_u2_b2_Buyout <- t.test(DPI ~ UpDown2, data = buyout_df, subset = UpDown2 %in% c("up", "base"))
t_equal_dpi_u2_d2_Buyout <- t.test(DPI ~ UpDown2, data = buyout_df, subset = UpDown2 %in% c("up", "down"))
t_equal_dpi_b2_d2_Buyout <- t.test(DPI ~ UpDown2, data = buyout_df, subset = UpDown2 %in% c("base", "down"))

t_unequal_dpi_u1_d1_Buyout <- t.test(DPI ~ UpDown1, data = buyout_df, subset = UpDown1 %in% c("up", "down"), var.equal = FALSE)
t_unequal_dpi_u2_b2_Buyout <- t.test(DPI ~ UpDown2, data = buyout_df, subset = UpDown2 %in% c("up", "base"), var.equal = FALSE)
t_unequal_dpi_u2_d2_Buyout <- t.test(DPI ~ UpDown2, data = buyout_df, subset = UpDown2 %in% c("up", "down"), var.equal = FALSE)
t_unequal_dpi_b2_d2_Buyout <- t.test(DPI ~ UpDown2, data = buyout_df, subset = UpDown2 %in% c("base", "down"), var.equal = FALSE)



#### Net IRR
levene_NetIRR_u1_d1_Buyout <- leveneTest(Net_IRR ~ UpDown1, data = buyout_df, subset = UpDown1 %in% c("up", "down"))
levene_NetIRR_u2_b2_Buyout <- leveneTest(Net_IRR ~ UpDown2, data = buyout_df, subset = UpDown2 %in% c("up", "base"))
levene_NetIRR_u2_d2_Buyout <- leveneTest(Net_IRR ~ UpDown2, data = buyout_df, subset = UpDown2 %in% c("up", "down"))
levene_NetIRR_b2_d2_Buyout <- leveneTest(Net_IRR ~ UpDown2, data = buyout_df, subset = UpDown2 %in% c("base", "down"))

t_equal_NetIRR_u1_d1_Buyout <- t.test(Net_IRR ~ UpDown1, data = buyout_df, subset = UpDown1 %in% c("up", "down"))
t_equal_NetIRR_u2_b2_Buyout <- t.test(Net_IRR ~ UpDown2, data = buyout_df, subset = UpDown2 %in% c("up", "base"))
t_equal_NetIRR_u2_d2_Buyout <- t.test(Net_IRR ~ UpDown2, data = buyout_df, subset = UpDown2 %in% c("up", "down"))
t_equal_NetIRR_b2_d2_Buyout <- t.test(Net_IRR ~ UpDown2, data = buyout_df, subset = UpDown2 %in% c("base", "down"))

t_unequal_NetIRR_u1_d1_Buyout <- t.test(Net_IRR ~ UpDown1, data = buyout_df, subset = UpDown1 %in% c("up", "down"), var.equal = FALSE)
t_unequal_NetIRR_u2_b2_Buyout <- t.test(Net_IRR ~ UpDown2, data = buyout_df, subset = UpDown2 %in% c("up", "base"), var.equal = FALSE)
t_unequal_NetIRR_u2_d2_Buyout <- t.test(Net_IRR ~ UpDown2, data = buyout_df, subset = UpDown2 %in% c("up", "down"), var.equal = FALSE)
t_unequal_NetIRR_b2_d2_Buyout <- t.test(Net_IRR ~ UpDown2, data = buyout_df, subset = UpDown2 %in% c("base", "down"), var.equal = FALSE)









summary_stats_distress_DPI_U1 <- distressed_df %>%
  group_by(UpDown1) %>%
  summarise(
    N = n(),
    Average = mean(DPI, na.rm = TRUE),
    Median = median(DPI, na.rm = TRUE),
    Percentile_75 = quantile(DPI, 0.75, na.rm = TRUE),
    Percentile_25 = quantile(DPI, 0.25, na.rm = TRUE),
    SD = sd(DPI, na.rm = TRUE)
  )

summary_stats_distress_DPI_U2 <- distressed_df %>%
  group_by(UpDown2) %>%
  summarise(
    N = n(),
    Average = mean(DPI, na.rm = TRUE),
    Median = median(DPI, na.rm = TRUE),
    Percentile_75 = quantile(DPI, 0.75, na.rm = TRUE),
    Percentile_25 = quantile(DPI, 0.25, na.rm = TRUE),
    SD = sd(DPI, na.rm = TRUE)
  )

summary_stats_distress_IRR_U1 <- distressed_df %>%
  group_by(UpDown1) %>%
  summarise(
    N = n(),
    Average = mean(Net_IRR, na.rm = TRUE),
    Median = median(Net_IRR, na.rm = TRUE),
    Percentile_75 = quantile(Net_IRR, 0.75, na.rm = TRUE),
    Percentile_25 = quantile(Net_IRR, 0.25, na.rm = TRUE),
    SD = sd(Net_IRR, na.rm = TRUE)
  )

summary_stats_distress_IRR_U2 <- distressed_df %>%
  group_by(UpDown2) %>%
  summarise(
    N = n(),
    Average = mean(Net_IRR, na.rm = TRUE),
    Median = median(Net_IRR, na.rm = TRUE),
    Percentile_75 = quantile(Net_IRR, 0.75, na.rm = TRUE),
    Percentile_25 = quantile(Net_IRR, 0.25, na.rm = TRUE),
    SD = sd(Net_IRR, na.rm = TRUE)
  )


#### DPI
levene_dpi_u1_d1_Distress <- leveneTest(DPI ~ UpDown1, data = distressed_df, subset = UpDown1 %in% c("up", "down"))
levene_dpi_u2_b2_Distress <- leveneTest(DPI ~ UpDown2, data = distressed_df, subset = UpDown2 %in% c("up", "base"))
levene_dpi_u2_d2_Distress <- leveneTest(DPI ~ UpDown2, data = distressed_df, subset = UpDown2 %in% c("up", "down"))
levene_dpi_b2_d2_Distress <- leveneTest(DPI ~ UpDown2, data = distressed_df, subset = UpDown2 %in% c("base", "down"))


t_equal_dpi_u1_d1_Distress <- t.test(DPI ~ UpDown1, data = distressed_df, subset = UpDown1 %in% c("up", "down"))
t_equal_dpi_u2_b2_Distress <- t.test(DPI ~ UpDown2, data = distressed_df, subset = UpDown2 %in% c("up", "base"))
t_equal_dpi_u2_d2_Distress <- t.test(DPI ~ UpDown2, data = distressed_df, subset = UpDown2 %in% c("up", "down"))
t_equal_dpi_b2_d2_Distress <- t.test(DPI ~ UpDown2, data = distressed_df, subset = UpDown2 %in% c("base", "down"))

t_unequal_dpi_u1_d1_Distress <- t.test(DPI ~ UpDown1, data = distressed_df, subset = UpDown1 %in% c("up", "down"), var.equal = FALSE)
t_unequal_dpi_u2_b2_Distress <- t.test(DPI ~ UpDown2, data = distressed_df, subset = UpDown2 %in% c("up", "base"), var.equal = FALSE)
t_unequal_dpi_u2_d2_Distress <- t.test(DPI ~ UpDown2, data = distressed_df, subset = UpDown2 %in% c("up", "down"), var.equal = FALSE)
t_unequal_dpi_b2_d2_Distress <- t.test(DPI ~ UpDown2, data = distressed_df, subset = UpDown2 %in% c("base", "down"), var.equal = FALSE)


### NET IRR
levene_NetIRR_u1_d1_Distress <- leveneTest(Net_IRR ~ UpDown1, data = distressed_df, subset = UpDown1 %in% c("up", "down"))
levene_NetIRR_u2_b2_Distress <- leveneTest(Net_IRR ~ UpDown2, data = distressed_df, subset = UpDown2 %in% c("up", "base"))
levene_NetIRR_u2_d2_Distress <- leveneTest(Net_IRR ~ UpDown2, data = distressed_df, subset = UpDown2 %in% c("up", "down"))
levene_NetIRR_b2_d2_Distress <- leveneTest(Net_IRR ~ UpDown2, data = distressed_df, subset = UpDown2 %in% c("base", "down"))


t_equal_NetIRR_u1_d1_Distress <- t.test(Net_IRR ~ UpDown1, data = distressed_df, subset = UpDown1 %in% c("up", "down"))
t_equal_NetIRR_u2_b2_Distress <- t.test(Net_IRR ~ UpDown2, data = distressed_df, subset = UpDown2 %in% c("up", "base"))
t_equal_NetIRR_u2_d2_Distress <- t.test(Net_IRR ~ UpDown2, data = distressed_df, subset = UpDown2 %in% c("up", "down"))
t_equal_NetIRR_b2_d2_Distress <- t.test(Net_IRR ~ UpDown2, data = distressed_df, subset = UpDown2 %in% c("base", "down"))

t_unequal_NetIRR_u1_d1_Distress <- t.test(Net_IRR ~ UpDown1, data = distressed_df, subset = UpDown1 %in% c("up", "down"), var.equal = FALSE)
t_unequal_NetIRR_u2_b2_Distress <- t.test(Net_IRR ~ UpDown2, data = distressed_df, subset = UpDown2 %in% c("up", "base"), var.equal = FALSE)
t_unequal_NetIRR_u2_d2_Distress <- t.test(Net_IRR ~ UpDown2, data = distressed_df, subset = UpDown2 %in% c("up", "down"), var.equal = FALSE)
t_unequal_NetIRR_b2_d2_Distress <- t.test(Net_IRR ~ UpDown2, data = distressed_df, subset = UpDown2 %in% c("base", "down"), var.equal = FALSE)



####### DPI var equal
t_equal_up_compareDPI <- t.test(DPI ~ Strategy, data = rbind(buyout_df, distressed_df),
                                subset = (UpDown2 == "up" & Strategy == "Buyout") |
                                  (UpDown2 == "up" & Strategy == "Distress"), var.equal = TRUE)

t_equal_up_vs_base_compareDPI <- t.test(DPI ~ Strategy, data = rbind(buyout_df, distressed_df),
                                        subset = (UpDown2 == "up" & Strategy == "Buyout") |
                                          (UpDown2 == "base" & Strategy == "Distress"), var.equal = TRUE)



t_equal_up_vs_down_compareDPI <- t.test(DPI ~ Strategy, data = rbind(buyout_df, distressed_df),
                                        subset = (UpDown2 == "up" & Strategy == "Buyout") |
                                          (UpDown2 == "down" & Strategy == "Distress"), var.equal = TRUE)


t_equal_base_vs_down_compareDPI <- t.test(DPI ~ Strategy, data = rbind(buyout_df, distressed_df),
                                          subset = (UpDown2 == "base" & Strategy == "Buyout") |
                                            (UpDown2 == "down" & Strategy == "Distress"), var.equal = TRUE)


t_equal_base_vs_base_compareDPI <- t.test(DPI ~ Strategy, data = rbind(buyout_df, distressed_df),
                                          subset = (UpDown2 == "base" & Strategy == "Buyout") |
                                            (UpDown2 == "base" & Strategy == "Distress"), var.equal = TRUE)


t_equal_base_vs_up_compareDPI <- t.test(DPI ~ Strategy, data = rbind(buyout_df, distressed_df),
                                        subset = (UpDown2 == "base" & Strategy == "Buyout") |
                                          (UpDown2 == "up" & Strategy == "Distress"), var.equal = TRUE)



t_equal_down_vs_up_compareDPI <- t.test(DPI ~ Strategy, data = rbind(buyout_df, distressed_df),
                                        subset = (UpDown2 == "down" & Strategy == "Buyout") |
                                          (UpDown2 == "up" & Strategy == "Distress"), var.equal = TRUE)

t_equal_down_vs_base_compareDPI <- t.test(DPI ~ Strategy, data = rbind(buyout_df, distressed_df),
                                          subset = (UpDown2 == "down" & Strategy == "Buyout") |
                                            (UpDown2 == "base" & Strategy == "Distress"), var.equal = TRUE)



t_equal_down_vs_down_compareDPI <- t.test(DPI ~ Strategy, data = rbind(buyout_df, distressed_df),
                                          subset = (UpDown2 == "down" & Strategy == "Buyout") |
                                            (UpDown2 == "down" & Strategy == "Distress"), var.equal = TRUE)


### DPI unequal var

t_unequal_up_compareDPI <- t.test(DPI ~ Strategy, data = rbind(buyout_df, distressed_df),
                                subset = (UpDown2 == "up" & Strategy == "Buyout") |
                                  (UpDown2 == "up" & Strategy == "Distress"), var.equal = FALSE)

t_unequal_up_vs_base_compareDPI <- t.test(DPI ~ Strategy, data = rbind(buyout_df, distressed_df),
                                        subset = (UpDown2 == "up" & Strategy == "Buyout") |
                                          (UpDown2 == "base" & Strategy == "Distress"), var.equal = FALSE)



t_unequal_up_compareDPI <- t.test(DPI ~ Strategy, data = rbind(buyout_df, distressed_df),
                                subset = (UpDown2 == "up" & Strategy == "Buyout") |
                                  (UpDown2 == "up" & Strategy == "Distress"), var.equal = FALSE)

t_unequal_up_vs_base_compareDPI <- t.test(DPI ~ Strategy, data = rbind(buyout_df, distressed_df),
                                        subset = (UpDown2 == "up" & Strategy == "Buyout") |
                                          (UpDown2 == "base" & Strategy == "Distress"), var.equal = FALSE)



t_unequal_up_vs_down_compareDPI <- t.test(DPI ~ Strategy, data = rbind(buyout_df, distressed_df),
                                        subset = (UpDown2 == "up" & Strategy == "Buyout") |
                                          (UpDown2 == "down" & Strategy == "Distress"), var.equal = FALSE)


t_unequal_base_vs_down_compareDPI <- t.test(DPI ~ Strategy, data = rbind(buyout_df, distressed_df),
                                          subset = (UpDown2 == "base" & Strategy == "Buyout") |
                                            (UpDown2 == "down" & Strategy == "Distress"), var.equal = FALSE)


t_unequal_base_vs_base_compareDPI <- t.test(DPI ~ Strategy, data = rbind(buyout_df, distressed_df),
                                          subset = (UpDown2 == "base" & Strategy == "Buyout") |
                                            (UpDown2 == "base" & Strategy == "Distress"), var.equal = FALSE)


t_unequal_base_vs_up_compareDPI <- t.test(DPI ~ Strategy, data = rbind(buyout_df, distressed_df),
                                        subset = (UpDown2 == "base" & Strategy == "Buyout") |
                                          (UpDown2 == "up" & Strategy == "Distress"), var.equal = FALSE)



t_unequal_down_vs_up_compareDPI <- t.test(DPI ~ Strategy, data = rbind(buyout_df, distressed_df),
                                        subset = (UpDown2 == "down" & Strategy == "Buyout") |
                                          (UpDown2 == "up" & Strategy == "Distress"), var.equal = FALSE)

t_unequal_down_vs_base_compareDPI <- t.test(DPI ~ Strategy, data = rbind(buyout_df, distressed_df),
                                          subset = (UpDown2 == "down" & Strategy == "Buyout") |
                                            (UpDown2 == "base" & Strategy == "Distress"), var.equal = FALSE)



t_unequal_down_vs_down_compareDPI <- t.test(DPI ~ Strategy, data = rbind(buyout_df, distressed_df),
                                          subset = (UpDown2 == "down" & Strategy == "Buyout") |
                                            (UpDown2 == "down" & Strategy == "Distress"), var.equal = FALSE)


############# NET IRR var equal

t_equal_up_compareNet_IRR <- t.test(Net_IRR ~ Strategy, data = rbind(buyout_df, distressed_df),
                                subset = (UpDown2 == "up" & Strategy == "Buyout") |
                                  (UpDown2 == "up" & Strategy == "Distress"), var.equal = TRUE)

t_equal_up_vs_base_compareNet_IRR <- t.test(Net_IRR ~ Strategy, data = rbind(buyout_df, distressed_df),
                                        subset = (UpDown2 == "up" & Strategy == "Buyout") |
                                          (UpDown2 == "base" & Strategy == "Distress"), var.equal = TRUE)


t_equal_up_vs_down_compareNet_IRR <- t.test(Net_IRR ~ Strategy, data = rbind(buyout_df, distressed_df),
                                            subset = (UpDown2 == "up" & Strategy == "Buyout") |
                                              (UpDown2 == "down" & Strategy == "Distress"), var.equal = TRUE)


t_equal_base_vs_down_compareNet_IRR <- t.test(Net_IRR ~ Strategy, data = rbind(buyout_df, distressed_df),
                                              subset = (UpDown2 == "base" & Strategy == "Buyout") |
                                                (UpDown2 == "down" & Strategy == "Distress"), var.equal = TRUE)


t_equal_base_vs_base_compareNet_IRR <- t.test(Net_IRR ~ Strategy, data = rbind(buyout_df, distressed_df),
                                              subset = (UpDown2 == "base" & Strategy == "Buyout") |
                                                (UpDown2 == "base" & Strategy == "Distress"), var.equal = TRUE)


t_equal_base_vs_up_compareNet_IRR <- t.test(Net_IRR ~ Strategy, data = rbind(buyout_df, distressed_df),
                                            subset = (UpDown2 == "base" & Strategy == "Buyout") |
                                              (UpDown2 == "up" & Strategy == "Distress"), var.equal = TRUE)



t_equal_down_vs_up_compareNet_IRR <- t.test(Net_IRR ~ Strategy, data = rbind(buyout_df, distressed_df),
                                            subset = (UpDown2 == "down" & Strategy == "Buyout") |
                                              (UpDown2 == "up" & Strategy == "Distress"), var.equal = TRUE)

t_equal_down_vs_base_compareNet_IRR <- t.test(Net_IRR ~ Strategy, data = rbind(buyout_df, distressed_df),
                                              subset = (UpDown2 == "down" & Strategy == "Buyout") |
                                                (UpDown2 == "base" & Strategy == "Distress"), var.equal = TRUE)



t_equal_down_vs_down_compareNet_IRR <- t.test(Net_IRR ~ Strategy, data = rbind(buyout_df, distressed_df),
                                              subset = (UpDown2 == "down" & Strategy == "Buyout") |
                                                (UpDown2 == "down" & Strategy == "Distress"), var.equal = TRUE)


#### NET IRR unequal var


t_unequal_up_compareNet_IRR <- t.test(Net_IRR ~ Strategy, data = rbind(buyout_df, distressed_df),
                                subset = (UpDown2 == "up" & Strategy == "Buyout") |
                                  (UpDown2 == "up" & Strategy == "Distress"), var.equal = FALSE)

t_unequal_up_vs_base_compareNet_IRR <- t.test(Net_IRR ~ Strategy, data = rbind(buyout_df, distressed_df),
                                        subset = (UpDown2 == "up" & Strategy == "Buyout") |
                                          (UpDown2 == "base" & Strategy == "Distress"), var.equal = FALSE)


t_unequal_up_vs_down_compareNet_IRR <- t.test(Net_IRR ~ Strategy, data = rbind(buyout_df, distressed_df),
                                            subset = (UpDown2 == "up" & Strategy == "Buyout") |
                                              (UpDown2 == "down" & Strategy == "Distress"), var.equal = FALSE)


t_unequal_base_vs_down_compareNet_IRR <- t.test(Net_IRR ~ Strategy, data = rbind(buyout_df, distressed_df),
                                              subset = (UpDown2 == "base" & Strategy == "Buyout") |
                                                (UpDown2 == "down" & Strategy == "Distress"), var.equal = FALSE)


t_unequal_base_vs_base_compareNet_IRR <- t.test(Net_IRR ~ Strategy, data = rbind(buyout_df, distressed_df),
                                              subset = (UpDown2 == "base" & Strategy == "Buyout") |
                                                (UpDown2 == "base" & Strategy == "Distress"), var.equal = FALSE)


t_unequal_base_vs_up_compareNet_IRR <- t.test(Net_IRR ~ Strategy, data = rbind(buyout_df, distressed_df),
                                            subset = (UpDown2 == "base" & Strategy == "Buyout") |
                                              (UpDown2 == "up" & Strategy == "Distress"), var.equal = FALSE)



t_unequal_down_vs_up_compareNet_IRR <- t.test(Net_IRR ~ Strategy, data = rbind(buyout_df, distressed_df),
                                            subset = (UpDown2 == "down" & Strategy == "Buyout") |
                                              (UpDown2 == "up" & Strategy == "Distress"), var.equal = FALSE)

t_unequal_down_vs_base_compareNet_IRR <- t.test(Net_IRR ~ Strategy, data = rbind(buyout_df, distressed_df),
                                              subset = (UpDown2 == "down" & Strategy == "Buyout") |
                                                (UpDown2 == "base" & Strategy == "Distress"), var.equal = FALSE)


t_unequal_down_vs_down_compareNet_IRR <- t.test(Net_IRR ~ Strategy, data = rbind(buyout_df, distressed_df),
                                              subset = (UpDown2 == "down" & Strategy == "Buyout") |
                                                (UpDown2 == "down" & Strategy == "Distress"), var.equal = FALSE)






############# DPIComparison Levene

tempRbindDf = rbind(buyout_df, distressed_df)

Levene_up_compareDPI <- leveneTest(DPI ~ Strategy, 
                                   data = rbind(buyout_df, distressed_df),
                                   subset = (UpDown2 == "up" & (Strategy == "Buyout" | Strategy == "Distress")))


tempData = subset(tempRbindDf, (UpDown2 == "up" & Strategy == "Buyout") | (UpDown2 == "base" & Strategy == "Distress") )
Levene_up_vs_base_compareDPI <- leveneTest(DPI ~ Strategy, 
                                           data = tempData)

tempData_up_vs_down <- subset(rbind(buyout_df, distressed_df), 
                              (UpDown2 == "up" & Strategy == "Buyout") |
                                (UpDown2 == "down" & Strategy == "Distress"))

tempData_base_vs_down <- subset(rbind(buyout_df, distressed_df), 
                                (UpDown2 == "base" & Strategy == "Buyout") |
                                  (UpDown2 == "down" & Strategy == "Distress"))

tempData_base_vs_base <- subset(rbind(buyout_df, distressed_df), 
                                (UpDown2 == "base" & Strategy == "Buyout") |
                                  (UpDown2 == "base" & Strategy == "Distress"))

tempData_base_vs_up <- subset(rbind(buyout_df, distressed_df), 
                              (UpDown2 == "base" & Strategy == "Buyout") |
                                (UpDown2 == "up" & Strategy == "Distress"))

tempData_down_vs_up <- subset(rbind(buyout_df, distressed_df), 
                              (UpDown2 == "down" & Strategy == "Buyout") |
                                (UpDown2 == "up" & Strategy == "Distress"))

tempData_down_vs_base <- subset(rbind(buyout_df, distressed_df), 
                                (UpDown2 == "down" & Strategy == "Buyout") |
                                  (UpDown2 == "base" & Strategy == "Distress"))

tempData_down_vs_down <- subset(rbind(buyout_df, distressed_df), 
                                (UpDown2 == "down" & Strategy == "Buyout") |
                                  (UpDown2 == "down" & Strategy == "Distress"))

Levene_up_vs_down_compareDPI <- leveneTest(DPI ~ Strategy, data = tempData_up_vs_down)

Levene_base_vs_down_compareDPI <- leveneTest(DPI ~ Strategy, data = tempData_base_vs_down)

Levene_base_vs_base_compareDPI <- leveneTest(DPI ~ Strategy, data = tempData_base_vs_base)

Levene_base_vs_up_compareDPI <- leveneTest(DPI ~ Strategy, data = tempData_base_vs_up)

Levene_down_vs_up_compareDPI <- leveneTest(DPI ~ Strategy, data = tempData_down_vs_up)

Levene_down_vs_base_compareDPI <- leveneTest(DPI ~ Strategy, data = tempData_down_vs_base)

Levene_down_vs_down_compareDPI <- leveneTest(DPI ~ Strategy, data = tempData_down_vs_down)




############# NET IRR Comparison Levene

tempRbindDf = rbind(buyout_df, distressed_df)

Levene_up_compareNet_IRR <- leveneTest(Net_IRR ~ Strategy, 
                                       data = rbind(buyout_df, distressed_df),
                                       subset = (UpDown2 == "up" & (Strategy == "Buyout" | Strategy == "Distress")))


tempData = subset(tempRbindDf, (UpDown2 == "up" & Strategy == "Buyout") | (UpDown2 == "base" & Strategy == "Distress") )
Levene_up_vs_base_compareNet_IRR <- leveneTest(Net_IRR ~ Strategy, 
                                               data = tempData)

tempData_up_vs_down <- subset(rbind(buyout_df, distressed_df), 
                              (UpDown2 == "up" & Strategy == "Buyout") |
                                (UpDown2 == "down" & Strategy == "Distress"))

tempData_base_vs_down <- subset(rbind(buyout_df, distressed_df), 
                                (UpDown2 == "base" & Strategy == "Buyout") |
                                  (UpDown2 == "down" & Strategy == "Distress"))

tempData_base_vs_base <- subset(rbind(buyout_df, distressed_df), 
                                (UpDown2 == "base" & Strategy == "Buyout") |
                                  (UpDown2 == "base" & Strategy == "Distress"))

tempData_base_vs_up <- subset(rbind(buyout_df, distressed_df), 
                              (UpDown2 == "base" & Strategy == "Buyout") |
                                (UpDown2 == "up" & Strategy == "Distress"))

tempData_down_vs_up <- subset(rbind(buyout_df, distressed_df), 
                              (UpDown2 == "down" & Strategy == "Buyout") |
                                (UpDown2 == "up" & Strategy == "Distress"))

tempData_down_vs_base <- subset(rbind(buyout_df, distressed_df), 
                                (UpDown2 == "down" & Strategy == "Buyout") |
                                  (UpDown2 == "base" & Strategy == "Distress"))

tempData_down_vs_down <- subset(rbind(buyout_df, distressed_df), 
                                (UpDown2 == "down" & Strategy == "Buyout") |
                                  (UpDown2 == "down" & Strategy == "Distress"))

Levene_up_vs_down_compareNet_IRR <- leveneTest(Net_IRR ~ Strategy, data = tempData_up_vs_down)

Levene_base_vs_down_compareNet_IRR <- leveneTest(Net_IRR ~ Strategy, data = tempData_base_vs_down)

Levene_base_vs_base_compareNet_IRR <- leveneTest(Net_IRR ~ Strategy, data = tempData_base_vs_base)

Levene_base_vs_up_compareNet_IRR <- leveneTest(Net_IRR ~ Strategy, data = tempData_base_vs_up)

Levene_down_vs_up_compareNet_IRR <- leveneTest(Net_IRR ~ Strategy, data = tempData_down_vs_up)

Levene_down_vs_base_compareNet_IRR <- leveneTest(Net_IRR ~ Strategy, data = tempData_down_vs_base)

Levene_down_vs_down_compareNet_IRR <- leveneTest(Net_IRR ~ Strategy, data = tempData_down_vs_down)






#### Code for D/U1 comparisons (Leven, equal T, unequal T for DPI then Net IRR)

 # Levene DPI
tempDataUp1_Up1 = subset(tempRbindDf, (UpDown1 == "up" & Strategy == "Buyout") | (UpDown1 == "up" & Strategy == "Distress"))
tempDataup1_vs_down1 = subset(tempRbindDf, (UpDown1 == "up" & Strategy == "Buyout") | (UpDown1 == "down" & Strategy == "Distress"))
tempDatadown1_vs_up1 = subset(tempRbindDf, (UpDown1 == "down" & Strategy == "Buyout") | (UpDown1 == "up" & Strategy == "Distress"))
tempDatadown1_vs_down1 = subset(tempRbindDf, (UpDown1 == "down" & Strategy == "Buyout") | (UpDown1 == "down" & Strategy == "Distress"))


Levene_up1_vs_up1_compareDPI <- leveneTest(DPI ~ Strategy, 
                                               data = tempDataUp1_Up1)
Levene_up1_vs_down1_compareDPI <- leveneTest(DPI ~ Strategy, 
                                               data = tempDataup1_vs_down1)
Levene_down1_vs_up1_compareDPI <- leveneTest(DPI ~ Strategy, 
                                               data = tempDatadown1_vs_up1)
Levene_down1_vs_down1_compareDPI <- leveneTest(DPI ~ Strategy, 
                                               data = tempDatadown1_vs_down1)


# LEvene NET_IRR
tempDataUp1_Up1 = subset(tempRbindDf, (UpDown1 == "up" & Strategy == "Buyout") | (UpDown1 == "up" & Strategy == "Distress"))
tempDataup1_vs_down1 = subset(tempRbindDf, (UpDown1 == "up" & Strategy == "Buyout") | (UpDown1 == "down" & Strategy == "Distress"))
tempDatadown1_vs_up1 = subset(tempRbindDf, (UpDown1 == "down" & Strategy == "Buyout") | (UpDown1 == "up" & Strategy == "Distress"))
tempDatadown1_vs_down1 = subset(tempRbindDf, (UpDown1 == "down" & Strategy == "Buyout") | (UpDown1 == "down" & Strategy == "Distress"))


Levene_up1_vs_up1_compareNet_IRR <- leveneTest(Net_IRR ~ Strategy, 
                                               data = tempDataUp1_Up1)
Levene_up1_vs_down1_compareNet_IRR <- leveneTest(Net_IRR ~ Strategy, 
                                                 data = tempDataup1_vs_down1)
Levene_down1_vs_up1_compareNet_IRR <- leveneTest(Net_IRR ~ Strategy, 
                                                 data = tempDatadown1_vs_up1)
Levene_down1_vs_down1_compareNet_IRR <- leveneTest(Net_IRR ~ Strategy, 
                                                   data = tempDatadown1_vs_down1)


# T equal DPI
T_Equal_up1_vs_up1_compareDPI <- t.test(DPI ~ Strategy, 
                                               data = tempDataUp1_Up1, var.equal = TRUE)
T_Equal_up1_vs_down1_compareDPI <- t.test(DPI ~ Strategy, 
                                                 data = tempDataup1_vs_down1,var.equal = TRUE)
T_Equal_down1_vs_up1_compareDPI <- t.test(DPI ~ Strategy, 
                                                 data = tempDatadown1_vs_up1,var.equal = TRUE)
T_Equal_down1_vs_down1_compareDPI <- t.test(DPI ~ Strategy, 
                                                   data = tempDatadown1_vs_down1,var.equal = TRUE)


# T equal NET_IRR
T_Equal_up1_vs_up1_compareNet_IRR <- t.test(Net_IRR ~ Strategy, 
                                            data = tempDataUp1_Up1, var.equal = TRUE)
T_Equal_up1_vs_down1_compareNet_IRR <- t.test(Net_IRR ~ Strategy, 
                                              data = tempDataup1_vs_down1,var.equal = TRUE)
T_Equal_down1_vs_up1_compareNet_IRR <- t.test(Net_IRR ~ Strategy, 
                                              data = tempDatadown1_vs_up1,var.equal = TRUE)
T_Equal_down1_vs_down1_compareNet_IRR <- t.test(Net_IRR ~ Strategy, 
                                                data = tempDatadown1_vs_down1,var.equal = TRUE)


# T Unequal DPI
T_Unequal_up1_vs_up1_compareDPI <- t.test(DPI ~ Strategy, 
                                        data = tempDataUp1_Up1, var.equal = FALSE)
T_Unequal_up1_vs_down1_compareDPI <- t.test(DPI ~ Strategy, 
                                          data = tempDataup1_vs_down1,var.equal = FALSE)
T_Unequal_down1_vs_up1_compareDPI <- t.test(DPI ~ Strategy, 
                                          data = tempDatadown1_vs_up1,var.equal = FALSE)
T_Unequal_down1_vs_down1_compareDPI <- t.test(DPI ~ Strategy, 
                                            data = tempDatadown1_vs_down1,var.equal = FALSE)


# T Unequal NET_IRR
T_Unequal_up1_vs_up1_compareNet_IRR <- t.test(Net_IRR ~ Strategy, 
                                            data = tempDataUp1_Up1, var.equal = FALSE)
T_Unequal_up1_vs_down1_compareNet_IRR <- t.test(Net_IRR ~ Strategy, 
                                              data = tempDataup1_vs_down1,var.equal = FALSE)
T_Unequal_down1_vs_up1_compareNet_IRR <- t.test(Net_IRR ~ Strategy, 
                                              data = tempDatadown1_vs_up1,var.equal = FALSE)
T_Unequal_down1_vs_down1_compareNet_IRR <- t.test(Net_IRR ~ Strategy, 
                                                data = tempDatadown1_vs_down1,var.equal = FALSE)





#### Regression Analysis
reg_modelBuyout <- lm(Net_IRR ~ DFI, data = buyout_df)
reg_modelBuyoutSummary = summary(reg_modelBuyout)

R_squared <- reg_modelBuyoutSummary$r.squared
R_squared_adjusted <- reg_modelBuyoutSummary$adj.r.squared
coefficients <- coef(reg_modelBuyout)
C <- coefficients[1]
C_std_error <- sqrt(diag(vcov(reg_modelBuyout)))[1]
beta_DFI <- coefficients[2]
beta_DFI_std_error <- sqrt(diag(vcov(reg_modelBuyout)))[2]
C_p_value <- coef(summary(reg_modelBuyout))[1, 4]
beta_DFI_p_value <- coef(summary(reg_modelBuyout))[2, 4]

result_tableBuyout <- data.frame(
  Statistic = c("R-squared", "Adjusted R-squared", "C", "C (std. error)", "Beta DFI", "Beta DFI (std. error)", "C p-value", "Beta DFI p-value"),
  Value = c(R_squared, R_squared_adjusted, C, C_std_error, beta_DFI, beta_DFI_std_error, C_p_value, beta_DFI_p_value)
)


### Distress
reg_modelDistress <- lm(Net_IRR ~ DFI, data = distressed_df)
reg_modelDistressSummary = summary(reg_modelDistress)

R_squared <- reg_modelDistressSummary$r.squared
R_squared_adjusted <- reg_modelDistressSummary$adj.r.squared
coefficients <- coef(reg_modelDistress)
C <- coefficients[1]
C_std_error <- sqrt(diag(vcov(reg_modelDistress)))[1]
beta_DFI <- coefficients[2]
beta_DFI_std_error <- sqrt(diag(vcov(reg_modelDistress)))[2]
C_p_value <- coef(summary(reg_modelDistress))[1, 4]
beta_DFI_p_value <- coef(summary(reg_modelDistress))[2, 4]

result_tableDistress <- data.frame(
  Statistic = c("R-squared", "Adjusted R-squared", "C", "C (std. error)", "Beta DFI", "Beta DFI (std. error)", "C p-value", "Beta DFI p-value"),
  Value = c(R_squared, R_squared_adjusted, C, C_std_error, beta_DFI, beta_DFI_std_error, C_p_value, beta_DFI_p_value)
)


stargazer(reg_modelBuyout, reg_modelDistress,  type = "html", out="Moody_Regressions.doc")