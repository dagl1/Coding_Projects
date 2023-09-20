### Example preset for data analysis

rm(list=ls())
# Create functions when you notice using the same thing over and over!


#Example data
n = 10
num_columns <- 3
df <- data.frame(matrix(nrow = n, ncol = 0))
for (col_index in 1:num_columns) {
  length <- numeric(n)
  for (i in 1:n) {
    length[i] <- 5 - rnorm(1, mean = i * 0.05 + runif(1, -0.02, 0.02), sd = 0.1)
  }
  colname <- paste0("length", col_index)
  df[, colname] <- length
}
molarity <- seq(0, 0.9, by = 0.1)
df$Molarity <- molarity


# Example import from excel
if(!require(readxl)) { install.packages("readxl") }
setwd("E:/Downloads")
df <- read_excel("exampleDataSetDanaData.xlsx", col_names = FALSE)
df <- df[,2:ncol(df)]
colnames(df) <- c("lenght1", "length2", "length3", "Molarity")

### Analyze code
#df$Length_average <- rowMeans(df[c("lenght1", "length2", "length3")])
df$Length_average <- apply(df[c("lenght1", "length2", "length3")],1,mean)
df$Length_Sd <- apply(df[c("lenght1", "length2", "length3")], 1 , sd)

plot(Length_average ~ Molarity ,data = df)
segments(x0 = df$Molarity, y0 = df$Length_average, 
         x1 = df$Molarity, y1 = df$Length_average + df$Length_Sd,
         col = "blue")
# Fit a linear regression model
lm_model <- lm(Length_average ~ Molarity, data = df)
interceptLine <- lm_model$coefficients[1]
MolaritySlope <- lm_model$coefficients[2]


# Create the plot
plot(Length_average ~ Molarity, data = df, ylim = c(min(df$Length_average - df$Length_Sd), max(df$Length_average + df$Length_Sd)))

# Add points
points(Length_average ~ Molarity, data = df)

# Add error bars (one standard deviation)
segments(x0 = df$Molarity, y0 = df$Length_average - df$Length_Sd, 
         x1 = df$Molarity, y1 = df$Length_average + df$Length_Sd, col = "blue")

# Add the regression line
abline(lm_model, col = "red", lty = 2)
