library(readxl)
library(ggplot2)  # For plotting
library(dplyr)    # For data manipulation
library(robust) 
library(robustbase)

# Load necessary libraries
library(readr)    # For reading CSV files
library(ggplot2)  # For plotting
library(tidyr) #para usar dropna

IC1613_VI <- read_excel("IC1613 VI.xlsx")

# Extract the relevant columns
periodos_i <- IC1613_VI$P_I
periodos_v <- IC1613_VI$P_V
p_i <- na.omit(periodos_i)
p_v <- na.omit(periodos_v)
log_pi <- log10(p_i)
log_pv <- log10(p_v)

i <- IC1613_VI$I
v <- IC1613_VI$V

mag_i <- na.omit(i)
mag_v <- na.omit(v)

# Create a data frame for regression
regression_data <- data.frame(log_pi, mag_i)

# Perform Major Axis (MM) regression using robust method
mm_model <- lmrob(mag_i ~ log_pi, data = regression_data)

LMCmed <- lm(mag_i ~ log_pi, data = regression_data)
yhat<- predict(LMCmed, se.fit =FALSE)
plot(yhat, LMCmed$residuals,xlim = rev(range(yhat)),  xlab="Predicción I", ylab="Residuos I", main="Residuos I vs. Predicción I", sub = "IC1613",col.sub = "purple")
plot(log_pi, LMCmed$residuals,xlab="Log P", ylab="Residuos I", main="Residuos I vs. Log P", sub = "IC1613",col.sub = "purple")


#Forzando pendiente
mm_model_forzi <- lmrob(mag_i ~ 1+offset(-2.9155*log_pi), data = regression_data)
summary(mm_model_forzi)

# Extract slope and intercept
slope <- coef(mm_model)[2]  # Slope is the second coefficient
intercept <- coef(mm_model)[1]  # Intercept is the first coefficient

# Calculate standard errors for slope and intercept
slope_se <- summary(mm_model)$coefficients[2, "Std. Error"]
intercept_se <- summary(mm_model)$coefficients[1, "Std. Error"]

# Print the results
cat("Slope:", slope, "±", slope_se, "\n")
cat("Intercept:", intercept, "±", intercept_se, "\n")

#Graph
plot(log_pi, mag_i,ylim = rev(range(mag_i)), main="Regresión MM para I vs. LogP", xlab="LogP", ylab="Magnitud I", sub = "IC1613",col.sub = "purple" )
#scale_y_reverse()
#add the fitted regression line to the scatterplot
abline(mm_model, col="steelblue")
# Add annotations for slope and intercept
annotate("text", x = min(log_p), y = max(mag_j), 
         label = paste("Slope:", round(slope, 3), "\nIntercept:", round(intercept, 3)), 
         hjust = 0, vjust = 1, size = 4, color = "red")

#Calculos para V

# Create a data frame for regression
regression_data2 <- data.frame(log_pv, mag_v)

# Perform Major Axis (MM) regression using robust method
mm_model2 <- lmrob(mag_v ~ log_pv, data = regression_data2)

LMCmed2 <- lm(mag_v ~ log_pv, data = regression_data2)
yhat2<- predict(LMCmed2, se.fit =FALSE)
plot(yhat2, LMCmed2$residuals,xlim = rev(range(yhat2)),  xlab="Predicción V", ylab="Residuos V", main="Residuos V vs. Predicción V", sub = "IC1613",col.sub = "purple")
plot(log_pv, LMCmed2$residuals,xlab="Log P", ylab="Residuos V", main="Residuos V vs. Log P", sub = "IC1613",col.sub = "purple")

#Forzando pendiente
mm_model_forzv <- lmrob(mag_v ~ 1+offset(-2.670*log_pv), data = regression_data2)
summary(mm_model_forzv)

# Extract slope and intercept
slope2 <- coef(mm_model2)[2]  # Slope is the second coefficient
intercept2 <- coef(mm_model2)[1]  # Intercept is the first coefficient

# Calculate standard errors for slope and intercept
slope_se2 <- summary(mm_model2)$coefficients[2, "Std. Error"]
intercept_se2 <- summary(mm_model2)$coefficients[1, "Std. Error"]

# Print the results
cat("Slope:", slope2, "±", slope_se2, "\n")
cat("Intercept:", intercept2, "±", intercept_se2, "\n")

#Graph
plot(log_pv, mag_v,ylim = rev(range(mag_v)), main="Regresión MM para V vs. LogP", xlab="LogP", ylab="Magnitud V", sub = "IC1613",col.sub = "purple" )
#scale_y_reverse()
#add the fitted regression line to the scatterplot
abline(mm_model2, col="steelblue")
