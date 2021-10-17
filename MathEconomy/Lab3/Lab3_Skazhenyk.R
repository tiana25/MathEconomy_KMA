library(car)

baseball <- read.delim(file = "C:\\Users\\Tiana_\\Documents\\MathEconomy\\Lab3\\bsbl.txt", header = TRUE, sep = "\t", dec = ",")


## TASK (A) Building the maximum model for my case.
modBaseball0 <- lm(Y~., data=baseball)
model0_summary <- summary(modBaseball0)

model0_summary
# Call:
#   lm(formula = Y ~ ., data = baseball)
# 
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -0.03970 -0.01143 -0.00101  0.01044  0.03444 
# 
# Coefficients:
#               Estimate Std. Error t value Pr(>|t|)    
# (Intercept)   0.18316    0.01714  10.685 3.79e-13 ***
#   X1          0.44668    0.10963   4.074 0.000219 ***
#   X2          0.99090    0.31309   3.165 0.003005 ** 
#   X3          0.62160    0.58070   1.070 0.291004    
#   X4            0.27374    0.16935   1.616 0.114060    
#   X5            -0.28456  0.05177   -5.497 2.59e-06 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.01748 on 39 degrees of freedom
# Multiple R-squared:  0.8601,	Adjusted R-squared:  0.8422 
# F-statistic: 47.96 on 5 and 39 DF,  p-value: 1.261e-15




## TASK (B) Making a prediction Y^ for my case
## Table for Y^
y_ <- modBaseball0$fitted.values
y_
# 1         2         3         4         5         6         7 
# 0.2825848 0.2686287 0.2862903 0.3099221 0.3030509 0.3357017 0.2474080 
# 8         9        10        11        12        13        14 
# 0.2284412 0.3002305 0.2374328 0.2737341 0.2929252 0.2725585 0.2281848 
# 15        16        17        18        19        20        21 
# 0.3276655 0.3033359 0.2280375 0.2809447 0.3465602 0.3343353 0.3072689 
# 22        23        24        25        26        27        28 
# 0.3154073 0.2544547 0.3432168 0.3345496 0.2434417 0.2345624 0.3313101 
# 29        30        31        32        33        34        35 
# 0.2298060 0.3210102 0.2299599 0.3397977 0.2104322 0.2966244 0.2271066 
# 36        37        38        39        40        41        42 
# 0.3131416 0.2808626 0.3154073 0.2891987 0.2019141 0.2281848 0.2429335 
# 43        44        45 
# 0.2664472 0.2922471 0.2837422 

## X1 - runs; X2 - doubles; X3 - triples;
## X4 - home runs; x5 - strike outs

  beta_coeffs <- model0_summary$coefficients[, 1]
  b0 <- beta_coeffs[1]
  b1 <- beta_coeffs[2]
  b2 <- beta_coeffs[3]
  b3 <- beta_coeffs[4]
  b4 <- beta_coeffs[5]
  b5 <- beta_coeffs[6]
  
  forecast_values <- data.frame(X1 = 0.124,X2 = 0.045, X3 = 0.002, X4 = 0.02, X5 = 0.06)
  
  bsbl_pred <- b0 + b1*forecast_values[1]+b2*forecast_values[2]+
    b3*forecast_values[3]+b4*forecast_values[4]+b5*forecast_values[5]
  bsbl_pred
   #(Intercept) 
   #0.2727862

  
## TASK (C) Analysis by t-test for all regression coefficients

## We can see t_j values in the third column in the summary above
tj_values <- model0_summary$coefficients[, 3]
tj_values
# (Intercept)          X1          X2          X3          X4          X5 
# 10.685499    4.074258    3.164952    1.070434    1.616442   -5.496510 

# Finding the critical value of t
t_val <- qt(p=.05/2, df=39, lower.tail=FALSE)
t_val
#[1] 2.022691

##−t_cr > t_j > t_cr
## We can see that after applying the inequality above, we can sacrifice 
## X3 and X4 variables and write 0 instead of them



## TASK(D) Find and write confidence intervals
##         for multiple regression coefficients

confint(modBaseball0)
# 2.5 %     97.5 %
#   (Intercept)  0.14849140  0.2178343
# X1           0.22492299  0.6684373
# X2           0.35762671  1.6241816
# X3          -0.55297711  1.7961835
# X4          -0.06879646  0.6162718
# X5          -0.38927671 -0.1798432



## TASK (E) Find and write confidence intervals
##          for regression values Y^ namely for Y^1)

## Here is the data for the first row in our table
## X1 = 0.144; X2 = 0.049; X3 = 0.012; X4 = 0.013; X5 = 0.086
row1 <- data.frame(X1 = 0.144, X2 = 0.049, 
                   X3 = 0.012, X4 = 0.013, X5 = 0.086 )
predict(modBaseball0, newdata = row1, interval = "confidence")

#fit       lwr      upr
#1 0.2825848 0.2761005 0.289069


## TASK (F) Find the forecast value for the next period
##          and record the confidence intervals

predict(modBaseball0, newdata = forecast_values, interval = "prediction")

#fit       lwr       upr
#1 0.2727862 0.2351721 0.3104004


## TASK (G) Make an analysis using Fisher's test (F-test)
# This function calculates a simple anova for the linear model

  simpleAnova <- function(object, ...) {
  
  # Обчислити таблицю anova
  tab <- anova(object, ...)
  
  # Отримати кількість предикторів
  p <- nrow(tab) - 1
  
  # Додайте рядок предикторів
  predictorsRow <- colSums(tab[1:p, 1:2])
  predictorsRow <- c(predictorsRow, predictorsRow[2] / predictorsRow[1])
  
  # F-значення
  Fval <- predictorsRow[3] / tab[p + 1, 3]
  pval <- pf(Fval, df1 = p, df2 = tab$Df[p + 1], lower.tail = FALSE)
  predictorsRow <- c(predictorsRow, Fval, pval)
  
  # Спрощена таблиця
  tab <- rbind(predictorsRow, tab[p + 1, ])
  row.names(tab)[1] <- "Predictors"
  return(tab)
  
}
simpleAnova(modBaseball0)

# Analysis of Variance Table
# Response: Y
#             Df   Sum Sq   Mean  Sq F value    Pr(>F)    
# Predictors  5   0.073275 0.0146551  47.965 1.261e-15 ***
#   Residuals 39  0.011916 0.0003055                      
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

f_val <- qf(.95, df1=5, df2=39)
f_val
#[1] 2.455831

## We can see from above that F value = 47.965 and F_cr = 2.45
## F > F_cr  => this suggests that at least some coefficient
## is different from zero, so we can rely on this model
## and make further evaluation



## TASK (H) Make model improvements by removing unimportant factors

## Let's remove X3
mod2baseball <- lm(Y ~ . - X3, data = baseball)
summary(mod2baseball)

# Call:
#   lm(formula = Y ~ . - X3, data = baseball)
# 
# Residuals:
#   Min        1Q    Median        3Q       Max 
# -0.036851 -0.011615 -0.000982  0.009218  0.035792 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  0.17867    0.01665  10.731 2.43e-13 ***
#   X1           0.50770    0.09382   5.411 3.17e-06 ***
#   X2           1.10295    0.29561   3.731 0.000592 ***
#   X4           0.19950    0.15478   1.289 0.204808    
# X5          -0.29501    0.05094  -5.792 9.27e-07 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.01751 on 40 degrees of freedom
# Multiple R-squared:  0.856,	Adjusted R-squared:  0.8416 
# F-statistic: 59.45 on 4 and 40 DF,  p-value: 2.657e-16

## Let's remove X3 and X4
mod3baseball <- lm(Y ~ . - X3 -X4, data = baseball)
summary(mod3baseball)

# Call:
#   lm(formula = Y ~ . - X3 - X4, data = baseball)
# 
# Residuals:
#   Min        1Q    Median        3Q       Max 
# -0.034709 -0.011472 -0.001311  0.011062  0.034968 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  0.16941    0.01514  11.188 4.92e-14 ***
#   X1           0.57494    0.07861   7.314 5.96e-09 ***
#   X2           1.11908    0.29772   3.759 0.000533 ***
#   X5          -0.26431    0.04539  -5.824 7.71e-07 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.01765 on 41 degrees of freedom
# Multiple R-squared:   0.85,	Adjusted R-squared:  0.8391 
# F-statistic: 77.47 on 3 and 41 DF,  p-value: < 2.2e-16

## So in last model, after removing x3 and x4
## we can see that each coefficient is important (***)



## TASK (I) Analysis of 3 improved models from
##          the previous task for R^2 and R^2adj

car::compareCoefs(modBaseball0, mod2baseball, mod3baseball)

# Calls:
# 1: lm(formula = Y ~ ., data = baseball)
# 2: lm(formula = Y ~ . - X3, data = baseball)
# 3: lm(formula = Y ~ . - X3 - X4, data = baseball)
# 
#              Model 1 Model 2 Model 3
# (Intercept)  0.1832  0.1787  0.1694
# SE           0.0171  0.0166  0.0151
# 
# X1           0.4467  0.5077  0.5749
# SE           0.1096  0.0938  0.0786
# 
# X2            0.991   1.103   1.119
# SE            0.313   0.296   0.298
# 
# X3            0.622                
# SE            0.581                
# 
# X4            0.274   0.200        
# SE            0.169   0.155        
# 
# X5          -0.2846 -0.2950 -0.2643
# SE           0.0518  0.0509  0.0454

## Here we can see that the lowest SE (mostly) is in the third model
## Let's compare by R^2 and R^2 adj

# Model 3 (without x3 and x4)
summary(mod3baseball)$r.squared ##[1] 0.850037
summary(mod3baseball)$adj.r.squared ##[1] 0.8390641
summary(mod3baseball)$fstatistic

# value    numdf    dendf 
# 77.46693  3.00000 41.00000 

# Model 2 (without x3)
summary(mod2baseball)$r.squared ## [1] 0.8560176
summary(mod2baseball)$adj.r.squared ## [1] 0.8416194
summary(mod2baseball)$fstatistic

# value    numdf    dendf 
# 59.45294  4.00000 40.00000 

# Initial model (whole)
model0_summary$r.squared ##[1] 0.8601271
model0_summary$adj.r.squared  ##[1] 0.8421947
model0_summary$fstatistic

# value    numdf    dendf 
# 47.96492  5.00000 39.00000


## We can see that the biggest F-value is in the Model 3,
## the biggest R^2adj and R^2 is in the initial model.
## But F-value is more important than other criteria, so the best model
## is Model 3.

## Task (J)
## From the beginning, I assumed that x3(triples) and x4(home runs)
## variables had the least effect
## on the final model, because other variables
## are more important in baseball (runs, doubles, strike outs)
## And my assumptions came true.

## So we can see that Pr(>|t|) - p-value for t-test is the smallest
## in intercept (3.79e-13) which stands for b0 coeff,
## (so this is the best coefficient),
## then goes X5 (2.59e-06), after x2 (0.003005), then x4(0.114060)
## and the worst is x3 (0.291004).
## These coefficients are important by 86%.

    model0_summary

    # Call:
    #   lm(formula = Y ~ ., data = baseball)
    # 
    # Residuals:
    #   Min       1Q   Median       3Q      Max 
    # -0.03970 -0.01143 -0.00101  0.01044  0.03444 
    # 
    # Coefficients:
    #   Estimate Std. Error t value Pr(>|t|)    
    # (Intercept)  0.18316    0.01714  10.685 3.79e-13 ***
    #   X1           0.44668    0.10963   4.074 0.000219 ***
    #   X2           0.99090    0.31309   3.165 0.003005 ** 
    #   X3           0.62160    0.58070   1.070 0.291004    
    # X4           0.27374    0.16935   1.616 0.114060    
    # X5          -0.28456    0.05177  -5.497 2.59e-06 ***
    #   ---
    #   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
    # 
    # Residual standard error: 0.01748 on 39 degrees of freedom
    # Multiple R-squared:  0.8601,	Adjusted R-squared:  0.8422 
    # F-statistic: 47.96 on 5 and 39 DF,  p-value: 1.261e-15


