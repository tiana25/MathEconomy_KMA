
baseball <- read.delim(file = "C:\\Users\\Tiana_\\Documents\\MathEconomy\\Lab3\\bsbl.txt", header = TRUE, sep = "\t", dec = ",")

# FILE -> REOPEN WITH ENCODING -> UTF-8
# For ukrainian

#---------------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------------------

## Завдання (А)
## Будуємо лінійні моделі від кожного фактору та визначаємо
##                    найменш лінійну змінну

## однофакторна лінійна модель для X1
md1 <- lm(Y~X1, data=baseball)
r_sq1 <- summary(md1)$r.squared
r_sq1
#R^2
# [1] 0.6805454

SSE_md1 = sum(md1$residuals^2)
SSE_md1
# SSE
# [1] 0.02721472

## однофакторна лінійна модель для X2
md2 <- lm(Y~X2, data=baseball)
r_sq2 <- summary(md2)$r.squared
r_sq2
# R^2
# [1] 0.370906

SSE_md2 = sum(md2$residuals^2)
SSE_md2
# SSE
# [1] 0.05359327

## однофакторна лінійна модель для X3
md3 <- lm(Y~X3, data=baseball)
r_sq3 <- summary(md3)$r.squared
r_sq3
# R^2
# [1] 0.4377319

SSE_md3 = sum(md3$residuals^2)
SSE_md3
# SSE
# [1] 0.04790029

## однофакторна лінійна модель для X4
md4 <- lm(Y~X4, data=baseball)
r_sq4 <- summary(md4)$r.squared
r_sq4
# R^2
# [1] 0.0969923

SSE_md4 = sum(md4$residuals^2)
SSE_md4
# SSE
# [1] 0.07692831

## однофакторна лінійна модель для X5
md5 <- lm(Y~X5, data=baseball)
r_sq5 <- summary(md5)$r.squared
r_sq5

# R^2
# [1] 0.4255461

SSE_md5 = sum(md5$residuals^2)
SSE_md5
# SSE
# [1] 0.04893841

## Будуємо табличку для порівняння R^2 моделей вище
tbl <- matrix(c(r_sq1,r_sq2,r_sq3,r_sq4,r_sq5),ncol=5,byrow=TRUE)
colnames(tbl) <- c("X1","X2","X3","X4","X5")
rownames(tbl) <- c("R^2")
tbl <- as.table(tbl)
tbl
## Виходить ось така таблиця
#          X1        X2        X3        X4        X5
# R^2 0.6805454 0.3709060 0.4377319 0.0969923 0.4255461

# Тут можна побачити, що найменший R^2 у моделі для X4 змінної
# R^2 = 0.0969923

#Побудуємо графіки для моделей вище
#Графіки розміщуються у вигляді матриці
attach(mtcars)
par(mfrow=c(3,2))
plot(baseball$X1, baseball$Y,xlab="X1",ylab="Y")
abline(md1)
plot(baseball$X2, baseball$Y,xlab="X2",ylab="Y")
abline(md2)
plot(baseball$X3, baseball$Y,xlab="X3",ylab="Y")
abline(md3)
plot(baseball$X4, baseball$Y,xlab="X4",ylab="Y")
abline(md4)
plot(baseball$X5, baseball$Y,xlab="X5",ylab="Y")
abline(md5)

#  Порівнюючи SSE, R^2, приходимо до висновку, що 
#  найменш лінійний зв'язок з Y має змінна - X4,
#  Її однофакторна модель має найменший R^2 (0.0969923),
#  найбільший SSE (0.07692831)
#  Однофакторна модель для X4 найбільш нелінійно поводить себе на графіку.

##Після того, як знайшли найбільш нелінійну змінну - X4, замінюємо її.

# ---------------------------------------------------------------------------------------------------------------

# 1) Замінюємо x4 на x4^2
# Будуємо нелінійну модель

md_nonlin1 <- lm(Y~X1+X2+X3+I(X4^2)+X5, data = baseball)
summary(md_nonlin1)
# Multiple R-squared:  0.8632,	Adjusted R-squared:  0.8457 
# F-statistic: 49.23 on 5 and 39 DF,  p-value: 8.189e-16

SSE_nonlin1 = sum(md_nonlin1$residuals^2)
SSE_nonlin1
#SSE
# [1] 0.01165215

#----------------------------------------------------------------------------------------------------------------

# 2) Замінюємо x4 на ln(x4)
# Будуємо нелінійну модель

md_nonlin2 <- lm(Y~X1+X2+X3+log(X4)+X5, data = baseball)
summary(md_nonlin2)
# Multiple R-squared:  0.8635,	Adjusted R-squared:  0.846 
# F-statistic: 49.34 on 5 and 39 DF,  p-value: 7.893e-16

SSE_nonlin2 = sum(md_nonlin2$residuals^2)
SSE_nonlin2
#SSE
# [1] 0.0116299

#----------------------------------------------------------------------------------------------------------------

# 3) Замінюємо x4 на x4^3-ln(|x4|)+2^(x4)
# Будуємо нелінійну модель

md_nonlin3 <- lm(Y~X1+X2+X3+I(X4^3-log(abs(X4)+2^(X4)))+X5, data = baseball)
summary(md_nonlin3)
# Multiple R-squared:  0.8646,	Adjusted R-squared:  0.8473 
# F-statistic: 49.82 on 5 and 39 DF,  p-value: 6.703e-16

SSE_nonlin3 = sum(md_nonlin3$residuals^2)
SSE_nonlin3
#SSE
# [1] 0.01153174

#----------------------------------------------------------------------------------------------------------------

# 4) Побудуємо ще звичайну лінійну модель для порівняння
md_lin <- lm(Y~.,data = baseball)
summary(md_lin)
# Multiple R-squared:  0.8646,	Adjusted R-squared:  0.8472 
# F-statistic:  49.8 on 5 and 39 DF,  p-value: 6.75e-16

SSE_lin = sum(md_lin$residuals^2)
SSE_lin
#SSE
# [1] 0.01153587

#----------------------------------------------------------------------------------------------------------------

# Побудуємо графіки
plot(md_nonlin1, 1) # Нелінійний з x4^2
plot(md_nonlin2, 2) # Нелінійний з ln(x4)
plot(md_nonlin3, 3) # Нелінійний з x4^3-ln(|x4|)+2^(x4)

#----------------------------------------------------------------------------------------------------------------

# Порівняємо SSE, R^2

# Найменший SSE (краща модель) :  md_nonlin3
#                                 SSE = 0.01153174
# Найбільший SSE (гірша модель):  md_nonlin1
#                                 SSE = 0.01165215

# Найбільший R^2 (краща модель):  md_nonlin3, md_lin
#                                 R^2 = 0.8646
# Найменший R^2 (гірша модель):   md_nonlin1
#                                 R^2 = 0.8632

# The best model in (A): md_nonlin3

#----------------------------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------------------

# Завдання(B)
# Побудуйте нелінійну за деякими факторами, що найбільше потребують, модель для
# вашого кейсу використовуючи полiномiальні перетворення, а саме полiноми
# Лежандра.

#----------------------------------------------------------------------------------------------------------------

# degree = 1 poly
mod_legendre0 <- lm(Y ~ X1+X2+X3+poly(X4, degree = 1)+X5, data = baseball)
summary(mod_legendre0)
# Multiple R-squared:  0.8646,	Adjusted R-squared:  0.8472 
# F-statistic:  49.8 on 5 and 39 DF,  p-value: 6.75e-16

SSE_lg1 = sum(mod_legendre0$residuals^2)
SSE_lg1
# SSE
#[1] 0.01153587

#----------------------------------------------------------------------------------------------------------------

# quadratic poly
mod_legendre1 <- lm(Y ~ X1+X2+X3+poly(X4, degree = 2)+X5, data = baseball)
summary(mod_legendre1)
# Multiple R-squared:  0.865,	Adjusted R-squared:  0.8437 
# F-statistic: 40.58 on 6 and 38 DF,  p-value: 4.787e-15

SSE_lg2 = sum(mod_legendre1$residuals^2)
SSE_lg2
# SSE
#[1] 0.01150177

#----------------------------------------------------------------------------------------------------------------

# cubic
mod_legendre2 <- lm(Y ~ X1+X2+X3+poly(X4, degree = 3)+X5, data = baseball)
summary(mod_legendre2)
# Multiple R-squared:  0.8651,	Adjusted R-squared:  0.8395 
# F-statistic: 33.88 on 7 and 37 DF,  p-value: 3.186e-14

SSE_lg3 = sum(mod_legendre2$residuals^2)
SSE_lg3
# SSE
#[1] 0.01149632

#----------------------------------------------------------------------------------------------------------------

#degree = 4 poly
mod_legendre3 <- lm(Y ~ X1+X2+X3+poly(X4, degree = 4)+X5, data = baseball)
summary(mod_legendre3)
# Multiple R-squared:  0.8657,	Adjusted R-squared:  0.8358 
# F-statistic:    29 on 8 and 36 DF,  p-value: 1.789e-13

SSE_lg4 = sum(mod_legendre3$residuals^2)
SSE_lg4
# SSE
#[1] 0.01144227

#----------------------------------------------------------------------------------------------------------------

#degree = 5 poly
mod_legendre4 <- lm(Y ~ X1+X2+X3+poly(X4, degree = 5)+X5, data = baseball)
summary(mod_legendre4)
# Multiple R-squared:  0.8678,	Adjusted R-squared:  0.8338 
# F-statistic: 25.53 on 9 and 35 DF,  p-value: 7.653e-13

SSE_lg5 = sum(mod_legendre4$residuals^2)
SSE_lg5
# SSE
#[1] 0.01125956

#----------------------------------------------------------------------------------------------------------------

#degree = 10 poly
mod_legendre5 <- lm(Y ~ X1+X2+X3+poly(X4, degree = 10)+X5, data = baseball)
summary(mod_legendre5)
# Multiple R-squared:  0.8797,	Adjusted R-squared:  0.8236 
# F-statistic: 15.67 on 14 and 30 DF,  p-value: 4.232e-10

SSE_lg6 = sum(mod_legendre5$residuals^2)
SSE_lg6
# SSE
#[1] 0.01024754

#----------------------------------------------------------------------------------------------------------------
# Порівняємо SSE, R^2

# Найменший SSE (краща модель) : degree = 10 poly
#                                SSE = 0.01024754
# Найбільший SSE (гірша модель): degree = 1 poly
#                                SSE = 0.01153587

# Найбільший R^2 (краща модель):  degree = 10 poly
#                                 R^2 = 0.8797
# Найменший R^2 (гірша модель):   degree = 1 poly
#                                 R^2 = 0.8646

# The best model in (B): degree = 10 poly

#----------------------------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------------------

# Завдання (C)
# Побудуйте максимальну нелінійну модель для вашого кейсу використовуючи модель
# взаємодії між факторами

max_model <- lm(Y ~ X1*X2*X3*X4*X5, data = baseball)
summary(max_model)

# Call:
#   lm(formula = Y ~ X1 * X2 * X3 * X4 * X5, data = baseball)
# 
# Residuals:
#   Min         1Q     Median         3Q        Max 
# -0.0236638 -0.0031495 -0.0005465  0.0027382  0.0202967 
# 
# Coefficients:
#                  Estimate  Std. Error t value Pr(>|t|)  
# (Intercept)     2.584e+00  1.632e+00   1.584   0.1373  
# X1             -8.594e+00  9.439e+00  -0.910   0.3791  
# X2             -5.227e+01  3.494e+01  -1.496   0.1585  
# X3             -2.859e+02  1.388e+02  -2.060   0.0600 .
# X4             -2.217e+02  1.240e+02  -1.787   0.0972 .
# X5             -2.118e+01  1.358e+01  -1.560   0.1428  
# X1:X2           2.080e+02  1.951e+02   1.066   0.3057  
# X1:X3           1.359e+03  7.679e+02   1.770   0.1002  
# X2:X3           6.207e+03  2.971e+03   2.089   0.0569 .
# X1:X4           1.090e+03  6.930e+02   1.573   0.1397  
# X2:X4           4.930e+03  2.704e+03   1.823   0.0913 .
# X3:X4           2.067e+04  1.015e+04   2.035   0.0627 .
# X1:X5           8.155e+01  7.927e+01   1.029   0.3224  
# X2:X5           4.432e+02  2.896e+02   1.530   0.1499  
# X3:X5           2.595e+03  1.304e+03   1.990   0.0680 .
# X4:X5           2.023e+03  1.091e+03   1.854   0.0865 .
# X1:X2:X3       -2.896e+04  1.580e+04  -1.833   0.0899 .
# X1:X2:X4       -2.435e+04  1.488e+04  -1.636   0.1258  
# X1:X3:X4       -1.032e+05  5.350e+04  -1.929   0.0759 .
# X2:X3:X4       -4.579e+05  2.215e+05  -2.067   0.0592 .
# X1:X2:X5       -1.641e+03  1.634e+03  -1.004   0.3335  
# X1:X3:X5       -1.215e+04  7.355e+03  -1.651   0.1226  
# X2:X3:X5       -5.425e+04  2.738e+04  -1.981   0.0691 .
# X1:X4:X5       -1.031e+04  6.274e+03  -1.644   0.1241  
# X2:X4:X5       -4.400e+04  2.373e+04  -1.854   0.0866 .
# X3:X4:X5       -2.097e+05  9.946e+04  -2.108   0.0550 .
# X1:X2:X3:X4     2.273e+06  1.144e+06   1.988   0.0683 .
# X1:X2:X3:X5     2.367e+05  1.468e+05   1.612   0.1309  
# X1:X2:X4:X5     2.200e+05  1.347e+05   1.633   0.1263  
# X1:X3:X4:X5     1.074e+06  5.393e+05   1.992   0.0678 .
# X2:X3:X4:X5     4.533e+06  2.132e+06   2.126   0.0532 .
# X1:X2:X3:X4:X5 -2.254e+07  1.126e+07  -2.002   0.0666 .
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.01239 on 13 degrees of freedom
# Multiple R-squared:  0.9766,	Adjusted R-squared:  0.9207 
# F-statistic: 17.47 on 31 and 13 DF,  p-value: 1.538e-06

# The most important coefficients with the smallest Pr(>|t|) in this model:
# X2:X3:X4:X5  Pr(>|t|) 0.0532
# X3:X4:X5     Pr(>|t|) 0.0550
# X2:X3        Pr(>|t|) 0.0569

SSE_maxmod = sum(max_model$residuals^2)
SSE_maxmod
# SSE
#0.001996783

#----------------------------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------------------

# Завдання (D)
# Застосувати до моделей (А), (В) та (С) алгоритм пошуку адекватної моделі. Адекватну
# модель обираємо за інформаційним критерієм Байєса.
BIC(mod_legendre2, mod_legendre3, mod_legendre4, mod_legendre5)

#----------------------------------------------------------------------------------------------------------------
# FOR TASK (A)
#----------------------------------------------------------------------------------------------------------------

# Застосовуємо алгоритм пошуку в лінійній моделі

modBIC0 <- MASS::stepAIC(md_lin, k = log(nrow(baseball)))
# Короткий зміст найкращої моделі з точки зору BIC
summary(modBIC0)
# Multiple R-squared:  0.8602,	Adjusted R-squared:  0.8499 
# F-statistic: 84.06 on 3 and 41 DF,  p-value: < 2.2e-16

# Алгоритм залишив тільки змінні X1,X2,X5

# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  0.17907    0.01533  11.685 1.26e-14 ***
#   X1           0.60688    0.07408   8.192 3.62e-10 ***
#   X2           0.86749    0.28871   3.005  0.00452 ** 
#   X5          -0.28284    0.04510  -6.272 1.78e-07 ***

SSE_bic0 = sum(modBIC0$residuals^2)
SSE_bic0
# SSE
#[1] 0.01191372

#----------------------------------------------------------------------------------------------------------------

# Застосовуємо алгоритм пошуку в моделі,
# де X4 заміняли на x4^2

modBIC1 <- MASS::stepAIC(md_nonlin1, k = log(nrow(baseball)))
# Короткий зміст найкращої моделі з точки зору BIC
summary(modBIC1)
# Multiple R-squared:  0.8602,	Adjusted R-squared:  0.8499 
# F-statistic: 84.06 on 3 and 41 DF,  p-value: < 2.2e-16

# Алгоритм знову залишив тільки змінні X1,X2,X5

# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  0.17907    0.01533  11.685 1.26e-14 ***
#   X1           0.60688    0.07408   8.192 3.62e-10 ***
#   X2           0.86749    0.28871   3.005  0.00452 ** 
#   X5          -0.28284    0.04510  -6.272 1.78e-07 ***

SSE_bic1 = sum(modBIC1$residuals^2)
SSE_bic1
# SSE
#[1] 0.01191372

#----------------------------------------------------------------------------------------------------------------

# Застосовуємо алгоритм пошуку в моделі,
# де X4 заміняли на log(X4)

modBIC2 <- MASS::stepAIC(md_nonlin2, k = log(nrow(baseball)))
# Короткий зміст найкращої моделі з точки зору BIC
summary(modBIC2)
# Multiple R-squared:  0.8602,	Adjusted R-squared:  0.8499 
# F-statistic: 84.06 on 3 and 41 DF,  p-value: < 2.2e-16

# Алгоритм знову залишив тільки змінні X1,X2,X5

# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  0.17907    0.01533  11.685 1.26e-14 ***
#   X1           0.60688    0.07408   8.192 3.62e-10 ***
#   X2           0.86749    0.28871   3.005  0.00452 ** 
#   X5          -0.28284    0.04510  -6.272 1.78e-07 ***

SSE_bic2 = sum(modBIC2$residuals^2)
SSE_bic2
# SSE
#[1] 0.01191372

#----------------------------------------------------------------------------------------------------------------

# Застосовуємо алгоритм пошуку в моделі,
# де X4 заміняли на x4^3-ln(|x4|)+2^(x4)

modBIC3 <- MASS::stepAIC(md_nonlin3, k = log(nrow(baseball)))
# Короткий зміст найкращої моделі з точки зору BIC
summary(modBIC3)

# Multiple R-squared:  0.8602,	Adjusted R-squared:  0.8499 
# F-statistic: 84.06 on 3 and 41 DF,  p-value: < 2.2e-16

# Алгоритм і тут залишив тільки змінні X1,X2,X5

# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  0.17907    0.01533  11.685 1.26e-14 ***
#   X1           0.60688    0.07408   8.192 3.62e-10 ***
#   X2           0.86749    0.28871   3.005  0.00452 ** 
#   X5          -0.28284    0.04510  -6.272 1.78e-07 ***

SSE_bic3 = sum(modBIC3$residuals^2)
SSE_bic3
# SSE
#[1] 0.01191372

#----------------------------------------------------------------------------------------------------------------

# the aim is to minimize BIC, so if you are in a negative territory, 
# a negative number that has the largest modulus 
# (deepest down in the negative territory) indicates the preferred model.

BIC(md_lin,md_nonlin1,md_nonlin2, md_nonlin3)
# df       BIC
# md_lin      7 -217.7519   - 2nd place
# md_nonlin1  7 -217.3006   - 4th place
# md_nonlin2  7 -217.3866   - 3d place
# md_nonlin3  7 -217.7681   - 1st place

# So md_nonlin3 with degree = 1 is the winner. 
# This is the preferred model in TASK (A)

#----------------------------------------------------------------------------------------------------------------


#----------------------------------------------------------------------------------------------------------------
# FOR TASK (B)
#----------------------------------------------------------------------------------------------------------------

#degree = 1 poly

modBIC4 <- MASS::stepAIC(mod_legendre0, k = log(nrow(baseball)))
# Короткий зміст найкращої моделі з точки зору BIC
summary(modBIC4)

# Multiple R-squared:  0.8602,	Adjusted R-squared:  0.8499 
# F-statistic: 84.06 on 3 and 41 DF,  p-value: < 2.2e-16

# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  0.17907    0.01533  11.685 1.26e-14 ***
#   X1           0.60688    0.07408   8.192 3.62e-10 ***
#   X2           0.86749    0.28871   3.005  0.00452 ** 
#   X5          -0.28284    0.04510  -6.272 1.78e-07 ***

SSE_bic4 = sum(modBIC4$residuals^2)
SSE_bic4
# SSE
#[1] 0.01191372

#----------------------------------------------------------------------------------------------------------------

modBIC5 <- MASS::stepAIC(mod_legendre1, k = log(nrow(baseball)))
# Короткий зміст найкращої моделі з точки зору BIC
summary(modBIC5)

SSE_bic5 <- 0.01191372
#The same model as above
#SSE = 0.01191372

#----------------------------------------------------------------------------------------------------------------

modBIC6 <- MASS::stepAIC(mod_legendre2, k = log(nrow(baseball)))
# Короткий зміст найкращої моделі з точки зору BIC
summary(modBIC6)

#The same model as above
#SSE = 0.01191372

#----------------------------------------------------------------------------------------------------------------

modBIC7 <- MASS::stepAIC(mod_legendre3, k = log(nrow(baseball)))
# Короткий зміст найкращої моделі з точки зору BIC
summary(modBIC7)

#The same model as above
#SSE = 0.01191372

#----------------------------------------------------------------------------------------------------------------

modBIC8 <- MASS::stepAIC(mod_legendre4, k = log(nrow(baseball)))
# Короткий зміст найкращої моделі з точки зору BIC
summary(modBIC8)

#The same model as above
#SSE = 0.01191372

#----------------------------------------------------------------------------------------------------------------

modBIC9 <- MASS::stepAIC(mod_legendre5, k = log(nrow(baseball)))
# Короткий зміст найкращої моделі з точки зору BIC
summary(modBIC9)

#The same model as above
#SSE = 0.01191372

#----------------------------------------------------------------------------------------------------------------

# the aim is to minimize BIC, so if you are in a negative territory, 
# a negative number that has the largest modulus 
# (deepest down in the negative territory) indicates the preferred model.

BIC(mod_legendre1,mod_legendre2, mod_legendre3, mod_legendre4, mod_legendre5)
#               df    BIC
# mod_legendre1  8 -214.0785     - 1st place
# mod_legendre2  9 -210.2931     - 2nd place
# mod_legendre3 10 -206.6986     - 3d place
# mod_legendre4 11 -203.6163     - 4th place
# mod_legendre5 16 -188.8210     - 5th place

# So poly with degree = 1 is the winner. 
# This is the preferred model in TASK (B)

#----------------------------------------------------------------------------------------------------------------

# Застосовуємо алгоритм пошуку в
# максимальній моделі(модель взаємодії між факторами)

modBIC10 <- MASS::stepAIC(max_model, k = log(nrow(baseball)))
# Короткий зміст найкращої моделі з точки зору BIC
summary(modBIC10)

# Call:
#   lm(formula = Y ~ X1 * X2 * X3 * X4 * X5, data = baseball)
# 
# Residuals:
#   Min         1Q     Median         3Q        Max 
# -0.0236638 -0.0031495 -0.0005465  0.0027382  0.0202967 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)  
# (Intercept)     2.584e+00  1.632e+00   1.584   0.1373  
# X1             -8.594e+00  9.439e+00  -0.910   0.3791  
# X2             -5.227e+01  3.494e+01  -1.496   0.1585  
# X3             -2.859e+02  1.388e+02  -2.060   0.0600 .
# X4             -2.217e+02  1.240e+02  -1.787   0.0972 .
# X5             -2.118e+01  1.358e+01  -1.560   0.1428  
# X1:X2           2.080e+02  1.951e+02   1.066   0.3057  
# X1:X3           1.359e+03  7.679e+02   1.770   0.1002  
# X2:X3           6.207e+03  2.971e+03   2.089   0.0569 .
# X1:X4           1.090e+03  6.930e+02   1.573   0.1397  
# X2:X4           4.930e+03  2.704e+03   1.823   0.0913 .
# X3:X4           2.067e+04  1.015e+04   2.035   0.0627 .
# X1:X5           8.155e+01  7.927e+01   1.029   0.3224  
# X2:X5           4.432e+02  2.896e+02   1.530   0.1499  
# X3:X5           2.595e+03  1.304e+03   1.990   0.0680 .
# X4:X5           2.023e+03  1.091e+03   1.854   0.0865 .
# X1:X2:X3       -2.896e+04  1.580e+04  -1.833   0.0899 .
# X1:X2:X4       -2.435e+04  1.488e+04  -1.636   0.1258  
# X1:X3:X4       -1.032e+05  5.350e+04  -1.929   0.0759 .
# X2:X3:X4       -4.579e+05  2.215e+05  -2.067   0.0592 .
# X1:X2:X5       -1.641e+03  1.634e+03  -1.004   0.3335  
# X1:X3:X5       -1.215e+04  7.355e+03  -1.651   0.1226  
# X2:X3:X5       -5.425e+04  2.738e+04  -1.981   0.0691 .
# X1:X4:X5       -1.031e+04  6.274e+03  -1.644   0.1241  
# X2:X4:X5       -4.400e+04  2.373e+04  -1.854   0.0866 .
# X3:X4:X5       -2.097e+05  9.946e+04  -2.108   0.0550 .
# X1:X2:X3:X4     2.273e+06  1.144e+06   1.988   0.0683 .
# X1:X2:X3:X5     2.367e+05  1.468e+05   1.612   0.1309  
# X1:X2:X4:X5     2.200e+05  1.347e+05   1.633   0.1263  
# X1:X3:X4:X5     1.074e+06  5.393e+05   1.992   0.0678 .
# X2:X3:X4:X5     4.533e+06  2.132e+06   2.126   0.0532 .
# X1:X2:X3:X4:X5 -2.254e+07  1.126e+07  -2.002   0.0666 .
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.01239 on 13 degrees of freedom
# Multiple R-squared:  0.9766,	Adjusted R-squared:  0.9207 
# F-statistic: 17.47 on 31 and 13 DF,  p-value: 1.538e-06

# Residual standard error: 0.01239 on 13 degrees of freedom
# Multiple R-squared:  0.9766,	Adjusted R-squared:  0.9207 
# F-statistic: 17.47 on 31 and 13 DF,  p-value: 1.538e-06

# The Pr(>|t|) values are much smaller than in original max model
# almost all  with dots



SSE_bic10 <- sum(modBIC10$residuals^2)
#SSE = 0.001996783 

#----------------------------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------------------

# Завдання (E)
# (E) Обрати із всіх моделей найкращу, зробивши порівняльну характеристику моделей.

# The best models from tasks:
# TASK (A) : modBIC3 (which is model from TASK (A) with x4^3-ln(|x4|)+2^(x4))
# TASK (B) : modBIC5 (which is poly with degree 1 from TASK (B))
# TASK (C) : modBIC10 (which is max model from TASK (C))

#Let's check the best with BIC function

BIC(modBIC3, modBIC5, modBIC10)
# Here we should take modulus from BIC values
# the larger modulus - the better model

#          df       BIC
# modBIC3   5 -223.9149 <- winner
# modBIC5   5 -223.9149 <- winner
# modBIC10 33 -197.7053 


# Let's compare by SSE, F, R^2 :

#BY R^2

tbl1 <- matrix(c(summary(modBIC3)$r.squared,summary(modBIC5)$r.squared,summary(modBIC10)$r.squared),ncol=3,byrow=TRUE)
colnames(tbl1) <- c("modBIC3","modBIC5","modBIC10")
rownames(tbl1) <- c("R^2")
tbl1 <- as.table(tbl1)
tbl1
## Виходить ось така таблиця

#       modBIC3   modBIC5  modBIC10
# R^2 0.8601531 0.8601531 0.9765612

#Тож найбільший R^2 в modBIC10

#BY F

tbl2 <- matrix(c(summary(modBIC3)$fstatistic,summary(modBIC5)$fstatistic,summary(modBIC10)$fstatistic),ncol=3,byrow=TRUE)
rownames(tbl2) <- c("modBIC3","modBIC5","modBIC10")
tbl2 <- as.table(tbl2)
tbl2
## Виходить ось така таблиця

# A        B        C
# modBIC3  84.05927  3.00000 41.00000
# modBIC5  84.05927  3.00000 41.00000
# modBIC10 17.47209 31.00000 13.00000

#Тож найбільший F в modBIC3 та modBIC5

#BY SSE

tbl3 <- matrix(c(SSE_bic3,SSE_bic5,SSE_bic10),ncol=3,byrow=TRUE)
colnames(tbl3) <- c("modBIC3","modBIC5","modBIC10")
tbl3 <- as.table(tbl3)
tbl3

# Виходить ось така таблиця
#   modBIC3     modBIC5    modBIC10
#  0.011913722 0.011913720 0.001996783
# The best by SSE <- modBIC10

#----------------------------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------------------

#Висновок:
# Отже, підсумовуючи увесь аналіз, найкраща модель вийшла modBIC10,
# що відповідає максимальній нелінійній моделі побудованої використовуючи модель
# взаємодії між факторами.
# Вона лідує по всім оцінкам, окрім F-statistic.

#----------------------------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------------------
