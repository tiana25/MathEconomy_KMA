library(car)

amzn <- read.delim(file = "C:\\Users\\Tiana_\\Documents\\MathEconomy\\Lab4\\amazon.txt", header = TRUE, sep = "\t", dec = ",")
amzn_lin <- lm(Y~., data=amzn)
amzmlin_sum <- summary(amzn_lin)

amzmlin_sum
 
#Model1 y = ab^x
#Знаходимо alpha and beta
md1 = lm(log(y)~x,data=amzn)
alpha1 <- exp(coef(md1)[1])
alpha1
# (Intercept) 
# 31.60431 
beta1 <- exp(coef(md1)[2])
beta1
# x 
# 1.001382
x_ave1 <- mean(amzn$X)
#Знаходимо коеф.еластичності
elcoeff1 <- log(beta1)*x_ave1
elcoeff1
# x 
# 2.995128 
conf_interv1 <- exp(predict(md1,data=amzn,interval="confidence"))
plot(amzn$X,amzn$Y)
curve(alpha1*(beta1^x),col="RED", add=TRUE)
lines(amzn$X, conf_interv1[,2], col="green", lty=2)
lines(amzn$X, conf_interv1[,3], col="blue", lty=2)

y1_pred<- alpha1*beta1^x
e1<-(amzn$Y-y1_pred)^2
sum(e1)
#SSE

#Прогноз
x1_pred <-data.frame(x=max(x)*1.05)
y1_pred <-alpha1*(beta1^x1_pred)
progn1 <-exp(predict(md1,newdata=x1_pred,interval="prediction"))
points(x1_pred,progn1[1])
points(x1_pred,progn1[2])
points(x1_pred,progn1[3])

#Model2 y=ae^(bx)
#Знаходимо alpha and beta
md2 = lm(log(y)~x,data=amzn)
alpha2 <- exp(coef(md2)[1])
alpha2
# (Intercept) 
# 31.60431 
beta2 <- coef(md2)[2]
beta2
# x 
# 0.001381443
x_ave2 <- mean(amzn$X)
#Знаходимо коеф.еластичності
elcoeff2 <- beta2*x_ave
elcoeff2
# x 
# 2.995128 
conf_interv2 <- exp(predict(md2,data=amzn,interval="confidence"))
plot(amzn$X,amzn$Y)
curve(alpha2*exp(1)^(beta2*x),col="RED", add=TRUE)
lines(amzn$X, conf_interv2[,2], col="green", lty=2)
lines(amzn$X, conf_interv2[,3], col="blue", lty=2)

#Прогноз
x2_pred <-data.frame(x=max(x)*1.05)
y2_pred <-alpha2*exp(1)^(beta2*x2_pred)
progn2 <-exp(predict(md2,newdata=x2_pred,interval="prediction"))
points(x2_pred,progn2[1])
points(x2_pred,progn2[2])
points(x2_pred,progn2[3])

y2_pred<- alpha2*exp(1)^(beta2*x)
e2<-(amzn$Y-y2_pred)^2
sum(e2)
#SSE

#Model3 y=e^(b0+b1x)
#Знаходимо alpha and beta
md3 = lm(log(y)~x,data=amzn)
alpha3 <- coef(md3)[1]
alpha3
# (Intercept) 
# 3.453294 
beta3 <- coef(md3)[2]
beta3
# x 
# 0.001381443
x_ave3 <- mean(amzn$X)
#Знаходимо коеф.еластичності
elcoeff3 <- beta3*x_ave3
elcoeff3
# x 
# 2.995128 
conf_interv3 <- exp(predict(md3,data=amzn,interval="confidence"))
plot(amzn$X,amzn$Y)
curve(exp(1)^(alpha3+beta3*x),col="RED", add=TRUE)
lines(amzn$X, conf_interv3[,2], col="green", lty=2)
lines(amzn$X, conf_interv3[,3], col="blue", lty=2)

y3_pred<- exp(1)^(alpha3+beta3*x)
e3<-(amzn$Y-y3_pred)^2
sum(e3)
#SSE

#Прогноз
x3_pred <-data.frame(x=max(x)*1.05)
progn3 <-exp(predict(md3,newdata=x3_pred,interval="prediction"))
points(x3_pred,progn3[1])
points(x3_pred,progn3[2])
points(x3_pred,progn3[3])

#Model4 y=10^(a+bx)
#Знаходимо alpha and beta
md4 = lm(log(y)~x,data=amzn)
alpha4 <- coef(md4)[1]/log(10)
alpha4
# (Intercept) 
# 1.499746 
beta4 <- coef(md4)[2]/log(10)
beta4
# x 
# 0.0005999529 
x_ave4 <- mean(amzn$X)
#Знаходимо коеф.еластичності
elcoeff4 <- beta4*x_ave4*log(10)
elcoeff4
# x 
# 2.995128 
conf_interv4 <- exp(predict(md4,data=amzn,interval="confidence"))
plot(amzn$X,amzn$Y)
curve(10^(alpha4+beta4*x),col="RED", add=TRUE)
lines(amzn$X, conf_interv4[,2], col="green", lty=2)
lines(amzn$X, conf_interv4[,3], col="blue", lty=2)

y4_pred<- 10^(alpha4+beta4*x)
e4<-(amzn$Y-y4_pred)^2
sum(e4)
#SSE

#Прогноз
x4_pred <-data.frame(x=max(x)*1.05)
progn4 <-exp(predict(md4,newdata=x4_pred,interval="prediction"))
points(x4_pred,progn4[1])
points(x4_pred,progn4[2])
points(x4_pred,progn4[3])

#Model5 степенева y=ax^b
#Знаходимо alpha and beta
md5 = lm(log(y)~log(x),data=amzn)
alpha5 <- exp(coef(md5)[1])
alpha5
# (Intercept) 
# 1.871712e-07 
beta5 <- coef(md5)[2]
beta5
# log(x) 
# 2.872338 

#Знаходимо коеф.еластичності
elcoeff5 <- beta5
elcoeff5
# log(x) 
# 2.872338 
conf_interv5 <- exp(predict(md5,data=amzn,interval="confidence"))
plot(amzn$X,amzn$Y)
curve(alpha5*x^(beta5),col="RED", add=TRUE)
lines(amzn$X, conf_interv5[,2], col="green", lty=2)
lines(amzn$X, conf_interv5[,3], col="blue", lty=2)

y5_pred<- alpha5*x^beta5
e5<-(amzn$Y-y5_pred)^2
sum(e5)
#SSE

#Прогноз
x5_pred <-data.frame(x=max(x)*1.05)
progn5 <-exp(predict(md5,newdata=x5_pred,interval="prediction"))
points(x5_pred,progn5[1])
points(x5_pred,progn5[2])
points(x5_pred,progn5[3])

#Model6 зворотна y=alpha+beta*(1/x)
#Знаходимо alpha and beta
x6 <- 1/amzn$X
md6 = lm(y~x6,data=amzn)
alpha6 <- coef(md6)[1]
alpha6
# (Intercept) 
# 3100.062
beta6 <- coef(md6)[2]
beta6
# x6 
# -4307558 
x_ave6 <- mean(amzn$X)
#Знаходимо коеф.еластичності
elcoeff6 <- -beta6/(x_ave6*alpha6+beta6)
elcoeff6
# x6 
# 1.784602 
conf_interv6 <- predict(md6,data=amzn,interval="confidence")
plot(x6,amzn$Y)
curve(alpha6+beta6*x,col="RED", add=TRUE)
lines(x6, conf_interv6[,2], col="green", lty=2)
lines(x6, conf_interv6[,3], col="blue", lty=2)

y6_pred<- alpha6+beta6*(1/x)
e6<-(amzn$Y-y6_pred)^2
sum(e6)
#SSE

#Прогноз
x6_pred <-data.frame(x=max(x)*1.05)
progn6 <-predict(md6,newdata=x6_pred,interval="prediction")
points(x6_pred,progn6[1])
points(x6_pred,progn6[2])
points(x6_pred,progn6[3])

#Model7 квадратична y=b0+b1x+b2x^2
x7 <- (amzn$X)^2
md7 = lm(y~x+x7,data=amzn)
b0 <- coef(md7)[1]
b0
# (Intercept) 
# 1087.25 
b1 <- coef(md7)[2]
b1
# x 
# -1.475448
b2 <- coef(md7)[3]
b2
# x7 
# 0.0005971266 
plot(amzn$X,amzn$Y)
curve(b0+b1*x+b2*x*x,col="RED", add=TRUE)

#Model8 модифікована експонента y=ab^x+g
x<-amzn$X
y<-amzn$Y

t1<-median(x[1:40])
t2<-median(x[41:80])
t3<-median(x[81:120])

y1<-median(y[1:40])
y2<-median(y[41:80])
y3<-median(y[81:120])

delta <-t2-t1
delta
# [1] 672.6324
beta8 <-exp((1/delta)*log(((y3-y2)/(y2-y1)), base=exp(1)))
beta8
# [1] 1.002327
alpha8 <-((y3-y2)/(beta8^t3 - beta8^t2))
alpha8
# [1] 8.807164
gama8 <- y1 - alpha8*(beta8^t1)
gama8
# [1] 126.3523

plot(amzn$X, amzn$Y, xlab="X", ylab="Y", col=4)
curve(alpha8*(beta8^x)+gama8,col="RED", add=TRUE)

y8_pred<-alpha8*(beta8^x)+gama8
e8<-(amzn$Y-y8_pred)^2
sum(e8)
#SSE
# [1] 37285919

#Model9 крива Гомперця y=e^(ab^x+g)
y9<-log(amzn$Y)

t1<-median(x[1:40])
t2<-median(x[41:80])
t3<-median(x[81:120])

y1_9<-median(y9[1:40])
y2_9<-median(y9[41:80])
y3_9<-median(y9[81:120])

delta <- t2-t1
delta
# [1] 672.6324
beta9 <-exp((1/delta)*log(((y3_9-y2_9)/(y2_9-y1_9)), base=exp(1)))
beta9
# [1] 1.000308
alpha9 <-((y3_9-y2_9)/(beta9^t3 - beta9^t2))
alpha9
# [1] 2.195872
gama9 <- y1_9 - alpha9*(beta9^t1)
gama9
# [1] 238.7846

plot(amzn$X, amzn$Y, xlab="X", ylab="Y", col=4)
curve(exp(1)^(alpha9*(beta9^x)+gama9),col="RED", add=TRUE)

y9_pred<-exp(1)^(alpha9*(beta9^x)+gama9)
e9<-(amzn$Y-y9_pred)^2
sum(e9)
#SSE
# [1] 33538120

#Model10 логістична крива y=1/(ab^x+g)

y10 <- 1/(amzn$Y)

t1<-median(x[1:40])
t2<-median(x[41:80])
t3<-median(x[81:120])

y1_10<-median(y10[1:40])
y2_10<-median(y10[41:80])
y3_10<-median(y10[81:120])

delta <- t2-t1
delta
# [1] 672.6324
beta10 <-exp((1/delta)*log(((y3_10-y2_10)/(y2_10-y1_10)), base=exp(1)))
beta10
# [1] 0.9988384
alpha10 <-((y3_10-y2_10)/(beta10^t3 - beta10^t2))
alpha10
# [1] 0.02137931
gama10 <- y1_10 - alpha10*(beta10^t1)
gama10
# [1] -2.906698e-05

plot(amzn$X, amzn$Y, xlab="X", ylab="Y", col=4)
curve(exp(1)^(alpha10*(beta10^x)+gama10),col="RED", add=TRUE)

y10_pred<-1/(alpha10*(beta10^x)+gama10)
e10<-(amzn$Y-y10_pred)^2
sum(e10)
#SSE
# [1] 11570754

#Порівняльна характеристика найкращих моделей більш детально описана
#в excel
#В порівнянні за F-value,R^2,r та SSE найкращою вишла перша модель.
anova(md1)
# Analysis of Variance Table
# 
# Response: log(y)
# Df Sum Sq Mean Sq F value    Pr(>F)    
# x           1  90.05  90.050  2104.2 < 2.2e-16 ***
#   Residuals 118   5.05   0.043                      
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Тут ми можемо побачити, що тест Фішера (F-value) дорівнює
# 2104.2, df = 118, SSR = 90.050

