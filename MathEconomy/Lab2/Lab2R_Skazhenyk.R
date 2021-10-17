baseball <- read.delim(file = "C:\\Users\\Tiana_\\Documents\\MathEconomy\\Lab2\\bsbl.txt", header = TRUE, sep = "\t", dec = ",")
summary(baseball)

batting_average <- baseball$X1
runs_scored_atbat <- baseball$X2
doubles_atBat <- baseball$X3
triples_atbat <-baseball$X4
homeruns_atbat<-baseball$X5
strikeouts_atbat <- baseball$X6

##Model 1 ~ runs_scored_atbat
model1 <- lm(batting_average ~ runs_scored_atbat, data = baseball)
summary(model1)

plot(runs_scored_atbat, batting_average)
abline(model1,col="RED")

##Model 2 ~ doubles_atBat
model2 <- lm(batting_average ~ doubles_atBat, data = baseball)
summary(model2)

plot(doubles_atBat, batting_average)
abline(model2,col="RED")

##Model 3 ~ triples_atbat
model3 <- lm(batting_average ~ triples_atbat, data = baseball)
summary(model3)

plot(triples_atbat, batting_average)
abline(model3,col="RED")

##Model 4 ~ homeruns_atbat
model4 <- lm(batting_average ~ homeruns_atbat, data = baseball)
summary(model4)

plot(homeruns_atbat, batting_average)
abline(model4,col="RED")

##Model 5 ~ strikeouts_atbat
model5 <- lm(batting_average ~ strikeouts_atbat, data = baseball)
summary(model5)

plot(strikeouts_atbat, batting_average)
abline(model5,col="RED")


##5-factor Model
baseball_model <- lm(X1 ~ X2 + X3 + X4 + X5+X6,
                 data = baseball)
summary(baseball_model)

##MATRIX
X <- cbind(1, runs_scored_atbat, doubles_atBat, triples_atbat,
           homeruns_atbat, strikeouts_atbat)
Y <- batting_average

beta <- solve(t(X) %*% X) %*% t(X) %*% Y
beta
