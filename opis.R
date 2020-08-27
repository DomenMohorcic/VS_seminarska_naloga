library(tidyverse)
library(psych)
library(lawstat)
library(R.utils)
library(ggplot2)
library(car)
library(lmtest)

# nalozi podatke
setwd('C:/Users/dmoho/Documents/FRI/2_letnik_1_semester/VS/podatki')
stanovanja <- read.csv('stanovanja_vic.csv', sep = ";")

# uredi podatke
data <- stanovanja[complete.cases(stanovanja), ]

# opisi podatke
summary(data)
describe(data)

par(mar=c(2,2,0,0))
hist(data$letoGradnje, main="")
hist(data$povrsina, main="")
hist(data$oddaljenost, main="")
hist(data$skCena, main="")

round(cor(data), 3)
pairs(select(data, 2, 4:6), lower.panel = NULL)

shapiro.test(data$letoGradnje)
symmetry.test(data$letoGradnje)
shapiro.test(data$povrsina)
symmetry.test(data$povrsina)
shapiro.test(data$oddaljenost)
symmetry.test(data$oddaljenost)
shapiro.test(data$skCena)
symmetry.test(data$skCena)

# multipla regresija
model <- lm(skCena~letoGradnje+povrsina+oddaljenost+parkirisce, data = data)
summary(model)

par(mar=c(2,2,2,2))
par(mfrow=c(2,2))
plot(model, which=1:4)

shapiro.test(model$residuals)

# isto
ncvTest(model)
bptest(model, studentize = FALSE)

a <- which(cooks.distance(model) > 4/118)
cooks.distance(model)[a]>=qf(0.5, 4, 115)

b <- which(abs(rstandard(model)) > 2)
cooks.distance(model)[b]>=qf(0.5, 4, 115)

confint(model)

# alternativni modeli

data$letoGradnje_SQ = data$letoGradnje*data$letoGradnje
data$letoGradnje_SQRT = sqrt(data$letoGradnje)
data$letoGradnje_LOG = log(data$letoGradnje)
data$letoGradnje_EXP = exp(data$letoGradnje)
data$povrsina_SQ = data$povrsina*data$povrsina
data$povrsina_SQRT = sqrt(data$povrsina)
data$povrsina_LOG = log(data$povrsina)
data$povrsina_EXP = exp(data$povrsina)
data$oddaljenost_SQ = data$oddaljenost*data$oddaljenost
data$oddaljenost_SQRT = sqrt(data$oddaljenost)
data$oddaljenost_LOG = log(data$oddaljenost)
data$oddaljenost_EXP = exp(data$oddaljenost)

model_1 <- lm(skCena ~ letoGradnje_LOG+letoGradnje_SQRT+letoGradnje_SQ+letoGradnje+
                          povrsina_EXP+povrsina_LOG+povrsina_SQRT+povrsina_SQ+povrsina+
                          oddaljenost_EXP+oddaljenost_LOG+oddaljenost_SQRT+oddaljenost_SQ+oddaljenost+parkirisce, data = data)
summary(model_1)
par(mfrow=c(2,2))
plot(model_1, which=1:4)


data$povrsina_1 = data$povrsina
data$povrsina_2 = data$povrsina_SQ
data$povrsina_3 = data$povrsina_2*data$povrsina
data$povrsina_4 = data$povrsina_3*data$povrsina
data$povrsina_1_ = 1/data$povrsina_1
data$povrsina_2_ = 1/data$povrsina_2
data$povrsina_3_ = 1/data$povrsina_3
data$povrsina_4_ = 1/data$povrsina_4

model_2 <- lm(skCena ~ povrsina_4_+povrsina_3_+povrsina_2_+povrsina_1_+povrsina_1+povrsina_2+povrsina_3+povrsina_4, data = data)
summary(model_2)
par(mfrow=c(2,2))
plot(model_2, which=1:4)


model_3 <- lm(skCena ~ letoGradnje+povrsina_1+povrsina_2+oddaljenost+parkirisce, data = data)
summary(model_3)
par(mfrow=c(2,2))
plot(model_3, which=1:4)

data_small <- data[which(data$skCena < 1500),]
model_4 <- lm(skCena ~ letoGradnje+povrsina+oddaljenost+parkirisce, data = data_small)
summary(model_4)
par(mfrow=c(2,2))
plot(model_4, which=1:4)
shapiro.test(model_4$residuals)
ncvTest(model_4)


anova(fit1, fit2) #primerja dva modela

rstandard(fit1) #vec kot 2

#reg <- regtabela(fit)
#kex(reg)
