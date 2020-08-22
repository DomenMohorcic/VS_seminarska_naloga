library(tidyverse)
library(psych)
library(lawstat)
library(R.utils)
library(ggplot2)

# nalozi podatke
setwd('C:/Users/dmoho/Documents/FRI/2_letnik_1_semester/VS/podatki')
stanovanja <- read.csv('stanovanja_vic.csv', sep = ";")

# uredi podatke
filtered <- select(stanovanja, 1, 4:6, 8:15, 17:19)
data <- filtered[complete.cases(filtered), ]
data <- transform(data, nadstropje = as.numeric(nadstropje))
data_small <- select(data, 1,4,7,11,12,15)

# opisi podatke
summary(data[2:15])
describe(data[2:15])
for(val in colnames(data[2:15])) {
  shap <- shapiro.test(data[,val])
  sym <- symmetry.test(data[,val])
  printf("%s\n", val)
  printf("shapiro.test\n")
  printf("w = %f, p = %e\n", shap$statistic, shap$p.value)
  printf("symmetry.test\n")
  printf("Test statistics = %f, p = %e\n", sym$statistic, sym$p.value)
  printf("%s\n\n", sym$alternative)
}
round(cor(data[2:12]), 3)
pairs(data[2:12], lower.panel = NULL)

for(val in colnames(data_small[2:6])) {
  shap <- shapiro.test(data[,val])
  sym <- symmetry.test(data[,val])
  printf("%s\n", val)
  printf("shapiro.test\n")
  printf("w = %f, p = %e\n", shap$statistic, shap$p.value)
  printf("symmetry.test\n")
  printf("Test statistics = %f, p = %e\n", sym$statistic, sym$p.value)
}

# multipla regresija
fit <- lm(skCena~nadstropje+vsaNadstropja+letoGradnje+stSob+stParkirisc+parkirisce+shramba+opremljenost+zunanjePovrsine+povrsina+oddaljenost, data = data)
fit <- lm(skCena~letoGradnje+opremljenost+povrsina+oddaljenost+parkirisce, data = data)
summary(fit)

fit_small <- lm(skCena~letoGradnje+povrsina+oddaljenost+parkirisce, data = data_small)
summary(fit_small)

#reg <- regtabela(fit)
#kex(reg)

ggplot(data_small, aes(x=letoGradnje)) + geom_histogram(binwidth = 5, fill="#69b3a2")
ggplot(data_small, aes(x=povrsina)) + geom_histogram(binwidth = 5, fill="#69b3a2")
ggplot(data_small, aes(x=oddaljenost)) + geom_histogram(binwidth = 0.15, fill="#69b3a2")
ggplot(data_small, aes(x=skCena)) + geom_histogram(binwidth = 80, fill="#69b3a2")

par(mar=c(2,2,0,0))
hist(data$letoGradnje, main="")
hist(data$povrsina, main="")
hist(data$oddaljenost, main="")
hist(data$skCena, main="")
