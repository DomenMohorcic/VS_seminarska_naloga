library(tidyverse)
library(psych)
library(lawstat)
library(R.utils)

# nalozi podatke
setwd('C:/Users/dmoho/Documents/FRI/2_letnik_1_semester/VS/podatki')
stanovanja <- read.csv('stanovanja_vic.csv', sep = ";")

# uredi podatke
filtered <- select(stanovanja, 1, 4:6, 8:14, 16:18)
data <- filtered[complete.cases(filtered), ]
data <- transform(data, nadstropje = as.numeric(nadstropje))

# opisi podatke
summary(data[2:14])
describe(data[2:14])
for(val in colnames(data[2:14])) {
  shap <- shapiro.test(data[,val])
  sym <- symmetry.test(data[,val])
  printf("%s\n", val)
  printf("shapiro.test\n")
  printf("w = %f, p = %e\n", shap$statistic, shap$p.value)
  printf("symmetry.test\n")
  printf("Test statistics = %f, p = %e\n", sym$statistic, sym$p.value)
  printf("%s\n\n", sym$alternative)
}
cor(data[2:14])
pairs(data[2:14], lower.panel = NULL)

# multipla regresija
fit <- lm(mesecno~nadstropje+vsaNadstropja+letoGradnje+stSob+stParkirisc+shramba+opremljenost+zunanjePovrsine+povrsina+oddaljenost, data = data)
summary(fit)
#reg <- regtabela(fit)
#kex(reg)
