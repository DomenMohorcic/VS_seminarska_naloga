options(digits=3, scipen=T) #prikaz na manj dec. mest; "penaliziramo" "znanstveni" prikaz vrednosti
setwd("vpišite svojo mapo")
library(car)
library(psych)

# Hitro kopiranje v Excel
kex<-function(X){
write.table(X,"clipboard",sep="\t",dec=",",row.names=T)
}

#Frekvenèna tabela za veè spremenljivk, s kumulativnimi frek. in %
#manjkajoèe vrednosti so izpušèene
frektabela<-function(X){
  X<-as.data.frame(X)
  frektabela<-vector("list",dim(X)[2])
  for (j in 1:dim(X)[2]){
    x<-X[,j]
    ime<-names(X)[j]
    f<-table(x)
    i<-names(f)
    f<-as.vector(f)
    fc<-cumsum(f)
    p<-100*f/sum(f)
    cp<-cumsum(p)
    tabela<-data.frame(i,f,fc,p,cp)
    names(tabela)<-c(ime,"f","cf","%","cum%")
    cat("\n")
    print(tabela)
    cat("\n")
    if(max(fc)<dim(X)[1]){
      cat("Spremenljivka ",ime," ima ",dim(X)[1]-max(fc)," manjkajoèih vrednosti.")} 
    cat("\n")
    frektabela[[j]]<-tabela
  }
  names(frektabela)<-names(X)
  return(frektabela)
}

# Histogram s krivuljo gostote in normalno krivuljo
histkriv <- function(x,ime="Vrednost"){
  hist(x,breaks="FD",freq=F,main=NULL,xlab=ime,ylab="Verj. gostota",ylim=c(0,max(c(max(dnorm(min(x):max(x),mean=mean(x),sd=sd(x)),max(hist(x,breaks="FD",plot=F)$density),max(density(x)$y))))))
  lines(x=min(x):max(x),y=dnorm(min(x):max(x),mean=mean(x),sd=sd(x)),lty=2)
  lines(density(x))  
}


# Q-Q grafikon in zaboj z roèaji Mahalanobisovih razdalj
# Zahteva paket car!
mahalqq <-function(X){
  require(car)
  D2 <- mahalanobis(X, colMeans(X,na.rm=T), var(X,na.rm=T))
  n<-dim(X)[1]
  p<-dim(X)[2]
  qqplot(qchisq(ppoints(n), df = p), D2,xlab="Kvantili hi**2",ylab="Kvadrirana Mahal. razd.")
  abline(0, 1, col = 'gray')
  Boxplot(D2)
}


#V tabeli izpiše kljuène rezultate reg.analize
regtabela<-function(lmr){
  y<-lmr$model[,1]
  X<-lmr$model[,-1]
  p<-dim(X)[2]
  tab <- data.frame(summary(lmr)$coefficients[,1:4],confint(lmr))
  
  zy<-scale(lmr$model)[,1]
  zx<-scale(lmr$model)[,-1]
  beta <- coef(lm(zy~zx))
  
  r2 <- summary(lmr)$r.squared
  kspr<- rep(NA,p+1)
  toler <- rep(NA,p+1)
  for (i in 1:p){
    lmi <- lm(y~as.matrix(X[,-i]))
    kspr[i+1] <- r2 - summary(lmi)$r.squared
    lmi <- lm(X[,i]~as.matrix(X[,-i]))
    toler[i+1] <- 1 - summary(lmi)$r.squared
  }
  tab <- cbind(tab,beta,kspr,c(NA,vif(lmr)),toler)
  names(tab) <- c("b","SE","t","p","IZ95%sm","IZ95%zm","beta","kspr","VIF","Tol.")  
  print(round(tab,digits=3))
  tab<-cbind(row.names(tab),tab)
  names(tab)[1]<-" "
  return(tab)
}

# Ocene regr.koeficientov z zankanjem
# (deloma preplonkano od Foxa & Weisberga, 2011)
regzank <- function(lmr){
betahat.boot <- bootCase(lmr, B=999)
usualEsts <- summary(lmr)$coef[ , 1:2]
bootSD <- apply(betahat.boot, 2, sd) 
bootEst <- colMeans(betahat.boot)
bootBias <- (bootEst - usualEsts[ , 1])/usualEsts[ , 2]
bootCI <- apply(betahat.boot, 2, function(x) quantile(x, c(.025,.975)))
bsrez <- data.frame(usualEsts, bootBias, bootSD, t(bootCI))
names(bsrez) <- c("b","SE","pristr.","SE_bs","95%IZsm","95%IZzm")
print(bsrez, digits=3)
bsrez<-cbind(row.names(bsrez),bsrez)
names(bsrez)[1]<-" "
return(bsrez)
}


#Preizkus prileganja modela logistiène regresije po Hosmerju in Lemeshowu
# x = vektor izidov(1/0), y = vektor napovedanih verjetnosti, g = število intervalov
#Avtor kode: Peter Solymos
hoslem<-function (x, y, g = 10) 
{
  DNAME <- paste(deparse(substitute(x)), deparse(substitute(y)), 
                 sep = ", ")
  METHOD <- "Hosmer and Lemeshow goodness of fit (GOF) test"
  yhat <- y
  y <- x
  qq <- unique(quantile(yhat, probs = seq(0, 1, 1/g)))
  cutyhat <- cut(yhat, breaks = qq, include.lowest = TRUE)
  observed <- xtabs(cbind(y0 = 1 - y, y1 = y) ~ cutyhat)
  expected <- xtabs(cbind(yhat0 = 1 - yhat, yhat1 = yhat) ~ 
                      cutyhat)
  chisq <- sum((observed - expected)^2/expected)
  cat("Prièakovane in dejanske frekvence \n")
  print(cbind(expected[,2],observed[,2]))
  PVAL = 1 - pchisq(chisq, g - 2)
  PARAMETER <- g - 2
  names(chisq) <- "X-squared"
  names(PARAMETER) <- "df"
  structure(list(statistic = chisq, parameter = PARAMETER, 
                 p.value = PVAL, method = METHOD, data.name = DNAME, observed = observed, 
                 expected = expected), class = "htest")
}

#Klasifikacijska tabela za logistièno regresijo (ali drug glm model)
klastabela<-function(model){
  usp0<-100*max(table(model$y))/sum(table(model$y))
  napovedane <- round(model$fitted.values)
  dejanske <- model$y
  cat("Klasifikacijska tabela: \n")
  kt <- table(napovedane, dejanske)
  print(kt)
  usp1 <- 100*sum(diag(kt))/sum(kt)
  cat(" \n")
  cat("Klasifikacijska tabela v %: \n")
  print(prop.table(kt))
  cat(" \n")
  cat("Odstotek uspešnih napovedi s praznim modelom: ", usp0, "\n")
  cat("Odstotek uspešnih napovedi z izbranim modelom: ", usp1, "\n")
  kt
}

