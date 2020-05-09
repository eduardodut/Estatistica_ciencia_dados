#library("csv")
#library("Rcpp")

prefsAB <- read.csv("prefsAB.csv")
View(prefsAB)
prefsAB$Subject=factor(prefsAB$Subject)
summary(prefsAB)
plot(prefsAB$Pref)

Prefs=xtabs(~Pref, data = prefsAB)
Prefs
chisq.test(Prefs)
binom.test(Prefs)


prefsABC <- read.csv("prefsABC.csv")
View(prefsABC)
prefsABC$Subject=factor(prefsABC$Subject)
summary(prefsABC)
View(prefsABC)
plot(prefsABC$Pref)
Prefs=xtabs(~Pref, data = prefsABC)
Prefs
chisq.test(Prefs)


## OPEN TABLE GENDER TABLES

prefsABsex <- read.csv("prefsABsex.csv")

View(prefsABsex)
prefsABsex$Subject=factor(prefsABsex$Subject)
View(prefsABsex)
summary(prefsABsex)
plot(prefsABsex[prefsABsex$Sex == "M",]$Pref)
plot(prefsABsex[prefsABsex$Sex == "F",]$Pref)

Prefs=xtabs(~Pref+Sex, data = prefsABsex)
Prefs
View (Prefs)
chisq.test(Prefs)

prefsABCsex <- read.csv("prefsABCsex.csv")
View(prefsABCsex)
prefsABCsex$Subject=factor(prefsABCsex$Subject)
View(prefsABCsex)
summary(prefsABCsex)
plot(prefsABCsex[prefsABCsex$Sex == "M",]$Pref)
plot(prefsABCsex[prefsABCsex$Sex == "F",]$Pref)

pgviews = read.csv("pgviews.csv")
View(pgviews)
pgviews$Subject = factor(pgviews$Subject) # convert to nominal factor
summary(pgviews)

# descriptive statistics by Site
library(plyr)
ddply(pgviews, ~ Site, function(data) summary(data$Pages))
ddply(pgviews, ~ Site, summarise, Pages.mean=mean(Pages), Pages.sd=sd(Pages))

# graph histograms
hist(pgviews[pgviews$Site == "A",]$Pages)
hist(pgviews[pgviews$Site == "B",]$Pages)

#boxplot
#Variável Dependente = Variável calculada (resultado): Fica na Eixo Y (Páginas)
#Variável Independente = Variável que você não tem controle : Fica na Eixo X (site)
#Y ~ X
boxplot(Pages ~ Site, data=pgviews)

# view descriptive statistics tempopagina120.csv
Tempo-pag <- read.csv("tempopagina120.csv")
View(tempopagina120)

tempopagina120$Subject=factor(tempopagina120$Subject)
summary(tempopagina120$Site == "A")
summary(tempopagina120$Site == "B")
summary(tempopagina120)

# view descriptive statistics PAGES by SITE

mean(tempopagina120[tempopagina120$Site == "A",]$Pages)
var(tempopagina120[tempopagina120$Site == "A",]$Pages)
sd(tempopagina120[tempopagina120$Site == "A",]$Pages)
cv<-function(x){coef<-sd(x)/mean(x)*100
return(coef)}
cv(tempopagina120[tempopagina120$Site == "A",]$Pages)
hist(tempopagina120[tempopagina120$Site == "A",]$Pages)

mean(tempopagina120[tempopagina120$Site == "B",]$Pages)
var(tempopagina120[tempopagina120$Site == "B",]$Pages)
sd(tempopagina120[tempopagina120$Site == "B",]$Pages)
cv(tempopagina120[tempopagina120$Site == "B",]$Pages)
hist(tempopagina120[tempopagina120$Site == "B",]$Pages)

boxplot(Pages ~ Site, data=tempopagina120)

#H0= distribuição é normal, p >0.05
#Ha = distribuição não é normal, p <0.05

shapiro.test(tempopagina120[tempopagina120$Site == "A",]$Pages)
shapiro.test(tempopagina120[tempopagina120$Site == "B",]$Pages)

# view descriptive statistics TEMPO by SITE
library(plyr)
ddply(tempopagina120, ~ Site, function(data) summary(data$time))
ddply(tempopagina120, ~ Site, summarise, Time.mean=mean(time), Time.sd=sd(time))

boxplot(time ~ Site, data=tempopagina120)

#H0= distribuição é normal, p >0.05
#Ha = distribuição não é normal, p <0.05
shapiro.test(tempopagina120[tempopagina120$Site == "A",]$time) 
#W = 0.95044, p-value = 0.0222, distribuição não é normal, p <0.05
#W = 0.95486, p-value = 0.03523, distribuição continua não sendo normal, p <0.05
shapiro.test(tempopagina120[tempopagina120$Site == "B",]$time)
#W = 0.97319, p-value = 0.177, distribuição é normal,
#W = 0.97782, p-value = 0.3027, distribuição é normal,


## 5 de maio 2020
## Análise da Distribuição da Amostra
## transformação em lognormal

View(meet_file)
summary(meet_file)

## Estatistica descritiva
library(plyr)
ddply(meet_file, Tempo ~ Meet, function(data) summary(data$Tempo))
ddply(meet_file, ~ Meet, function(data) summary(data$Tempo))

ddply(meet_file, ~ Meet, summarise, Time.mean=mean(Tempo), Time.sd=sd(Tempo))

## histograma e boxplot
hist(meet_file[meet_file$Meet == "Zoom",]$Tempo)
hist(meet_file[meet_file$Meet == "Hangout",]$Tempo)
boxplot(Tempo ~ Meet, data=meet_file)

#H0= distribuição é normal, p >0.05
#Ha = distribuição não é normal, p <0.05

shapiro.test(meet_file[meet_file$Meet == "Zoom",]$Tempo) 
#W = 0.84372, p-value = 0.004191, distribuição não é normal, p <0.05
qqnorm(meet_file[meet_file$Meet == "Zoom",]$Tempo) 
shapiro.test(meet_file[meet_file$Meet == "Hangout",]$Tempo)
#W = 0.87213, p-value = 0.01281, distribuição não é normal, p <0.05
qqnorm(meet_file[meet_file$Meet == "Hangout",]$Tempo); 

# Kolmogorov-Smirnov test for log-normality
# fit the distribution to a lognormal to estimate fit parameters
# then supply those to a K-S test with the lognormal distribution fn (see ?plnorm)
# see ?Distributions for many other named probability distributions

library(MASS)
fit = fitdistr(meet_file[meet_file$Meet == "Zoom",]$Tempo, "lognormal")$estimate
ks.test(meet_file[meet_file$Meet == "Zoom",]$Tempo, "plnorm", meanlog=fit[1], sdlog=fit[2], exact=TRUE)
fit = fitdistr(meet_file[meet_file$Meet == "Hangout",]$Tempo, "lognormal")$estimate
ks.test(meet_file[meet_file$Meet == "Hangout",]$Tempo, "plnorm", meanlog=fit[1], sdlog=fit[2], exact=TRUE)

## Data transformation

# create a new column in defined as log(Time)
meet_file$logTime = log(meet_file$Tempo) # log transform
View(meet_file) # verify

# explore for intuition-building
hist(meet_file[meet_file$Meet == "Zoom",]$logTime)
hist(meet_file[meet_file$Meet == "Hangout",]$logTime)
boxplot(logTime ~ Meet, data=meet_file) # boxplot

shapiro.test(meet_file[meet_file$Meet == "Zoom",]$logTime) 
#W = 0.95825, p-value = 0.5094, distribuição normal, p >0.05
qqnorm(meet_file[meet_file$Meet == "Zoom",]$logTime)
shapiro.test(meet_file[meet_file$Meet == "Hangout",]$logTime)
#W = 0.93905, p-value = 0.23, distribuição normal, p >0.05
qqnorm(meet_file[meet_file$Meet == "Hangout",]$logTime)
qqline(meet_file[meet_file$Meet == "Hangout",]$logTime)
