## 5 de maio 2020
## Análise da Distribuição da Amostra (1 fator e dois níveis)
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
t.test(Tempo ~ Meet, data=meet_file, var.equal=TRUE)

#####rejeita H0 (se p<0.5), então não é distribuição normal#####

shapiro.test(meet_file[meet_file$Meet == "Zoom",]$Tempo) 
#W = 0.84372, p-value = 0.004191, distribuição não é normal, p <0.05
qqnorm(meet_file[meet_file$Meet == "Zoom",]$Tempo) 
shapiro.test(meet_file[meet_file$Meet == "Hangout",]$Tempo)
#W = 0.87213, p-value = 0.01281, distribuição não é normal, p <0.05
qqnorm(meet_file[meet_file$Meet == "Hangout",]$Tempo); 


t.test(Tempo ~ Meet, data=meet_file, var.equal=FALSE)

# Kolmogorov-Smirnov test for log-normality
# fit the distribution to a lognormal to estimate fit parameters
# then supply those to a K-S test with the lognormal distribution fn (see ?plnorm)
# see ?Distributions for many other named probability distributions

#####rejeita H0 (se p<0.5), então não é distribuição lognormal#####
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
#W = 0.95825, p-value = 0.5094, não consigo rejeitar a H0, mas não posso garantir que é uma distribuição normal
qqnorm(meet_file[meet_file$Meet == "Zoom",]$logTime)
shapiro.test(meet_file[meet_file$Meet == "Hangout",]$logTime)
#W = 0.93905, p-value = 0.23, não consigo rejeitar a H0, mas não posso garantir que é uma distribuição normal
qqnorm(meet_file[meet_file$Meet == "Hangout",]$logTime)
qqline(meet_file[meet_file$Meet == "Hangout",]$logTime)

# test homoscedasticity
library(car)
leveneTest(logTime ~ Meet, data=meet_file, center=mean) # Levene's test
# independent-samples t-test (now suitable for logTime)


t.test(logTime ~ Meet, data=meet_file, var.equal=TRUE)

## compare os resultados dos testes t.test!!!

## 7 de maio 2020
## Análise da Distribuição da Amostra com tres níveis
## transformação em lognormal

View(meet3_file)
summary(meet3_file)

## Estatistica descritiva
library(plyr)
ddply(meet3_file, ~ Meet, summarise, Time.mean=mean(Tempo), Time.sd=sd(Tempo))

## histograma e boxplot
hist(meet3_file[meet3_file$Meet == "Zoom",]$Tempo)
hist(meet3_file[meet3_file$Meet == "Hangout",]$Tempo)
hist(meet3_file[meet3_file$Meet == "Skype",]$Tempo)
boxplot(Tempo ~ Meet, data=meet3_file)

shapiro.test(meet3_file[meet3_file$Meet == "Zoom",]$Tempo) 
#W = 0.84372, p-value = 0.004191, p<0,05, rejeita a H0
shapiro.test(meet3_file[meet3_file$Meet == "Hangout",]$Tempo)
#W = 0.87213, p-value = 0.01281,  p<0,05, rejeita a H0, 
shapiro.test(meet3_file[meet3_file$Meet == "Skype",]$Tempo)
#W = 0.88623, p-value = 0.02294, p<0,05, rejeita a H0

# Kolmogorov-Smirnov test for log-normality
# fit the distribution to a lognormal to estimate fit parameters (mean, sd)
# then supply those to a K-S test with the lognormal distribution fn (see ?plnorm)
# see ?Distributions for many other named probability distributions

library(MASS)
fit = fitdistr(meet3_file[meet3_file$Meet == "Zoom",]$Tempo, "lognormal")$estimate
ks.test(meet3_file[meet3_file$Meet == "Zoom",]$Tempo, "plnorm", meanlog=fit[1], sdlog=fit[2], exact=TRUE)
fit = fitdistr(meet3_file[meet3_file$Meet == "Hangout",]$Tempo, "lognormal")$estimate
ks.test(meet3_file[meet3_file$Meet == "Hangout",]$Tempo, "plnorm", meanlog=fit[1], sdlog=fit[2], exact=TRUE)
fit = fitdistr(meet3_file[meet3_file$Meet == "Skype",]$Tempo, "lognormal")$estimate
ks.test(meet3_file[meet3_file$Meet == "Skype",]$Tempo, "plnorm", meanlog=fit[1], sdlog=fit[2], exact=TRUE)

## Data transformation

# create a new column in defined as log(Time)
meet3_file$logTime = log(meet3_file$Tempo) # log transform
View(meet3_file) # verify

# explore for intuition-building
boxplot(logTime ~ Meet, data=meet3_file) # boxplot

shapiro.test(meet3_file[meet3_file$Meet == "Zoom",]$logTime) 
#W = 0.95825, p-value = 0.5094, não pode rejeitar a H0, mas não pode garantir que é uma distribuição normal, p >0.05
qqnorm(meet3_file[meet3_file$Meet == "Zoom",]$logTime)
shapiro.test(meet3_file[meet3_file$Meet == "Hangout",]$logTime)
#W = 0.93905, p-value = 0.23, distribuição normal, p >0.05
shapiro.test(meet3_file[meet3_file$Meet == "Skype",]$logTime) 
#W = 0.96579, p-value = 0.6648, distribuição normal, p >0.05
qqnorm(meet3_file[meet3_file$Meet == "Skype",]$logTime)
qqline(meet3_file[meet3_file$Meet == "Skype",]$logTime)

# test homoscedasticity
library(car)
leveneTest(logTime ~ Meet, data=meet3_file, center=mean) # Levene's test
leveneTest(logTime ~ Meet, data=meet3_file, center=median)

