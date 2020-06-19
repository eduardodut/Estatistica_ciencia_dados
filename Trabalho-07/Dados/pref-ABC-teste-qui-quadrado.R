#library("csv")
#library("Rcpp")

# read in a data file with 3 response categories
#prefsABC = read.csv("prefsABC.csv")

View(prefsABC)
prefsABC$Subject = factor(prefsABC$Subject) # convert to nominal factor
summary(prefsABC)

# Pearson chi-square test
prfs = xtabs( ~ Pref, data=prefsABC)
prfs # show counts
chisq.test(prfs)

# multinomial test
library(XNomial)
xmulti(prfs, c(1/3, 1/3, 1/3), statName="Prob")

# post hoc binomial tests with correction for multiple comparisons
aa = binom.test(sum(prefsABC$Pref == "A"), nrow(prefsABC), p=1/3)
bb = binom.test(sum(prefsABC$Pref == "B"), nrow(prefsABC), p=1/3)
cc = binom.test(sum(prefsABC$Pref == "C"), nrow(prefsABC), p=1/3)
p.adjust(c(aa$p.value, bb$p.value, cc$p.value), method="holm")


Prefs=xtabs(~Pref, data = prefsABC)
Prefs
chisq.test(Prefs)

# revisit our data file with 3 response categories, but now with sex (M/F)
#prefsABCsex <- read.csv("prefsABCsex.csv")
View(prefsABCsex)
prefsABCsex$Subject=factor(prefsABCsex$Subject)
View(prefsABCsex)
summary(prefsABCsex)

# Pearson chi-square test
prfs = xtabs( ~ Pref + Sex, data=prefsABCsex)
View(prfs)
chisq.test(prfs)

# G-test
G.test(prfs)

# Fisher's exact test
fisher.test(prfs)

# manual post hoc binomial tests for (m)ales -- do any prefs for A-C sig. differ from chance for males?
ma = binom.test(sum(prefsABCsex[prefsABCsex$Sex == "M",]$Pref == "A"), nrow(prefsABCsex[prefsABCsex$Sex == "M",]), p=1/3)
mb = binom.test(sum(prefsABCsex[prefsABCsex$Sex == "M",]$Pref == "B"), nrow(prefsABCsex[prefsABCsex$Sex == "M",]), p=1/3)
mc = binom.test(sum(prefsABCsex[prefsABCsex$Sex == "M",]$Pref == "C"), nrow(prefsABCsex[prefsABCsex$Sex == "M",]), p=1/3)
p.adjust(c(ma$p.value, mb$p.value, mc$p.value), method="holm") # correct for multiple comparisons

# manual post hoc binomial tests for (f)emales -- do any prefs for A-C sig. differ from chance for females?
fa = binom.test(sum(prefsABCsex[prefsABCsex$Sex == "F",]$Pref == "A"), nrow(prefsABCsex[prefsABCsex$Sex == "F",]), p=1/3)
fb = binom.test(sum(prefsABCsex[prefsABCsex$Sex == "F",]$Pref == "B"), nrow(prefsABCsex[prefsABCsex$Sex == "F",]), p=1/3)
fc = binom.test(sum(prefsABCsex[prefsABCsex$Sex == "F",]$Pref == "C"), nrow(prefsABCsex[prefsABCsex$Sex == "F",]), p=1/3)
p.adjust(c(fa$p.value, fb$p.value, fc$p.value), method="holm") # correct for multiple comparisons

prfs = xtabs( ~ Pref + Sex, data=prefsABCsex)
View(prfs)
chisq.test(prfs)

