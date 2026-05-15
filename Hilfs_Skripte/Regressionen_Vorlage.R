###### Skript R-Kurs04 (Regression/Korrelation) #####

# Teil 1 - Voraussetzungen für bivariate Korrelationen/Regression prüfen
# Teil 2 - bivariate Produkt-Moment-Korrelation
# Teil 3 - Spearman-Rang-Korrelation (als Ausweichverfahren)
# Teil 4 - bivariate lineare Regression
# Teil 5 - multiple lineare Regression
# Teil 6 - Voraussetzungen der multiplen Regression überprüfen (im Nachhinein)
# Teil 7 - Effektstärken 


### Teil 1 - Voraussetzungen für bivariate Korrelationen/Regression prüfen
# Datensatz öffnen
KR<-read.csv2("KorrReg.csv")

# Außreißer checken
# summary(KR$Sonne)
# IQR(KR$Sonne)
# 6.75+1.5*4.275
# 2.475-1.5*4.275
boxplot(KR$Sonne)
# summary(KR$Besucher)
# IQR(KR$Besucher)
# 48000+1.5*19775
# 28225-1.5*19775
boxplot(KR$Besucher)

# Normalverteilung checken (bei n<30)
shapiro.test(KR$Sonne)
shapiro.test(KR$Besucher)
# hist(KR$Sonne)
# hist(KR$Besucher)
# hist(KR$Sonne, breaks=4, main="Histogramm Sonne", xlab="Sonnenstunden", ylab="Häufigkeit",col=c('#880022', '#aa0033', '#cc0044'))
# hist(KR$Besucher, breaks=4, main="Histogramm Besucher", xlab="Anzahl der Besucher", ylab="Häufigkeit",col=c('#228822', '#44aa44', '#66cc66'))


### Teil 2 - bivariate Produkt-Moment-Korrelation

cor(KR$Sonne, KR$Besucher)
cor.test(KR$Sonne, KR$Besucher)

# GrafiK:
plot(KR$Sonne, KR$Besucher)


### Teil 3 - Spearman-Rang-Korrelation (als Ausweichverfahren)

# Signifikanztest ist problematisch, da keine Normalverteilung und n<30!
# Deswegen auf Spearman ausweichen
cor.test(KR$Sonne, KR$Besucher, method = "spearman")


### Teil 4 - bivariate lineare Regression

# lineare Regression
# lm(Besucher~Sonne, data = KR)
# Um sich alle wichtigen Kennwerte der Regression anschauen zu können, muss diese einem Objekt zugewiesn werden.
# Das Objekt mit der Regression bitte mit "summary" öffnen!
RegM<-lm(Besucher~Sonne, data = KR)
# RegM<-lm(KR$Besucher~KR$Sonne)
summary(RegM)

# Zur ermittelung der standardisierten Beta-Gewichte müssen beide Variablen z-Transformiert (standardisiert) werden.
lm(scale(Besucher)~scale(Sonne), data = KR)

# Homoskedastizität checken
# plot(RegM$fitted.values, RegM$residuals)
# abline(lm(RegM$residuals~RegM$fitted.values),col="red")
plot(RegM,1)
install.packages("lmtest")
library(lmtest)
bptest(RegM)

# Normalverteilung der Residuen prüfen
plot(RegM,2)
shapiro.test(RegM$residuals)

# Grafik
plot(Besucher~Sonne, data = KR)
abline(lm(Besucher~Sonne, data = KR))


### Teil 5 - multiple lineare Regression

Daten<-read.csv2("Multi_Reg.csv")
#Modell<-lm(Daten$IQ~Daten$Fehler+Daten$Note)
Modell<-lm(IQ~Fehler+Note, data = Daten)
summary(Modell)


### Teil 6 - Voraussetzungen der multiplen Regression überprüfen (im Nachhinein)

# Normalverteilung der Residuen
plot(Modell,2)
shapiro.test(Modell$residuals)


# Homoskedastizität 
# grafischer Homoskedastizitätstest
plot(Modell, 1)
# Inferenzstatistische Überprüfung der Konstanz der Varianz (Varianzgleichheit)
bptest(Modell)

# bei nicht vorliegender Normalverteilung der Residuen sollte der White_Test dem Breusch-Pagan-Test vorgezogen werden!!!
install.packages("skedastic")
library(skedastic)
white(Modell)

# Multikollinearität
# vif<2 problemlos, vif>=2 bedenklich, vif<10 nicht mehr akzeptabel
install.packages("car")
library(car)
vif(Modell)


### Teil 7 - Effektstärken

# Gesamteffektstärke 
# R2>=.02 klein; R2>=.15 mittel; R2>=.26 groß
# oder als globales f2 ausgedrückt:
# Globale Effektstärken f2>=.02 klein; f2>=.15 mittel; f>=.35 groß
R2<-summary(Modell)$r.squared
f2<-R2 / (1 - R2)
f2

# Partielle Effektstärken f2>=.02 klein; f2>=.15 mittel; f>=.35 groß
install.packages("sensemakr")
library(sensemakr)
partial_f2(Modell, covariates = NULL)

