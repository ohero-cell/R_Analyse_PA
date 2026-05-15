###### Skript R-Kurs02 (einfache Analysen) #####

# Teil 1 - Datenmanagement
# Teil 2 - Maße der zentralen Tendenz
# Teil 3 - Dispersionsmaße
# Teil 4 - Tabellen und Kreuztabellen
# Teil 5 - einfache Mittelwertsvergleiche
# Teil 6 - bivariate Korrelationen
# Teil 7 - die lineare bivariate Regression


### Teil 1. Datenmanagement

# Datensatz öffnen
Kunden<-read.csv2("Kunden.csv")

# überflüssige Spalten löschen
Kunden<-Kunden[ ,1:2]

# Fälle mit fehlenden Werten löschen (Vorsicht! - Komma nicht vergessen)
# Kunden<-Kunden[Kunden$WERT!=999, ] # oder
# Kunden<-Kunden[Kunden$Rang!="k.A", ]

# Fehlende Werte definieren
Kunden$WERT[Kunden$WERT==999]<-NA
Kunden$Rang[Kunden$Rang=="k.A."]<-NA

# Datensatz sortieren
Kunden<-Kunden[order(Kunden$WERT), ]
Kunden<-Kunden[order(Kunden$WERT, decreasing = TRUE), ]


### Teil 2 - Maße der zentralen Tendenz

## Mittelwert 
# mean(Kunden$WERT) # Wenn keine Missings (NA) vorhanden
mean(Kunden$WERT, na.rm = TRUE)

# als Objekt abspeichern
# g<-mean(Kunden$WERT, na.rm = TRUE)

# Missings durch Mittelwert ersetzen
# Kunden$WERT[is.na(Kunden$WERT)]<-g

# Alles auf einmal
Kunden$WERT[is.na(Kunden$WERT)]<-mean(Kunden$WERT, na.rm = TRUE)

# Mittelwerte der verschiedenen Kundenränge
a<-mean(Kunden$WERT[Kunden$Rang=="a"])
b<-mean(Kunden$WERT[Kunden$Rang=="b"])
c<-mean(Kunden$WERT[Kunden$Rang=="c"])

## Median
# median(Kunden$WERT)
median(Kunden$WERT, na.rm = TRUE)

# Quartile und IQR
IQR(Kunden$WERT, na.rm = TRUE)

# Verteilung ordinaler Daten in einem Überblick
summary(Kunden$WERT)

# Grafik
boxplot(Kunden$WERT)

## Modalwert (bei kategorialen Variablen)
# Für den Modus gibt's bei R keine eigene Funktion, er muss mit der Funktion table() ermittelt werden.
table(Kunden$Rang)


### Teil 3 - Dispersionsmaße

## Varianz
var(Kunden$WERT, na.rm = TRUE)

## Standardabweichung
sd(Kunden$WERT, na.rm = TRUE)
#sd(Kunden$WERT, na.rm = TRUE)^2

## Standartfehler
# Die Funktion describe() liefert einen Überblick über die wichtigsten statistischen Maßzahlen (Parameter) einer Variablen
# Die Missings müssen dabei nicht berücksichtigt werden!
# Package installieren und laden:
install.packages("psych")
# wenn bereits installiert, kann auch bei Packages ein Haken gesetzt werden.
library(psych)
describe(Kunden$WERT)
# oder selber rechnen:
# sd(Vector1)/sqrt(length(Vector1))
# Standardfehler als eigene Funtion!!!
# std.error <- function(x) sd(x)/sqrt(length(x))
# std.error(Kunden$WERT)


# Teil 4 - Tabellen und Kreuztabellen

# Datensatz, der ausschließlich aus Nominaldaten besteht öffnen
Kontakte<-read.csv2("CHIQ.CSV")

# Die Daten tabellisieren
Tabelle<-table(Kontakte)
Tabelle

# inferenzstatistische Überprüfung der Verteilungsunterschiede
chisq.test(Tabelle)

# Cramer's V (Effektstärkemaß)
install.packages("rcompanion")
library(rcompanion)
# alternativ ohne Tabellisierung
cramerV(Tabelle)
#cramerV(Kontakte$Kunde, Kontakte$Kontakt) # alternativ

# Ausschluß bestimmter Fälle
# Kontakte01<-Kontakte[Kontakte$Kontakt !=2, ]
# Tabelle01<-table(Kontakte01)
# Tabelle01
# chisq.test(Tabelle01)
# phi(Tabelle01)
# cor(Kontakte01$Kunde,Kontakte01$Kontakt)

# Grafik
barplot(Tabelle, legend.text = c("kein Kunde","Kunde"), beside = TRUE, args.legend = list(x="topright", inset = c(0.7, -0.1),bty = "n"), names.arg = c("kein Kontakt","einmal", "zweimal"))


# Teil 5 - einfache Mittelwertsvergleiche

# tTest mit einer Stichprobe
Daten<-read.csv2("t-Test.CSV")
t.test(Daten$Leistung, mu=100)

# t-Test für zwei unabhängige Stichproben
# erstmal die Varianzhomogenität checken
install.packages("car")
library(car)
# Gruppierungsvariable faktorisieren
Daten$Sex<-as.factor(Daten$Sex)
# Der eigentlich Test auf Varianzgleichheit
leveneTest(Daten$Leistung,Daten$Sex)

# Der eigentliche t-Test
# var.equal auf "TRUE" weil Levene-Test nicht signifikant
t.test(Daten$Leistung~Daten$Sex, var.equal = TRUE)

# Grafik 
Männer<-mean(Daten$Leistung[Daten$Sex==0])
Frauen<-mean(Daten$Leistung[Daten$Sex==1])
barplot(c(Männer,Frauen), names.arg = c("Männer","Frauen"), col = c("blue","red"))
barplot(c(Männer,Frauen), names.arg = c("Männer","Frauen"), col = c("#4298ff","#f95210"))
# barplot(c(103.2,104.2424), names.arg = c("Männer","Frauen"), col = c("#4298ff","#f95210"))

# nicht parametrisch (wenn Ordinal)
wilcox.test(Daten$Leistung~Daten$Sex)

# gepaarter tTest
# Datensatz öffnen
Daten2<-read.csv2("Paare.csv")
t.test(Daten2$Schaetzung, Daten2$Messung, paired = TRUE)
# t.test(Daten2$Schaetzung, Daten2$Messung, paired = FALSE)


# Teil 6 - bivariate Korrelationen

KR<-read.csv2("KorrReg.csv")

# Außreißer checken
summary(KR$Sonne)
IQR(KR$Sonne)
6.75+1.5*4.275
2.475-1.5*4.275
boxplot(KR$Sonne)
summary(KR$Besucher)
IQR(KR$Besucher)
48000+1.5*19775
28225-1.5*19775
boxplot(KR$Besucher)

# Normalverteilung checken
shapiro.test(KR$Sonne)
shapiro.test(KR$Besucher)
hist(KR$Sonne)
hist(KR$Besucher)
hist(KR$Sonne, breaks=4, main="Histogramm Sonne", xlab="Sonnenstunden", ylab="Häufigkeit",col=c('#880022', '#aa0033', '#cc0044'))
hist(KR$Besucher, breaks=4, main="Histogramm Besucher", xlab="Anzahl der Besucher", ylab="Häufigkeit",col=c('#228822', '#44aa44', '#66cc66'))

cor(KR$Sonne, KR$Besucher)
# Signifikanztest ist problematisch, da keine Normalverteilung und n<30!
# Deswegen auf Spearman ausweichen
cor.test(KR$Sonne, KR$Besucher, method = "spearman")

# GrafiK:
plot(KR$Sonne, KR$Besucher)


# Teil 7 - die lineare bivariate Regression

# lineare Regression
# lm(Besucher~Sonne, data = KR)
# Um sich alle wichtigen Kennwerte der Regression anschauen zu können, muss diese einem Objekt zugewiesn werden.
# Das Objekt mit der Regression bitte mit "summary" öffnen!
RegM<-lm(Besucher~Sonne, data = KR)
summary(RegM)

# Zur ermittelung der standardisierten Beta-Gewichte müssen beide Variablen z-Transformiert (standardisiert) werden.
lm(scale(Besucher)~scale(Sonne), data = KR)

# Homoskedastizität checken
plot(RegM$fitted.values, RegM$residuals)
# abline(lm(RegM$residuals~RegM$fitted.values),col="red")
plot(RegM,1)

# Grafik
plot(Besucher~Sonne, data = KR)
abline(lm(Besucher~Sonne, data = KR))

