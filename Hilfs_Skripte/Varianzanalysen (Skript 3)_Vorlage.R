###### Skript R-Kurs03 (Varianzanalysen) #####

# Teil 1 - einfache Mittelwertvergleiche (t-Tests)
# Teil 2 - die einfaktorielle Varianzanalyse
# Teil 3 - Ausweichverfahren bei nicht vorhandenen Voraussetzungen
# Teil 4 - die zweifaktorielle Varianzanalyse
# Teil 5 - die einfaktorielle Varianzanalyse mit Messwiederholungen
# Teil 6 - die zweifaktorielle Varianzanalyse mit Messwiederholungen


### Teil 1 - einfache Mittelwertvergleiche (t-Tests)

Daten<-read.csv2("t-Test.CSV")
# Voraussetzung der Normalverteilung prüfen (bei n<30)
# shapiro.test(Daten$Leistung)
# tTest mit einer Stichprobe
t.test(Daten$Leistung, mu=100)
# t.test(Daten$Leistung, mu=100, alternative="greater")

# t-Test für zwei unabhängige Stichproben
# erstmal die Varianzhomogenität checken
# Package für Levene-Test installieren und laden
install.packages("car")
library(car)
# Gruppierungsvariable faktorisieren
Daten$Sex<-as.factor(Daten$Sex)
# Der eigentlich Test auf Varianzgleichheit
leveneTest(Daten$Leistung,Daten$Sex)
# Der eigentliche t-Test
# var.equal auf "TRUE" setzen, weil Levene-Test nicht signifikant
t.test(Daten$Leistung~Daten$Sex, var.equal = TRUE)

# Grafik 
Männer<-mean(Daten$Leistung[Daten$Sex==0])
Frauen<-mean(Daten$Leistung[Daten$Sex==1])
Mittelwerte<-c(Männer, Frauen)
sdM<-sd(Daten$Leistung[Daten$Sex==0])
sdF<-sd(Daten$Leistung[Daten$Sex==1])
sd<-c(sdM,sdF)
barplot(c(Männer,Frauen), names.arg = c("Männer","Frauen"), col = c("blue","red"))
# bp<-barplot(c(Männer,Frauen), names.arg = c("Männer","Frauen"), col = c("#4298ff","#f95210"), ylim = c(0,130))
# arrows(x0=bp, y0 = Mittelwerte - sd, x1=bp, y1 = Mittelwerte + sd, angle =90, code = 3, lenght = 0.05)
# barplot(c(103.2,104.2424), names.arg = c("Männer","Frauen"), col = c("#4298ff","#f95210"))

# nicht parametrisch (wenn Ordinal oder keine Normalverteilung bei n<30)
wilcox.test(Daten$Leistung~Daten$Sex)

# gepaarter tTest
# Datensatz öffnen
Daten01<-read.csv2("Paare.csv")
shapiro.test(Daten01$Schaetzung)
shapiro.test(Daten01$Messung)
t.test(Daten01$Schaetzung, Daten01$Messung, paired = TRUE)
# zum Vergleich, wieviel sensitiver ein gepaarter t-Test ist!!!
# t.test(Daten01$Schaetzung, Daten01$Messung, paired = FALSE)

# nicht parametrisch (wenn Ordinal oder keine Normalverteilung bei n<30)
wilcox.test(Daten01$Schaetzung, Daten01$Messung, paired = TRUE)


### Teil 2 - die einfaktorielle Varianzanalyse

# Datensatz
VarianzA<-read.csv2("ANOVA.CSV")

# Die Gruppierungsvariablen müssen unbedingt als Faktoren definiert werden!!!
VarianzA$Schulung<-as.factor(VarianzA$Schulung)
VarianzA$SEX<-as.factor(VarianzA$SEX)

# Mittelwerte der drei unterschiedlichen Schulungsgruppen

# Mittelwertberechnungen
keine<-mean(VarianzA$Fehler[VarianzA$Schulung==0])
eins<-mean(VarianzA$Fehler[VarianzA$Schulung==1])
zwei<-mean(VarianzA$Fehler[VarianzA$Schulung==2])
# Vektorisierung der Mittelwerte
Mittelwerte<-c(keine, eins, zwei)

# Test auf Varianzhomogenität (Homoskedastizität)
library(car)
leveneTest(VarianzA$Fehler~VarianzA$Schulung, center = mean)
# besser auch grafisch prüfen (s.u.):

# Die egentliche ANOVA
# Die Funktion muss einem Objekt zugewiesen und dieses mit "summary" geöffnet werden!
ANOVA1<-aov(VarianzA$Fehler~VarianzA$Schulung)
summary(ANOVA1)
# Varianzhomogenität grafisch prüfen
plot(ANOVA1, 1)
# Normalverteilung grafisch prüfen - wichtig bei kleineren Stichproben (n<30)
plot(ANOVA1, 2)

# weitergehende Analysen
# install.packages("DescTools")
library(DescTools)
# Bobferroni-Test fuer die einzelnen Gruppenunterschiede
PostHocTest(ANOVA1, method = "bonf")

# Effrektstaerke Eta-Quadrat: ab 0.01 klein / ab 0,06 mittel ab 0,14 groß
EtaSq(ANOVA1)

### Ergebnisdarstellung

# Liniendiagramm
Schulungen<-c(0,1,2)
plot(Schulungen, Mittelwerte, type="b", axes=FALSE, col="#0000ff", ylim=c(0,23), xlab = "Art der Schulung", ylab = "Ø Anzahl der Fehler")
# plot(Schulungen, Mittelwerte, type="l", axes=FALSE, col="#0000ff", ylim=c(0,23))
# points(Schulungen,Mittelwerte, col="#0000ff")
axis(1, at=seq(0,2, 1),labels = c("keine Schulung","Schulung 1", "Schulung 2"))
axis(2, at=seq(0,23, 1))


### Teil 3 - Ausweichverfahren bei nichterfüllten Voraussetzungen

# Bei ungleichen Varianzen
oneway.test(VarianzA$Fehler~VarianzA$Schulung)

# nichtparametrisch (wenn keine Normalverteilung gegeben - funktioniert nur bei einem Faktor!
kruskal.test(VarianzA$Fehler~VarianzA$Schulung)


### Teil 4 - die zweifaktorielle Varianzanalyse

# Voraussetzungen nach der Analyse Prüfen!

# Die eigentliche Varianzanalyse (Interaktion muss extra angegeben werden!)
ZfANOVA<-aov(Fehler~SEX+Schulung+SEX*Schulung, data=VarianzA)
summary(ZfANOVA)

# ohne Interaktion
# ZfANOVA1<-aov(Fehler~SEX+Schulung, data=VarianzA)
# summary(ZfANOVA1)

# Normalverteilung prüfen (Bei n<30)
plot(ZfANOVA, 2)

# Varianzhomogenität prüfen
plot(ZfANOVA, 1)

# Effrektstaerken Eta-Quadrate: ab 0.01 klein / ab 0,06 mittel ab 0,14 groß
EtaSq(ZfANOVA)

# Grafik (Line: Fehler/Schulungen getrennt nach Geschlecht)
Mittel00<-mean(VarianzA$Fehler[VarianzA$SEX==0 & VarianzA$Schulung==0])
Mittel01<-mean(VarianzA$Fehler[VarianzA$SEX==0 & VarianzA$Schulung==1])
Mittel02<-mean(VarianzA$Fehler[VarianzA$SEX==0 & VarianzA$Schulung==2])
Mittel10<-mean(VarianzA$Fehler[VarianzA$SEX==1 & VarianzA$Schulung==0])
Mittel11<-mean(VarianzA$Fehler[VarianzA$SEX==1 & VarianzA$Schulung==1])
Mittel12<-mean(VarianzA$Fehler[VarianzA$SEX==1 & VarianzA$Schulung==2])
#Mittel<-matrix(c(Mittel00, Mittel10, Mittel01,Mittel11, Mittel02, Mittel12), nrow=2)
#Mittel
FehlerM<-c(Mittel00, Mittel01, Mittel02)
FehlerF<-c(Mittel10, Mittel11, Mittel12)
Schulungen<-c(0,1,2)
plot(Schulungen, FehlerM, type="l", axes=FALSE, col="#0000ff", ylim=c(0,23))
points(Schulungen,FehlerM, col="#0000ff")
lines(Schulungen, FehlerF, col="#ff0000")
points(Schulungen,FehlerF, col="#ff0000")
axis(1, at=seq(0,2, 1),labels = c("keine","Schulung 1", "Schulung 2"))
axis(2, at=seq(0,23, 1))

# Gesamt Sum q ohne Residuals / Gesamt Sum Sq (= .88)

### Teil 5 - die einfaktorielle Varianzanalyse mit Messwiederholungen

#Datensatz
ANOVA_MW<-read.csv2("AnovaWiederholungR.csv")
library(rstatix)
# Anova mit Meßwiederholung
anovaMW<-anova_test(data = ANOVA_MW, dv = Werte, wid=ID, within = Zeitpunkt)
# Varianzhomogenität zwischen den Stufen (Sphärizität) checken
anovaMW$`Mauchly's Test for Sphericity`
# "Ergebnisausgabe"
get_anova_table(anovaMW)


### Teil 6 - die zweifaktorielle Varianzanalyse mit Messwiederholungen

# klassisch mit einem "Wiedeholungsfaktor" und einem herkömmlichen Faktor ohne Messwiederholung
zfANOVA_MW<-read.csv2("2fANOVA_MW.CSV")
zfANOVA_MW$Within<-as.factor(zfANOVA_MW$Within)
zfANOVA_MW$Between<-as.factor(zfANOVA_MW$Between)
ZFanovaMW<-anova_test(data = zfANOVA_MW, dv = Werte, wid=ID, within = Within, between = Between)
get_anova_table(ZFanovaMW)

# echte zweifaktorielle ANOVA mit MW, mit zwei "Wiederholungsfaktoren" (keine "Between-Faktoren")
anovaMW<-anova_test(data = Daten, dv = Werte, wid=ID, within = c(Faktor1, Faktor2))
