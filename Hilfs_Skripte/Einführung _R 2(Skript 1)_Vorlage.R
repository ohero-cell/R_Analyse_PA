###### Skript R-Kurs01 (Einführung) #####

# Teil 1 - Erste einfache Operationen
# Teil 2 - Vektoren und Matrizen
# Teil 3 - Arbeiten mit Datensätzen
# Teil 4 - Speichern und aufrufen von Datensätzen
# Teil 5 - einfaches Datenmanagement

### Teil 1 - Erste einfache Operationen

# R kann wie ein Taschenrechner benutzt werden!
1+2

# Zuweisung von Werten oder Operationen zu Objekten mit dem Zuweisungpfeil "<-"
x<-1
y<-2
x+y
z<-x+y
# Wenn der Wert eines Opjektes angezeigt werden soll, muss nur das Objekt angegeben  (und natürlich danach "Run" durchgeführt) werden.
z
# Das geht auch mit Text, der muss aber immer in Anführungszeichen gesetzt werden.
Nachricht<-"Hallo Welt"
Nachricht


### Teil 2 -  Vektoren und Matrizen

# Es gibt nicht nur einzelne Zahlen, sondern auch Zahlenreihen, sogenannte Vektoren.
# Diese werden mit der c()-Funtion (concatenate) verknüpft.
Vektor1<-c(1,2,3)
Vektor2<-c(4,5,6)
Vektor3<-c(7,8,99)
Vektor4<-c("a","b","c")

# Um einen bestimmten Wert anzuzeigen, muss der entsprechende Index angegeben werden.
Vektor2[3] # Es wir die "5" angezeigt.

# Das können auch mehrere Werte sein:
Vektor1[-1] # Es werden die "2" und die "3" angezeigt
Vektor4[2:3] # Es werden "b" und "c" angezeigt

# In den eckigen Klammern können auch, statt Indizes, bestimmte Bedingungen angegeben werden.
Vektor3[Vektor3<99] # 7 8

# Die Bedingung bezieht sich diesmal auf einen anderen Vektor!
Vektor4[Vektor3==99] # Gleichzeichen immer doppelt! c

# Mit den Indizes kann auch gerechnet werden.
Vektor4[Vektor3[2]-Vektor2[3]] # b

# mehrere Zahlenreihen können mit der matrix()-Funktion zu einer Matrix zusammengefasst werden.
# Dabei muss die c()-Funktion integriert und durch das ncal- bzw. nrow-Argument definiert werden.
Matrix1<-matrix(c(Vektor1,Vektor2,Vektor3,Vektor4), ncol = 4, nrow = 3)
colnames(Matrix1)<-c("Frage1","Frage2","Frage3", "Name")

# Die Matrix kann auch transformiert werden.
Matrix2<-t(Matrix1)

# Um einen bestimmten Wert anzeigen zu können, müssen Zeilen- und Spaltenindizes angegeben werden.
Matrix1[2,3] # 8

# Hier können auch wieder gleich mehrere Werte gleichzeitig angezeigt werden.
Matrix1[2,2:3] # 5 8
Matrix1[1:2,] # Erste und zweite Zeile / alle Spalten
Matrix1[-3,] # s.o.


### Teil 3 - Arbeiten mit Datensätzen

# Eine Matrix kann in einen Datensatz zusammengefasst werden.
Daten<-data.frame(Matrix1)

# bessere Möglichkeit!
# mehrere Vektoren können auch direkt mit der data.frame()-Funktion zu einem Datensatz zusammengefasst werden.
Daten1<-data.frame(Vektor1,Vektor2,Vektor3,Vektor4)

# Mit der Funktion colnenames() können die Variablen neubenannt werden.
Variablennamen<-c("Frage1","Frage2","Frage3", "Name")
colnames(Daten1)<-Variablennamen

# Das geht auch schneller bzw. kürzer!
colnames(Daten1)<-c("Frage1","Frage2","Frage3", "Name")

#Werte können einfach dadurch aufgerufen werden, indem man zuerst den Datensatz und danach, getrennt durch ein Dollarzeichen, die Variable nennt.
Daten1$Frage2 # 4 5 6 
Daten1$Frage2[1:2] # 4 5


### Teil 4 - Speichern und aufrufen von Datensätzen

# Datensätze (Dataframes) sollte man am besten als csv-Datei speichern.
# speichern
write.csv2(Daten1, "Daten.csv")
# aufrufen
Daten3<-read.csv2("Daten.csv")

# automatisch entstandenen Indexvariable löschen
Daten3<-Daten3[-1]

# oder alternativ
Daten3$X<-NULL

# Speicher als rda-Datei bietet sich an, wenn man ausschließlich mit R arbeitet, da es direkt angeklickt werden kann.
save(Daten3, file = "Daten2.rda")


### Teil 5 - einfaches Datenmanagement 

# Bereits bestehenden CSV-Datensätze öffnen

# 1. Teildatensatz öffnen
Probanden_00<-read.csv2("Probanden_00.CSV")

# 2. Teildatensatz öffnen
Probanden_01<-read.csv2("Probanden_01.CSV")

# Den 2. Teildatensatz an den 1. Teildatensatz unten anfügen
Probanden_g<-rbind(Probanden_00,Probanden_01)

# Liste mit Namen der Probanden in den Gesamtdatensatz einfügen
# Liste öffnen
Namen<-read.csv2("Namen.csv")

# Variable "Namen" im Datensatz Probanden_g erschaffen (Probanden$Namen) und dieser dann die Variable "Name" aud dem Datensatz "Name" zuweisen
Probanden_g$Namen<-Namen$Namen

# Alternativ
# erstmal rückgangig machen
Probanden_g<-Probanden_g[,-6]

# Datensätze zusammenfügen
Probanden_g<-data.frame(Probanden_g,Namen)
# überflüssige Variable löschen
Probanden_g$ID.1<-NULL

# Variablen sortieren - Die Namen der Probanden sollen gleich auf die ID folgen.
Probanden_g<-Probanden_g[c(1,6,2,3,4,5)]

# Alternativ
# rückängig
Probanden_g<-Probanden_g[c(1,3,4,5,6,2)]
Probanden_g<-Probanden_g[c("ID","Namen","SEX","Punkte","Frage1", "Frage2")]

# Datensatz nach einer bestimmten Variablen sortieren
Probanden_SEX<-Probanden_g[order(Probanden_g$SEX),]

# Werte umcodieren und dabei automatisch den Datentyp in "Character" ändern
Probanden_g$SEX[Probanden_g$SEX=="1"]<-"Frau"
Probanden_g$SEX[Probanden_g$SEX=="0"]<-"Mann"

# Datentyp ändern
# Charakter oder Numeric in Factor ändern (häufig notwendig)
Probanden_g$SEX<-as.factor(Probanden_g$SEX)

# Fehlende Werte im ganzen Datensatz definieren
Probanden_g[Probanden_g==999]<-NA

# Fehlende Werte in einer bestimmten Variablen definieren
Probanden_g$Frage2[Probanden_g$Frage2==998]<-NA

# Neue Variable definieren und gleichzeitig berechnen (z.B. E x W-Berechnung)
Probanden_g$Motivation<-Probanden_g$Frage1*Probanden_g$Frage2
