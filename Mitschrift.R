# Alles was hinter einem '#' steht ist ein Kommentar
# und wird von R nicht benutzt!
#
# Zuerst laden wir das Paket "mosaic". In der Regel kann man den
# roten Informationstext einfach ignorieren.
library(mosaic)
# ggf. vorher:
# install.packages("here")
library(here)

# Wir wollen eine Datemtabelle aus dem Internet laden und 
# und nutzen die Variable "daten_url" um eine Zeichenkette
# mit der URL zu belegen. Zuweisungsoperator: "<-" 
daten_url <- "http://statistix.org/Data/SaratogaHouses.csv"

# Daten aus dem Internet einlesen
# Mit dem Befehl "read.csv2" können Datentabellen im
# csv-Format geladen wirden, genauer ist bei "csv2" die Feldtrennung ";" 
# und der Dezimaltrenner "," (bei reinen CSV Dateien wäre es "," und ".")
Houses <- read.csv2(daten_url)
# Alternativ können Sie die Daten auch lokal laden:
Houses <- read.csv2(here("SaratogaHouses.csv"))

# Zeige die ersten 7 Zeilen der Tabelle an:
head(Houses, 7)

# Zeige die letzten 7 Zeilen der Tabelle an:
tail(Houses, 7)

# Daten inspizieren
inspect(Houses)

# Klimaanlage eine kategorielle Variable
# Säulendiagramm
gf_bar( ~ Klimaanlage, data = Houses)

# Tabelle
tally( ~ Klimaanlage, data = Houses)

# Anteilswerte (Häuser mit Klimaanlage):
prop( ~ Klimaanlage, success = "Ja", data = Houses)

# Anteilswerte (Häuser ohne Klimaanlage):
prop( ~ Klimaanlage, success = "Nein", data = Houses)

# Tabelle mit relativen Häufigkeiten
tally( ~ Klimaanlage, format = "proportion", data = Houses)

# Analyse des Preises (numerisches Merkmal)
# Histogram
gf_histogram( ~ Preis, data = Houses)

# Breite der Bins: binwidth / Mitte der Bins: center
gf_histogram( ~ Preis, binwidth = 50000, center = 25000, data = Houses)

# Kennzahlen
favstats( ~ Preis, data = Houses)
favstats(Preis ~ Klimaanlage, data = Houses)
favstats( ~ Preis | Klimaanlage, data = Houses)

# Histogram (So funktioniert es leider nicht ...)
gf_histogram( Preis ~ Klimaanlage, data = Houses)

# Histogramm (... aber so)
gf_histogram( ~ Preis | Klimaanlage, data = Houses)

# AV ~ UV (kategorial/kategorial)
# Kamin ~ Klimaanlage
mosaicplot(Kamin ~ Klimaanlage, data = Houses)

# Kreunztabelle 
tally(Kamin ~ Klimaanlage, data = Houses)

# Kreunztabelle (besser aber so, dann passt es zur Ausgabe vom mosaicplot)
tally(Klimaanlage ~ Kamin, data = Houses)

# Chi-Quadrat-Test:
xchisq.test(Kamin ~Klimaanlage, data = Houses)

# AV ~ UV (metrisch/metrisch)
# Preis ~ Wohnfläche

# Streudiagramm
gf_point(Preis ~ Wohnflaeche, data = Houses)

gf_point(Preis ~ Wohnflaeche, color = ~ Heizung, data = Houses)

gf_point(Preis ~ Wohnflaeche, color = ~ Heizung, size = ~ Klimaanlage, data = Houses)

gf_point(Preis ~ Wohnflaeche, color = ~ Klimaanlage, size = ~ Heizung, data = Houses)

# Zur Darstellung: 2e+05 = 2* 10^5 - Schöner wäre eine Darstellung in 1000 Euro 
# Dazu erzeugen wir eine neue Variable "Preis_TEuro" und rechnen die Preise entsprechend um.
# Das Ergebnis speichern wir in der Tabelle 'Houses2'
Houses %>% 
    mutate(Preis_TEuro = Preis / 1000) -> Houses2
gf_point(Preis_TEuro ~ Wohnflaeche, data = Houses2) %>% gf_labs(y = "Preis in 1000 Euro")

# Alles in "einer" Zeile ohne neue Tabelle:
Houses %>% 
  mutate(Preis_TEuro = Preis / 1000) %>%
  gf_point(Preis_TEuro ~ Wohnflaeche, data = .) %>% 
  gf_labs(
    title = "Preise von Häusern in Saratoga",
    subtitle = "Preise von 2006",
    x = "Wohnfläche in qm",
    y = "Preis in 1000 Euro") %>%
  gf_theme(theme_classic())

cor(Preis ~ Wohnflaeche, data = Houses)

cor.test(Preis ~ Wohnflaeche, data = Houses)

## Lineare Modell
linmod <- lm(Preis ~ Wohnflaeche, data = Houses)

linmod
summary(linmod)

linmod2 <- lm(Preis ~ Wohnflaeche + Kamin, data = Houses)
linmod2
summary(linmod2)
plotModel(linmod2)

linmod3 <- lm(Preis ~ Wohnflaeche * Kamin, data = Houses)
linmod3
summary(linmod3)
plotModel(linmod3)
