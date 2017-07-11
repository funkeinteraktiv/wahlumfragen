needs(tidyr,dplyr,ggplot2)

polls <- read.csv("umfragedaten.csv", stringsAsFactors = F) # Umfragedaten aus CSV einlesen
# Daten enthalten folgende Informationen    
# date     Datum der Umfragenveröffentlichung
# inst     Name des Umfrageinstituts
# party    Kürzel der Partei (CDU=cdu, SPD=spd, Linke/PDS=lin, Bündnis90/Die Grünen=gru, Piraten=pir, AfD=afd)
# value    Umfragewert in Prozent
# color    parteispezifische Farbe

polls$date <- as.Date(polls$date, origin="1970-01-01") # Datum der Umfragenveröffentlichung in Datumsobjekt umwandeln

# Grafik mit Loesskurve erstellen
pollsviz <-  polls %>% filter(date %in% as.Date(as.Date('2005/9/18'):as.Date('2017/9/24'), origin="1970-01-01")) %>% 
    ggplot(aes(x=date, y=value, color=color)) + 
    scale_y_continuous(limits = c(0, 50), expand = c(0, 0)) +
    scale_x_date(limits = as.Date(c('2005/7/18', '2017/9/24'), origin="1970-01-01"), expand = c(0, 0)) +
    geom_hline(yintercept=c(0,10,20,30,40,50),
               color="grey") +
    geom_vline(xintercept=as.numeric(as.Date(c('2005/9/18', '2009/9/27', '2013/9/22', '2017/9/24'), origin="1970-01-01")),
               color="grey") +
    geom_point(alpha=0.2) + 
    geom_smooth(se=F, method="loess", span=0.05, alpha=0.1) + 
    labs(x=" ", y=" ") +
    theme_classic() + 
    scale_colour_identity()
pollsviz

# Grafik als pdf speichern
pdf("umfrage.pdf",16,10)
print(pollsviz)
dev.off()