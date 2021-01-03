#------------------------------------------------------------------------#
# MyFirstOrchardSim
#------------------------------------------------------------------------#
#
# Simulation of MyFirstOrchard
# January 2021
#
#------------------------------------------------------------------------#


####Vorbereitung####
set.seed(1988)
library(mosaic)
library(ggplot2)

#Skirpt laden:
source("Spiel.R", encoding="UTF-8")

####Auszuführender Code ####

#10000 * 100 Spiele:
erg_rational <- do(10000)*multSpiele(100, rational=T)     #Rationale Entscheidung: Nimm vom vollsten Baun
erg_favorit <- do(10000)*multSpiele(100, rational = F)    #Nimm Lieblingsfrucht (vordefinierte Favoritenreihenfolge)
erg <- rbind(cbind(erg_rational, Decision="rational"), cbind(erg_favorit, Decision="favorite fruit"))
erg$Decision <- as.factor(erg$Decision)
grp_mean <- mean(RabeVerliert~Decision, data=erg)

#Histogram zeichnen:
gg <- ggplot(data=erg, aes(x=RabeVerliert, fill=Decision)) + 
  geom_histogram(binwidth = 1, position="identity", color="black", alpha=0.5) + 
  theme_minimal()+labs(x="No. of times the raven loses in 100 games", title="My First Orchard Simulation") + 
  scale_fill_manual(values=c("#5D85C3", "#E9503E")) +  
  theme(legend.position="bottom") + 
  geom_vline(xintercept = grp_mean[1], color="#5D85C3", linetype="dashed", size=1.5) +
  geom_vline(xintercept = grp_mean[2], color="#E9503E", linetype="dashed", size=1.5)

plot(gg)
