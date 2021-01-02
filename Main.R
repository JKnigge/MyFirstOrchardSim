#------------------------------------------------------------------------#
# MyFirstOrchardSim
#------------------------------------------------------------------------#
#
# Simulation of MyFirstOrchard
# January 2021
#
#------------------------------------------------------------------------#


####Vorbereitung####
library(mosaic)
library(ggplot2)

#Skirpt laden:
source("Spiel.R", encoding="UTF-8")

####Auszuführender Code ####

#10000 * 100 Spiele:
erg <- do(10000)*multSpiele(100)

#Histogram zeichnen:
gg <- ggplot(data=erg, aes(x=RabeVerliert)) + 
  geom_histogram(color="black", fill="#5D85C3", binwidth = 1) + 
  theme_minimal()+labs(x="Raven loses", title="My First Orchard") + 
  stat_bin(binwidth= 1, geom="text", aes(label=..count..), vjust = -1) + 
  geom_vline(xintercept = mean(~RabeVerliert, data=erg), color="red") + 
  geom_vline(xintercept = qdata(~RabeVerliert, data=erg, p=.975), color="red", linetype="dashed") + 
  geom_vline(xintercept = qdata(~RabeVerliert, data=erg, p=.025), color="red", linetype="dashed")

plot(gg)