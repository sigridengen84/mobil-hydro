library(rgdal)
library(dplyr)
library(raster)
library(rgeos)
library(prettymapr)
library(styler)
library(tidyverse)
library(SDMTools)
library(mapmisc)
library(graticule)
library(spatialEco)
### Load data

#Norway counties
counties<-readOGR(dsn="C://github//nor-prep//prep//administrative//raw", "NO_Fylker_pol")

#subset the northermost counties
NN<-subset(counties,NAVN=="Troms" | NAVN=="Finnmark" | NAVN=="Nordland")
Troms<-subset(counties,NAVN=="Troms")

#Hydropower plant in Norway 
hydro<-readOGR(dsn="C://Users//sigrid.engen//OneDrive - NINA//DATA//Reindriftsartikkel//Vannkraftverk//NVEData//Vannkraft", "Vannkraft_Vannkraftverk")

#subset large >10MW plants and 1-10MW plant that have been developed
largehyd_built<-hydro[which(hydro@data$status == "D" 
                          & hydro@data$ytelse_NW > 10),] #large hydro >10MW that has been built

largehyd_Licens<-hydro[which(hydro@data$status == "V" 
                         & hydro@data$ytelse_NW > 10 
                         & hydro@data$konsStatus=="5"),]#large hydro >10MW that has been licenced (gitt konsesjon)

largehyd_UnderCon<-hydro[which(hydro@data$status == "U" 
                             & hydro@data$ytelse_NW > 10),]#large hydro >10MW under contstruction

largehyd_Refused<-hydro[which(hydro@data$status == "V" 
                             & hydro@data$ytelse_NW > 10 
                             & hydro@data$konsStatus=="6"),]#large hydro >10MW

smallhyd_built<-hydro[which(hydro@data$status == "D" 
                        & hydro@data$ytelse_NW <= 10 
                        & hydro@data$ytelse_NW >= 1),]#small hydro 1-10 MW built/utbygd - no small hydro under construction 

smallhyd_Licens<-hydro[which(hydro@data$status == "V" 
                        & hydro@data$ytelse_NW <= 10 
                        & hydro@data$ytelse_NW >= 1
                        & hydro@data$konsStatus=="5"),]#small hydro 1-10 MW licensed

smallhydro_appeal<-hydro[which(hydro@data$status == "V" 
                         & hydro@data$ytelse_NW <= 10 
                         & hydro@data$ytelse_NW >= 1 
                         & hydro@data$konsStatus== "23"),]#Small hydro where appeal is being processed

smallhyd_Refused<-hydro[which(hydro@data$status == "V" 
                              & hydro@data$ytelse_NW <= 10 
                              & hydro@data$ytelse_NW >= 1 
                              & hydro@data$konsStatus=="6"),]

#no small hydro under construction, no large hydro with ongoing appeals

#plot Hydropower
par(mar = c(2, 2, 2, 2))
plot(Troms, border="darkgrey")
plot(counties, add=TRUE,border="darkgrey", col="lightgrey")
plot(Troms, add=TRUE, border="darkgrey", col="white")
plot(largehyd_built,add=T, pch=15,cex=1.2)#utbygd
plot(largehyd_Licens, add=T, pch=15, col="blue",cex=1.2)#gitt konsesjon
plot(largehyd_Refused, add=T, pch=15, col="red",cex=1.2)#avsl?tt

plot(smallhyd_built, add=T, pch= 0,col="black",cex=1.2)#utbygd
plot(smallhyd_Licens, add=T, pch= 0, col="blue", cex=1.2)#gitt konsesjon
plot(smallhydro_appeal, add=T, pch= 0, col="green", cex=1.2)#klagebehandling
plot(smallhyd_Refused, add=T, pch= 0, col="red", cex=1.2)#refused

#locator()#find point on map to place legend

scaleBar(Troms,x=799012.2,y=7635618,cex=0.75, pt.cex=0, box.col="white")

legend(490302.7,7846481,inset=.00, title="Large hydro >10MW",
       c("Built","Licensed","Refused"), 
       fill=c("black", "blue","red"),
       bg="transparent", horiz=FALSE, cex=.8, box.col="white")

legend(490302.7,7777000, inset=0, title="Small hydro 1-10MW",
       c("Built","Licensed","Refused","Appeal"), 
       border=c("black", "blue","red", "green"),
       fill=c("white", "white","white","white"),
       bg="transparent",  box.lty=0,
       horiz=FALSE, cex=.8, box.col="white")

addnortharrow(pos = "bottomright", padin = c(0.30, 0.15), scale = 0.3,
            lwd = 1, border = "black", 
            cols = c("white", "black"),
            text.col = "black")

#### statistics on hydropower in Troms county ####
nrow(largehyd_built)
sum((largehyd_built@data$ytelse_NW))
nrow(smallhyd_built)
sum((smallhyd_built@data$ytelse_NW))
nrow(smallhyd_Licens)
nrow(smallhyd_Refused)
nrow(smallhydro_appeal)

#look at whether built small and large hydro overlaps with reindeer pastures
#change coordinate reference system so that it matches the reindeer pastures
smallhyd_built_transf  <- spTransform(smallhyd_built, proj4string(sommerT))
largehyd_built_transf  <- spTransform(largehyd_built, proj4string(sommerT))

new_shape<-rbind(sommerT, h?stT, vinterT, v?rT, h?vintT)
new_shape2 <- point.in.poly(smallhyd_built_transf, new_shape)

h?stT<-crop(h?st, Troms2)
vinterT<-crop(vinter, Troms2)
v?rT<-crop(v?r, Troms2)
sommerT<-crop(sommer, Troms2)
h?vintT<-crop(h?stvint, Troms2)
flyttT<-crop(flytt, Troms2)
trekkT<-crop(trekk, Troms2)

#dev.copy(jpeg(file ="C://Users//sigrid.engen//OneDrive - NINA//MIne Artikler//Reindriftsartikkel//Hydropower_Troms.jpeg"))
#jpeg(file ="C://Users//sigrid.engen//OneDrive - NINA//MIne Artikler//Reindriftsartikkel//Hydropower_Troms.jpeg")
#dev.off()


#graticules r https://cran.r-project.org/web/packages/graticule/vignettes/graticule.html

#######Map hydropower potential################################################
###############################################################################
#Norway counties
counties<-readOGR(dsn="C://github//nor-prep//prep//administrative//raw", "NO_Fylker_pol")

#subset the northermost counties
NN<-subset(counties,NAVN=="Troms" | NAVN=="Finnmark" | NAVN=="Nordland")
Troms<-subset(counties,NAVN=="Troms")

#Potential for hydropower Troms 
pot<-readOGR(dsn="C://Users//sigrid.engen//OneDrive - NINA//DATA//Reindriftsartikkel//Potensial for vannkraft", "Smaakraft_Troms")
potPrice3<-pot[which(pot@data$PRISPRKWH <3),]
potPrice5<-pot[which(pot@data$PRISPRKWH >=3),]

#plotting potential for hydropower
par(mar = c(2, 2, 2, 2))
plot(Troms, border="darkgrey")
plot(counties, add=TRUE,border="darkgrey", col="lightgrey")
plot(Troms, add=TRUE, border="darkgrey", col="white")
plot(potPrice3, pch=15,col="black",cex=0.5,add=TRUE)
plot(potPrice5, pch=1,bg="white", col="black",cex=0.5,add=TRUE)

legend(490302.7,7846481,inset=0, title="Small hydro potential",
       c("<.33 USD per kWh","0.33-0.55 USD per kWh"), 
       pch=c(15,1),
       border=c("transparent", "transparent"),
       fill=c("transparent", "transparent"),
       bg="transparent",
       horiz=FALSE, cex=.8, box.col="white")

###############Plot reindeer pastures Norway#################################
#############################################################################
#Norway counties
counties<-readOGR(dsn="C://github//nor-prep//prep//administrative//raw", "NO_Fylker_pol")

#subset the northermost counties
NN<-subset(counties,NAVN=="Troms" | NAVN=="Finnmark" | NAVN=="Nordland")
Troms<-subset(counties,NAVN=="Troms")

flytt<-readOGR(dsn="C://Users//sigrid.engen//OneDrive - NINA//DATA//Reindriftsartikkel//Reindriftskart NIBIO Kilden lasted ned 3 nov 2019//Flyttlei", "0000_25833_reindrift_flyttlei_36cc82_SHAPE")
trekk<-readOGR(dsn="C://Users//sigrid.engen//OneDrive - NINA//DATA//Reindriftsartikkel//Reindriftskart NIBIO Kilden lasted ned 3 nov 2019//Trekklei", "0000_25833_reindrift_trekklei_2a098e_SHAPE")
h?st<-readOGR(dsn="C://Users//sigrid.engen//OneDrive - NINA//DATA//Reindriftsartikkel//Reindriftskart NIBIO Kilden lasted ned 3 nov 2019//H?stbeite", "0000_25833_reindrift_arstidsbeite_hostbeite_6362b7_SHAPE")
vinter<-readOGR(dsn="C://Users//sigrid.engen//OneDrive - NINA//DATA//Reindriftsartikkel//Reindriftskart NIBIO Kilden lasted ned 3 nov 2019//Vinterbeite", "0000_25833_reindrift_arstidsbeite_vinterbeite_582c6b_SHAPE")
v?r<-readOGR(dsn="C://Users//sigrid.engen//OneDrive - NINA//DATA//Reindriftsartikkel//Reindriftskart NIBIO Kilden lasted ned 3 nov 2019//V?rbeite", "0000_25833_reindrift_arstidsbeite_varbeite_1eea8_SHAPE")
sommer<-readOGR(dsn="C://Users//sigrid.engen//OneDrive - NINA//DATA//Reindriftsartikkel//Reindriftskart NIBIO Kilden lasted ned 3 nov 2019//Sommerbeite", "0000_25833_reindrift_arstidsbeite_sommerbeite_7ba2e8_SHAPE")
h?stvint<-readOGR(dsn="C://Users//sigrid.engen//OneDrive - NINA//DATA//Reindriftsartikkel//Reindriftskart NIBIO Kilden lasted ned 3 nov 2019//H?stvinter", "0000_25833_reindrift_arstidsbeite_hostvinterbeite_4204c5_SHAPE")

#change coordinate reference system so that it matches the layers
Troms2  <- spTransform(Troms, proj4string(h?st))

h?stT<-crop(h?st, Troms2)
vinterT<-crop(vinter, Troms2)
v?rT<-crop(v?r, Troms2)
sommerT<-crop(sommer, Troms2)
h?vintT<-crop(h?stvint, Troms2)
flyttT<-crop(flytt, Troms2)
trekkT<-crop(trekk, Troms2)

#pasture <- aggregate(rbind(h?st, vinter, v?r, sommer, h?stvint))
#https://www.nceas.ucsb.edu/~frazier/RSpatialGuides/OverviewCoordinateReferenceSystems.pdf

par(mfrow=c(2,3))
par(mar = c(0, 0, 0, 0))
#par(oma = c(2,2,0,0))
plot(Troms2, border="grey45")
title("a) Winter", line = -4)
plot(vinterT, add=T, col="lightblue3", border="transparent")

plot(Troms2, border="grey45")
title("b) Spring", line = -4)
plot(v?rT, add=T, col="green", border="transparent")

plot(Troms2,border="grey45")
title("c) Summer", line = -4)
plot(sommerT, add=T, col="green4", border="transparent")

plot(Troms2, border="grey45")
title("d) Fall & Fall-Winter", line = -4)
plot(h?stT, add=T, col="olivedrab", border="transparent")
plot(h?vintT, add=T, col="olivedrab", border="transparent")

plot(Troms2, border="grey45")
title("e) Migration routes", line = -4)
plot(flyttT, add=T, col="steelblue4", border="transparent")
plot(trekkT, add=T, col="steelblue4", border="transparent")

