#Data from http://www.wahlrecht.de/umfragen/landtage/index.htm

#Very helpful guide: https://cran.r-project.org/web/packages/tmap/vignettes/tmap-nutshell.html

#Packages from USA GE 2012
library("tmap")
library("tmaptools")
library("leaflet")
library("rio")
library("plyr")
library("dplyr")
library("maptools")
library("scales")

#New Packages
library("raster")


#[Section Title] Import Dataset
#Data is from sources listed at top. Data for each state is from different polling firms
#the argument encoding="UTF-8" insures the umlauts are read
#this is a very rough draft
DE_Polling <- read.csv("C:\\R\\R_Projects\\Elections\\Data\\German_Polling_Data_Dummy.csv", encoding="UTF-8")
glimpse(DE_Polling)

#Make sure everything is a numeric vector
DE_Polling$CDU.CSU <- as.numeric(DE_Polling$CDU.CSU)
DE_Polling$SPD <- as.numeric(DE_Polling$SPD)
DE_Polling$GRÜNE <- as.numeric(DE_Polling$GRÜNE)
DE_Polling$FDP <- as.numeric(DE_Polling$FDP)
DE_Polling$LINKE <- as.numeric(DE_Polling$LINKE)
DE_Polling$AfD <- as.numeric(DE_Polling$AfD)
DE_Polling$Sonstige_Total <- as.numeric(DE_Polling$Sonstige_Total)


#turn NAs into 0s, this works with this data becasue the FDP has NA's but the sum of other parties polls in that region is still 100
#this should ONLY be done situationally
DE_Polling[is.na(DE_Polling)] <- 0


#Put percent in decimal notation
DE_Polling$CDU.CSU_Dec <- (DE_Polling$CDU.CSU * 0.01)
DE_Polling$SPD_Dec <- (DE_Polling$SPD * 0.01)
DE_Polling$GRÜNE_Dec <- (DE_Polling$GRÜNE * 0.01)
DE_Polling$FDP_Dec <- (DE_Polling$FDP * 0.01)
DE_Polling$LINKE_Dec <- (DE_Polling$LINKE* 0.01)
DE_Polling$AfD_Dec <- (DE_Polling$AfD * 0.01)
DE_Polling$Sonstige_Total_Dec <- (DE_Polling$Sonstige_Total * 0.01)

#create a 'win' column to display winner of each state
#!!!!!!!!! lots of warning messages, but seems to work(?)    
#next time this might have to be done through subsetting. seems to work here
DE_Polling$Largest <- colnames(DE_Polling)[apply(DE_Polling,1,which.max)]


#!!!!!!!!!May be able to fix fill problems by including desired color pallet INSIDE the DF
#COME BACK TO THIS
#https://cran.r-project.org/web/packages/tmap/vignettes/tmap-nutshell.html


#Import .shp file
#.shp file at state level
D_Temp <- "C:\\R\\R_Projects\\Elections\\Map_Files\\Deutschland_shp_files\\DEU_adm1\\DEU_adm1.shp"
DSF1 <- read_shape(file=D_Temp)
qtm(DSF1)

#merge .shp and polling df
str(DSF1@data)
str(DE_Polling)
sum(duplicated(DSF1@data))
sum(duplicated(DE_Polling))

DSF1 <- DSF1[order(DSF1@data$HASC_1),]
DE_Polling <- DE_Polling[order(DE_Polling$HASC_1),]

identical(DSF1@data$HASC_1, DE_Polling$HASC_1)

DEmap <- append_data(DSF1, DE_Polling, key.shp = "HASC_1", key.data = "HASC_1")

#'dumb' static heatmap projection of party support, uncomment to view
#qtm(DEmap, "CDU.CSU")
#qtm(DEmap, "SPD")
qtm(DEmap, "GRÜNE")
#qtm(DEmap, "FDP")
#qtm(DEmap, "LINKE")
#qtm(DEmap, "AfD")
#qtm(DEmap, "Sonstige_Total")

#we can create a similar map with tm_shape, but with easier maniputability
tm_shape(DEmap) +
  tm_fill("GRÜNE_Dec", title="GRÜNE support", palette = "BuGn") +
  tm_borders(alpha=.7) +
  tm_text("NAME_1", size=0.5) +
  tm_legend(position=c(0.87, -.01), frame=TRUE)

#same as above but with ability to change the outer and inner margins, not sure how it works but it could be helpful
tm_shape(DEmap) +
  tm_fill("GRÜNE_Dec", title="GRÜNE support", palette = "BuGn") +
  tm_borders(alpha=.7) +
  tm_text("NAME_1", size=0.5) +
  tm_legend(position=c(0.72, -.01), frame=TRUE) +
  tm_layout(outer.margins=c(.1,0,0,0), inner.margins=c(0,0,0,0), asp=0)



#[Section Title] static map projections and types

#static map of the winner in each state
  #this works, but just assigns random colors
qtm(DEmap, fill="Largest")

  #some progress in assinging colors to qtm() projection,
  #assignment syntax a mystery, success achieved by playing with it. assignmet arbitrary.
colors1 = c('black','green','red')
colors2 = c("#2a202b", "#e41a1c", "#33c47f", "#edbe04",  "#680d0d", "#3c1faf", "#e530e5")
qtm(DEmap, fill="Largest", fill.palette = colors1 )

tm_shape(DEmap) +
  tm_fill("Largest", title="Party with Greatest Support", fill.palette = colors1) +
  tm_borders(alpha=.7) +
  tm_text("NAME_1", size=0.5) +
  tm_legend(position=c(.02, .02), frame=TRUE) 

#[Creating the 3d map with popup menues]

#Simple 3d map, basic colors for state winners
#if there are "NA"s in data, this section will break

#CDU - Black - "#2a202b"
#SPD- Red - "#e41a1c"
#GRÜNE  - Green - "#33c47f"
#FDP - Yellow - "#edbe04"
#Die Linke - Darker Red - "#680d0d"
#AfD - Blue - "#3c1faf"
#Sonstige_Total - Magenta - "#e530e5"

minpct <- min(c(DEmap$CDU.CSU, DEmap$SPD, DEmap$GRÜNE, DEmap$FDP, DEmap$LINKE, DEmap$AfD, DEmap$Sonstige_Total))
maxpct <- max(c(DEmap$CDU.CSU, DEmap$SPD, DEmap$GRÜNE, DEmap$FDP, DEmap$LINKE, DEmap$AfD, DEmap$Sonstige_Total))

CDUPalette <- colorNumeric(palette = "Greys", domain=c(minpct, maxpct))
SPDPalette <- colorNumeric(palette = "Reds", domain = c(minpct, maxpct))
GRÜNEPalette <- colorNumeric(palette = "Greens", domain = c(minpct, maxpct))
FDPPalette <- colorNumeric(palette = "Yellow", domain = c(minpct, maxpct))
DieLinkePalette <- colorNumeric(palette = "Oranges", domain = c(minpct, maxpct))
AfDPalette <- colorNumeric(palette = "Blues", domain = c(minpct, maxpct))
Sonstige_TotalPalette <- colorNumeric(palette = "Purples", domain = c(minpct, maxpct))

largestPalette <- colorFactor(palette=c("#2a202b", "#e530e5", "#edbe04",  "#33c47f", "#680d0d", "#3c1faf", "#e41a1c"), 
  domain = DEmap$Largest)
    
DEpopup <- paste0("State of ", DEmap@data$NAME_1 , "<br>",
                  "<br>",
                  "CDU.CSU: ", percent(DEmap@data$CDU.CSU_Dec), "<br>",
                  "SPD: ", percent(DEmap@data$SPD_Dec), "<br>",
                  "GRÜNE: ", percent(DEmap@data$GRÜNE_Dec), "<br>",
                  "FDP: ", percent(DEmap@data$FDP_Dec), "<br>",
                  "Linke:  ", percent(DEmap@data$LINKE_Dec), "<br>",
                  "AfD: ", percent(DEmap@data$AfD_Dec), "<br>",
                  "Sonstige_Total: ", percent(DEmap@data$Sonstige_Total_Dec), "<br>")
  
  leaflet(DEmap) %>%
    addProviderTiles("CartoDB.Positron") %>%
    addPolygons(stroke=TRUE,
                weight=1,
                smoothFactor = 0.2,
                fillOpacity = .75,
                popup=DEpopup, 
                color= ~largestPalette(DEmap@data$Largest),
                group="Largest"
    ) %>%
    addLegend(position="bottomleft", colors=c("#2a202b", "#e41a1c", "#33c47f", "#edbe04",  "#680d0d", "#3c1faf", "#e530e5"),
                labels=c("CDU.CSU", "SPD", "GRÜNE", "FDP", "Die Linke", "AfD", "Sonstige_Total"))
  
#Using German Data with leaflet
#need to get to this part...
  
  USpopup <- paste0("State of  ", USmap@data$STATE, "<br>",
                    " Won by: ", USmap@data$Winner, "<br>",
                    "<br>",
                    "Obama: ", percent(USmap@data$ObamaVotePercent), "<br>",
                    "Romney: ", percent(USmap@data$RomneyVotePercent)
  )
  
  leaflet(USmap) %>%
    addProviderTiles("CartoDB.Positron") %>%
    addPolygons(stroke=TRUE,
                weight=1,
                smoothFactor = 0.2,
                fillOpacity = .75,
                popup=USpopup, 
                color= ~winnerPalette(USmap@data$Winner),
                group="Winners"
    ) %>%
    addLegend(position="bottomleft", colors=c("#984ea3", "#e41a1c"), labels=c("Obama", "Romney"))
  
  
#[Section Title] Layered heat map of party support for each State
  #we currently can't get the leaflet map to do the correct fills for party, this next step might help diagnose.
    #problem overall still expected to persist
  #also broken =(  
  
DELayeredmap <- leaflet(DEmap) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(stroke=TRUE,
              weight=1,
              smoothFactor = 0.2,
              fillOpacity = .75,
              popup=DEpopup, 
              color= ~largestPalette(DEmap@data$Largest),
              group="Largest"
  ) %>% 
  addLegend(position="bottomleft", colors=c("#2a202b", "#e41a1c", "#33c47f", "#edbe04",  "#680d0d", "#3c1faf", "#e530e5"),
            labels=c("CDU.CSU", "SPD", "GRÜNE", "FDP", "Die Linke", "AfD", "Sonstige_Total"))  %>% 
  
  addPolygons(stroke=TRUE,
              weight=1,
              smoothFactor = 0.2, 
              fillOpacity = .75, 
              popup=DEpopup, 
              color= ~CDUPalette(DEmap@data$CDU.CSU_Dec),
              group="CDU Support"
  ) %>%
  
  addPolygons(stroke=TRUE,
              weight=1,
              smoothFactor = 0.2, 
              fillOpacity = .75, 
              popup=DEpopup, 
              color= ~SPDPalette(DEmap@data$SPD.CSU_Dec),
              group="SPD Support"
              
  ) %>%
  
  addPolygons(stroke=TRUE,
              weight=1,
              smoothFactor = 0.2, 
              fillOpacity = .75, 
              popup=DEpopup, 
              color= ~GRÜNEPalette(DEmap@data$GRÜNE_Dec),
              group="GRÜNE Support"
              
  ) %>%
  
  addPolygons(stroke=TRUE,
              weight=1,
              smoothFactor = 0.2, 
              fillOpacity = .75, 
              popup=DEpopup, 
              color= ~FDPPalette(DEmap@data$FDP_Dec),
              group="FDP Support"
              
  ) %>%
  
  addPolygons(stroke=TRUE,
              weight=1,
              smoothFactor = 0.2, 
              fillOpacity = .75, 
              popup=DEpopup, 
              color= ~DieLinkePalette(DEmap@data$LINKE_Dec),
              group="Linke Support"
              
  ) %>%
  
  addPolygons(stroke=TRUE,
              weight=1,
              smoothFactor = 0.2, 
              fillOpacity = .75, 
              popup=DEpopup, 
              color= ~AfDPalette(DEmap@data$AfD_Dec),
              group="AfD Support"
              
  ) %>%
  
  addPolygons(stroke=TRUE,
              weight=1,
              smoothFactor = 0.2, 
              fillOpacity = .75, 
              popup=DEpopup, 
              color= ~Sonstige_TotalPalette(DEmap@data$Sonstige_Total_Dec),
              group="Sonstige Support"
  ) %>%
  
  addLayersControl(
    baseGroups=c("Largest", "CDU.CSU", "SPD", "GRÜNE", "FDP", "Die Linke", "AfD", "Sonstige_Total"),
    position = "bottomleft",
    options = layersControlOptions(collapsed = FALSE)
  ) 

#and this displays the map
DELayeredmap  
