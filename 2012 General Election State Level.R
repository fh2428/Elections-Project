#This is a demonstration of concept from a election map tutorial found here:
  #http://www.computerworld.com/article/3038270/data-analytics/create-maps-in-r-in-10-fairly-easy-steps.html
#additionally: "If you're interested in the ins and outs of this type of geospatial object, 
  #/known as a SpatialPolygonsDataFrame, see Robin Lovelace's excellent Creating maps in R tutorial, 
  #/especially the section on "The structure of spatial data in R."
  #URL to tutorial:
    #https://github.com/Robinlovelace/Creating-maps-in-R
#pdf slideshow of varioius tmap functions 
  #http://von-tijn.nl/tijn/research/presentations/tmap_user2015.pdf


#USGeneral Election (Presidential Results) 2012, State Level 


#packages
  #tmap is for quick static maps
  #tmaptools is sometimes necissary to run tmaps, not always necissary, unsure why
  #leaflet is for interactive maps
  #rio is for importing/exporting data quickly (not used)
  #plyr/dplyr is for some exploritory data functions, not in article
  #maptools is just from me, may not be used...
  #found at https://cran.r-project.org/web/packages/maptools/vignettes/combine_maptools.pdf
  #scales is for the pop up graphics (may be optional?)
    #scales built under R 3.2.5
    #'may have been replaced by 'Warning: namespace 'scales' is not available and has 
    #\'been replaced by .GlobalEnv when processing object 'mypalette'"
      #had to install package through dropdown menu, not by calling install.package() 

library("tmap")
library("tmaptools")
library("leaflet")
library("rio")
library("dplyr")
library("maptools")
library("scales")

#[0] color selector cheatsheet
library("colorspace") 
#choose_palette()

  #built in colors in R
#colors()

#[1] General Election Data
GE2012 <- read.csv("C:\\R\\R_Projects\\Elections\\Data\\USA_2012_GE_StateLevel_FEC.csv") 
colnames(GE2012) <- c("STATE", "EV.Obama", "EV.Romney", "PV.Obama", "PV.Romney", "PV.Others", "TV", "Winner")
GE2012 = GE2012[-1,]
GE2012 <- GE2012[complete.cases(GE2012$STATE),]
GE2012 <- GE2012[complete.cases(GE2012$PV.Obama),]
glimpse(GE2012)

  #make data integers (currently w/ commas and stored as Factor)
GE2012$EV.Obama <- as.numeric(gsub(",", "", GE2012$EV.Obama))
GE2012$PE.Romney <- as.numeric(gsub(",", "", GE2012$EV.Romney))
GE2012$PV.Obama <- as.numeric(gsub(",", "", GE2012$PV.Obama))
GE2012$PV.Romney <- as.numeric(gsub(",", "", GE2012$PV.Romney))
GE2012$PV.Others <- as.numeric(gsub(",", "", GE2012$PV.Others))
GE2012$TV <- as.numeric(gsub(",", "", GE2012$TV))

#[2] Create win percentage numbers
GE2012$ObamaPercentAsDec<- ((GE2012$PV.Obama - GE2012$PV.Romney)/(GE2012$PV.Obama + GE2012$PV.Romney))
GE2012$RomneyPercentAsDec <- ((GE2012$PV.Romney - GE2012$PV.Obama)/(GE2012$PV.Obama + GE2012$PV.Romney))

GE2012$ObamaPercentAsPer <- (GE2012$ObamaPercentAsDec * 100)
GE2012$RomneyPercentAsPer <- (GE2012$RomneyPercentAsDec * 100)

GE2012$ObamaVotePercent <- (GE2012$PV.Obama / GE2012$TV)
GE2012$RomneyVotePercent <- (GE2012$PV.Romney / GE2012$TV)

GE2012$ObamaPercentAsPer_Rounded <- round(GE2012$ObamaPercentAsPer, 2)
GE2012$RomneyPercentAsPer_Rounded <- round(GE2012$RomneyPercentAsPer, 2)

  #this was in the tutorial, don't understand this step... seems wrong?
GE2012$ObamaMarginalPercentage <- GE2012$ObamaPercentAsDec- GE2012$RomneyPercentAsDec

#[3] Get your Geographic Data
usshapefile <- "C:\\R\\R_Projects\\Elections\\Map Files\\US_Shape_File\\states.shp"
usgeo <- read_shape(file=usshapefile)
qtm(usgeo)

#[4] Merge spacial and results data
  #This is a real pain in the ass the row's aren't already nearly callable
    #I attempted some maniputlations on an aborted proof of concept earlier
    #Not sure how the .shp files information is stored
      #may be able to manipulate data like a normal DF, maybe its more complicated... 
  #view(usgeo@data) if you need to look at the .shp call data
str(usgeo@data)
str(GE2012)
sum(duplicated(usgeo@data))
sum(duplicated(GE2012))

usgeo <- usgeo[order(usgeo@data$STATE_ABBR),]
GE2012 <- GE2012[order(GE2012$STATE),]

identical(usgeo@data$STATE_ABBR, GE2012$STATE)

USmap <- append_data(usgeo, GE2012, key.shp = "STATE_ABBR", key.data="STATE")

#[5] Create A Static Projection
qtm(USmap, "ObamaPercentAsPer")

  #or project differnetly (below), also shows some of the functions that allow more customizablility
tm_shape(USmap) +
  tm_fill("ObamaPercentAsPer", title="Obama Margin of Victory (Percent)", palette = "PRGn") +
  tm_borders(alpha=.7) +
  tm_text("STATE_ABBR", size=0.5)

  #change position of legend on the map like so (sorry Hawaii):
tm_shape(USmap) +
  tm_fill("ObamaPercentAsPer", title="Obama Margin of Victory (Percent)", palette = "PRGn") +
  tm_borders(alpha=.7) +
  tm_text("STATE_ABBR", size=0.5) +
  tm_legend(position=c(.02, .02), frame=TRUE)

#This gives us the static map
  #placement of the legend is pretty terrible, not a lot of choices, can't resize, covering HI
  #AK and HI are throwing proportions of the map off pretty bad

#[6] and [7] Create palette and pop-ups for interactive map
  #This will utilize the leaflet package, whereas previously we used tmap

mypalette <- colorNumeric(palette = "RdYlBu", domain = 
                             USmap$ObamaMarginalPercentage)

USApopup <- paste0("State: ", USmap@data$STATE,
                   " - Obama ", percent(USmap@data$ObamaVotePercent),  " - Romney ", percent(USmap@data$RomneyVotePercent))

leaflet(USmap) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(stroke=FALSE, 
              smoothFactor = 0.2,
              fillOpacity = .8, 
              popup=USApopup,
              color= ~mypalette(USmap$ObamaMarginalPercentage))
#we've created a pop up map superimposed on a world map!
  #some things to work out
    #maybe we don't always want this on a wold map?
    #color palette is pretty atrocious
    #how to print more or less data in popup

#lets try a different setup for the last popup map
    #change the color pallete being called
    #this doesn't fully work, Florida is Red when should be Blue
    #need a way to 'ground' results so that it's not by quartiles...
mypalette <- colorQuantile(palette = "RdBu", domain = 
                            USmap$ObamaMarginalPercentage, n=6)

  #reorder the text in the popup
  #adding/taking away text appears easy, need to be able to format it
  #text might be in html format, we may have considerable flexibility within the popup
USApopup <- paste0("State: ", USmap@data$STATE, "<br>", "<br>",
                   "Of the Popular Vote: ", "<br>",
                   percent(USmap@data$ObamaVotePercent), " For Obama","<br>",
                   percent(USmap@data$RomneyVotePercent), " For Romney")


leaflet(USmap) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(stroke=FALSE, 
              smoothFactor = 0.2,
              fillOpacity = .8, 
              popup=USApopup,
              color= ~mypalette(USmap$ObamaMarginalPercentage))


#[8] Palettes for multi-layer maps
  #the tutorial next does the SC 2016 GOP Primary for 3 candidates
    #the goal is to use pallets to convey the strength of a candidates 
  #it would be nice to make this map be able to scale colors with vote %
  #CAN get all the states the right color, not just seperated by Standard Deviation

#From the Tutorial
  # Obama <-> Trump
  # Romney <-> Rubio

minpct <- min(c(USmap$ObamaVotePercent, USmap$RomneyVotePercent))
maxpct <- max(c(USmap$ObamaVotePercent, USmap$RomneyVotePercent))

ObamaPalette <- colorNumeric(palette = "Blues", domain=c(minpct, maxpct))
RomneyPalette <- colorNumeric(palette = "Reds", domain = c(minpct, maxpct))

winnerPalette <- colorFactor(palette=c("#984ea3", "#e41a1c"), domain = USmap$Winner)

  #FIX THE FOLLOWING LINE FOR A GOOD DIVERGENT COLOR PALLET THAT REFELCTS WHO WON EACH STATE AND THE MARGIN
edPalette <- colorNumeric(palette = "RdBu", domain=USmap@data$ObamaPercentAsDec)

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
  
  
#[9] Layers!!!

USGEmap <- leaflet(USmap) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(stroke=TRUE,
              weight=1,
              smoothFactor = 0.2,
              fillOpacity = .75,
              popup=USpopup, 
              color= ~winnerPalette(USmap@data$Winner),
              group="Winners"
  ) %>% 
  addLegend(position="bottomleft", colors=c("#984ea3", "#e41a1c"), labels=c("Obama", "Romney"))  %>%
  
  addPolygons(stroke=TRUE,
              weight=1,
              smoothFactor = 0.2, 
              fillOpacity = .75, 
              popup=USpopup, 
              color= ~ObamaPalette(USmap@data$ObamaVotePercent),
              group="Obama"
  ) %>%
  
  addPolygons(stroke=TRUE,
              weight=1,
              smoothFactor = 0.2, 
              fillOpacity = .75, 
              popup=USpopup, 
              color= ~RomneyPalette(USmap@data$RomneyVotePercent),
              group="Romney"

  ) %>%
  
  addPolygons(stroke=TRUE,
              weight=1,
              smoothFactor = 0.2, 
              fillOpacity = .75, 
              popup=USpopup, 
              color= ~edPalette(USmap@data$ObamaPercentAsDec),
              group="Obama Win Percent"
  ) %>%
  
  addLayersControl(
    baseGroups=c("Winners", "Obama", "Romney", "Obama Win Percent"),
    position = "bottomleft",
    options = layersControlOptions(collapsed = FALSE)
  ) 

#and this displays the map
USGEmap

#[10] Sharing your R maps
