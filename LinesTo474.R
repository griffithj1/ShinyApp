# Load required packages
library(shiny)
library(shinythemes)
library(htmltools)
library(RSocrata)
library(fontawesome)

#"Dogfooding" Packages
library(httr)

# Visuals Libraries
library(leaflet)
library(leaflet.extras)
library(DT)
library(sp)
library(rgdal)

# Data Transform
library(plyr)
library(dplyr)
library(tidyr)
library(zoo)
library(lubridate)
library(stringi)
library(stringr)

# Turn off Scientific Notation
options(scipen = 999)

httr::set_config(config(ssl_verifypeer = 0L))

token <- "alnqt7sv5sqllwk66osa8f8f2"

# Function to read backslashes correctly
chartr0 <- function(foo) chartr('\\','\\/',foo)

# Function to Check Screenwidth
getWidth <- '$(document).on("shiny:connected", function(e) {
var jsWidth = screen.width;
Shiny.onInputChange("GetScreenWidth",jsWidth);
});'

# Make it work when Downloading stuff
httr::set_config(config(ssl_verifypeer = 0L))

dollarsComma <- function(x){
  x <- round(x, 2)
  x <- prettyNum(x, big.mark = ",")
  x <- paste0("$", x)
  return(x)
}

### Use One Boundary for Both
load.ComDist <- read.socrata("https://data.cityofgainesville.org/resource/wyk2-qtgk.json")
# City Boundaries
load.CityBound <- read.socrata("https://data.cityofgainesville.org/resource/wubr-vuft.json")

# 311 Input & Icons
request_types <- read.socrata("https://data.cityofgainesville.org/Community-Model/311-Service-Requests-myGNV-/78uv-94ar")
str(request_types)
status_types <- c("Acknowleged", "Open", "Archived")
requests311 <-c("Building (Vacant/Damaged)","Drainage/Ditch Maintainence","Graffiti","Grass (Tall/High)","Mosquito Control",
                "Parking Meter Malfunction","Request for New Streetlight","Residential Rental Maintainence","Right-of-Way Maintainance",
                "Road Repair","Sidewalk","Street Sign","Trash/Debris (Private Property)","Trash/Debris (Public Property)","Tree/Limbs",
                "Tree Planting Suggestion","Vehicle (Abandoned/Non-Operational)","General Code Issue","Other","Noise Complaint",
                "Parking Enforcement","Traffic Enforcement","General Police Enforcement","Streetlight","Parking in Yard (Other than Driveway)",
                "Traffic Signal")


# 311 Selections
icons_311 <- awesomeIconList(
  Building_Damaged = makeAwesomeIcon('house-damaged', library = 'fa', markerColor = 'lightblue', iconColor = '1BBAAA', spin = TRUE),
  DrainageDitch = makeAwesomeIcon('water', library = 'fa', markerColor = 'lightblue', iconColor = '1BBAAA', spin = TRUE),
  Graffiti = makeAwesomeIcon('spray-can', library = 'fa', markerColor = 'lightblue', iconColor = '1BBAAA', spin = TRUE),
  Grass = makeAwesomeIcon('leaf', library = 'fa', markerColor = 'lightblue', iconColor = '1BBAAA', spin = TRUE),
  Mosquito = makeAwesomeIcon('debug', library = 'fa', markerColor = 'lightblue', iconColor = '1BBAAA', spin = TRUE),
  ParkingMeter = makeAwesomeIcon('parking-slash', library = 'fa', markerColor = 'lightblue', iconColor = '1BBAAA', spin = TRUE),
  Streetlight = makeAwesomeIcon('lighbulb', library = 'fa', markerColor = 'lightblue', iconColor = '1BBAAA', spin = TRUE),
  ResRental = makeAwesomeIcon('tools', library = 'fa', markerColor = 'lightblue', iconColor = '1BBAAA', spin = TRUE),
  RightOfWay = makeAwesomeIcon('traffic-cone', library = 'fa', markerColor = 'lightblue', iconColor = '1BBAAA', spin = TRUE),
  RoadRepair = makeAwesomeIcon('road', library = 'fa', markerColor = 'lightblue', iconColor = '1BBAAA', spin = TRUE),
  StreetSign = makeAwesomeIcon('octagon', library = 'fa', markerColor = 'lightblue', iconColor = '1BBAAA', spin = TRUE),
  TrashPrivate = makeAwesomeIcon('trash', library = 'fa', markerColor = 'lightblue', iconColor = '1BBAAA', spin = TRUE),
  TrashPublic = makeAwesomeIcon('trash-alt', library = 'fa', markerColor = 'lightblue', iconColor = '1BBAAA', spin = TRUE),
  TreeLimbs = makeAwesomeIcon('tree-alt', library = 'fa', markerColor = 'lightblue', iconColor = '1BBAAA', spin = TRUE),
  TreePlanting = makeAwesomeIcon('hand-holding-seed', library = 'fa', markerColor = 'lightblue', iconColor = '1BBAAA', spin = TRUE),
  AbandonedVehicle = makeAwesomeIcon('car-mechanic', library = 'fa', markerColor = 'lightblue', iconColor = '1BBAAA', spin = TRUE),
  General = makeAwesomeIcon('cog', library = 'fa', markerColor = 'lightblue', iconColor = '1BBAAA', spin = TRUE),
  Multiple = makeAwesomeIcon('cog-multiple', library = 'fa', markerColor = 'lightblue', iconColor = '1BBAAA', spin = TRUE),
  Noise = makeAwesomeIcon('volume-mute', library = 'fa', markerColor = 'lightblue', iconColor = '1BBAAA', spin = TRUE),
  Parking = makeAwesomeIcon('parking-circle-slash', library = 'fa', markerColor = 'lightblue', iconColor = '1BBAAA', spin = TRUE),
  Traffic = makeAwesomeIcon('cars', library = 'fa', markerColor = 'lightblue', iconColor = '1BBAAA', spin = TRUE),
  GenPolice = makeAwesomeIcon('badge-sheriff', library = 'fa', markerColor = 'lightblue', iconColor = '1BBAAA', spin = TRUE),
  ParkingYard = makeAwesomeIcon('parking-circle-slash', library = 'fa', markerColor = 'lightblue', iconColor = '1BBAAA', spin = TRUE),
)

# Building Code Violations
violations <- read.socrata("https://data.cityofgainesville.org/resource/vu9p-a5f7.json")

#Match these to dataset
inspect_var <- c("Commercial Building Code", "Unpaved Parking Plan", "Boarded Dangerous Building", "Dangerous Building", "Dangerous Building/Hazardous Land", "Driveway Inspections", "General Regulations for All Signs", "Graffiti", "Home Occupation Permit", "HOP Denied", "HOP Violation", "Landlord Permits", "Properties in Lien Process", "Major Housing Violations", "Minor Housing Violations", "Multiple Violations", "Noise Violations", "Non-Conforming and Abandoned Signs", "Obstructing Street or Right of Way", "Over Occupancy Limits", "Off Street Parking", "Permanent Signs", "Prohibited Signs", "Public Records Request", "Rehab First", "Repeat Violator", "Revocation", "Right-Of-Way Obstruction", "Sec 30-9 Signs", "Sec. 30-7 Parking and Loading", "Sign Violation", "Sign Applicability", "Solid Waste", "Special Event Permits", "Temporary Signs", "UF Special Event", "Vacant Land Related", "Non Operational Vehicle", "Vending Booth Permit", "Vision Triangle", "Commercial Zoning Violation", "Residential Zoning Violation"
)

# Icons for Building Code Violations
icons_violations <- awesomeIconList(
  Commericial = makeAwesomeIcon('exclamation-circle', library = 'fa', markerColor = 'lightgray', iconColor = '59B9DE', spin = TRUE), 
  UnpavedPark = makeAwesomeIcon('exclamation-circle', library = 'fa', markerColor = 'lightgray', iconColor = '59B9DE', spin = TRUE), 
  BoardedBuild = makeAwesomeIcon('house-damaged', library = 'fa', markerColor = 'lightgray', iconColor = '59B9DE', spin = TRUE), 
  DanBuild = makeAwesomeIcon('house-damaged', library = 'fa', markerColor = 'lightgray', iconColor = '59B9DE', spin = TRUE), 
  HazLand = makeAwesomeIcon('house-damaged', library = 'fa', markerColor = 'lightgray', iconColor = '59B9DE', spin = TRUE),
  Driveway = makeAwesomeIcon('exclamation-circle', library = 'fa', markerColor = 'lightgray', iconColor = '59B9DE', spin = TRUE),
  Signs = makeAwesomeIcon('sign', library = 'fa', markerColor = 'lightgray', iconColor = '59B9DE', spin = TRUE), 
  Graffiti = makeAwesomeIcon('spray-can', library = 'fa', markerColor = 'lightgray', iconColor = '59B9DE', spin = TRUE), 
  HomeOcc = makeAwesomeIcon('clipboard-user', library = 'fa', markerColor = 'lightgray', iconColor = '59B9DE', spin = TRUE),
  HopDe = makeAwesomeIcon('exclamation-circle', library = 'fa', markerColor = 'lightgray', iconColor = '59B9DE', spin = TRUE), 
  HopVio = makeAwesomeIcon('exclamation-circle', library = 'fa', markerColor = 'lightgray', iconColor = '59B9DE', spin = TRUE),
  Landlord = makeAwesomeIcon('clipboard-user', library = 'fa', markerColor = 'lightgray', iconColor = '59B9DE', spin = TRUE),
  Lien = makeAwesomeIcon('exclamation-circle', library = 'fa', markerColor = 'lightgray', iconColor = '59B9DE', spin = TRUE), 
  MajHous = makeAwesomeIcon('home-alt', library = 'fa', markerColor = 'lightgray', iconColor = '59B9DE', spin = TRUE), 
  MinorHous = makeAwesomeIcon('home-lg-alt', library = 'fa', markerColor = 'lightgray', iconColor = '59B9DE', spin = TRUE), 
  Multi = makeAwesomeIcon('exclamation-circle', library = 'fa', markerColor = 'lightgray', iconColor = '59B9DE', spin = TRUE),
  Noise = makeAwesomeIcon('volume-mute', library = 'fa', markerColor = 'lightgray', iconColor = '59B9DE', spin = TRUE), 
  AbanSign = makeAwesomeIcon('sign', library = 'fa', markerColor = 'lightgray', iconColor = '59B9DE', spin = TRUE), 
  RightofWay = makeAwesomeIcon('road', library = 'fa', markerColor = 'lightgray', iconColor = '59B9DE', spin = TRUE), 
  Occupancy = makeAwesomeIcon('users', library = 'fa', markerColor = 'lightgray', iconColor = '59B9DE', spin = TRUE), 
  OffPark = makeAwesomeIcon('parking-slash', library = 'fa', markerColor = 'lightgray', iconColor = '59B9DE', spin = TRUE), 
  PermSign = makeAwesomeIcon('sign', library = 'fa', markerColor = 'lightgray', iconColor = '59B9DE', spin = TRUE), 
  ProhSign = makeAwesomeIcon('times-octagon', library = 'fa', markerColor = 'lightgray', iconColor = '59B9DE', spin = TRUE), 
  PubRec = makeAwesomeIcon('file-search', library = 'fa', markerColor = 'lightgray', iconColor = '59B9DE', spin = TRUE), 
  Rehab = makeAwesomeIcon('exclamation-circle', library = 'fa', markerColor = 'lightgray', iconColor = '59B9DE', spin = TRUE), 
  Repeat = makeAwesomeIcon('repeat', library = 'fa', markerColor = 'lightgray', iconColor = '59B9DE', spin = TRUE), 
  Revoc = makeAwesomeIcon('exclamation-circle', library = 'fa', markerColor = 'lightgray', iconColor = '59B9DE', spin = TRUE), 
  RightOW = makeAwesomeIcon('traffic-cone', library = 'fa', markerColor = 'lightgray', iconColor = '59B9DE', spin = TRUE), 
  Sec30Sign = makeAwesomeIcon('sign', library = 'fa', markerColor = 'lightgray', iconColor = '59B9DE', spin = TRUE), 
  Sec30Park = makeAwesomeIcon('parking-slash', library = 'fa', markerColor = 'lightgray', iconColor = '59B9DE', spin = TRUE), 
  SignVio = makeAwesomeIcon('sign', library = 'fa', markerColor = 'lightgray', iconColor = '59B9DE', spin = TRUE), 
  SignApp = makeAwesomeIcon('sign', library = 'fa', markerColor = 'lightgray', iconColor = '59B9DE', spin = TRUE), 
  SolidWaste = makeAwesomeIcon('dumpster', library = 'fa', markerColor = 'lightgray', iconColor = '59B9DE', spin = TRUE), 
  SpecEvents = makeAwesomeIcon('file-exclamation', library = 'fa', markerColor = 'lightgray', iconColor = '59B9DE', spin = TRUE), 
  TempSign = makeAwesomeIcon('sign', library = 'fa', markerColor = 'lightgray', iconColor = '59B9DE', spin = TRUE), 
  UfEvent = makeAwesomeIcon('university', library = 'fa', markerColor = 'lightgray', iconColor = '59B9DE', spin = TRUE), 
  UfSpec = makeAwesomeIcon('university', library = 'fa', markerColor = 'lightgray', iconColor = '59B9DE', spin = TRUE), 
  VacLand = makeAwesomeIcon('exclamation-circle', library = 'fa', markerColor = 'lightgray', iconColor = '59B9DE', spin = TRUE), 
  NonOpp = makeAwesomeIcon('car-mechanic', library = 'fa', markerColor = 'lightgray', iconColor = '59B9DE', spin = TRUE), 
  Vending = makeAwesomeIcon('clipboard-user', library = 'fa', markerColor = 'lightgray', iconColor = '59B9DE', spin = TRUE), 
  VisTri = makeAwesomeIcon('exclamation-circle', library = 'fa', markerColor = 'lightgray', iconColor = '59B9DE', spin = TRUE), 
  CommVio = makeAwesomeIcon('exclamation-circle', library = 'fa', markerColor = 'lightgray', iconColor = '59B9DE', spin = TRUE), 
  ResVio = makeAwesomeIcon('exclamation-circle', library = 'fa', markerColor = 'lightgray', iconColor = '59B9DE', spin = TRUE), 
)
  
  # Arrests Input & Icons
crime_Cat <- c("Violent", "Property", "Drug-Related")
offenses <- read.socrata("https://data.cityofgainesville.org/resource/gvua-xt9q.json")

crime <- awesomeIconList(
  violent <- makeAwesomeIcon('fingerprint', library = 'fa', markerColor = 'darkblue', iconColor = 'F2665E', spin = TRUE), 
  property <- makeAwesomeIcon('house-damage', library = 'fa', markerColor = 'darkblue', iconColor = 'F2665E', spin = TRUE),
  drugs <- makeAwesomeIcon('cannabis', library = 'fa', markerColor = 'darkblue', iconColor = 'F2665E', spin = TRUE)
)

#Fire Hydrants
fire_hyd <- read.socrata("https://data.cityofgainesville.org/resource/ewpq-mxae.json")
hydrant <- makeAwesomeIcon('fire-extinguisher', library = 'fa', markerColor = 'lightgray', iconColor = 'B59D09', spin = TRUE)

# Capital Projects Inputs & Icons
Cap_Projects <- read.socrata("https://data.cityofgainesville.org/resource/qrmq-gdav.json")
icon_cproj <- makeAwesomeIcon('hard-hat', library = 'fa', markerColor = 'lightgrey', iconColor = 'FACE00', spin = TRUE)

# Collisions
crash_types <- c("Bicycle", "Pedestrian", "Car-Single", "Car-Multiple", "Motorcycles", "Mopeds", "Bus")
taccidents <- read.socrata("https://data.cityofgainesville.org/resource/iecn-3sxx.json")

icons_crashes <- awesomeIconList(
  Bicycle = makeAwesomeIcon('bicycle', library = 'fa', markerColor = 'white', iconColor = 'F2665E', spin = TRUE),
  Pedestrian = makeAwesomeIcon('walking', library = 'fa', markerColor = 'white', iconColor = 'F2665E', spin = TRUE),
  Car-Single = makeAwesomeIcon('car', library = 'fa', markerColor = 'white', iconColor = 'F2665E', spin = TRUE),
  Car-Multiple = makeAwesomeIcon('cars', library = 'fa', markerColor = 'white', iconColor = 'F2665E', spin = TRUE),
  Motorcycles = makeAwesomeIcon('motorcycle', library = 'fa', markerColor = 'white', iconColor = 'F2665E', spin = TRUE),
  Mopeds = makeAwesomeIcon('motorcycle', library = 'fa', markerColor = 'white', iconColor = 'F2665E', spin = TRUE),
  Bus = makeAwesomeIcon('bus', library = 'fa', markerColor = 'white', iconColor = 'F2665E', spin = TRUE)
)
#Active Buisnesses
active_businesses <- read.socrata("https://data.cityofgainesville.org/resource/hk2b-em59.json
")
businesses <- makeAwesomeIcon('badge-dollar', library = 'fa', markerColor = 'lightgray', iconColor = 'E52207', spin = TRUE)

#Building Permits: Do Categorization, similar to 311, set icons. 
building_permits <- read.socrata("https://data.cityofgainesville.org/resource/p798-x3nx.json")
permits <- makeAwesomeIcon('clipboard', library = 'fa', markerColor = 'white', iconColor = '67688C', spin = TRUE)

#RTS: Use GTFS Data instead
rts <- read.socrata("https://data.cityofgainesville.org/resource/w6tc-fr7z.json")
stops <- makeAwesomeIcon('bus', library = 'fa', markerColor = 'darkred', iconColor = '0F191C', spin = TRUE)

#Trash Pickup Zones
trash_zones <- read.socrata('https://data.cityofgainesville.org/resource/45px-xrah.json')
