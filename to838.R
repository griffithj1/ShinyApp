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
status_types <- c("Acknowleged", "Open", "Archived")
requests311 <- unique(request_types$request_type)

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
violation_type <- unique(violations$case_type)

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
crime_cat <- c("Violent", "Property", "Drug-Related", "Miscellaneous")
offenses <- read.socrata("https://data.cityofgainesville.org/resource/gvua-xt9q.json")
crime_ind <- unique(offenses$narrative)

violentcrime <- c("Domestic Simple Battery", "Battery (simple)", "Dating Violence Simple Battery", "Domestic Battery by Strangulation",
                  "Assault (simple)", "Robbery", "Robbery by Sudden Snatching", "Dating Violence Aggravated Assault", "Domestic Aggravated Battery",
                  "Assault (aggravated)", "Robbery (strong Arm)", "Battery (felony)", "Battery (aggravated)", "Domestic Aggravated Assualt", "Assault (police Officer
                  Aggravated)", "Robbery (armed)", "Homicide", "Dating Violence Aggravated Battery by Strangulation", "Battery on a Person 65 Yoa or Older",
                  "Battery (simple - With Other Weapon)", "Shooting/throwing a Deadly Missile Into a Dwelling/vehicle", "Robbery (home Invasion)", "Battery (police Officer)", 
                  "Battery on a School Board Employee", "Dating Violence Aggravated Battery", "Battery on a Emergency Medicalcare Provider", "Dating Violence Felony Battery", "Domestic Assault"
                  , "Att Homicide", "Battery (police Officer Aggravated", "Battery on Detention or Commitment Facility Staff (simple)", 
                  "Robbery (carjacking)", "Battery (security Guard/officer)", "Battery (security Guard/officer Aggravated)"
                  )
propertycrime <- c("Damage to Property", "Theft Grand - From Building", "Burglary to Conveyance", "Theft Petit - From Vehicle/not Parts",
                  "Theft Petit - Retail", "Theft Petit - Other", "Stolen Vehicle (auto)", "Theft Petit - Bicycle", "Stolen Vehicle (truck)",
                  "Burglary to Residence", "Theft Grand - Retail", "Arson", "Theft Grand - Trailer", "Theft Petit - From Building", "Damage to City Property",
                  "Burglary to Buisness", "Theft Grand - Bicycle", "Theft Grand - From Vehicle (not Parts)", "Theft Petit - Pocket-picking", "Stolen Vehicle (other)",
                  "Theft Petit - From Vehicle (vehicle Parts)", "Stolen Vehicle (motorcycle)", "Burglary to a Structure", "Theft Grand - From Vehicle (vehicle Parts)",
                  "Trash Dumping", "Stolen Property (selling/distributing)", "Theft Petit - From Vending Machine", "Theft Grand - Pocket-picking", 
                  "Stolen Property (possession/conceal)", "Theft Petit - Purse Snatching", "Stolen Property (buying/receiving)", "Theft Grand - From Vending Machine",
                  "Theft Grand - Purse Snatching", "Theft Petit - Trailer", "Theft Grand - Value 300 to 4,999")
drugcrimes <- c("Drug Violation (sid)", "Drug Violation", "Drug Poss. of Controlled Substance", "Drug Violation (using)", "Drug Violation (selling)", "Drug Equip/paraphernalia",
                "Possession of Less Than 20g of Cannabis", "Drug Violation (buying)")

#function to filter crime types

crime_type <- offenses %>% mutate(crimetype = case_when(offenses$narrative %in% violentcrime == TRUE ~ "Violent",
                                                        offenses$narrative %in% propertycrime == TRUE ~ "Property",
                                                        offenses$narrative %in% drugcrimes == TRUE ~ "Drug-Related",
                                                        offenses$narrative %in% violentcrime == FALSE & offenses$narrative %in% propertycrime == FALSE & offenses$narrative %in% drugcrimes == FALSE ~ "Miscellaneous"
))

crime <- awesomeIconList(
  violent <- makeAwesomeIcon('fingerprint', library = 'fa', markerColor = 'darkblue', iconColor = 'F2665E', spin = TRUE), 
  property <- makeAwesomeIcon('house-damage', library = 'fa', markerColor = 'darkblue', iconColor = 'F2665E', spin = TRUE),
  drugs <- makeAwesomeIcon('cannabis', library = 'fa', markerColor = 'darkblue', iconColor = 'F2665E', spin = TRUE),
  misc <- makeAwesomeIcon('user-secret', library = 'fa', markerColor = 'darkblue', iconColor = 'F2665E', spin = TRUE)
)

#Fire Hydrants
fire_hyd <- read.socrata("https://data.cityofgainesville.org/resource/ewpq-mxae.json")
hydrant <- makeAwesomeIcon('fire-extinguisher', library = 'fa', markerColor = 'lightgray', iconColor = 'B59D09', spin = TRUE)

# Capital Projects Inputs & Icons
Cap_Projects <- read.socrata("https://data.cityofgainesville.org/resource/qrmq-gdav.json")
icon_cproj <- makeAwesomeIcon('hard-hat', library = 'fa', markerColor = 'lightgrey', iconColor = 'FACE00', spin = TRUE)

# Collisions
ta <- read.socrata("https://data.cityofgainesville.org/resource/iecn-3sxx.json")
taccidents <- ta %>% mutate(casetype = case_when(ta$totalfatalities > 0 ~ "Fatality",
  ta$numberofpedestrians > 0 & ta$totalfatalities == 0 & ta$numberofbicyclesinvolved == 0 ~ "Pedestrian",
  ta$numberofbicyclesinvolved > 0 & ta$totalfatalities == 0 & ta$numberofpedestriansinvolved == 0 ~ "Bicycle",
  ta$numberofmopedsinvolved > 0 & ta$totalfatalities == 0 & ta$numberofpedestriansinvolved == 0 & ta$numberofbicyclesinvolved == 0  ~ "Moped",
  ta$numberofmotorcylesinvolved > 0 & ta$totalfatalities == 0 & ta$numberofpedestriansinvolved == 0 & ta$numberofbicyclesinvolved == 0 & ta$numberofmopedsinvolved == 0 ~ "Motorcycle",
  ta$numberofbusesinvolved > 0 & ta$totalfatalities == 0 & ta$numberofpedestriansinvolved == 0 & ta$numberofbicyclesinvolved == 0 & ta$numberofmopedsinvolved == 0 & ta$numberofmopedsinvolved == 0 ~ "Bus",
  ta$totalvehiclesinvolve > 0 & ta$totalfatalities == 0 & ta$numberofpedestriansinvolved == 0 & ta$numberofbicyclesinvolved == 0 & ta$numberofmopedsinvolved == 0 & ta$numberofmopedsinvolved == 0 & ta$numberofbusesinvolved == 0 ~ "Car"
))

crashtype <- unique(taccidents$casetype)

icons_crashes <- awesomeIconList(
  Bicycle = makeAwesomeIcon('bicycle', library = 'fa', markerColor = 'white', iconColor = 'F2665E', spin = TRUE),
  Pedestrian = makeAwesomeIcon('walking', library = 'fa', markerColor = 'white', iconColor = 'F2665E', spin = TRUE),
  Car = makeAwesomeIcon('cars', library = 'fa', markerColor = 'white', iconColor = 'F2665E', spin = TRUE),
  Motorcycles = makeAwesomeIcon('motorcycle', library = 'fa', markerColor = 'white', iconColor = 'F2665E', spin = TRUE),
  Mopeds = makeAwesomeIcon('motorcycle', library = 'fa', markerColor = 'white', iconColor = 'F2665E', spin = TRUE),
  Bus = makeAwesomeIcon('bus', library = 'fa', markerColor = 'white', iconColor = 'F2665E', spin = TRUE),
  Fatality = makeAwesomeIcon('skull', library = 'fa', markerColor = 'white', iconColor = 'F2665E', spin = TRUE)
)
#Active Buisnesses
active_businesses <- read.socrata("https://data.cityofgainesville.org/resource/hk2b-em59.json")
business_types <- unique(active_businesses$business_type)
businesses <- makeAwesomeIcon('badge-dollar', library = 'fa', markerColor = 'lightgray', iconColor = 'E52207', spin = TRUE)

#Building Permits: Do Categorization, similar to 311, set icons. 
building_permits <- read.socrata("https://data.cityofgainesville.org/resource/p798-x3nx.json")
permit_types <- unique(building_permits$classification)
permits <- makeAwesomeIcon('clipboard', library = 'fa', markerColor = 'white', iconColor = '67688C', spin = TRUE)

#RTS: Use GTFS Data instead
rts <- read.socrata("https://data.cityofgainesville.org/resource/w6tc-fr7z.json")
stops <- makeAwesomeIcon('bus', library = 'fa', markerColor = 'darkred', iconColor = '0F191C', spin = TRUE)

#Trash Pickup Zones
trash_zones <- read.socrata('https://data.cityofgainesville.org/resource/45px-xrah.json')

ui <- ui <- function(request) {
  navbarPage(id = "navTab",
             #Working Title
             windowTitle = "ShinyGNV Points",
             selected = "Points",
             collapsible = TRUE,
             fluid = TRUE,
             theme = shinytheme("readable"),
             #Replace Title Image
             title = HTML('<img src="burghs_eyeview_logo_small.png" alt="ShinyGNV" height="85%">'),
             position = "static-top",
             tabPanel('Points', class = "Points", value = "Points", id = "Points",
                      # Run script to determine if user is loading from a mobile device
                      tags$script(getWidth),
                      # Google Tag Manager Script to Head
                      tags$head(includeScript("tag-manager-head.js")),
                      # Notification Centered and Color Fix
                      tags$head(tags$style(type = "text/css", 
                                           ".shiny-notification {
                                                    position: fixed;
                                                    background: #074B69;
                                                    top: calc(50%);;
                                                    left: calc(50%);;
                                                    width: calc(25%);;
                                                    min-width: 200px;
                                                    transform: translate(-50%, 0);}"))),
             tags$head(tags$style(HTML(".shiny-notification-close { color: white; }"))),
             # Set favicon - REPLACE THIS
             tags$head(tags$link(rel = "icon", type = "image/png", href="favicon.png")),
             tags$head(HTML('<link rel="apple-touch-icon-precomposed" href="apple-touch-icon-precomposed.png" />
                                         <link rel="apple-touch-icon" sizes="76x76" href="apple-icon-76x76-precomposed.png" />
                                         <link rel="apple-touch-icon" sizes="114x114" href="apple-icon-120x120-precomposed.png" />
                                         <link rel="apple-touch-icon" sizes="152x152" href="apple-icon-152x152-precomposed.png" />')),
             tags$head(HTML('<!-- You can use Open Graph tags to customize link previews.
                                         Learn more: https://developers.facebook.com/docs/sharing/webmasters -->
                                         <meta property="og:url"           content="http://www.your-domain.com/your-page.html" />
                                         <meta property="og:type"          content="website" />
                                         <meta property="og:title"         content="Burgh&#39;s Eye View" />
                                         <meta property="og:description"   content="Pittsburgh&#39;s one stop shop for geographic City Data" />
                                         <meta property="og:image"         content="http://apps.pittsburghpa.gov/cis/burgh-seye-icon.png" />')),
             # Add Google Analytics Script to page
             tags$head(includeScript("google-analytics.js")),
             # Add Tag Manager Script to Body
             tags$body(tags$noscript(tags$iframe(src='https://www.googletagmanager.com/ns.html?id=GTM-TCTCQVD', height = 0, width = 0, style="display:none;visibility:hidden"))),
             # Layout CSS
             tags$style(type="text/css", ".shiny-output-error { visibility: hidden;}
                                                       .shiny-output-error:before { visibility: hidden; }
                                                       .container-fluid { padding:0; }
                                                       .navbar-header {margin:auto;}
                                                       .navbar-static-top {margin-bottom:0;}
                                                       .navbar-brand {height:60px; 
                                                                      padding:0;}
                                                       .navbar {border-right-width: 20px;
                                                                border-left-width: 65px;}
                                                       .leaflet-popup-content {overflow-y: auto; 
                                                                               max-height: 400px !important;}
                                                       .form-group {margin-bottom: 0px;}
                                                       @media only screen and (min-width: 600px) {
                                                         #map {height: calc(100vh - 55px) !important; 
                                                               z-index: 0;}
                                                         #tPanel {opacity: 0.88;
                                                                  max-height: calc(100vh - 90px);}
                                                         .btn.collapsed {display: none;}
                                                         #mobile {display: initial;}
                                                         #outer {position: relative; padding-bottom: 0px;}
                                                         #search {width: 275px;}
                                                       }
                                                       @media only screen and (max-width: 600px) {
                                                         #map {height: calc(100vh - 115px) !important;
                                                               position: absolute !important;
                                                               top: 60px;
                                                               z-index: 0;}
                                                         .mapBack {height: calc(100vh);}
                                                         #aPanel {top: 60px !important; 
                                                                  left: 0px !important; 
                                                                  width: 100% !important;}
                                                         .assetsBack {position: absolute;
                                                                      width: 100%;
                                                                      z-index: -1;
                                                                      left: 0px;
                                                                      top: 55px;}
                                                         #tPanel {margin-bottom:0px; 
                                                                  padding:0px !important; 
                                                                  overflow-y:scroll !important; 
                                                                  max-height: calc(100vh - 65) !important; 
                                                                  min-height: 55px !important; 
                                                                  padding-left: 10px !important; 
                                                                  padding-right: 10px !important;
                                                                  border: none;
                                                                  width: 100%;
                                                                  opacity: 1 !important;}
                                                         #search {width: calc(100vw - 85px) !important; margin-left:10px !important;}
                                                         #outer {margin-top: 5px !important; position: absolute;}
                                                         .btn.collapsed {display: in-line !important;}
                                                       }"),
             # Generate Map - Replace Background image
             div(class="mapBack", style='position: absolute;
                                                      background-image: url("loading.png");
                                                      background-repeat: no-repeat;
                                                      background-position: center;
                                                      background-size: contain;
                                                      width: 100%;
                                                      z-index: -1;
                                                      left: 0px;
                                                      top: 55px', 
                 leafletOutput("map")),
             absolutePanel(
               # Input panel for Desktops (alpha'd)
               top = 70, left = 50, width = '325px', style = "z-index: 1000", id = "aPanel",
               wellPanel(id = "tPanel", style = "overflow-y:auto; min-height: 65px;",
                         HTML('<div id="outer" style="z-index: 9; background-color:#1dc2ae;">'),
                         div(style="display:inline-block;", 
                             textInput("search", 
                                       value = ifelse(eDay == Sys.Date() | pDay == Sys.Date(), "Vote!", ""),
                                       label = NULL, 
                                       placeholder = "Search")),
                         tags$style(style="text/css", chartr0('#tPanel #outer .btn .fa:before { content: "\\f056";  }
                                                         #tPanel #outer .btn.collapsed .fa:before { content: "\\f055";  }')),
                         HTML('<button class="btn collapsed" data-toggle="collapse" data-target="#mobile" stye="display: block;"><i class="fa fa-search-plus" aria-hidden="true"></i></button></div>
                         <div id="mobile" class="collapse" style="margin-top:55px;">'),
                         HTML('<small style="font-size:11px;margin-left:3px">Locations are not exact. (See &rsquo;About&rsquo; for details.)</small><br><br>'),
                         dateRangeInput("dates",
                                        label = NULL,
                                        start = Sys.Date()-10,
                                        end = Sys.Date(),
                                        #Ask About Proper Start Date
                                        min = as.Date("2000-01-01"),
                                        max = Sys.Date() + 30,
                                        startview = "day"),
                         tags$br(),
                         actionButton("heatVision",
                                      label = "Enable Heat Map",
                                      icon = icon("eye")),
                         #311
                         HTML('<font color="#1BBAAA">'),
                         checkboxInput("toggle311",
                                       label = "311 Requests",
                                       value = TRUE),
                         HTML('</font>'),
                         selectInput("req.type",
                                     label = NULL,
                                     c(`Request Type`='', requests311),
                                     multiple = TRUE,
                                     selectize = TRUE),
                         selectInput("status_type",
                                     label = NULL,
                                     c(`Request Status`='', status_types),
                                     multiple = TRUE),
                         #Crime
                         HTML('<font color="#F2665E">'),
                         checkboxInput("toggleBlotter",
                                       label = "Crime",
                                       value= TRUE),
                         HTML('</font>'),
                         selectInput("crime_ind",
                                     label = NULL,
                                     c(`Crime Types`='', crime_ind),
                                     multiple = TRUE,
                                     selectize = TRUE),
                         selectInput("crime_cat",
                                     label = NULL,
                                     c(`Crime Category`='', crime_cat),
                                     multiple = TRUE,
                                     selectize = TRUE),
                         HTML('<font color= "#F2665E"'),
                         #Traffic Accidents
                         checkboxInput("toggleCitations",
                                       label = "Traffic Accidents",
                                       value = TRUE),
                         selectInput("crash_types",
                                     label = NULL,
                                     c('Accident Type'='', crashtype),
                                     multiple = TRUE, 
                                     selectize = TRUE),
                         selectInput("dow_select",
                                     label = NULL,
                                     c(`Day of the Week` = '', c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")),
                                     multiple = TRUE,
                                     selectize = TRUE),
                         sliderInput("times",
                                     label = "Collision Time (24-hour clock)",
                                     min = 0,
                                     max = 24,
                                     value = c(0,24),
                                     step = 1),
                         # Building Permits
                           HTML('<font color="#67688C">'),
                           checkboxInput("togglePermits",
                                         label = "Building Permits",
                                         value = TRUE),
                           HTML('</font>'),
                           selectInput("permit_types",
                                       label = NULL,
                                       c(`Permit Type`='', permit_types),
                                       multiple = TRUE,
                                       selectize=TRUE),
                         #Code Violations
                         HTML('<font color="#59B9DE">'),
                         checkboxInput("toggleViolations",
                                       label = "Code Violations", 
                                       value = TRUE),
                         HTML('</font>'),
                         selectInput("violation_type",
                                     label = NULL,
                                     c('Violation'='', violation_type),
                                     multiple = TRUE,
                                     selectize=TRUE),
                         #Capital Projects
                         HTML('<font color="#b9a5c1">'),
                         checkboxInput("toggleCproj",
                                       label = "Capital Projects",
                                       value = TRUE),
                         #Active Businesses
                         HTML('<font color="#E52207">'),
                         checkboxInput("toggleActBus",
                                       label = "Active Businesses",
                                       value = FALSE),
                         HTML('</font>'),
                         selectInput("Business_type",
                                     label = NULL,
                                     c(`Business Type`='', business_types),
                                     multiple = TRUE,
                                     selectize=TRUE),
                         HTML('<font>'), 
                         selectInput("basemap_select",
                                     label = "Basemap",
                                     choices = c(`OSM Mapnik` = "OpenStreetMap.Mapnik", `Gainesville mapStack` = "mapStack", `OSM France` = "OpenStreetMap.France", `OSM Humanitarian` = "OpenStreetMap.HOT", `Stamen Toner` = "Stamen.Toner", `Esri Satellite` = "Esri.WorldImagery", Esri = "Esri.WorldStreetMap", `OSM Dark Matter` = "CartoDB.DarkMatter", `OSM Positron` = "CartoDB.Positron"),
                                     selected = "OpenStreetMap.Mapnik"),

                         # Conditional Filter Panels
               
                         conditionalPanel("input.filter_select == 'Council District'",
                                          selectInput("council_select",
                                                      label = NULL,
                                                      c(`Council District`='', levels(load.ComDist$districtid)),
                                                      multiple = TRUE,
                                                      selectize=TRUE)
                         ),
                         HTML("</div>")
               )
             )
  )
  #tabPanel(a("Places", href="https://pittsburghpa.shinyapps.io/BurghsEyeViewPlaces/", style = "padding-top: 0px;
   #                       padding-bottom: 0px; bottom: 19; top: -19; bottom: 19px")),
  #tabPanel(a("Parcels", href="https://pittsburghpa.shinyapps.io/BurghsEyeViewParcels/", style = "padding-top: 0px; padding-bottom: 0px; bottom: 19; top: -19; bottom: 19px")),
 #link panel to Open Data Portal, Do we want Export Functionality?
   tabPanel('Data: Points', class = "Data: Points", value = "Data: Points",
           # Select Dataset for Export
           inputPanel(
             selectInput("report_select", 
                         tagList(shiny::icon("map-marker"), "Select Layer:"),
                         choices = c("311 Requests", "Crime", "Capital Projects", "Code Violations", "Traffic Accidents", "Fire Hydrant Locations", "Active Businesses"),
             # Define Button Position
             uiOutput("buttonStyle")
           ),
           # Clean up the Data Table CSS
           tags$style(type = "text/css", ".dataTables_length {margin-left: 10px;}"),
           tags$style(type = "text/css", ".dataTables_info {margin-left: 10px;}"),
           tags$style(type = "text/css", ".dataTables_filter {margin-right: 5px;}"),
           dataTableOutput("report.table")
  ),
  tabPanel('About', class = "About", value = "About",
           includeHTML('about.html'),
           # Twitter Button
           tags$script(HTML("var header = $('.navbar > .container-fluid > .navbar-collapse');
                                         header.append('<div class =\"twit\" style=\"float:right;margin-top: 15px;\"><a href=\"https://twitter.com/share\" class=\"twitter-share-button\" align=\"middle\" data-url=\"data.pittsburghpa.gov/BurghsEyeView\" data-text=\"Check out Burgh&#39;s Eye View! A new tool to view city data in Pittsburgh: https://goo.gl/z4cZ30\" data-size=\"large\">Tweet</a></div>');
                                         console.log(header)")),
           tags$script(HTML("!function(d,s,id){
                                         var js,fjs=d.getElementsByTagName(s)[0],p=/^http:/.test(d.location)?'http':'https';
                                         if(!d.getElementById(id)){
                                         js=d.createElement(s);
                                         js.id=id;
                                         js.src=p+'://platform.twitter.com/widgets.js';
                                         fjs.parentNode.insertBefore(js,fjs);
                                         }
                                         }(document, 'script', 'twitter-wjs');")),
           # Facebook Button
           HTML('<div id="fb-root"></div>'),
           tags$script(HTML("(function(d, s, id) {
                               var js, fjs = d.getElementsByTagName(s)[0];
                               if (d.getElementById(id)) return;
                               js = d.createElement(s); js.id = id;
                               js.src = \"//connect.facebook.net/en_US/sdk.js#xfbml=1&version=v2.8\";
                               fjs.parentNode.insertBefore(js, fjs);
                               }(document, 'script', 'facebook-jssdk'));")),
           tags$script(HTML('header.append(\'<div class="fb-share-button" style="float:right;margin-top: 15px;margin-right: 5px;" data-href="http://pittsburghpa.shinyapps.io/BurghsEyeView/?utm_source=facebook_button&amp;utm_campaign=facebook_button&amp;utm_medium=facebook%2Fsocial\" data-layout="button" data-size="large" data-mobile-iframe="true"><a class="fb-xfbml-parse-ignore" target="_blank" href="https://www.facebook.com/sharer/sharer.php?u=http%3A%2F%2Fpittsburghpa.shinyapps.io%2FBurghsEyeView%2F%23utm_source%3Dfacebook_button%26utm_campaign%3Dfacebook_button%26utm_medium%3Dfacebook%252Fsocial&amp;src=sdkpreparse">Share</a></div>\');
                               console.log(header)'))
  )
  )
}
