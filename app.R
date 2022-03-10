##**Discussion**

#For my Shiny dashboard, I wanted to provide visualizations and tables on tornado activity, obtained from the Storm Events Database that is created and maintained by the National Oceanic and Atmospheric Administration (NOAA).  The Storm Events Database can be accessed here: [Storm Events Database](https://www.ncdc.noaa.gov/stormevents/ftp.jsp).  On the left side of the page, one can access the [NWS Documentation PDF](https://www.nws.noaa.gov/directives/sym/pd01016005curr.pdf), which provides detailed information on how this data is compiled and in-depth guidance on the terminology used.  The actual CSV files I downloaded are [here](https://www.ncei.noaa.gov/pub/data/swdi/stormevents/csvfiles/) - note that I chose to limit my analysis to the past decade, so I utilized the "details" files for years 2012 - 2021.  To replicate this work, one would first need to download those 10 files and save them as variables year_xxxx, where xxxx is the 4 digit calendar year of the csv being loaded into R.  Another helpful PDF is available on this page, titled [Storm-Data-Bulk-csv-Format](https://www.ncei.noaa.gov/pub/data/swdi/stormevents/csvfiles/Storm-Data-Bulk-csv-Format.pdf) - this PDf provides quick explanations for the variables in the csv files.

#The Storm Events Database houses information on storm activity throughout the United States, going all the way back to 1950.  There is a lot of information available within these files, but I chose to narrow my focus to only analyze tornado activity, in order to avoid having too complicated or messy figures (and because I have always been fascinated by tornadoes). 

#I wanted viewers of the dashboard to be able to see information on tornadoes through a variety of ways; I chose to use a mapping visualization to show locations of tornado activity, and gave the user the ability to filter by year in order to keep the map from becoming too cluttered.  On the same tab, I included a table that uses the same filtering input, to show detailed information for the same year as the tornadoes captured on the map.  The search functionality of the table allows a user to see a tornado on the map and look it up by the Tornado ID in the table, to see additional information.  On the second tab, I chose to do a time series visualization, which shows deaths, injuries, property damage, and crop damage over time; this graphic also has a filter that allows the user to drill down into specific states, or to view the whole country if they want to.  Again, I included a table below this visualization that also runs on the same "state" filter, and allows users to gain more information from the table on specific dates and times.  On the last tab, I chose to include a dendogram (collapsible tree) that shows information on the "worst" tornadoes of each year, where "worst" can be categorized a few different ways; this allows users to focus in on the most "interesting" or catastrophic tornadoes, if they want to.

#load necessary packages
library(shiny)
library(shinydashboard)
library(plotly)
library(collapsibleTree)
library(dplyr)
library(readr)
library(scales)
library(tidyr)
library(tidyverse)
library(leaflet)
library(DT)
library(stringr)

#force R not to convert numeric values to scientific notation
options(scipen = 999)

#load all files
year_2012 <- read.csv("StormEvents_details-ftp_v1.0_d2012_c20220107.csv.gz") %>%
    select(YEAR, EVENT_TYPE, EVENT_ID, STATE, CZ_NAME, BEGIN_DATE_TIME, CZ_TIMEZONE, INJURIES_DIRECT, INJURIES_INDIRECT, DEATHS_DIRECT, DEATHS_INDIRECT, DAMAGE_PROPERTY, DAMAGE_CROPS, BEGIN_LAT, BEGIN_LON, TOR_F_SCALE, TOR_LENGTH, TOR_WIDTH, EPISODE_NARRATIVE, SOURCE) %>%
    filter(EVENT_TYPE == "Tornado")
year_2013 <- read.csv("StormEvents_details-ftp_v1.0_d2013_c20220124.csv.gz") %>%
    select(YEAR, EVENT_TYPE, EVENT_ID, STATE, CZ_NAME, BEGIN_DATE_TIME, CZ_TIMEZONE, INJURIES_DIRECT, INJURIES_INDIRECT, DEATHS_DIRECT, DEATHS_INDIRECT, DAMAGE_PROPERTY, DAMAGE_CROPS, BEGIN_LAT, BEGIN_LON, TOR_F_SCALE, TOR_LENGTH, TOR_WIDTH, EPISODE_NARRATIVE, SOURCE) %>%
    filter(EVENT_TYPE == "Tornado")
year_2014 <- read.csv("StormEvents_details-ftp_v1.0_d2014_c20211217.csv.gz") %>%
    select(YEAR, EVENT_TYPE, EVENT_ID, STATE, CZ_NAME, BEGIN_DATE_TIME, CZ_TIMEZONE, INJURIES_DIRECT, INJURIES_INDIRECT, DEATHS_DIRECT, DEATHS_INDIRECT, DAMAGE_PROPERTY, DAMAGE_CROPS, BEGIN_LAT, BEGIN_LON, TOR_F_SCALE, TOR_LENGTH, TOR_WIDTH, EPISODE_NARRATIVE, SOURCE) %>%
    filter(EVENT_TYPE == "Tornado")
year_2015 <- read.csv("StormEvents_details-ftp_v1.0_d2015_c20211217.csv.gz") %>%
    select(YEAR, EVENT_TYPE, EVENT_ID, STATE, CZ_NAME, BEGIN_DATE_TIME, CZ_TIMEZONE, INJURIES_DIRECT, INJURIES_INDIRECT, DEATHS_DIRECT, DEATHS_INDIRECT, DAMAGE_PROPERTY, DAMAGE_CROPS, BEGIN_LAT, BEGIN_LON, TOR_F_SCALE, TOR_LENGTH, TOR_WIDTH, EPISODE_NARRATIVE, SOURCE) %>%
    filter(EVENT_TYPE == "Tornado")
year_2016 <- read.csv("StormEvents_details-ftp_v1.0_d2016_c20211217.csv.gz") %>%
    select(YEAR, EVENT_TYPE, EVENT_ID, STATE, CZ_NAME, BEGIN_DATE_TIME, CZ_TIMEZONE, INJURIES_DIRECT, INJURIES_INDIRECT, DEATHS_DIRECT, DEATHS_INDIRECT, DAMAGE_PROPERTY, DAMAGE_CROPS, BEGIN_LAT, BEGIN_LON, TOR_F_SCALE, TOR_LENGTH, TOR_WIDTH, EPISODE_NARRATIVE, SOURCE) %>%
    filter(EVENT_TYPE == "Tornado")
year_2017 <- read.csv("StormEvents_details-ftp_v1.0_d2017_c20220124.csv.gz") %>%
    select(YEAR, EVENT_TYPE, EVENT_ID, STATE, CZ_NAME, BEGIN_DATE_TIME, CZ_TIMEZONE, INJURIES_DIRECT, INJURIES_INDIRECT, DEATHS_DIRECT, DEATHS_INDIRECT, DAMAGE_PROPERTY, DAMAGE_CROPS, BEGIN_LAT, BEGIN_LON, TOR_F_SCALE, TOR_LENGTH, TOR_WIDTH, EPISODE_NARRATIVE, SOURCE) %>%
    filter(EVENT_TYPE == "Tornado")
year_2018 <- read.csv("StormEvents_details-ftp_v1.0_d2018_c20220124.csv.gz") %>%
    select(YEAR, EVENT_TYPE, EVENT_ID, STATE, CZ_NAME, BEGIN_DATE_TIME, CZ_TIMEZONE, INJURIES_DIRECT, INJURIES_INDIRECT, DEATHS_DIRECT, DEATHS_INDIRECT, DAMAGE_PROPERTY, DAMAGE_CROPS, BEGIN_LAT, BEGIN_LON, TOR_F_SCALE, TOR_LENGTH, TOR_WIDTH, EPISODE_NARRATIVE, SOURCE) %>%
    filter(EVENT_TYPE == "Tornado")
year_2019 <- read.csv("StormEvents_details-ftp_v1.0_d2019_c20220124.csv.gz") %>%
    select(YEAR, EVENT_TYPE, EVENT_ID, STATE, CZ_NAME, BEGIN_DATE_TIME, CZ_TIMEZONE, INJURIES_DIRECT, INJURIES_INDIRECT, DEATHS_DIRECT, DEATHS_INDIRECT, DAMAGE_PROPERTY, DAMAGE_CROPS, BEGIN_LAT, BEGIN_LON, TOR_F_SCALE, TOR_LENGTH, TOR_WIDTH, EPISODE_NARRATIVE, SOURCE) %>%
    filter(EVENT_TYPE == "Tornado")
year_2020 <- read.csv("StormEvents_details-ftp_v1.0_d2020_c20220124.csv.gz") %>%
    select(YEAR, EVENT_TYPE, EVENT_ID, STATE, CZ_NAME, BEGIN_DATE_TIME, CZ_TIMEZONE, INJURIES_DIRECT, INJURIES_INDIRECT, DEATHS_DIRECT, DEATHS_INDIRECT, DAMAGE_PROPERTY, DAMAGE_CROPS, BEGIN_LAT, BEGIN_LON, TOR_F_SCALE, TOR_LENGTH, TOR_WIDTH, EPISODE_NARRATIVE, SOURCE) %>%
    filter(EVENT_TYPE == "Tornado")
year_2021 <- read.csv("StormEvents_details-ftp_v1.0_d2021_c20220124.csv.gz") %>%
    select(YEAR, EVENT_TYPE, EVENT_ID, STATE, CZ_NAME, BEGIN_DATE_TIME, CZ_TIMEZONE, INJURIES_DIRECT, INJURIES_INDIRECT, DEATHS_DIRECT, DEATHS_INDIRECT, DAMAGE_PROPERTY, DAMAGE_CROPS, BEGIN_LAT, BEGIN_LON, TOR_F_SCALE, TOR_LENGTH, TOR_WIDTH, EPISODE_NARRATIVE, SOURCE) %>%
    filter(EVENT_TYPE == "Tornado")

all_years <- rbind(year_2012, year_2013, year_2014, year_2015, year_2016, year_2017, year_2018, year_2019, year_2020, year_2021)

#Data cleaning and preparation for all visualizations; The csv files obtained from the NOAA have the property damage and crop damage variables as character fields, with amounts shown with K for thousands, M for millions, and B for billions.  Here I convert these two fields to numeric by identifying the presence of a K, M, or B, and multiplying the numeric portion of the string accordingly to get the right value.  Then, I replace any NA values within the full data frame with a 0.  Last, I create new columns that are the sum of the direct and indirect deaths and injuries, to provide columns for total deaths and total injuries.

all_years$DAMAGE_PROPERTY <- dplyr::case_when(
    stringr::str_detect(all_years$DAMAGE_PROPERTY, 'K') ~ readr::parse_number(all_years$DAMAGE_PROPERTY) * 1000,
    stringr::str_detect(all_years$DAMAGE_PROPERTY, 'M') ~ readr::parse_number(all_years$DAMAGE_PROPERTY) * 1000000,
    stringr::str_detect(all_years$DAMAGE_PROPERTY, 'B') ~ readr::parse_number(all_years$DAMAGE_PROPERTY) * 1000000000,
    TRUE ~ parse_number(all_years$DAMAGE_PROPERTY)
)

all_years$DAMAGE_CROPS <- dplyr::case_when(
    stringr::str_detect(all_years$DAMAGE_CROPS, 'K') ~ readr::parse_number(all_years$DAMAGE_CROPS) * 1000,
    stringr::str_detect(all_years$DAMAGE_CROPS, 'M') ~ readr::parse_number(all_years$DAMAGE_CROPS) * 1000000,
    stringr::str_detect(all_years$DAMAGE_CROPS, 'B') ~ readr::parse_number(all_years$DAMAGE_CROPS) * 1000000000,
    TRUE ~ parse_number(all_years$DAMAGE_CROPS)
)

all_years[is.na(all_years)] = 0

#combine direct and indirect deaths to have one value for total number of deaths
x <- all_years %>%
    select(DEATHS_DIRECT, DEATHS_INDIRECT)
combined_deaths <- mutate(x, TOTAL_DEATHS = rowSums(x)) %>%
    select(TOTAL_DEATHS)
all_years <- cbind(all_years, combined_deaths)

#combine direct and indirect injuries to have one value for total number of injuries
y <- all_years %>%
    select(INJURIES_DIRECT, INJURIES_INDIRECT)
combined_injuries <- mutate(y, TOTAL_INJURIES = rowSums(y)) %>%
    select(TOTAL_INJURIES)
all_years <- cbind(all_years, combined_injuries)


#Create specifics for collapsible tree

crops <- all_years %>%
    group_by(YEAR) %>%
    filter(DAMAGE_CROPS == max(DAMAGE_CROPS)) %>%
    mutate(loss_type = "Highest Crop Damage")

property <- all_years %>%
    group_by(YEAR) %>%
    filter(DAMAGE_PROPERTY == max(DAMAGE_PROPERTY)) %>%
    mutate(loss_type = "Highest Property Damage")

deaths <- all_years %>%
    group_by(YEAR) %>%
    filter(TOTAL_DEATHS == max(TOTAL_DEATHS)) %>%
    mutate(loss_type = "Highest # of Fatalities")

injuries <- all_years %>%
    group_by(YEAR) %>%
    filter(TOTAL_INJURIES == max(TOTAL_INJURIES)) %>%
    mutate(loss_type = "Highest # of Injuries")

Tornadoes <- rbind(crops, property, deaths, injuries)

Tornadoes$DAMAGE_CROPS <- dollar(Tornadoes$DAMAGE_CROPS)
Tornadoes$DAMAGE_PROPERTY <- dollar(Tornadoes$DAMAGE_PROPERTY)

Tornadoes$Location <- paste(Tornadoes$CZ_NAME, Tornadoes$STATE, sep=", ")
Tornadoes$Beginning_Date_and_Time <- paste(Tornadoes$BEGIN_DATE_TIME, Tornadoes$CZ_TIMEZONE, sep=" ")
Tornadoes$EVENT_ID <- paste("Tornado ID #", Tornadoes$EVENT_ID, sep="")

Tornadoes <- Tornadoes %>%
    select(YEAR, loss_type, EVENT_ID, Location, Beginning_Date_and_Time, TOTAL_DEATHS, TOTAL_INJURIES, DAMAGE_PROPERTY, DAMAGE_CROPS)

Tornadoes$TOTAL_DEATHS <- as.character(Tornadoes$TOTAL_DEATHS)
Tornadoes$TOTAL_INJURIES <- as.character(Tornadoes$TOTAL_INJURIES)

colnames(Tornadoes) <- c("Year", "Loss Type", "Event ID", "Location", "Beginning Date and Time", "# of Fatalities", "# of Injuries", "Property Damage", "Crop Damage")

Tornadoes <- Tornadoes %>%
    pivot_longer(
        cols = c("Location", "Beginning Date and Time", "# of Fatalities", "# of Injuries", "Property Damage", "Crop Damage"),
        names_to = "Details",
        names_sep = NULL,
        values_to = "Information")

title_color <- "saddlebrown"
year_color <- as.matrix(rep("sandybrown", times=10))
loss_type_color <- as.matrix(rep("darkkhaki", times = 40))
event_id_color <- as.matrix(rep("darkolivegreen", times = 45))
details_color <- as.matrix(rep("darkseagreen", times = 270))
information_color <- as.matrix(rep("white", times = 270))
node_colors <- rbind(title_color, year_color, loss_type_color, event_id_color, details_color, information_color)

#Create specifics for leaflet map and the tables

all_years$TOR_F_SCALE = dplyr::case_when(
    all_years$TOR_F_SCALE == "EF0" ~ "EF0 (40-72 MPH)",
    all_years$TOR_F_SCALE == "EF1" ~ "EF1 (73-112 MPH)",
    all_years$TOR_F_SCALE == "EF2" ~ "EF2 (113-157 MPH)",
    all_years$TOR_F_SCALE == "EF3" ~ "EF3 (158-206 MPH)",
    all_years$TOR_F_SCALE == "EF4" ~ "EF4 (207-260 MPH)",
    all_years$TOR_F_SCALE == "EF5" ~ "EF5 (261-318 MPH)",
    TRUE ~ "Unknown")

#I also combine the beginning date and time column with the timezone columns, to have more complete time information
all_years$Beginning_Date_and_Time <- paste(all_years$BEGIN_DATE_TIME, all_years$CZ_TIMEZONE, sep=" ")

colnames(all_years) <-  c("Year", "Event Type", "Tornado ID", "State", "CZ Name", "BEGIN_DATE_TIME", "CZ Timezone", "Direct Injuries", "Indirect Injuries", "Direct Deaths", "Indirect Deaths", "Property Damage", "Crop Damage", "LAT", "LON", "Enhanced Fujita Scale", "TOR_LENGTH", "TOR_WIDTH", "Episode Narrative", "Source", "Total Deaths", "Total Injuries", "Begin Date and Time")

#Create specifics for the time series visualization

full_country <- all_years %>%
    mutate(begin_date = as.Date(BEGIN_DATE_TIME, "%d-%b-%y")) %>%
    select(begin_date, `Property Damage`, `Crop Damage`, `Total Deaths`, `Total Injuries`) %>%
    group_by(begin_date) %>%
    summarise(injuries = sum(`Total Injuries`), deaths = sum(`Total Deaths`), property_damage = sum(`Property Damage`), crop_damage = sum(`Crop Damage`)) %>%
    mutate(State = "ALL")

each_state <- all_years %>%
    mutate(begin_date = as.Date(BEGIN_DATE_TIME, "%d-%b-%y")) %>%
    select(begin_date, State, `Property Damage`, `Crop Damage`, `Total Deaths`, `Total Injuries`) %>%
    group_by(State, begin_date) %>%
    summarise(injuries = sum(`Total Injuries`), deaths = sum(`Total Deaths`), property_damage = sum(`Property Damage`), crop_damage = sum(`Crop Damage`))

time_series <- rbind(full_country, each_state)

#Create the Shiny Dashboard

ui <- dashboardPage(
    dashboardHeader(
        title="Tornadoes"
    ),
    dashboardSidebar(
        sidebarMenu(
            menuItem("Map of Tornado Activity by Year", tabName="Map"),
            menuItem("Time Series by State", tabName="Time"),
            menuItem("Worst Tornadoes of Each Year", tabName="worst")
        )
    ),
    dashboardBody(
        tabItems(
            tabItem("Time",
                    box(selectInput("state", "State:", choices = c("ALL", "ALABAMA", "ARIZONA", "ARKANSAS", "CALIFORNIA", "COLORADO", "CONNECTICUT", "DELAWARE", "DISTRICT OF COLUMBIA", "FLORIDA", "GEORGIA", "HAWAII", "IDAHO", "ILLINOIS", "INDIANA", "IOWA", "KANSAS", "KENTUCKY", "LOUISIANA", "MAINE", "MARYLAND", "MASSACHUSETTS", "MICHIGAN", "MINNESOTA", "MISSISSIPPI", "MISSOURI", "MONTANA", "NEBRASKA", "NEVADA", "NEW HAMPSHIRE", "NEW JERSEY", "NEW MEXICO", "NEW YORK", "NORTH CAROLINA", "NORTH DAKOTA", "OHIO", "OKLAHOMA", "OREGON", "PENNSYLVANIA", "PUERTO RICO", "RHODE ISLAND", "SOUTH CAROLINA", "SOUTH DAKOTA", "TENNESSEE", "TEXAS", "UTAH", "VERMONT", "VIRGIN ISLANDS", "VIRGINIA", "WASHINGTON", "WEST VIRGINIA", "WISCONSIN", "WYOMING")),
                        br(),
                        plotlyOutput("time"), width=500),
                    box(dataTableOutput("statetable"), width = 500)
            ),
            tabItem("Map",
                    box(selectInput("year", "Year:", choices = c("2012", "2013", "2014", "2015", "2016", "2017", "2018", "2019", "2020", "2021"))),
                    leafletOutput("mapping", height=500),
                    box(dataTableOutput("yeartable"), width = 500)),
            tabItem("worst",
                    collapsibleTreeOutput("dendogram", height = 700),
                    p("The number of injuries includes direct and indirect injuries caused by the tornado; the same is true for the number of deaths.")
            ))
    ))

server <- function(input, output) {
    output$dendogram <- renderCollapsibleTree({
        collapsibleTree(
            Tornadoes,
            hierarchy = c("Year", "Loss Type", "Event ID", "Details", "Information"),
            root = "The Worst Tornadoes of Each Year",
            fill = node_colors,
            fillByLevel = TRUE,
            width = 1500,
            height = 1500,
            fontSize = 17,
            zoomable = FALSE
        )
    })
    
    output$time <- renderPlotly({
        filter_state <- input$state
        to_graph <- time_series %>% filter(State == filter_state)
        
        deaths_plot <- plot_ly(data = to_graph, x = ~begin_date) %>%
            add_lines(y = ~deaths, name = "Deaths")
        
        injuries_plot <- plot_ly(data = to_graph, x = ~begin_date) %>%
            add_lines(y = ~injuries, name = "Injuries")
        
        property_plot <- plot_ly(data = to_graph, x = ~begin_date)%>%
            add_lines(y = ~property_damage, name = "Property Damage")
        
        crop_plot <- plot_ly(data = to_graph, x = ~begin_date)%>%
            add_lines(y = ~crop_damage, name = "Crop Damage")
        
        all_figs <- subplot(deaths_plot, injuries_plot, property_plot, crop_plot, nrows = 4, shareX = TRUE) %>%
            rangeslider() %>%
            layout(hovermode = "x") %>%
            layout(xaxis = list(title = "Date"))
        
        all_figs
    })
    
    output$mapping <- renderLeaflet({
        filter_year <- input$year
        to_map <- all_years %>% filter(Year == filter_year)
        to_map$`Crop Damage` <- dollar(to_map$`Crop Damage`)
        to_map$`Property Damage` <- dollar(to_map$`Property Damage`)
        labs <- lapply(seq(nrow(to_map)),
                       function(i) {
                           paste0("Tornado ID: ", as.character(to_map[i, "Tornado ID"]), '<br>',
                                  "Begin Date and Time: ", as.character(to_map[i, "Begin Date and Time"]), '<br>',
                                  "Enhanced Fujita Scale: ", as.character(to_map[i, "Enhanced Fujita Scale"]), '<br>',
                                  "Tornado Length (in miles): ", as.character(to_map[i, "TOR_LENGTH"]), '<br>',
                                  "Tornado Width (in feet): ", as.character(to_map[i, "TOR_WIDTH"]), '<br>',
                                  "Total Deaths: ", as.character(to_map[i, "Total Deaths"]), '<br>',
                                  "Total Injuries: ", as.character(to_map[i, "Total Injuries"]), '<br>',
                                  "Property Damage: ", as.character(to_map[i, "Property Damage"]), '<br>',
                                  "Crop Damage: ", as.character(to_map[i, "Crop Damage"]), '<br>')
                       })
        to_map %>% leaflet() %>%
            addProviderTiles(providers$Esri.WorldStreetMap, group = "Street Map") %>%
            addMarkers(label = ~lapply(labs, htmltools::HTML),
                       clusterOptions = markerClusterOptions())
    })
    
    output$yeartable <- renderDataTable({
        filter_year <- input$year
        year_table <- all_years %>%
            filter(Year == filter_year) %>%
            select(`Tornado ID`, State, `Begin Date and Time`, `Direct Injuries`, `Indirect Injuries`, `Direct Deaths`, `Indirect Deaths`, `Property Damage`, `Crop Damage`, `Enhanced Fujita Scale`, `Episode Narrative`, Source)
        year_table$`Crop Damage` <- dollar(year_table$`Crop Damage`)
        year_table$`Property Damage` <- dollar(year_table$`Property Damage`)
        year_table <- datatable(year_table, options = list(
            autoWidth = TRUE,
            columnDefs = list(list(width="350px", targets=11), list(width="250px", targets=3), list(width="150px", targets=10),
                              list(className = 'dt-center', targets=1:9)),
            scrollX = 200, scrollY=400))
        year_table
    })
    
    output$statetable <- renderDataTable({
        filter_state <- input$state
        state_table <- all_years %>%
            filter(State == filter_state) %>%
            select(`Tornado ID`, Year, `Begin Date and Time`, `Direct Injuries`, `Indirect Injuries`, `Direct Deaths`, `Indirect Deaths`, `Property Damage`, `Crop Damage`, `Enhanced Fujita Scale`, `Episode Narrative`, Source)
        state_table$`Crop Damage` <- dollar(state_table$`Crop Damage`)
        state_table$`Property Damage` <- dollar(state_table$`Property Damage`)
        state_table <- datatable(state_table, options = list(
            autoWidth = TRUE,
            columnDefs = list(list(width="350px", targets=11), list(width="250px", targets=3), list(width="150px", targets=10),
                              list(className = 'dt-center', targets=1:9)),
            scrollX = 200, scrollY=400))
        state_table
    })
}

shinyApp(ui, server)