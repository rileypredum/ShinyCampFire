library(scales)
library(shiny)
require(shinydashboard)
library(rvest)
library(taskscheduleR)
library(ggplot2)

#terminate previous app so that new info can be generated
#HOW CAN I DO THIS?


#test dataframe to create plot in app
containment_rate <- data.frame("Date" = (c("11/08", "11/09", "11/10", "11/11", "11/12")), "Containment (%)" = c(0, 20, 40, 65, 100))

###MODEL GRAPH NOT ACCURATE DATA###
#p <- ggplot(containment_rate, aes(containment_rate$Date, containment_rate$Containment...., group="Containment (%)")) +
#  geom_line(aes(color="green", size=1)) +
#  labs(x = "Date", y = "Containment (%)") +
#  theme(legend.position="none")

#FIRST ATTEMPT AT AUTO UPDATING DATA
#campfire <- "C:/Users/riley/Desktop/Coding/R/shinycampfire/campfire.R"
#schedule this script to run daily at midnight and 12 noon
#taskscheduler_create(taskname = "grablatestdata", rscript = campfire, 
#                     schedule = "DAILY", starttime = c('11:59','23:59'), startdate=format(Sys.Date()+1, "%m/%d/%y"))

#Specifying the url for desired website to be scrapped
url <- 'http://www.fire.ca.gov/current_incidents'

air_quality_url <- 'http://www.baaqmd.gov/about-air-quality/current-air-quality/air-monitoring-data?DataViewFormat=daily&DataView=aqi&StartDate=11/14/2018&ParameterId=316'

#Reading the HTML code from the website
webpage <- read_html(url)

#got the air quality data from http://www.baaqmd.gov/about-air-quality/current-air-quality/air-monitoring-data?DataViewFormat=daily&DataView=aqi&StartDate=11/14/2018&ParameterId=316
#but it's NA for hours that have yet to be recorded and it's hard to determine the region since its one long stream
air_data_page <- read_html(air_quality_url)
air_data_html <- html_nodes(air_data_page,'.cData') %>% html_text() %>% as.integer()
air_data_html

#pull the table element containing fire information
fire_data_html <- html_nodes(webpage,'.odd:nth-child(4) .emphasized+ td') 
#specify the first one, which is Butte County (for now, this is not sustainable solution)
butte_county <- fire_data_html[1]

#Split out the HTML elements to get at the raw numbers
butte_county <- as.character(butte_county)
butte_fire <- strsplit(butte_county, ">")

#match for only numbers starting with numbers
list <- (gsub(".*?([0-9]+).", "\\1", butte_county))
list <- (gsub("(*[^0-9])", "", list))
list <- as.character(list)

#cut away that pesky two out of the numbers to get the acrage and % contained (acrage ends in zeros, the preceding nonzero int is beginning of percent contained number)
no_two <- gsub("^2", "", list)

#assign the respective values to each var, so that you have acre size of fire and percent contained
acre_size <- gsub(".[35]$", "", no_two)
acre_size <- as.integer(acre_size)

#capture last two numbers which are the percent contained
percent_contained <- gsub("?.*[0]", "", list)

#paste percentage sign to it
percent_contained <- paste(percent_contained, "%", sep="")

#----------------------------shiny app-----------------------------#


frow1 <- fluidRow(
  valueBoxOutput("value1")
  ,valueBoxOutput("value2")
)

frow2 <- fluidRow(
  box(
    title = "Fire Containment by Date"
    ,status = "primary"
    ,color='red'
    ,solidHeader = TRUE 
    ,collapsible = TRUE 
    ,plotOutput("containmentbydate", height = "300px")
  )
)


#Dashboard header carrying the title of the dashboard



# create the server functions for the dashboard  
server <- function(input, output) { 
  #load in the values
  percent_contained
  acre_size
  
  #creating the valueBoxOutput content
  output$value1 <- renderValueBox({
    valueBox(
      formatC(comma_format()(acre_size), format="d", big.mark=',')
      ,paste('Size of fire (in acres)')
      ,icon = icon("fire",lib='glyphicon')
      ,color = "olive")  
  })
  output$value2 <- renderValueBox({ 
    valueBox(
      formatC(percent_contained, format="d", big.mark=',')
      ,'Percent Contained'
      ,icon = icon("stats",lib='glyphicon')
      ,color = "red")  
  })
  
  output$containmentbydate <- renderPlot({
    p
  })
}

shinyApp(ui=dashboardPage(dashboardHeader(title='Butte County Fire Status',
                                          titleWidth=350),
                          dashboardSidebar(
                          sidebarMenu(
                            menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
                            menuItem("Data source", icon = icon("hdd",lib='glyphicon'), 
                                     href = "http://www.fire.ca.gov/current_incidents")
                          )),
                          dashboardBody(frow1), 
                          skin='red'),
                          server)
