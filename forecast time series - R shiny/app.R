library(shiny)
library(shinydashboard)
library(forecast)
library(readr)
library(rlist)
library(dplyr)

Logged = FALSE
my_username <- "time"
my_password <- "forecast"

########################################################################################################################################################################

ui <- dashboardPage(skin='blue',
                    
  dashboardHeader(title="Time Series - Forecasting Analysis",titleWidth = 340,
                  dropdownMenu(type="messages",
                               messageItem(from="Sailogesh",message="WELCOME TO the App")),
                  
                  dropdownMenu(type="notifications",
                               notificationItem(text="25 new users today",icon("users")),
                               notificationItem(text="Server load at 86%",icon=icon("exclamation-triangle"),status="warning"))
  ),
  
  
  dashboardSidebar(
    selectInput("select","choose your data dependent",choices = c("Yearly","Monthly","Daily"),selected = "Monthly"),
    numericInput("startyear","Starting Year *",value = 1958),
    numericInput("startmonth","Starting Month *",value = 01),
    numericInput("startdate","Starting Date(Optional)",value = 01),
    sliderInput("countforecast","Forecast prediction",min = 1,max = 36,value = 8),
    fileInput("file","Upload a Data",accept = c("text/csv","text/comma-separated-values,text/plain",".csv")),
    downloadButton("downloadData", "Download"),
    actionButton("myuser","Logout",icon=icon("user")),br(),
    tags$div(class = "header", checked = NA,
             tags$tbody("Need Help ?"),br(),
             tags$a(href = "https://github.com/sailogeshh", "Contact...")
    )
  ),
  dashboardBody(
    
    fluidPage(
      
      box(solidHeader = T ,width = 6,collapsible = T,collapsed = F,status = "success",
          title="Forecasting Values",tableOutput("table")),
      
      box(  h3(" Welcome - Time series forecasting"),
            h4("1. choose your data dependent-> Choose the given options based on your data type."),
            
            h4("2. Starting year -> Enter the starting year as per your data (format:yyyy)."),
            
            h4("3. starting month -> Enter the starting month as per your data."),
            
            h4("4. Starting date -> Enter the starting date as per your data(not mandotory)."),
            
            h4("5. slide option -> Choose your count of forecast prediction ?
               
               (Note : Forecast prediction output will depend on your past data.."),
            
            h4("6. Upload your data ( csv or text format only) 
               Note : Your analysing variable must be in second cloumn"),
            
            h4(" 7.Use one data one time otherwise please refersh the page and use it."),
            
            h4("8. finally Please Wait until your output displayed because it will take some time to analyse your data"),
            
            solidHeader = T,status = "danger", title="Readme before analysing your data..."
               ),
      
      box(solidHeader = T ,collapsible = T,collapsed = F,status = "primary",
          title="Forecast Plot",plotOutput("forecast")),
      
      box(solidHeader = T ,collapsible = T,collapsed = F,status = "primary",
          title="Overall Plot",plotOutput("arima")
            )),verbatimTextOutput("dataInfo")
      )
                    )
########################################################################################################################################################################       



server = function(input, output,session) {
  
  values <- reactiveValues(authenticated = FALSE)
  
  # Return the UI for a modal dialog with data selection input. If 'failed' 
  # is TRUE, then display a message that the previous value was invalid.
  dataModal <- function(failed = FALSE) {
    modalDialog(title = "Welcome...",
      textInput("username", "Username:"),
      passwordInput("password", "Password:"),
      footer = tagList(
        #modalButton("Cancel"),
        actionButton("ok", "OK")
      )
    )
  }
  
  # Show modal when button is clicked.  
  # This `observe` is suspended only whith right user credential
  
  obs1 <- observe({
    showModal(dataModal())
  })
  
  # When OK button is pressed, attempt to authenticate. If successful,
  # remove the modal. 
  
  obs2 <- observe({
    req(input$ok)
    isolate({
      Username <- input$username
      Password <- input$password
    })
    Id.username <- which(my_username == Username)
    Id.password <- which(my_password == Password)
    if (length(Id.username) > 0 & length(Id.password) > 0) {
      if (Id.username == Id.password) {
        Logged <<- TRUE
        values$authenticated <- TRUE
        obs1$suspend()
        removeModal()
        
      } else {
        values$authenticated <- FALSE
      }     
    } 
  })
  

  

  dataModal2 <- function(failed = FALSE) {
      modalDialog(fade = FALSE,title = tagList(h3("Thank You !!")),footer = NULL,
                  tags$div(class = "header", checked = NA,
                           tags$h4("Visit us for more..."),
                           tags$a(href = "https://github.com/sailogeshh", "Sai Logesh")
                  )
      )
  }
  

   obs4 <- observe({
    if(Logged <<- TRUE)
    req(input$myuser)
    showModal(dataModal2())
  })
   
   
   
  output$dataInfo <- renderPrint({
    
    out<-reactive({
      file1 <- input$file
      if(is.null(file1)) {return(NULL)}
      file3 <- read.csv(file1$datapath ,header=TRUE)
      withProgress(message='Your data is Loading please wait untill the outputs are displayed..',value=30,{
        n<-10 
        
        for(i in 1:n){
          incProgress(1/n,detail=paste("Doing Part", i, "out of", n))
          Sys.sleep(0.1)
        }
      }) 
      
      if (input$select == "Monthly") {  
        cpi <- ts(file3[,2],start=c(input$startyear,input$startmonth),frequency=12)
      }
      else if (input$select == "Daily") {
        cpi <- ts(file3[,2],start=c(input$startyear,input$startmonth),frequency=365)
      }
      else {
        cpi <- ts(file3[,2],start=c(input$startyear,input$startmonth),frequency=1)
      }
      
      arimafit1=auto.arima(cpi,stepwise=FALSE,approx=FALSE)
      new=forecast(arimafit1,h=input$countforecast)
      #Forecast_Month=c("Jan-2018","Feb-2018","Mar-2018","April-2018","May-2018","June-2018","July-2018","Aug-2018","Sep-2018","Oct-2018","Nov-2018","Dec-2018")
      final_table=data.frame(new[4])
      final_table$Forecast_value <- final_table$mean
      final_table <- data.frame(final_table[2])
      final_table
    })
    
    forecastplot<-reactive({
      file1 <- input$file
      if(is.null(file1)) {return(NULL)}
      file3 <- read.csv(file1$datapath ,header=TRUE)
      withProgress(message='Your data is Loading please wait untill the outputs are displayed..',value=30,{
        n<-10  
        
        for(i in 1:n){
          incProgress(1/n,detail=paste("Doing Part", i, "out of", n))
          Sys.sleep(0.1)
        }
      }) 
     
      
      if (input$select == "Monthly")
      {  cpi <- ts(file3[,2],start=c(input$startyear,input$startmonth),frequency=12)
      }
      else if (input$select == "Daily") {
        cpi <- ts(file3[,2],start=c(input$startyear,input$startmonth),frequency=365)
      }
      else {
        cpi <- ts(file3[,2],start=c(input$startyear,input$startmonth),frequency=1)
      }
      
      arimafit1=auto.arima(cpi,stepwise=FALSE,approx=FALSE)
      new=forecast(arimafit1,h=input$countforecast)
      final_table=data.frame(new[4])
      final_table$Forecast_value <- final_table$mean
      final_table <- data.frame(final_table[2])
      plot(final_table, main = "Forecast Plot")
    })
    
    arimaplot <-reactive({
      file1 <- input$file
      if(is.null(file1)) {return(NULL)}
      file3 <- read.csv(file1$datapath ,header=TRUE)
      withProgress(message='Your data is Loading please wait untill the outputs are displayed..',value=30,{
        n<-10  
        
        for(i in 1:n){
          incProgress(1/n,detail=paste("Doing Part", i, "out of", n))
          Sys.sleep(0.1)
        }
      }) 
      #file3=read.csv("time series data.csv",header = T)  
      
      if (input$select == "Monthly")
      {  cpi <- ts(file3[,2],start=c(input$startyear,input$startmonth),frequency=12)
      }
      else if (input$select == "Daily") {
        cpi <- ts(file3[,2],start=c(input$startyear,input$startmonth),frequency=365)
      }
      else {
        cpi <- ts(file3[,2],start=c(input$startyear,input$startmonth),frequency=1)
      }
      
      arimafit1=auto.arima(cpi,stepwise=FALSE,approx=FALSE)
      new=forecast(arimafit1,h=input$countforecast)
      plot(new, main = "Overall Plot")
    })
    
    
    output$table = renderTable ({
      out()
    })
    
    output$forecast = renderPlot ({
      forecastplot()
    })
    
    output$arima = renderPlot ({
      arimaplot()
    })
    
    output$downloadData <- downloadHandler(
      filename = function() {
        paste("Arima", ".csv", sep = "")
      },
      content = function(file) {
        write.csv(pred(), file, row.names = FALSE)
      }
    )
    
 
  })
  
}

############################################################################################################################################################

shinyApp(ui,server)
  
  