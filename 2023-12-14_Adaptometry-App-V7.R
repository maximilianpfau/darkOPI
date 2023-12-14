library(shiny)

library(magick)
library(ggplot2)
library(OPI)

library(tidyverse)


library(bslib)

thematic::thematic_shiny() 


# 1. Load functions  --------------------------------------------------------

source("R/2022-11-04_functions.R")


# 2. App ------------------------------------------------------------------




# Define UI  ----
ui <- fluidPage(
  
  # Set theme to dark mode
  theme = bs_theme(bg = "rgb(0, 0, 0)", fg = "rgb(255, 0, 0)", 
                   primary = "#000000", secondary = "#FF0000", success = "#FF0000"),
  
  # App title ----
  headerPanel("Dark Adaptometry App"),
  
  sidebarPanel(
    # Sidebar panel for inputs ----
  
    
    actionButton(inputId = "initialize",
                 label = "1. Initialize"),
    
    actionButton(inputId = "applySettings",
                 label = "2. Apply settings"),
    
    textInput("firstname", "Firstname:", value = ""),
    textInput("lastname", "Lastname:", value = ""),
    
    selectInput("laterality", "Laterality:",
                c("OD" = "OD",
                  "OS" = "OS")),
    
    actionButton(inputId = "open",
                 label = "3. Open"),
    
    # Input of Grid
    fileInput("gridFile", "Select grid: Choose CSV File",
              accept = c(
                "text/csv",
                "text/comma-separated-values,text/plain",
                ".csv")
    ),
    
    # Action Buttons
    actionButton(inputId = "savetime",
                 label = "4. Save time"),  
    
    # Between Exam Time
    numericInput('breakinterval','Break [seconds]:',value=1,min=0,max=99999,step=1),
    
    textOutput('timeleft'),
    
    # Action Buttons
    actionButton(inputId = "runtest",
                 label = "5. Run test"),
    actionButton(inputId = "stoptest",
                 label = "Pause test"),
    
  ),
  
  # Main panel for displaying outputs ----
  mainPanel(
    plotOutput("plot1"),
    plotOutput("plot3")
    
  )
)




# Define server logic to plot various variables against mpg ----
server <- function(input, output, session) {
  
  
  # Connect to device 
  observeEvent(input$initialize, {
    maia.opiInitialize(ip = "192.168.1.2", port=5555)
  })
    
  # Apply fixation
  observeEvent(input$applySettings, {
    
    change_fixation <- function() {
      writeLines(paste("OPI-SET-FIXATION",0,0,0,256), .OpiEnv$maia$socket)
      res <- readLines(.OpiEnv$maia$socket, n=1)
      s <- strsplit(res, " ", fixed=TRUE)[[1]]
      if (s[1] != 0) {
        return(list(error=paste("opiSetBackground: failed to set fixation: ", s[1])))
      }
    }
    
    try(change_fixation(), silent = TRUE)
    
  })
    
  # OPI open
  observeEvent(input$open, {
    initiation <<- maia.opiOpen(.OpiEnv$maia$socket)
    
    # Plot fundus image
    irfp <<- image_read(initiation$image)
    
    plot1 <<- image_ggplot(irfp) +
      geom_point(aes(x=1024/2 + 1024 / 36.5 * initiation$prl[1] ,
                     y=1024/2 - 1024 / 36.5 * initiation$prl[2] ),
                 color="red") +
      geom_point(aes(x=1024/2 + 1024 / 36.5 * initiation$onh[1] ,
                     y=1024/2 - 1024 / 36.5 * initiation$onh[2] ),
                 color="green")
    output$plot1 <- renderPlot({plot1})
    
  })
  
  
  observeEvent(input$savetime, {
    # Create folder and save initiation data and image
    
    timestamp = format(Sys.time(), '%Y-%m-%d_%H-%M-%S')
    
    new_dir <<- paste(input$firstname, input$lastname, input$laterality, timestamp, sep='_')
    
    new_dir <<- paste("results/", new_dir, sep="")
    
    dir.create(new_dir)
    
    saveRDS(initiation, paste(new_dir, 'initiation.RDS', sep="/"))
    image_write(irfp, paste(new_dir, 'fundus.png', sep="/"))
    })
  
  
  
  # Run the perimetry test
  rv <- reactiveVal(FALSE)
  observeEvent(input$runtest, {rv(TRUE)})
  observeEvent(input$stoptest, {rv(FALSE)})
  
  #observeEvent(input$runtest, {
  observe({
    invalidateLater(1000, session)
    if(rv()){
    
    # Get the perimetry grid
    inputGrid <<- read.csv(input$gridFile$datapath)
   
    
    # Adaptometry test
    
    
    # Create or clean currentResults
    if (!exists('currentResults')) {
      currentResults <<- inputGrid %>%
        mutate(stairResult=initial)
    } else {
      currentResults <<- currentResults %>%
        select(ID, x, y, tt, initial, color, stairResult)
    }
    
    
    # Run test
    currentResults <<- currentResults %>%
      group_by(ID) %>%
      nest() %>%
      mutate(state = map(data, ~fiveOne.allSteps(.$stairResult[1],
                                                 .$x[1],
                                                 .$y[1],
                                                 .$color[1])) )
    # Clean results
    currentResults <<- currentResults %>%
      unnest(data) %>%
      ungroup() %>%
      mutate(n = map(ID, ~ state[[.x]]$numPresentations),
             stimuli = map(ID, ~ state[[.x]]$stimuli),
             responses = map(ID, ~ state[[.x]]$responses),
             responseTimes = map(ID, ~ state[[.x]]$responseTimes),
             stairResult = map(ID, ~ state[[.x]]$stairResult),
             finalTimeStamp = map(ID, ~ state[[.x]]$finalTimeStamp) ) %>%
      unnest(c(n, stairResult))
    
    
    # Append all results
    if (!exists('allResults')) {allResults <<- currentResults} else {
      allResults <<- rbind(allResults, currentResults)}
    
    
    # Save the results
    timestamp <- format(Sys.time(), '%Y-%m-%d_%H-%M-%S')
    
    saveRDS(allResults, paste(new_dir,
                              paste(timestamp , 'allResults.RDS', sep="_"),
                              sep="/"))
    

    # Display results
    
    plot3 <<- allResults %>%
      unnest(finalTimeStamp) %>%
      mutate(time = as.numeric(difftime(finalTimeStamp, min(finalTimeStamp)))/60 ) %>%
      ggplot(aes(x=time, y=stairResult, group=ID, shape=color)) + geom_point(alpha=.5) + geom_line(alpha=.5) +
      ylab('Sensitivity [dB]') + xlab('Time [min]')
    
    output$plot3 <- renderPlot({plot3})
    
    # Break Timer
    withProgress(message = 'Break', {
      for(i in 1:input$breakinterval){
        
        # Sleep 1 s and Update progress
        Sys.sleep(1)
        incProgress(1/input$breakinterval)
        
      } })
    
  }})

} # Close server logic


shinyApp(ui, server)