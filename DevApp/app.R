#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
#SET WORKING DIRECTORY TO DEVAPP

library(shiny)
library(shinyjs)
library(ggplot2)
library(data.table)
source("read_more_text.R")
# to use loadTXT function: if you have  txt file "cats.txt", and you want it's variable name to be "cats"
# loadTXT("cats.txt","cats")    <--- Run this in the console
# then in the selectInput function, add "Cats Paragraph Title " = "cats" (has to match variable name)
load("paragraphs.RData")

ui <- fluidPage(useShinyjs(),
                
                # loads javascript file that tracks keystroke times
                tags$head(tags$script(src="jstyping.js")),
                
                tabsetPanel(
                  tabPanel("Typing",
                           # Sidebar with a slider input for number of bins 
                           sidebarLayout(
                             sidebarPanel(
                               selectInput(inputId="source",
                                           label="Text Source:",
                                           choices=c("Alice in Wonderland"="alice","Cats Simple Wiki" ="cats",
                                                     "Trigram Structured English"="trigram",
                                                     "Bigram Structured English"="bigram",
                                                     "Letter Frequency English"="unigram",
                                                     "Random Letters"="random",
                                                     "Chinese New Year" = "newyear"),
                                           selected = "Alice in Wonderland"),
                               uiOutput("outSlider"),
                              actionButton("load_paragraph","start")
                             ),
                             
                             # Show a plot of the generated distribution
                             mainPanel(
                               p(uiOutput("some_paragraph")),
                               textAreaInput("input_typing", "type here", value = "", width = 500, height = 200,cols = NULL, rows = 10, placeholder = NULL, resize = NULL)
                              
                               )
                           )
                  ),
                  tabPanel("Performance",
                           # Sidebar with a slider input for number of bins 
                           sidebarLayout(
                             sidebarPanel(
                               actionButton("get_typing_times","get results"),
                               sliderInput("bins",
                                           "Number of bins:",
                                           min = 1,
                                           max = 50,
                                           value = 30)
                             ),
                             
                             # Show a plot of the generated distribution
                             mainPanel(
                               plotOutput("distPlot"),
                               plotOutput("meanPlot")
                             )
                           )
                  )
                )
                

)

# Define server logic required to draw a histogram
server <- function(input, output) {
  output$outSlider=renderUI(
    {txtSrc=allParagraphs[[which(names(allParagraphs)==input$source)]]
     sliderInput("pnum","Paragraph #:",min=1,max=length(txtSrc),
                 value=1)
      
    }
    )
  
  output$some_paragraph<- renderUI({
    txtSrc=allParagraphs[[which(names(allParagraphs)==input$source)]]
    return(txtSrc[input$pnum])
  })
  
  # create a shiny reactive variable that will
  # receive javascript timestamps from keystrokes
  recent_typing_times <-reactive({
    return(input$typing_times)
  })
  
  # trigger function to update R with javascript timestamps
  # when this action button is pressed
  observeEvent(input$get_typing_times,{
    runjs('update_typing_times();')
  })
  
  # plot a histogram of IKSIs
  # note, this will automatically update whenver the
  # values in recent_typing_times are changed 
  # this occurs when button is pressed
  output$distPlot <- renderPlot({
    # generate bins based on input$bins from ui.R
    x    <- recent_typing_times()
    x <- x[x<1000]
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    
    # draw the histogram with the specified number of bins
    hist(x, breaks = bins, col = 'darkgray', border = 'white')
  })
  
  output$meanPlot <-renderPlot({
    IKSIs <- recent_typing_times()
    mean_IKSI <-mean(IKSIs)
    sd_IKSI <- sd(IKSIs)
    plot_df<-data.frame(values=c(mean_IKSI,
                                 sd_IKSI),
                        type = c("mean","sd"))
    ggplot(plot_df, aes(x=type,y=values))+
      geom_bar(stat="identity")
    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)


