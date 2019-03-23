# must login to save data, why is the perfchart each row not get saved incharacters table
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
#SET WORKING DIRECTORY TO DEVAPP
#jsTest.R is also referring to jstyping3.js and it only will save timestamps and characters when user
#keypresses inside the textarea.  app2.R only will save timestamps when user keypresses inside text area
# but will save characters when user is keypressing in the login area
library(shiny)
library(shinyjs)
library(ggplot2)
library(data.table)
library(stringr)
library(stringdist)
library(dplyr)
source("read_more_text.R")
# to use loadTXT function: if you have  txt file "cats.txt", and you want it's variable name to be "cats"
# loadTXT("cats.txt","cats")    <--- Run this in the console
# then in the selectInput function, add "Cats Paragraph Title " = "cats" (has to match variable name)
source("appMySQL.R")
load("paragraphs.RData")

fields=c("source","pnum")
ui <- fluidPage(useShinyjs(),
                
                # loads javascript file that tracks keystroke times
                tags$head(tags$script(src="jstyping3.js")),
                
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
                              actionButton("load_paragraph","start"),
                             uiOutput("username"),
                              uiOutput("password"),
                              actionButton("login_button","Login")
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
                               DT::dataTableOutput("history", width = 300),
                               plotOutput("distPlot"),
                               plotOutput("meanPlot"),
                               tableOutput("charTime"),
                               tableOutput("renderAccuracy"),
                               tableOutput("renderFormData"),
                               actionButton("saveResults","Save Results"),
                               textOutput("loginPrint"),
                               textInput("sessionNum","Session to Analyze:"),
                               DT::dataTableOutput("history2", width = 300),
                               tableOutput("renderCharacters")
                             )
                           )
                  ),
                  tabPanel("Register",
                           sidebarLayout(
                             sidebarPanel (
                               # this is referring to the regUser render ui function
                               uiOutput("regUser"),
                               uiOutput("regPw"),
                               actionButton("register","Register")
                             ),
                             mainPanel()
                           )
                           )
                )
                

)

# Define server logic required to draw a histogram
server <- function(input, output) {
  #txtSrc is the 100 word paragraph from the list of allParagraphs that pnum selects
  txtSrc=reactive({allParagraphs[[which(names(allParagraphs)==input$source)]]})
  #outSlider creates a slider that returns user selected paragraph number (pnum)
  
  output$outSlider=renderUI(
    {
     sliderInput("pnum","Paragraph #:",min=1,max=length(txtSrc()),
                 value=1)
      
    }
    )
  #some pargraph will be printed in the UI, it is selected again from txtSrc
  output$some_paragraph<- renderUI({
    return(txtSrc()[input$pnum])
  })
  # textInput box for username input$username will refer to whatever is typed in the box
  output$username=renderUI({
    textInput("username","Username: ","")
  })
  # textInput box for password input$password will refer to whatever is typed in the box
  output$password=renderUI({
    textInput("password","Password:","")
  })
  #$username  will constantly store whatever is in the username text box
  username=reactive({input$username})
  #$password  will constantly store whatever is in the password text box
  password=reactive({input$password})
  #check if userpw table in MySQL contains this username and pw pair ONLY WHEN user clicks login button
  #check login returns 0 if no matches, this function will catch this and tell the user
  observeEvent(input$login_button,{if(as.character(checkLogin(username(),password()))=="0")
    print("Incorrect Username And/Or Password")
    })
  #input$regUser is the text inside the output$regUser, 
  registerUser=reactive({input$regUser})
  registerPw=reactive({input$regPw})
  #the regUser tag, 
  output$regUser=renderUI({textInput("regUser","Username:","")})
  output$regPw=renderUI({textInput("regPw","Password:","")})
 #exists will store the result of the count command to find any existing usernames
  #register will execute the instertion but also give back exists which contains 0 if no matches
  #if not 0 then tell user there is a match and the function automatically skips insertion
   observeEvent(input$register, {
    if(registerUser()==""||registerPw()=="")
    {
      showModal(modalDialog(
        title = "Error",
        "One of the fields is empty.",
        easyClose = TRUE,
        footer = NULL
      ))
    }
    else
    {
      exists=register(registerUser(),registerPw())
     print(as.character(exists))
      if(as.character(exists)!="0")
      { showModal(modalDialog(
        title = "Error",
        "Username already exists. Try different username.",
        easyClose = TRUE,
        footer = NULL
      ))
      }
      else{
        showModal(modalDialog(
          title = "Success!",
          "Registration successful!  Login at the Typing Tab.",
          easyClose = TRUE,
          footer = NULL
        ))
      }
    }
      
    
  })
  
  output$loginPrint=renderPrint({username()})
  # create a shiny reactive variable that will
  # receive javascript timestamps from keystrokes
  recent_typing_times <-reactive({
    return(input$typing_times)
  })
  # recent_chars is a vector of letters & corresponds to js_chars in jstyping.js
  recent_chars = reactive({
      return(input$chars)
  })
  #endTime will store an integer from the last d.getTime() call in js
  endTime=reactive({
    return(input$finalTimeStamp)
  })
  
  # trigger function to update R with javascript timestamps
  # when this action button is pressed
  # trigger function to update recent_chars with js_chars 
  # when this action button is pressed
  #triggger function to update finalTimeStamp with the last time stamp as the endTime in our sessions table
  observeEvent(input$get_typing_times,{
    runjs('update_typing_times();')
    runjs('update_chars();')
    runjs('update_finalTimeStamp();')
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
  # performanceChart is a dataframe with the row header:
  # Character typed, Word that the character was a part of, Word Count, Word User actually typed,
  # Edit Distance between Correct Word & User's Typed Word, IKSI of the Character typed
  performanceChart=reactive({
    #correctPara is the 100 word vector 
    correctPara=txtSrc()[input$pnum]
    # correctwords is each word in correctPara
    correctWords=strsplit(correctPara,split=" ")[[1]]
    #IKSIs is the vector of IKSIs
    IKSIs= recent_typing_times()
    # Line up typed characters with the Word #
    #characters is every letter the user typed
    characters = recent_chars()
    wordCountVector=c()
  wordcount=1
  for (i in 1:(length(characters)-1))
  { #word count is only increased when a letter follows a space
    if(characters[i]==" "&&characters[i+1]!=" ")
    {
      wordcount=wordcount+1
    }   
    
    wordCountVector=c(wordCountVector,wordcount)
  }
  wordCountVector=c(wordCountVector,wordcount)
  
  
  # Line up Correct Words with the Word Number
  # line up each characters to their word which is repeated for each charatter 
  performance=data.frame(characters,wordCountVector)
  performance$characters=as.character(performance$characters)
  # only the number of words that the user attempted will be drawn from the correct paragraph
  #and analyzed in performance chart
  performance$word= correctWords[wordCountVector]
  #for each space surrounded character string user typed, make words out of them 
  typedWords=performance %>% 
    group_by(wordCountVector) %>%
    summarise(typed=paste(characters[characters!=" "],collapse=""))
  performance$typed=typedWords[wordCountVector,]$typed
  # calculate edit distance between typed string and correct word
  
  #if any of the typed words match the next correctWords, increase all subsequent wrdCntVctrs by 1 
  # once skipi count catches it will change all subsequents but not the original point deviation
  
  
  perfSum=performance %>%
    group_by(wordCountVector)%>%
    summarise(length=n()-1,word=word[1],typed=typed[1])
  perfSum[1,]$length=perfSum[1,]$length+1
  
  
  skipCount=0
  for(i in 1:nrow(perfSum))
  {currentWordCount=perfSum[i,]$wordCountVector
    if(currentWordCount<length(correctWords) && stringdist(perfSum[i,]$typed,correctWords[currentWordCount+1],method="dl")<2)
    skipCount=skipCount+1
  else
    skipCount=0
  
  if(currentWordCount>1 && currentWordCount<=length(correctWords) && stringdist(perfSum[i,]$typed,correctWords[currentWordCount-1],method="dl")<2)
    lagcount=lagcount+1
  else
    lagcount=0
  
  if(skipCount==2)
  {# when the typed word is matching the +1 correct word too well, change the wCV from that point on
    perfSum[(i-skipCount+1):nrow(perfSum),]$wordCountVector=perfSum[(i-skipCount+1):nrow(perfSum),]$wordCountVector+1
    # and get the new words that match the wCV from the correct Words list 
    perfSum$word=correctWords[perfSum$wordCountVector]
    skipCount=0
    #reset to 0 after every catch so that 1 forward type error doesnt have push forward wvc 2 words
  }
  if(i >1 && lagcount==2)
  {# when the typed word is matching the +1 correct word too well, change the wCV from that point on
    perfSum[(i-lagcount+1):nrow(perfSum),]$wordCountVector=perfSum[(i-lagcount+1):nrow(perfSum),]$wordCountVector-1
    # and get the new words that match the wCV from the correct Words list 
    perfSum$word=correctWords[perfSum$wordCountVector]
    lagcount=0
    #reset to 0 after every catch so that 1 forward type error doesnt have push forward wvc 2 words
  }
  }
  
  
  # make it put the edit Dists back in the performance chart
  performance$wordCountVector=rep(perfSum$wordCountVector,perfSum$length+1)[-1]
  performance$word= correctWords[performance$wordCountVector]
  performance = performance %>%
    mutate(editDist=stringdist(typed,word,method="dl"))
  performance$mean_IKSI=IKSIs
  performance
  })
  #charTime is a render function that the ui will call to print performanceChart
  output$charTime = renderTable({
    performanceChart()
  })
# accuracy is a number that represents the average errorRate of the session 
  accuracy=reactive({
  performance=performanceChart()
    perfSum=performance %>%
      group_by(wordCountVector)%>%
      summarise(length=n()-1,editDist=mean(editDist),word=word[1])
    perfSum[1,]$length=perfSum[1,]$length+1
    errorRate = mean(perfSum$editDist)/mean(perfSum$length)
    return(errorRate)
  })
  # displays the accuracy
  output$renderAccuracy=renderTable({
    
    1-accuracy()
  })
  
  output$renderFormData=renderTable({

    # sessionsData is a named vector that will hold the input$source & pnum which have to be gotten with
    # the sapply, the fields vector holds the variable names we want sapply to get the values of
    sessionsData=sapply(fields, function(x) input[[x]])
    sessionsData=c(username(),endTime(),sessionsData)
    names(sessionsData)=c("username","endtime","source","pnum")
    saveData(sessionsData,"sessions")
    sessionsData})
    output$renderCharacters=renderTable({
    #data is a NAMED vector that represents each record in the MySQL table characters
    #the names of each value (names(data)=c(...)) is the column name in the characters table
    #saveData will create a SQL query as a string and execute it
    results=performanceChart()
    sess_ID=loadSessionId()
    for(i in 1:nrow(results))
    {data=results[i,]
    data=c(data,sess_ID)
    #add session_id to the field names, SQL query to get the most recent serial id and put it here
    # as an integer
    
    names(data)=c("characters","wordCountVector","word","typed","editDist","mean_IKSI","session_id")
    
    saveData(data,"characters")  
    }
  })
  output$history= DT::renderDataTable({
    
    loadData(username(),"sessions","username")
  }) 
  sessionId=reactive({input$sessionNum})
  output$history2=DT::renderDataTable({
    
    loadData(sessionId(),"characters","session_id")
  }) 

}

# Run the application 
shinyApp(ui = ui, server = server)


