
packages <- c("jiebaR", "wordcloud2", "htmlwidgets", "dplyr", "shiny", "shinythemes", "shinyjs", "shinyBS", "stringr", "tidyverse")

lapply(packages,library,character.only = TRUE)

# Sys.setlocale("LC_ALL","chinese")

options(shiny.maxRequestSize=30*1024^2) 
options(warn = -1)

jsResetCode <- "shinyjs.reset = function() {history.go(0)}"

ui <- shinyUI(fluidPage(
  useShinyjs(),
  extendShinyjs(text = jsResetCode, functions = "reset"),
  
  tags$head(tags$link(rel="stylesheet", type="text/css",href="styles.css")),
  # shinythemes::themeSelector(),
  theme = shinytheme("superhero"),
  
  titlePanel(title = div("Chinese Word Cloud Application", img(src="gc_logo.png", height=80, width=160, style = "float:right; padding-right:25px"))),
  
  sidebarLayout(
    
    sidebarPanel(
      tags$h4("File Upload Section:"),
      br(),
      
      fileInput("file", "Upload the Text File"),
      
      fileInput("stop_file", "Upload the Stop Words File"),
      
      bsTooltip("stop_file", "You can add any stop words into the stop words template downloaded from below section, then upload here", placement = "bottom", trigger = "hover"),
      
      fileInput("dict_file", "Upload the User Defined Dictionary File"),
      
      bsTooltip("dict_file", "You can add Chinese terms into the dictionary template downloaded from below section, then upload here", placement = "bottom", trigger = "hover"),
      
      helpText("Max File Size is 30 MB,", 
               "the Uploaded Data File Must Be .txt Format with UTF-8 Encoding.",
               "Please Save Your Stopwords and Dictionary File in UTF-8 Encoding With the First Line As Blank Using Notepad."),
      
      hr(),
      
      tags$h4("Template Download Section:"),
      br(),
      
      downloadLink("download_stopwords","Stopwords Template"),
      bsTooltip("download_stopwords", "This stopwords template contains common Chinese stop words", placement = "bottom", trigger = "hover"),
      
      tags$h5("  "),
      downloadLink("download_dictionary","Dictionary Template"),
      bsTooltip("download_dictionary", "This dictionary template is used for word cloud NOT classified by word attributes", placement = "bottom", trigger = "hover"),
      
      tags$h5("  "),
      downloadLink("download_dictionary_tag","Tagged Dictionary Template"),
      bsTooltip("download_dictionary_tag", "This dictionary template is used for word cloud classified by word attributes", placement = "bottom", trigger = "hover"),
      
      hr(),
      
      tags$h4("Word Cloud Customization Section:"),
      br(),
      
      uiOutput("pop_slider"),
      
      sliderInput("size", "Word Size:", min = 0, max = 2, value = 1, step = 0.1),
      
      checkboxInput("flag", "Word Cloud by Attributes", value = FALSE),
      
      bsTooltip("flag", "Check it only if you want your word cloud classified by word attributes", placement = "bottom", trigger = "hover"),
      
      uiOutput("pop_dropdown_menu"),
      
      selectInput("shape", "Word Cloud Shape:", choices = c("circle","cardioid","diamond","triangle-forward","triangle","pentagon","star"), selected = "circle"),
      
      selectInput("color", "Word Color:", choices = c("random-dark","random-light"), selected = "random-light"),
      
      selectInput("backgroundcolor", "Background Color:", choices = c("white","black"), selected = "black"),
      hr(),
      tags$h4("Restart Application:"),
      br(),
      
      actionButton("reset_button", "Reset")
    ),
    
    mainPanel(
      
      uiOutput("tb")
    )
  )
)
)


server <- shinyServer(function(input, output, session){
  
  observeEvent(input$reset_button, {js$reset()})
  
  output$pop_dropdown_menu <- renderUI({
    if (input$flag == FALSE) {return(NULL)}
    
    if (input$flag == TRUE) {
      list(selectInput("word_attr", "Word Attribute:", choices = c("n","a","v"), selected = "n"))
    }
  })
  
  output$pop_slider <- renderUI({
    file1 <- input$file
    
    if(is.null(file1)){return()}
    
    file2 <- input$stop_file
    
    file3 <- input$dict_file
    
    if(is.null(file2)){return()}
    
    if(is.null(file3)){return()}
    
    max_freq <- max(data_file()$freq)
    
    sliderInput("frequency", "Minimum Word Frequency:", min = 1, max = max_freq, value = 1)
  })
  
  
  data_file <- reactive({
    
    file1 <- input$file
    
    if(is.null(file1)){return()} 
    
    content <- paste(readLines(file1$datapath), collapse=" ")
    
    # engine_user_stop <- worker(user='C://Users//CNU074VP//Documents//dictionary.dat', stop_word = "C://Users//CNU074VP//Documents//stopwords.dat")
    
    file2 <- input$stop_file
    
    file3 <- input$dict_file
    
    if(is.null(file2)){return()}
    
    if(is.null(file3)){return()}
    
    
    if (input$flag == TRUE) {
      engine_user_stop <- worker(type = "tag", user = file3$datapath, stop_word = file2$datapath)
      
      tag_result <- tagging(content,engine_user_stop)
      
      tag_table <- enframe(tag_result) 
      
      tag_names <- filter(tag_table, name == input$word_attr)
      
      word_frequency <- tag_names %>%
        count(value) %>%
        arrange(desc(n))
      
      # word_frequency <- count(tag_names$value) %>%
      #   arrange(desc(freq))
      
      colnames(word_frequency)[1] <- "word"
      colnames(word_frequency)[2] <- "freq"
      
      word_frequency
    }
    else {
      engine_user_stop <- worker(user = file3$datapath, stop_word = file2$datapath)
      
      segWords <- segment(content, engine_user_stop)
      
      segWords<-gsub("[0-9a-zA-Z]+?","",segWords)
      
      segWords <- str_trim(segWords)
      
      segWords <- segWords[segWords != ""]
      
      word_frequency <- freq(segWords)
      
      word_frequency
    }
    
  })
  
  wc <- reactive ({
    wc_rep <- repeatable(wordcloud2)
    
    if (input$flag == TRUE) {
      
      filter(data_file(), freq>=input$frequency) %>%
        
        wc_rep(size = input$size, shape=input$shape,color = input$color, backgroundColor = input$backgroundcolor, fontFamily = "微软雅黑")
    }
    else {
      
      filter(data_file()[order(data_file()$freq, decreasing = T), ], freq>=input$frequency) %>%
        
        wc_rep(size = input$size, shape=input$shape,color = input$color, backgroundColor = input$backgroundcolor, fontFamily = "微软雅黑")
    }
  })
  
  # wc_rep <- repeatable(wordcloud2)
  
  
  output$wc <- renderWordcloud2({
     wc()
    # if (input$flag == TRUE) {
    #   
    #   filter(data_file(), freq>=input$frequency) %>%
    #     
    #     wc_rep(size = input$size, shape=input$shape,color = input$color, backgroundColor = input$backgroundcolor, fontFamily = "微软雅黑")
    # }
    # else {
    #   filter(data_file()[order(data_file()$freq, decreasing = T), ], freq>=input$frequency) %>%
    #     
    #     wc_rep(size = input$size, shape=input$shape,color = input$color, backgroundColor = input$backgroundcolor, fontFamily = "微软雅黑") 
    # }
  })
  
  output$caption <- renderText({paste("This APP is developed by Jason Yang. It is used to generate Chinese word cloud. For any technical issue, please contact Jason, thank you!")})
  
  output$download_stopwords <- downloadHandler(
    filename = function() {
      paste("stopwords.dat")
    },
    content = function(file) {
      file.copy("www/stopwords.dat", file, overwrite=TRUE)
    }
  )
  
  output$download_dictionary <- downloadHandler(
    filename = function() {
      paste("dictionary.dat")
    },
    content = function(file) {
      file.copy("www/dictionary.dat", file, overwrite=TRUE)
    }
  )
  
  output$download_dictionary_tag <- downloadHandler(
    filename = function() {
      paste("user.dict.utf8")
    },
    content = function(file) {
      file.copy("www/user.dict.utf8", file, overwrite=TRUE)
    }
  )
  
  # output$download_wc <- downloadHandler(
  #   filename = function() {
  #     paste("wordcloud-", Sys.Date(), ".html", sep= "")
  #   },
  #   
  #   content = function(file) {
  #     # file.copy("wordcloud.png", file, overwrite=TRUE)
  #     # ggsave(file, plot = wc(), device = "png")
  #     ggsave(file, wordcloud2(data = filter(data_file(), freq>=input$frequency),
  #                             size = input$size, shape=input$shape,color = input$color, 
  #                             backgroundColor = input$backgroundcolor, fontFamily = "微软雅黑"
  #     ), device = "png")
  #   }
  # )
  
  # output$download_wc <- downloadHandler(
  #   filename = paste("wordcloud-", Sys.Date(), ".png", sep= ""),
  #   
  #   content = function(file) {
  #     owd <- setwd(tempdir())
  #     on.exit(setwd(owd))
  #     saveWidget(wc(), "temp.html", selfcontained = FALSE)
  #     webshot("temp.html", delay =5, file = file, cliprect = "viewport")
  #   }
  # )
  
  
  output$tb <- renderUI({
    if(is.null(data_file()))
      h5("Powered by", tags$img(src='R_logo.png', height=200, width=200), tags$img(src='aws3.png', heigth=200, width=400))
    else
      tabsetPanel(tabPanel("Word Cloud", wordcloud2Output("wc", width = "100%", height = "580px")), 
                  tabPanel("About the APP", textOutput("caption")))
  })
  
}  
)

shinyApp(ui = ui, server = server)


