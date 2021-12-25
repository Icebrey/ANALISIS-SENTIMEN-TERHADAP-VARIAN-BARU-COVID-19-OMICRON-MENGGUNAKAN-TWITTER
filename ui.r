library(shiny)
library(shinydashboard)
library(DT)
library(ggplot2)
library(wordcloud)
library(tm)
library(plotly)
library(plotrix)

data <- read.csv("Data/Hasil Sentimen Analisis.csv", header = TRUE)

# ui ---------------------------------------------------------------------------

ui <- dashboardPage(
  
  # title ----
  dashboardHeader(title = "Sentiment Analysis"),
  
  # sidebar ----
  dashboardSidebar(
    sidebarMenu(id = "sidebarid",
                menuItem("Sentiment Analysis", tabName = "page1"),
                menuItem("Bar Plot", tabName = "page2"),
                menuItem("Word Cloud", tabName = "page3"),
                menuItem("Pie Chart", tabName = "page4")
    )
  ),
  
  # body ----
  dashboardBody(
    tabItems(
      # page 1 ----
      tabItem(tabName = "page1",selectInput("select", "Select columns to display", names(data), multiple = TRUE),
              h2('sentiment analysis data'),
              br(),br(), dataTableOutput('table')),
      # page 2 ----
      tabItem(tabName = "page2",br(),br(),plotOutput("hist")),
      # page 3 ----
      tabItem(tabName = "page3",br(),br(), plotOutput("wordcloud") ),
      # page 4 ----
      tabItem(tabName = "page4",br(),br(), fluidRow(
        box(plotlyOutput("pie", height = "400px", width = "600px"))   
      ) )
    )
  )
)

# server -----------------------------------------------------------------------

server <- function(input, output, session) {
  
  word_freq <- read.csv("Data/Freq Word.csv", header = TRUE)
  word_freq2 <- read.csv("Data/Freq Word2.csv", header = TRUE)
  
  output$table = renderDataTable({
    columns = names(data)
    if (!is.null(input$select)) {
      columns = input$select
    }
    data[,columns,drop=FALSE]
  })
  
  output$hist <- renderPlot({
    ggplot(word_freq2, aes(y=word_freq2$Number, x=word_freq2$Word)) + 
      xlab("Word") + ylab("Word Frequency") +
      geom_bar(position="dodge", stat="identity")
  })
  
  output$wordcloud <- renderPlot({
    wordcloud(word_freq$Word, word_freq$Number, random.order=FALSE, colors=brewer.pal(6, "Dark2"),
              min.freq=1, scale=c(8,0.8),rot.per=.15,max.words=100)
  })
  
  POSP <- length(which(data$label == "POSITIVE"))/length(data$label)
  NEGP <- length(which(data$label == "NEGATIVE"))/length(data$label)
  ##Combine calculated values in new data frame
  W1P <- data.frame(PRP = c(POSP, NEGP), cat = c("POSITIVE", "NEGATIVE"))
  W1P$cat <- factor(W1P$cat,
                    levels = c("POSITIVE", "NEGATIVE"))
  
  output$pie <- renderPlotly({
    plot_ly(W1P, labels = W1P$cat, values = W1P$PRP, type = 'pie',
                    sort = FALSE,                    #Sort according to data set
                    direction = "clockwise",         #Arrange the variable orders
                    textposition = 'inside',
                    textinfo = 'label + percent',
                    insidetextfont = list(color = '#FFFFF'),
                    marker = list(colors = c('blue', 'red'),
                                  line = list(color = '#FFFFF', width = 0.25))) %>%
      layout(title = 'Pie Chart Sentiment Analysis', showlegend = TRUE,
             xaxis = list(showgrid = FALSE, zeroline = FALSE, 'showticklabels' = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, 'showticklabels' = FALSE))
  })
  
}

# shiny app --------------------------------------------------------------------

shinyApp(ui, server)