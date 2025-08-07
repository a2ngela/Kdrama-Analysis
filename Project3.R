#
# Project 3 - Angela Zhao
#

library(shiny)

kdramavars <- read.csv('kdramavars.csv')


# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Kdramas"),
  
  # Sidebar 
  sidebarLayout(
    sidebarPanel(
      "The kdrama dataset contains information about the top 250 kdramas based on ratings from MyDramaList as of January 2023.",
      br(), br(),
      
      #add image
      tags$img(
        src = "https://1.vikiplatform.com/c/40242c/938e643317.jpg?x=b&s=480x270&e=t&q=g",
        width = "80%"), 
      br(),
      p('Source: “The Family Is Coming: Korea: Drama: Watch with English Subtitles &amp; More ✔️.” Rakuten Viki, 3 Jan. 2015, www.viki.com/tv/40242c-the-family-is-coming. '),
      
      hr(),
      
      
      "Customize the graph using the options below: ",
      br(), br(),
      
      
      #allow user to change variable 
      selectInput("variable", label=h4("Choose a Variable:"),
                   choices=list("Duration"=1,"Days Aired"=2,"Number of Episodes" = 3, "Rating and Number of Episodes" = 4, "Days Aired and Rating" = 5), 
                   selected = 1),

      
      #Filter data by rating range
      sliderInput("filter", label = h4("Filter by Rating: "), min = 8.3, 
                  max = 9.2, value = c(8.3, 9.2)),
      
      
      #option to show descriptive stats 
      checkboxInput("stats", label="Display descriptive statistics", value=FALSE),
      
      
      #change color of numeric graphs
      selectInput("color", label=h4("Choose a Color: (numeric vars only)"),
                  choices=list("Purple"='lavender',"Blue"='lightcyan',"Yellow" = 'lemonchiffon', "Green" = 'darkseagreen1'), 
                  selected = 'lavender')
    ),
    
    
    
    # Display graph and descriptive statistics
    mainPanel(
      plotOutput("distPlot"),
      hr(),
      verbatimTextOutput("stattype"),
      verbatimTextOutput("stats")
    )
  )
)



server <- function(input, output) {
  
  #histogram output
  output$distPlot <- renderPlot({
    # draw histogram based on input options
    if (input$variable == 1) #Duration
      hist(kdramavars$Duration[kdramavars$Rating>input$filter[1] & kdramavars$Rating<input$filter[2]],main='Distribution of Duration of Kdramas',xlab='Duration (mins)',col=input$color,border='darkgrey')
    else if(input$variable == 2) #Days Aired
      barplot(table(kdramavars$Aired[kdramavars$Rating>input$filter[1] & kdramavars$Rating<input$filter[2]]),main='Distribution of Days Aired of Kdramas',xlab='Day(s) of the Week Aired',col=input$color,border='darkgrey')
    else if(input$variable == 3) #Num Episodes
      hist(kdramavars$NumEpisodes[kdramavars$Rating>input$filter[1] & kdramavars$Rating<input$filter[2]],main='Distribution of Number of Episodes of Kdramas',xlab='Number of Episodes',col=input$color,border='darkgrey')
    else if(input$variable == 4){ #Rating and NumEpisodes
      par(bg = 'darkgray')
      plot(kdramavars$Rating[kdramavars$Rating>input$filter[1] & kdramavars$Rating<input$filter[2]],kdramavars$NumEpisodes[kdramavars$Rating>input$filter[1] & kdramavars$Rating<input$filter[2]],main='Rating and Number of Episodes',xlab='Rating',ylab='Number of Episodes',col=input$color)
    } 
    else #Rating and Days Aired
      ggplot(filter(kdramavars,Rating > input$filter[1] & Rating < input$filter[2]), aes(x=Rating,fill=Aired)) + geom_histogram(position='identity',bins=45,col='black') + theme_classic() + labs(title='Rating of Kdramas by Day Aired',x='Rating')
  })
  
  #Display type of statistic if selected
  output$stattype <- renderPrint({
    if (input$stats == TRUE) {
      if (input$variable == 1)
        print("Five number summary(min,Q1,mean,Q2,max): ")
      else if (input$variable == 2)
        print("Proportion Table: ")
      else if (input$variable == 3)
        print("Five number summary(min,Q1,mean,Q2,max): ")
      else if (input$variable == 4)
        print("Correlation: ")
      else
        print("Median Rating: ")
    }
  })
  
  #Display descriptive stats if selected
  output$stats <- renderPrint({ 
    if (input$stats == TRUE) {
      if (input$variable == 1)
        #duration: five number summary
        fivenum(kdramavars$Duration[kdramavars$Rating>input$filter[1] & kdramavars$Rating<input$filter[2]])
      else if (input$variable == 2)
        #days aired: proportion table
        round(prop.table(table(kdramavars$Aired[kdramavars$Rating>input$filter[1] & kdramavars$Rating<input$filter[2]])),3)
      else if (input$variable == 3)
        #Num Episodes: five num summary
        fivenum(kdramavars$NumEpisodes[kdramavars$Rating>input$filter[1] & kdramavars$Rating<input$filter[2]])
      else if (input$variable == 4)
        #Rating and Num Episodes: correlation
        round(cor(kdramavars$Rating[kdramavars$Rating>input$filter[1] & kdramavars$Rating<input$filter[2]],kdramavars$NumEpisodes[kdramavars$Rating>input$filter[1] & kdramavars$Rating<input$filter[2]]),2)
      else {
        #Days Aired and Rating: median rating
        aggregate(Rating ~ Aired, data = filter(kdramavars,Rating > input$filter[1] & Rating < input$filter[2]), FUN = median)
      }
    }
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
