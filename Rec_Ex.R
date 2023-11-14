setwd("C:/Users/MarquezL/OneDrive - Kantar/Projects/RecruitmentExercise/")
library(shiny)
library(dplyr)
library(data.table)
library(stats)
library(ggplot2)

# Load the adstock data
df <- as.data.table(read.csv("data.csv"))

##########################################################################################

# Define UI for application
ui <- fluidPage(
  theme = bslib::bs_theme(bootswatch = "united"),
  titlePanel("Kantar Recruitment Exercise - Luisa Marquez Renteria"),
        sliderInput("adstock", 
                   "Retention Factor", 
                   0.5, 
                   min = 0, 
                   max = 1, 
                   step = 0.1),
       
      mainPanel(
        fluidRow(
          column(6, 
                 fluidRow(
                   column(12, plotOutput("ModelSamp"))
                 )
          ),
          column(6, 
                 tableOutput("modeltable"),
                 textOutput("fitmodel")
          )
        ),
        fluidRow(
          column(6, plotOutput("decay")),
          column(6, plotOutput("plot1"))
        ),
        textOutput("decayexp"),
        fluidRow(
          column(6, 
                 tableOutput("table"),
                 textOutput("searchdollar")
          ),
          column(6, 
                 fluidRow(
                   column(12, plotOutput("barplot"))
                 )
          )
        )
    )
  )


# Define server logic
server <- function(input, output) {
   
  adstock <- function(x, rate) {
    n <- length(x)
    y <- numeric(n)
    for (i in 1:n) {
      y[i] <- x[i] + (rate * ifelse(i > 1, y[i - 1], 0))
    }
    y
  }
  
  output$table <- renderTable({
    df <- aggregate(cbind(Media_Spend, Search_Volume) ~ Media_Campaign, df, sum) %>% 
      mutate(Searches_per_dollar = Search_Volume/Media_Spend)
  })
  
  output$barplot <- renderPlot({
    df <- aggregate(cbind(Media_Spend, Search_Volume) ~ Media_Campaign, df, sum) %>% 
      mutate(Searches_per_dollar = Search_Volume/Media_Spend)
    barplot(df$Searches_per_dollar, names.arg = df$Media_Campaign, xlab = "Media Campaign", ylab = "Searches_per_dollar")
  })
  
  output$searchdollar <- renderText({
    "The Campaign 2 is more efficient by generating more searches per spent dollar"
  })
  
  output$decay <- renderPlot({
    df <- df %>% mutate(Phantom_Inversion = ifelse(row_number() == 1, 100, 0)) %>% 
      filter(Media_Campaign == 1)
    df$Decay_Rate <- adstock(df$Phantom_Inversion, input$adstock)
    ggplot(df, aes(x = Week, y = Decay_Rate)) +
      geom_line() +
      labs(title = "Decay Rate", x = "Week", y = "Inversion")
  })
  
  output$plot1 <- renderPlot({
    df$adstock <- adstock(df$Media_Spend, input$adstock)
    ggplot(df, aes(x = Week, y = adstock)) +
      geom_line() +
      geom_bar(aes(x = Week, y = Media_Spend), stat = "identity", alpha = 0.5, color = "black") +
      labs(title = "Adstock Model", x = "Week", y = "Adstocked Media Spend")
  })
  
  output$decayexp <- renderText({
    "Per one hundred dollar of media spent, de decay would last a maximum of 43 weeks, considering a 0.9 or less of retention rate.
    For Campagin 3, adstock skyrockets vs the spent. However it does not generate a search explosion."
  })
  
  output$ModelSamp <- renderPlot({
    camp1 <- df %>% filter(Media_Campaign == 1)
    camp1$Adstock <- adstock(camp1$Media_Spend, input$adstock)
    model1 <- lm(Search_Volume ~ Adstock, data = camp1)
    camp1$Model <- as.data.frame(predict(model1, newdata = camp1))

    camp2 <- df %>% filter(Media_Campaign == 2)
    camp2$Adstock <- adstock(camp2$Media_Spend, input$adstock)
    model2 <- lm(Search_Volume ~ Adstock, data = camp2)
    camp2$Model <- as.data.frame(predict(model2, newdata = camp2))
    
    camp3 <- df %>% filter(Media_Campaign == 3)
    camp3$Adstock <- adstock(camp3$Media_Spend, input$adstock)
    model3 <- lm(Search_Volume ~ Adstock, data = camp3)
    camp3$Model <- as.data.frame(predict(model3, newdata = camp3))
    
    df <- rbind(camp1, camp2, camp3)
    rownames(df) <- make.names(rownames(df), unique = TRUE)
    
    ggplot(df, aes(x = Week, y = Search_Volume, color = "black")) +
      geom_line() +
      geom_line(aes(x = Week, y = Model, group = 1, color = "green")) +
      labs(title = "Search Volume Model Fit", x = "Week", y = "Search Volume") +
      scale_color_discrete(name = "Media Campaign", labels = c("Search Volume", "Search Volume Model"))
  })

  output$modeltable <- renderTable({
    camp1 <- df %>% filter(Media_Campaign == 1)
    camp1$Adstock <- adstock(camp1$Media_Spend, input$adstock)
    model1 <- lm(Search_Volume ~ Adstock, data = camp1)
    results1 <- as.data.frame(coef(model1))
    results1 <- rbind(results1, R_squared <- summary(model1)$r.squared)
    
    camp2 <- df %>% filter(Media_Campaign == 2)
    camp2$Adstock <- adstock(camp2$Media_Spend, input$adstock)
    model2 <- lm(Search_Volume ~ Adstock, data = camp2)
    results2 <- as.data.frame(coef(model2))
    results2 <- rbind(results2, R_squared <- summary(model2)$r.squared)
    
    camp3 <- df %>% filter(Media_Campaign == 3)
    camp3$Adstock <- adstock(camp3$Media_Spend, input$adstock)
    model3 <- lm(Search_Volume ~ Adstock, data = camp3)
    results3 <- as.data.frame(coef(model3))
    results3 <- rbind(results3, R_squared <- summary(model3)$r.squared)
    
    df <- cbind(results1, results2, results3)
    rownames(df) <- c("Intercept", "Slope", "R - Squared")
    colnames(df) <- c("Campaign 1", "Campaign 2", "Campaign 3")
    df
  }, rownames = TRUE)
  
  output$fitmodel <- renderText({
    "The model is a combination of trhee linear regressions, one per campaign for better fit, with Adstock as independent variable and Search Volume as a dependent variable. Smaller the retention factor, better the fit. The "
  })
}

# Run the application
shinyApp(ui = ui, server = server)
