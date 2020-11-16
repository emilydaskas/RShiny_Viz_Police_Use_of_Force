library(shiny)
library(ggplot2)
library(dplyr)
library(reshape2)
library(rsconnect)

uof <- read.csv("UOF_BY_DEPARTMENTS.csv")

##subjects data
uof_subjects <- uof %>% 
  select(county, white_pct_subjects, hispanic_pct_subjects, black_pct_subjects, 
         asian_pacific_islander_pct_subjects, total_incidents_2012_2016)
uof_subjects.long <- melt(uof_subjects, id = c("county", "total_incidents_2012_2016"))
uof_subjects.long <- melt(uof_subjects, id = c("county", "total_incidents_2012_2016"))
uof_subjects.long$incident_count <- uof_subjects.long$total_incidents_2012_2016 * uof_subjects.long$value / 100
uof_subjects.long.summary <- uof_subjects.long %>% group_by(county, variable) %>% 
  summarize(total_incidents_race_county = sum(incident_count), total_incidents_county = sum(total_incidents_2012_2016))
uof_subjects.long.summary$percent_incidents <- uof_subjects.long.summary$total_incidents_race_county / uof_subjects.long.summary$total_incidents_county


uof_arrests <- uof %>% 
  select(county, white_arrests_pct_2012_2016, black_arrests_pct_2012_2016, 
         api_arrests_pct_2012_2016, total_arrests_2012_2016)
uof_arrests.long <- melt(uof_arrests, id = c("county", "total_arrests_2012_2016"))
uof_arrests.long$incident_count <- uof_arrests.long$total_arrests_2012_2016 * uof_arrests.long$value / 100
uof_arrests.long.summary <- uof_arrests.long %>% group_by(county, variable) %>% 
  summarize(total_arrests_race_county = sum(incident_count), total_incidents_county = sum(total_arrests_2012_2016))
uof_arrests.long.summary$percent_incidents <- uof_arrests.long.summary$total_arrests_race_county / uof_arrests.long.summary$total_incidents_county


uof_odds <- subset(uof, select = c(odds_ratio_pop, odds_ratio_arrests, coverage_city, county))


ui = fluidPage(
  titlePanel("Group Module 2", window = "Group Module 2"),
  sidebarLayout(
    sidebarPanel(
      selectInput('selectCounty1', 'County 1', choices = sort(c(unique(uof$county))), selected = "Atlantic"),
      selectInput('selectCounty2', 'County 2', choices = sort(c(unique(uof$county))), selected= "Bergen"),          
      radioButtons('ethnType', 'Demographic View', choices = c("Use of Force Subjects", "Arrests"))
    ),
    mainPanel(
      plotOutput('percentPlot'),
      plotOutput('oddsPlot')
    )
  )
)

server <- function(input,output) {
  
  filtered_odds_2 <- reactive({
    uof_odds %>%
      filter(county == input$selectCounty1 | county == input$selectCounty2)
  })
  
  filtered <- reactive({
    if(is.null(input$selectCounty1) && is.null(input$selectCounty2)) {
      return(NULL)
    }
    
    uof_subjects.long.summary %>%
      filter(county == input$selectCounty1 || county == input$selectCounty2)
  })
  
  filteredArrests <- reactive({
    if(is.null(input$selectCounty1) && is.null(input$selectCounty2)) {
      return(NULL)
    }
    
    uof_arrests.long.summary %>%
      filter(county == input$selectCounty1 || county == input$selectCounty2)
  })
  
  
  output$subjectsPlot <- renderPlot({
    if (is.null(input$ethnType)){return ()}
    
    if(input$ethnType == "Use of Force Subjects") {
      
      ggplot(filtered_odds_2(), aes(x = coverage_city, y = odds_ratio_pop, group = county, fill = county)) + geom_bar(stat = "identity")+
        coord_flip() +
        scale_fill_manual(values = c("#654F6F", "#A8C69F")) + 
        labs(fill = "County", title = "Odds of a Use of Force Against a black person vs a white person") +
        theme(title = element_text(size = 14, face = "bold"), axis.title = element_blank(), legend.text = element_text(size = 10))
      
    }
  }, height = 700)

  output$percentPlot <- renderPlot({
    if (is.null(input$ethnType)){return ()}
    
    if(input$ethnType == "Arrests") {
      
      ggplot(filteredArrests(), aes(x= county, y= percent_incidents, group = variable, fill = variable)) + 
        geom_bar(position = "stack", stat = "identity", alpha = .8) + 
        scale_fill_manual(values = c( "yellow4", "darkblue", "maroon"), labels = c("White", "Black", "Asian/Pacifc Islander")) +
        labs(fill = "Race", title = "% Arrests of a Race vs Total Arrests") +
        theme(title = element_text(size = 14, face = "bold"), axis.title = element_blank(), axis.text = element_text(size = 15), legend.text = element_text(size = 10))    + 
        coord_flip() 
    }
    
    else if (input$ethnType == "Use of Force Subjects") {
      ggplot(filtered(), aes(x= county, y= percent_incidents, group = variable, fill = variable)) + 
        geom_bar(position = "stack", stat = "identity", alpha = .8) + 
        scale_fill_manual(values = c( "yellow4", "purple", "darkblue", "maroon"), labels = c("White", "Hispanic", "Black", "Asian/Pacifc Islander")) +
        labs(fill = "Race", title = "% Use of Force of a Race vs Total Use of Force ") +
        theme(title = element_text(size = 14, face = "bold"), axis.title = element_blank(), 
              axis.text = element_text(size = 15), legend.text = element_text(size = 10))   + 
        coord_flip()
    }
  })
  
  output$oddsPlot <- renderPlot({
    if (is.null(input$ethnType)){return ()}
    
    if(input$ethnType == "Arrests") {
      
      ggplot(filtered_odds_2(), aes(x = coverage_city, y = odds_ratio_arrests, group = county, fill = county)) + geom_bar(stat = "identity")+
        coord_flip() +
        scale_fill_manual(values = c("#654F6F", "#A8C69F")) + 
        labs(fill = "County", title = "Odds of arrest of a black person \nvs a white person in different counties") +
        theme(title = element_text(size = 14, face = "bold"), axis.title = element_blank(), legend.text = element_text(size = 10))
    }
  
     else if (input$ethnType == "Use of Force Subjects") {
      ggplot(filtered_odds_2(), aes(x = coverage_city, y = odds_ratio_pop, group = county, fill = county)) + geom_bar(stat = "identity")+
      coord_flip() +
      scale_fill_manual(values = c("#654F6F", "#A8C69F")) + 
      labs(fill = "County", title = "Odds of a Use of Force Against a black person \nvs a white person in different counties") +
      theme(title = element_text(size = 14, face = "bold"), axis.title = element_blank(), legend.text = element_text(size = 10))
    
    }
  }, height = 700)
}
    

shinyApp(ui, server)



