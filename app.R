library(shiny)
library(ggplot2)
library(dplyr)
library(shiny)
library(ggplot2)
library(dplyr)
library(reshape2)
library(rsconnect)
library(tidyverse)

uof <- read.csv("UOF_BY_DEPARTMENTS.csv")



### data prep functions

format_long_data <- function(data, total_incidents_2012_2016, total_incidents_race_county) {
  data.long <-  data.long %>% drop_na(total_incidents_2012_2016)
  
  #calculating total incedents
  data.long$incident_count <- data.long$total_incidents_2012_2016 * data.long$value / 100
  
  #summarizing data to get the percent incidents by race
  data.long.summary <- data.long %>% group_by(county, variable) %>% 
    summarize(total_incidents_race_county = sum(incident_count), total_incidents_county = sum(total_incidents_2012_2016))
  data.long.summary$percent_incidents <- data.long.summary$total_incidents_race_county / data.long.summary$total_incidents_county
  
  return (data)
}



### prepping data for use of force subjects

uof_subjects <- uof %>% 
  select(county, white_pct_subjects, hispanic_pct_subjects, black_pct_subjects, 
         asian_pacific_islander_pct_subjects, total_incidents_2012_2016)
uof_subjects.long <- melt(uof_subjects, id = c("county", "total_incidents_2012_2016"))
uof_subjects.long <- format_long_data(uof_subjects, total_incidents_2012_2016, total_incidents_race_county)



### prepping data for arrests subjects

uof_arrests <- uof %>% 
  select(county, white_arrests_pct_2012_2016, black_arrests_pct_2012_2016, 
         api_arrests_pct_2012_2016, total_arrests_2012_2016)
uof_arrests.long <- melt(uof_arrests, id = c("county", "total_arrests_2012_2016"))
uof_arrests.long <- format_long_data(uof_arrests, total_arrests_2012_2016, total_arrests_race_county)



### getting the odds for odds plot
uof_odds <- subset(uof, select = c(odds_ratio_pop, odds_ratio_arrests, coverage_city, county))



### getting rid of na values
uof_subjects_omit <- na.omit(uof_subjects)
uof_arrests_omit <- na.omit(uof_arrests)


### MARK: rshiny
ui = fluidPage(
  titlePanel("Use of Force and Arrests: Racial Breakdown by County", window = "Group Module 2"),
  sidebarLayout(
    sidebarPanel(
      selectInput('selectCounty1', 'County 1', choices = sort(c(unique(uof$county))), selected = "Atlantic"),
      selectInput('selectCounty2', 'County 2', choices = sort(c(unique(uof$county))), selected= "Bergen"),          
      radioButtons('ethnType', 'Select View', choices = c("Use of Force Subjects", "Arrests")),
      uiOutput('percentStats'),
      uiOutput("space"),
      textOutput("fact")
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
      filter(county == input$selectCounty1 | county == input$selectCounty2) %>% drop_na(odds_ratio_pop)
  })
  
  filtered_odds_3 <- reactive({
    uof_odds %>%
      filter(county == input$selectCounty1 | county == input$selectCounty2) %>% drop_na(odds_ratio_arrests)
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
  
  
  output$percentPlot <- renderPlot({
    if (is.null(input$ethnType)){return ()}
    
    if(input$ethnType == "Arrests") {
      
      ggplot(filteredArrests(), aes(x= "", y= percent_incidents,  fill = variable)) + 
        geom_bar(stat = "identity", alpha = .8) + coord_polar("y", start=0) +
        geom_text(aes(label = round(percent_incidents*100, 2)), position = position_stack(vjust = 0.5), 
                  size = 4, face = "bold", color = "white") +
        facet_grid(facets=. ~ county)  +
        scale_fill_manual(values = c( "yellow4", "darkblue", "maroon"), 
                          labels = c("White", "Black", "Asian/Pacifc Islander")) +
        labs(fill = "Race", title = "Percent of arrests by race",
             caption = "Certain populations may be under-represented. 
             Hispanic was not an option for arrests on forms in many counties") +
        theme(title = element_text(size = 14, face = "bold"), axis.title = element_blank(), 
              axis.text = element_blank(), legend.text = element_text(size = 10), 
              strip.text = element_text(face = 'bold')) 
    }
    
    else if (input$ethnType == "Use of Force Subjects") {
      ggplot(filtered(), aes(x= "", y = percent_incidents, fill = variable)) + 
        geom_bar(stat = "identity") + coord_polar("y", start=0) +
        geom_text(aes(label = round(percent_incidents*100, 2)), 
                  position = position_stack(vjust = 0.5), 
                  size = 4, face = "bold", color = "white") +
        facet_grid(facets=. ~ county)  +
        scale_fill_manual(values = c( "yellow4", "purple", "darkblue", "maroon"), 
                          labels = c("White", "Hispanic", "Black", "Asian/Pacifc Islander")) +
        labs(fill = "Race", title = "Percent of use of force victims by race ",
             caption = "Certain populations may be under-represented due to missing data.") +
        theme(title = element_text(size = 14, face = "bold"), axis.title = element_blank(), 
              axis.text = element_blank(), legend.text = element_text(size = 10), 
              strip.text = element_text(face = 'bold'))
    }
  })
  
  output$oddsPlot <- renderPlot({
    if (is.null(input$ethnType)){return ()}
    
    if(input$ethnType == "Arrests") {
      ggplot(filtered_odds_3(), aes(x = coverage_city, y = odds_ratio_arrests, group = county, fill = county)) + 
        geom_bar(stat = "identity")+ 
        coord_flip() +
        scale_fill_manual(values = c("#654F6F", "#A8C69F")) + 
        labs(fill = "County", title = paste0("Odds of a black person getting arrested over a white \nperson in ", input$selectCounty1, " and ", input$selectCounty2, " counties"),
             caption = "Towns with missing data are not represented in this graph.") + 
        scale_y_log10() +
        theme(axis.text.x = element_text(angle = 70, vjust = 1, hjust=1), title = element_text(size = 14, face = "bold"), axis.title = element_blank(), legend.text = element_text(size = 10)) 
    }
    
    else if (input$ethnType == "Use of Force Subjects") {
      ggplot(filtered_odds_2(), aes(x = coverage_city, y = odds_ratio_pop, group = county, fill = county)) +
        geom_bar(stat = "identity")+
        coord_flip() +
        scale_fill_manual(values = c("#654F6F", "#A8C69F")) + 
        labs(fill = "County", title = paste0("Odds of use of force being used against a black person \nover a white person in ", input$selectCounty1, " and ", input$selectCounty2, " counties"),
             caption = "Towns with missing data are not represented in this graph.")  +
        scale_y_log10() +
      theme(title = element_text(size = 14, face = "bold"), axis.title = element_blank(), legend.text = element_text(size = 10)) 
  # theme(axis.text.x = element_text(angle = 70, vjust = 1, hjust=1),
  }
  }, height = 740)
  # }}. height = 700))
  
  output$fact = renderText({
    paste("Disclaimer: Ethnicities marked on forms were up to the discretion of the officers filling out incident forms. In addition to this, some county populations may be under-represented due to missing data or forms lacking options (such as Hispanic not being an option for arrests).")
  })
  
  output$space <- renderUI({
    HTML(paste0("</br>"))
  })
    
  output$percentStats <- renderUI({
    if(input$ethnType == "Arrests") {
      tArrW <- summary(aov(white_arrests_pct_2012_2016~factor(county), data=uof_arrests_omit))
      tArrB <- summary(aov(black_arrests_pct_2012_2016~factor(county), data=uof_arrests_omit))
      tArrAPI <- summary(aov(api_arrests_pct_2012_2016~factor(county), data=uof_arrests_omit))
      
      HTML(paste("An ANOVA analysis was performed between the arrests of different ethnic groups and county. The probability >F value is for white suspects is <0.01 and the the F value is ",
                 format(tArrW[[1]][[1,4]],digits=4), ".",
                 "The probability >F value is for black suspects is <0.01 and the the F value is ", format(tArrB[[1]][[1,4]],digits=4),
                 ".", "The probability >F value is for API suspects is <0.01 and the the F value is ", format(tArrAPI[[1]][[1,4]],digits=4), ".",
                 "The null hypothesis would be there is no significant difference between the counties for suspects of different ethnicities.
                 Since all of probability >F values are extremely small and highly unlikely, we can reject the null hypothesis
                and assume there is a mean that is not equal to the rest."))
    }
    else if (input$ethnType == "Use of Force Subjects") {
      tForceW <- summary(aov(white_pct_subjects~factor(county), data=uof_subjects_omit))
      tForceB <- summary(aov(black_pct_subjects~factor(county), data=uof_subjects_omit))
      tForceH <- summary(aov(hispanic_pct_subjects~factor(county), data=uof_subjects_omit))
      tForceAPI <- summary(aov(asian_pacific_islander_pct_subjects~factor(county), data=uof_subjects_omit))
      
      HTML(paste("An ANOVA analysis was performed between the use of force on different ethnic groups and county. The probability >F value is for white suspects is <0.01 and the F value is ",
                 format(tForceW[[1]][[1,4]],digits=3), ".",
                 "The probability >F value is for black suspects is <0.01 and the the F value is ", format(tForceB[[1]][[1,4]],digits=3), ".", 
                 "The probability >F value is for API suspects is <0.01 and the the F value is ", format(tForceAPI[[1]][[1,4]],digits=3), ".",
                 "The probability >F value is for hispanic suspects is  <0.01 and the the F value is ", format(tForceB[[1]][[1,4]], digits=3), ".",
                 "The null hypothesis would be there is no significant difference between the counties for suspects of different ethnicities.
                Since all of probability >F values are extremely small and highly unlikely, we can reject the null hypothesis
                and assume there is a mean that is not equal to the rest."))
      
    }
  })
  
}


shinyApp(ui, server)
