library(shiny)
library(ggplot2)
library(dplyr)
library(reshape2)
library(rsconnect)
library(tidyverse)

uof <- read.csv("UOF_BY_DEPARTMENTS.csv")

##subjects data
uof_subjects <- uof %>% 
  select(county, white_pct_subjects, hispanic_pct_subjects, black_pct_subjects, 
         asian_pacific_islander_pct_subjects, total_incidents_2012_2016)


uof_subjects.long <- melt(uof_subjects, id = c("county", "total_incidents_2012_2016"))
uof_subjects.long <- melt(uof_subjects, id = c("county", "total_incidents_2012_2016"))
uof_subjects.long <-  uof_subjects.long %>% drop_na(total_incidents_2012_2016)
uof_subjects.long$incident_count <- uof_subjects.long$total_incidents_2012_2016 * uof_subjects.long$value / 100
uof_subjects.long.summary <- uof_subjects.long %>% group_by(county, variable) %>% 
  summarize(total_incidents_race_county = sum(incident_count), total_incidents_county = sum(total_incidents_2012_2016))
uof_subjects.long.summary$percent_incidents <- uof_subjects.long.summary$total_incidents_race_county / uof_subjects.long.summary$total_incidents_county


uof_arrests <- uof %>% 
  select(county, white_arrests_pct_2012_2016, black_arrests_pct_2012_2016, 
         api_arrests_pct_2012_2016, total_arrests_2012_2016)
uof_arrests.long <- melt(uof_arrests, id = c("county", "total_arrests_2012_2016"))
uof_arrests.long <-  uof_arrests.long %>% drop_na(total_arrests_2012_2016)
uof_arrests.long$incident_count <- uof_arrests.long$total_arrests_2012_2016 * uof_arrests.long$value / 100
uof_arrests.long.summary <- uof_arrests.long %>% group_by(county, variable) %>% 
  summarize(total_arrests_race_county = sum(incident_count), total_incidents_county = sum(total_arrests_2012_2016))
uof_arrests.long.summary$percent_incidents <- uof_arrests.long.summary$total_arrests_race_county / uof_arrests.long.summary$total_incidents_county


uof_odds <- subset(uof, select = c(odds_ratio_pop, odds_ratio_arrests, coverage_city, county))

uof_subjects_omit <- na.omit(uof_subjects)
uof_arrests_omit <- na.omit(uof_arrests)

ui = fluidPage(
  titlePanel("Comparing New Jersey Counties: Use of Force and Arrests by Race", window = "Group Module 2"),
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
        labs(fill = "Race", title = "Percent of Arrests of Different Races",
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
        labs(fill = "Race", title = "Percent of Races Who Were Victims of Use of Force ",
             caption = "Certain populations may be under-represented due to missing data.") +
        theme(title = element_text(size = 14, face = "bold"), axis.title = element_blank(), 
              axis.text = element_blank(), legend.text = element_text(size = 10), 
              strip.text = element_text(face = 'bold'))
    }
  })
  
  output$oddsPlot <- renderPlot({
    if (is.null(input$ethnType)){return ()}
    
    if(input$ethnType == "Arrests") {
      
      ggplot(filtered_odds_3(), aes(x = coverage_city, y = odds_ratio_arrests, group = county, fill = county)) + geom_bar(stat = "identity")+
        #coord_flip() +
        scale_fill_manual(values = c("#654F6F", "#A8C69F")) + 
        labs(fill = "County", title = paste0("Odds Ratio of arrest of a black person \nvs a white person in ", input$selectCounty1, " and ", input$selectCounty2, " Counties"),
             caption = "Towns with missing data are not represented in this graph.") +
        theme(axis.text.x = element_text(angle = 70, vjust = 1, hjust=1), title = element_text(size = 14, face = "bold"), axis.title = element_blank(), legend.text = element_text(size = 10)) 
    }
    
    else if (input$ethnType == "Use of Force Subjects") {
      ggplot(filtered_odds_2(), aes(x = coverage_city, y = odds_ratio_pop, group = county, fill = county)) + geom_bar(stat = "identity")+
        #coord_flip() +
        scale_fill_manual(values = c("#654F6F", "#A8C69F")) + 
        labs(fill = "County", title = paste0("Odds Ratio of Use of Force Against a Black Person \nvs a White Person in ", input$selectCounty1, " and ", input$selectCounty2, " Counties"),
             caption = "Towns with missing data are not represented in this graph.") +
        theme(axis.text.x = element_text(angle = 70, vjust = 1, hjust=1), title = element_text(size = 14, face = "bold"), axis.title = element_blank(), legend.text = element_text(size = 10))     }
  }, width = 700)#, height = 650)
  
  output$fact = renderText({
    paste("Disclaimer: Ethnicities marked on forms were up to the discretion of the officers filling out incident forms. In addition to this, some county populations may be under-represented due to missing data or forms lacking options (such as Hispanic arrests).")
  })
  
  output$space <- renderUI({
    HTML(paste0("</br>"))
  })
  
  output$percentStats <- renderUI({
    if(input$ethnType == "Arrests") {
      tArrW <- summary(aov(white_arrests_pct_2012_2016~factor(county), data=uof_arrests_omit))
      tArrB <- summary(aov(black_arrests_pct_2012_2016~factor(county), data=uof_arrests_omit))
      tArrAPI <- summary(aov(api_arrests_pct_2012_2016~factor(county), data=uof_arrests_omit))
      
      HTML(paste("An ANOVA analysis was performed between the arrests of different ethnic groups and county. The F value is for white suspects is  ",
                 format(tArrW[[1]][[1,4]],digits=4), ".",
                 "The F value is for black suspects is  ", format(tArrB[[1]][[1,4]],digits=4),
                 ".", "The F value is for API suspects is  ", format(tArrAPI[[1]][[1,4]],digits=4), ".", "</br>"))
    }
    else if (input$ethnType == "Use of Force Subjects") {
      tForceW <- summary(aov(white_pct_subjects~factor(county), data=uof_subjects_omit))
      tForceB <- summary(aov(black_pct_subjects~factor(county), data=uof_subjects_omit))
      tForceH <- summary(aov(hispanic_pct_subjects~factor(county), data=uof_subjects_omit))
      tForceAPI <- summary(aov(asian_pacific_islander_pct_subjects~factor(county), data=uof_subjects_omit))
      
      HTML(paste("An ANOVA analysis was performed between the use of force on different ethnic groups and county. The F value is for white suspects is  ",
                 format(tForceW[[1]][[1,4]],digits=3), ".",
                 "The F value is for black suspects is  ", format(tForceB[[1]][[1,4]],digits=3), ".", 
                 "The F value is for API suspects is  ", format(tForceAPI[[1]][[1,4]],digits=3), ".",
                 "The F value is for hispanic suspects is  ", format(tForceB[[1]][[1,4]], digits=3), "."))
      
    }
  })
  
}


shinyApp(ui, server)

