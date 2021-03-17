# RShiny Police Use of Force 

###### Data prep and visualization by Emily. Statistical tests by Kaitlyn.


### 614 Assignment (Module 2)

An RShiny App which uses interactive data visualization to explore a racial breakdown of use of force and arrests by police departments.



### Data

The data is a collection of 5 years of data by New Jersey police departments from forms that police are required to fill out whenever use of force is used. 

The data and more information about it can be found here: https://data.world/njdotcom/use-of-force-department-data

included in this repo at UOF_BY_DEPARTMENT.csv

### RShiny App

https://kaitlynabdo.shinyapps.io/GroupModule2/ 


Our shiny application is a simple racial breakdown of use of force and arrests among different counties. The user is able to interface with our app by selecting any two counties in New Jersey and by choosing what the visualizations will depict: use of force or arrests. The top visualization shows two pie charts, one per each county selected. These pie charts show what percent of each race makes up either the total use of force incidents or total arrests in the two selected counties, depending on which view option is selected. Below the pie chart is a visualization showing the odds ratio of a black person being the subject of use of force or arrests versus a white person. Again, the exact visualization will depend on the counties and view that the user chooses. We can see that, for most counties, there is a disparity between the two visualizations, both for arresets and use of force. Aside from the fact that in most counties there is a higher, or approximately equal, percentage of white people being subjects of use of force and arrest than a black person (according to the pie charts), the odds of a black person being a subject is much higher in most towns. This indicates that there is some sort of unfair racial disparity occuring. Finally, for our statistical test, we did an ANOVA analysis of the different races and counties. The results are displayed on the left of our shiny app, and like the visualizations, the test results depend on if the user chooses to view  use of force data or arrests data.



### Emily Daskas and Kaitlyn Abdo
