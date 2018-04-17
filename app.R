# Pleliminary Prototype Shiny App for Ed Learnosity Data

#Load libraries
library(shiny)
library(dplyr)
library(ggplot2)
library(lubridate)
library(scales)

df <- read.csv("data/FourDistrictsAnonSchoolAndClassNames.csv", colClasses = "character")

#Set data types properly
df$mo_yr_completed <- as.Date(floor_date(ymd_hms(df$date_completed),"month"))
df$item_score <- as.integer(df$item_score)
df$item_max_score <- as.integer(df$item_max_score)
df$time_in_secs <- as.integer(df$time_in_secs)
df <- df[nchar(df$grade)>0,]  #exclude records with grade of ""
df <- df[!is.na(df$grade),]   #exclude records without grade
df$grade <- paste("Grade", df$grade)


# Define UI for application that draws a facted graph
ui <- fluidPage(
   
   # Application title
   titlePanel("Ed Learnosity Data Exploration"),
   
   # Sidebar with inputs 
   fluidRow(
      column(3,    #Use 3 of the 12 columns for the sidebar
             wellPanel(
                selectInput(inputId = "district", label = strong("District"),
                            choices = sort(unique(df$district_name)),
                            selected = "District B"),
                checkboxInput("gradesorno", "Show grade-level detail?", FALSE),
                radioButtons("extraStats", "Choose Annotation",
                             c("Numbers of (active) Students" = "NumbersOfStudents",
                               "Average Scores" = "AvgScores",
                               "Average Durations (in seconds)" = "AvgDurations")),
                radioButtons("yaxis", "Vertical Axis Represents What?",
                             c("Items per (active) Student" = "ItemsPerStudent",
                               "Number of Items (total)" = "NumberOfItems"))
             )
      ),
      
      # Show a faceted graph

      column(9,      #Use 9 of the 12 panels for the graph
             img(src='hmh.png', align = "right"),
             plotOutput("distPlot")
      )
   )
)

# Define server logic required to draw a faceted graph
server <- function(input, output) {
  
     output$distPlot <- renderPlot({
       
       dist <- df %>% filter(district_name==input$district)
       
       if(!input$gradesorno){
       
          BySchoolMonth <- dist %>%
            group_by(school_name, mo_yr_completed) %>%
            summarize(NumberOfItems=n(),
                      NumbersOfStudents=n_distinct(student_personal_refid),
                      ItemsPerStudent=NumberOfItems/NumbersOfStudents,
                      AvgScores=round(100*sum(item_score)/sum(item_max_score)),
                      AvgDurations=round(mean(time_in_secs)))
          
            #Eliminate display of stats as user specifies
            if(input$extraStats=="None"){
              BySchoolMonth$NumbersOfStudents <- NA
              BySchoolMonth$AvgScores <- NA
              BySchoolMonth$AvgDurations <- NA
            }
            if(input$extraStats=="NumbersOfStudents"){
              BySchoolMonth$AvgScores <- NA
              BySchoolMonth$AvgDurations <- NA
            }
            if(input$extraStats=="AvgScores"){
              BySchoolMonth$NumbersOfStudents <- NA
              BySchoolMonth$AvgDurations <- NA
            }
            if(input$extraStats=="AvgDurations"){
              BySchoolMonth$NumbersOfStudents <- NA
              BySchoolMonth$AvgScores <- NA
            }

          ggplot(BySchoolMonth, aes(x=mo_yr_completed, y=eval(as.name(input$yaxis)))) +
            geom_bar(stat="identity", fill='goldenrod') +
            scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") +
            scale_y_continuous(labels=comma) +
            labs(title="Ed Learnosity Items by School and Month",
                 subtitle=paste("Blue Annotations:",input$extraStats),
                 x="", y=as.name(input$yaxis)) +
            facet_grid(school_name ~ .) +
            theme(strip.text.y = element_text(angle=0),
                  legend.position="none") + 
            geom_text(aes(label = eval(as.name(input$extraStats)), y=0),
                      position = position_dodge(0.9),
                      vjust=0, hjust=0.5, color="blue")


       }else{
         
          BySchoolGradeMonth <- dist %>%
            group_by(school_name, grade, mo_yr_completed) %>%
            summarize(NumberOfItems=n(),
                      NumbersOfStudents=n_distinct(student_personal_refid),
                      ItemsPerStudent=NumberOfItems/NumbersOfStudents,
                      AvgScores=round(100*sum(item_score)/sum(item_max_score)),
                      AvgDurations=round(mean(time_in_secs)))
          #Eliminate display of stats as user specifies
          if(input$extraStats=="None"){
            BySchoolGradeMonth$NumbesOfStudents <- NA
            BySchoolGradeMonth$AvgScores <- NA
            BySchoolGradeMonth$AvgDurations <- NA
          }
          if(input$extraStats=="NumbersOfStudents"){
            BySchoolGradeMonth$AvgScores <- NA
            BySchoolGradeMonth$AvgDurations <- NA
          }
          if(input$extraStats=="AvgScores"){
            BySchoolGradeMonth$NumbersOfStudents <- NA
            BySchoolGradeMonth$AvgDurations <- NA
          }
          if(input$extraStats=="AvgDurations"){
            BySchoolGradeMonth$NumbersOfStudents <- NA
            BySchoolGradeMonth$AvgScores <- NA
          }
          
          ggplot(BySchoolGradeMonth, aes(x=grade, y=eval(as.name(input$yaxis)))) +
              geom_bar(stat="identity", fill='goldenrod') +
              scale_y_continuous(labels=comma) +
              labs(title="Ed Learnosity Items by School, Month, and Grade",
                   subtitle=paste("Blue Annotations:",input$extraStats),
                   x="", y=as.name(input$yaxis)) +
              facet_grid(school_name ~ as.factor(substr(mo_yr_completed,1,7))) +
              theme(strip.text.y = element_text(angle=0),
                    axis.text.x = element_text(angle=90, vjust=0.5),
                    legend.position="none") +
              geom_text(aes(label = eval(as.name(input$extraStats)), y = 0),
                        position = position_dodge(0.9),
                        vjust=0, hjust=0.5, color="blue")
        }
   })
}

# Run the application 
shinyApp(ui = ui, server = server)