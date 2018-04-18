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


# Define UI for application that draws a faceted graph
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
                radioButtons("extraStats", "Annotation",
                             c("Numbers of (active) Students" = "NumbersOfStudents",
                               "Numbers of Items (total)" = "NumbersOfItems",
                               "Items per (active) Student" = "ItemsPerStudent",
                               "Average Scores" = "AvgScores",
                               "Average Durations (in seconds)" = "AvgDurations",
                               "None"="None"),
                             selected="None"),
                radioButtons("yaxis", "Vertical Axis",
                             c("Items per (active) Student" = "ItemsPerStudent",
                               "Number of Items (total)" = "NumbersOfItems"))
             )
      ),
      
      # Show a faceted graph

      column(9,      #Use 9 of the 12 columns for the graph
             img(src='hmh.png', align = "right"),
             plotOutput("distPlot")
      )
   )
)

# Define server logic required to draw a faceted graph
server <- function(input, output) {
  
     output$distPlot <- renderPlot({
       
       dist <- df %>% filter(district_name==input$district)
       
       #Get, as date and as string, the first month BEFORE any activity in the data.
       #Use this as placeholder for ALL category, to hold stats for each school (and district) over all time.
       mindateless1mo <- min(dist$mo_yr_completed) - months(1)
       mindateless1mostring <- substr(as.character(mindateless1mo),1,7)

       if(!input$gradesorno){
       
          BySchoolMonth <- dist %>%
            group_by(school_name, mo_yr_completed) %>%
            summarize(NumbersOfItems=n(),
                      NumbersOfStudents=n_distinct(student_personal_refid),
                      ItemsPerStudent=round(NumbersOfItems/NumbersOfStudents),
                      sum_item_score=sum(item_score),
                      sum_item_max_score=sum(item_max_score),
                      AvgScores=round(100*sum_item_score/sum_item_max_score),
                      sum_time_in_secs=sum(time_in_secs),
                      AvgDurations=round(mean(time_in_secs)),
                      None="")
          
           if(input$yaxis=="ItemsPerStudent"){   #Make marginal graphs
             #make marginals by school
             BySchool <- BySchoolMonth %>%
               group_by(school_name) %>%
               summarize(mo_yr_completed=mindateless1mo,
                         NumbersOfItems=sum(NumbersOfItems),
                         NumbersOfStudents=sum(NumbersOfStudents),
                         ItemsPerStudent=round(NumbersOfItems/NumbersOfStudents),
                         sum_item_score=sum(sum_item_score),
                         sum_item_max_score=sum(sum_item_max_score),
                         AvgScores=round(100*sum_item_score/sum_item_max_score),
                         sum_time_in_secs=sum(sum_time_in_secs),
                         AvgDurations=round(sum_time_in_secs/NumbersOfItems),
                         None="")
             
             ByMonth <- BySchoolMonth %>%
               group_by(mo_yr_completed) %>%
               summarize(school_name="ZZZ",    #assuming that ZZZ will be later alphabetically than any school name
                         NumbersOfItems=sum(NumbersOfItems),
                         NumbersOfStudents=sum(NumbersOfStudents),
                         ItemsPerStudent=round(NumbersOfItems/NumbersOfStudents),
                         sum_item_score=sum(sum_item_score),
                         sum_item_max_score=sum(sum_item_max_score),
                         AvgScores=round(100*sum_item_score/sum_item_max_score),
                         sum_time_in_secs=sum(sum_time_in_secs),
                         AvgDurations=round(sum_time_in_secs/NumbersOfItems),
                         None="")
             
             ByMonth <- ByMonth[c(2,1,3,4,5,6,7,8,9,10,11)] #order columns to match those of BySchoolMonth
             
             Overall <- ByMonth %>%
               summarize(mo_yr_completed=mindateless1mo, #temporarily put stats into earliest month - 1 month category
                         school_name="ZZZ",
                         NumbersOfItems=sum(NumbersOfItems),
                         NumbersOfStudents=sum(NumbersOfStudents),
                         ItemsPerStudent=round(NumbersOfItems/NumbersOfStudents),
                         sum_item_score=sum(sum_item_score),
                         sum_item_max_score=sum(sum_item_max_score),
                         AvgScores=round(100*sum_item_score/sum_item_max_score),
                         sum_time_in_secs=sum(sum_time_in_secs),
                         AvgDurations=round(sum_time_in_secs/NumbersOfItems),
                         None="")
             
             #Bind all the rows into a data frame:
             BySchoolMonth <- rbind.data.frame(BySchoolMonth, BySchool, ByMonth, Overall)
           }      

          #Change labels for Per-School marginals (stats over ALL time) from Aug 2017 to ALL:
          BySchoolMonth$mo_yr_completed <- as.factor(BySchoolMonth$mo_yr_completed)
          BySchoolMonth$mo_yr_completed <- substr(as.character(BySchoolMonth$mo_yr_completed),1,7)
          BySchoolMonth$mo_yr_completed[BySchoolMonth$mo_yr_completed==mindateless1mostring] <- "ALL"
          
          #Change labels for school_names so that ALL is displayed last:
          BySchoolMonth$school_name <- as.factor(BySchoolMonth$school_name)
          BySchoolMonth$school_name <- as.character(BySchoolMonth$school_name)
          BySchoolMonth <- arrange(BySchoolMonth, school_name)   #to ensure that ZZZ comes last
          BySchoolMonth$school_name <- as.factor(BySchoolMonth$school_name)
          levels(BySchoolMonth$school_name)[levels(BySchoolMonth$school_name)=="ZZZ"] <- "ALL"
   
          #Make the graph
          ggplot(BySchoolMonth, aes(x=mo_yr_completed, y=eval(as.name(input$yaxis)))) +
            geom_bar(stat="identity", fill='goldenrod') +
            #scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") +
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
            summarize(NumbersOfItems=n(),
                      NumbersOfStudents=n_distinct(student_personal_refid),
                      ItemsPerStudent=round(NumbersOfItems/NumbersOfStudents),
                      sum_item_score=sum(item_score),
                      sum_item_max_score=sum(item_max_score),
                      AvgScores=round(100*sum_item_score/sum_item_max_score),
                      sum_time_in_secs=sum((time_in_secs)),
                      AvgDurations=round(mean(time_in_secs)),
                      None="")
          
          if(input$yaxis=="ItemsPerStudent"){   #Make marginal graphs
            #make marginals by school
            BySchoolGrade <- BySchoolGradeMonth %>%
              group_by(school_name, grade) %>%
              summarize(mo_yr_completed=mindateless1mo,
                        NumbersOfItems=sum(NumbersOfItems),
                        NumbersOfStudents=sum(NumbersOfStudents),
                        ItemsPerStudent=round(NumbersOfItems/NumbersOfStudents),
                        sum_item_score=sum(sum_item_score),
                        sum_item_max_score=sum(sum_item_max_score),
                        AvgScores=round(100*sum_item_score/sum_item_max_score),
                        sum_time_in_secs=sum(sum_time_in_secs),
                        AvgDurations=round(sum_time_in_secs/NumbersOfItems),
                        None="")
            
            ByMonthGrade <- BySchoolGradeMonth %>%
              group_by(mo_yr_completed, grade) %>%
              summarize(school_name="ZZZ",    #assuming that ZZZ will be later alphabetically than any school name
                        NumbersOfItems=sum(NumbersOfItems),
                        NumbersOfStudents=sum(NumbersOfStudents),
                        ItemsPerStudent=round(NumbersOfItems/NumbersOfStudents),
                        sum_item_score=sum(sum_item_score),
                        sum_item_max_score=sum(sum_item_max_score),
                        AvgScores=round(100*sum_item_score/sum_item_max_score),
                        sum_time_in_secs=sum(sum_time_in_secs),
                        AvgDurations=round(sum_time_in_secs/NumbersOfItems),
                        None="")
          
           ByMonthGrade <- ByMonthGrade[c(3,2,1,4,5,6,7,8,9,10,11,12)] #order columns to match those of BySchoolGradeMonth

           Overall <- ByMonthGrade %>%
              group_by(grade) %>%
              summarize(mo_yr_completed=mindateless1mo, #temporarily put stats into earliest month - 1 month category
                        school_name="ZZZ",
                        NumbersOfItems=sum(NumbersOfItems),
                        NumbersOfStudents=sum(NumbersOfStudents),
                        ItemsPerStudent=round(NumbersOfItems/NumbersOfStudents),
                        sum_item_score=sum(sum_item_score),
                        sum_item_max_score=sum(sum_item_max_score),
                        AvgScores=round(100*sum_item_score/sum_item_max_score),
                        sum_time_in_secs=sum(sum_time_in_secs),
                        AvgDurations=round(sum_time_in_secs/NumbersOfItems),
                        None="")

            Overall <- Overall[c(3,1,2,4,5,6,7,8,9,10,11,12)] #order columns to match those of BySchoolGradeMonth
            
            #Bind all the rows into a data frame:
            BySchoolGradeMonth <- rbind.data.frame(BySchoolGradeMonth, BySchoolGrade, ByMonthGrade, Overall)
          }      
          
          #Change labels for Per-School marginals (stats over ALL time) from Aug 2017 to ALL:
          BySchoolGradeMonth$mo_yr_completed <- as.factor(BySchoolGradeMonth$mo_yr_completed)
          BySchoolGradeMonth$mo_yr_completed <- substr(as.character(BySchoolGradeMonth$mo_yr_completed),1,7)
          BySchoolGradeMonth$mo_yr_completed[BySchoolGradeMonth$mo_yr_completed==mindateless1mostring] <- "ALL"
          
          #Change labels for school_names so that ALL is displayed last:
          BySchoolGradeMonth$school_name <- as.factor(BySchoolGradeMonth$school_name)
          BySchoolGradeMonth$school_name <- as.character(BySchoolGradeMonth$school_name)
          BySchoolGradeMonth <- arrange(BySchoolGradeMonth, school_name)   #to ensure that ZZZ comes last
          BySchoolGradeMonth$school_name <- as.factor(BySchoolGradeMonth$school_name)
          levels(BySchoolGradeMonth$school_name)[levels(BySchoolGradeMonth$school_name)=="ZZZ"] <- "ALL"
          
          #Make the graph
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
              geom_text(aes(label = eval(as.name(input$extraStats)), y=0),
                        position = position_dodge(0.9),
                        vjust=0, hjust=0.5, color="blue")
        }
   }, height=600)
}

# Run the application 
shinyApp(ui = ui, server = server)