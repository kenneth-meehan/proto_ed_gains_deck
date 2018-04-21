# Pleliminary Prototype Shiny App for Ed Learnosity Data

#Load libraries
library(shiny)
library(dplyr)
library(ggplot2)
library(lubridate)
library(scales)

df <- read.csv("data/FourDistrictsAnonSchoolAndClassNames.csv", colClasses = "character")

#Set data types properly etc
df$mo_yr_completed <- as.Date(floor_date(ymd_hms(df$date_completed),"month"))
df$item_score <- as.integer(df$item_score)
df$item_max_score <- as.integer(df$item_max_score)
df$time_in_secs <- as.integer(df$time_in_secs)
df <- df[nchar(df$grade)>0,]  #exclude records with grade of ""
df <- df[!is.na(df$grade),]   #exclude records without grade
df$grade <- paste("Grade", df$grade)
colr <- 'goldenrod'


# Define UI for application that draws a faceted graph
ui <- fluidPage(
   
   # Application title
   titlePanel("Ed Data Exploration"),
   
   # Sidebar with inputs 
   fluidRow(
      column(3,    #Use 3 of the 12 columns for the sidebar
             wellPanel(
                selectInput(inputId = "district", label = strong("District"),
                            choices = sort(unique(df$district_name)),
                            selected = "District B"),
                checkboxGroupInput("dims", "Dimensions",
                                   c("School" = "school_name",
                                     "Month" = "mo_yr_completed",
                                     "Grade" = "grade"),
                                   selected=c("mo_yr_completed"),
                                   inline=TRUE),
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
                               "Numbers of Items (total)" = "NumbersOfItems"))
             )
      ),
      
      # Show the graph
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

         
         if(is.null(input$dims)){
           #START OF OVERALL CASE
           #make overall stats
           Overall <- dist %>%
             summarize(NumbersOfItems=n(),
                       NumbersOfStudents=n_distinct(student_personal_refid),
                       ItemsPerStudent=round(NumbersOfItems/NumbersOfStudents),
                       sum_item_score=sum(item_score),
                       sum_item_max_score=sum(item_max_score),
                       AvgScores=round(100*sum_item_score/sum_item_max_score),
                       sum_time_in_secs=sum(time_in_secs),
                       AvgDurations=round(mean(time_in_secs)),
                       None="")
           #make graph
           ggplot(Overall, aes(x="", y=eval(as.name(input$yaxis)))) +
             geom_bar(stat="identity", fill=colr) +
             scale_y_continuous(labels=comma) +
             labs(title="Items District-Wide",
                  subtitle=paste("Blue Annotations:",input$extraStats),
                  x="", y=as.name(input$yaxis)) +
             theme(strip.text.y = element_text(angle=0),
                   legend.position="none") +
             geom_text(aes(label = eval(as.name(input$extraStats)), y=0),
                       position = position_dodge(0.9),
                       vjust=0, hjust=0.5, color="blue")
           #END OF OVERALL CASE
           
         }else{
           if(!is.na(input$dims[1]) & input$dims[1]=="school_name" &
              !is.na(input$dims[2]) & input$dims[2]=="mo_yr_completed" &
              !is.na(input$dims[3]) & input$dims[3]=="grade"){
              
              #START OF SCHOOL MONTH GRADE CASE
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
                BySchoolGrade <- dist %>%
                  group_by(school_name, grade) %>%
                  summarize(mo_yr_completed=mindateless1mo,
                            NumbersOfItems=n(),
                            NumbersOfStudents=n_distinct(student_personal_refid),
                            ItemsPerStudent=round(NumbersOfItems/NumbersOfStudents),
                            sum_item_score=sum(item_score),
                            sum_item_max_score=sum(item_max_score),
                            AvgScores=round(100*sum_item_score/sum_item_max_score),
                            sum_time_in_secs=sum(time_in_secs),
                            AvgDurations=round(sum_time_in_secs/NumbersOfItems),
                            None="")
                #make marginals by month
                ByMonthGrade <- dist %>%
                                group_by(mo_yr_completed, grade) %>%
                                summarize(school_name="ZZZ",    #assuming that ZZZ will be later alphabetically than any school name
                                          NumbersOfItems=n(),
                                          NumbersOfStudents=n_distinct(student_personal_refid),
                                          ItemsPerStudent=round(NumbersOfItems/NumbersOfStudents),
                                          sum_item_score=sum(item_score),
                                          sum_item_max_score=sum(item_max_score),
                                          AvgScores=round(100*sum_item_score/sum_item_max_score),
                                          sum_time_in_secs=sum(time_in_secs),
                                          AvgDurations=round(sum_time_in_secs/NumbersOfItems),
                                          None="")
                ByMonthGrade <- ByMonthGrade[c(3,2,1,4,5,6,7,8,9,10,11,12)] #order columns to match those of BySchoolGradeMonth
                #make overall stats
                Overall <- dist %>%
                           group_by(grade) %>%
                           summarize(mo_yr_completed=mindateless1mo, #temporarily put stats into earliest month - 1 month category
                                     school_name="ZZZ",
                                     NumbersOfItems=n(),
                                     NumbersOfStudents=n_distinct(student_personal_refid),
                                     ItemsPerStudent=round(NumbersOfItems/NumbersOfStudents),
                                     sum_item_score=sum(item_score),
                                     sum_item_max_score=sum(item_max_score),
                                     AvgScores=round(100*sum_item_score/sum_item_max_score),
                                     sum_time_in_secs=sum(time_in_secs),
                                     AvgDurations=round(sum_time_in_secs/NumbersOfItems),
                                    None="")
                Overall <- Overall[c(3,1,2,4,5,6,7,8,9,10,11,12)] #order columns to match those of BySchoolGradeMonth

                #Bind all the rows into a data frame:
                BySchoolGradeMonth <- rbind.data.frame(BySchoolGradeMonth, BySchoolGrade, ByMonthGrade, Overall)
              } #end code for marginals

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
              
              #Strip Grade down to numeric only
              BySchoolGradeMonth$grade <- as.integer(gsub("Grade ", "", BySchoolGradeMonth$grade))

              #Make the graph
              ggplot(BySchoolGradeMonth, aes(x=grade, y=eval(as.name(input$yaxis)))) +
                     geom_bar(stat="identity", fill=colr) +
                     scale_y_continuous(labels=comma) +
                     scale_x_continuous(breaks=seq(min(BySchoolGradeMonth$grade), max(BySchoolGradeMonth$grade), 2)) +
                     labs(title="Items by School, Month, and Grade",
                          subtitle=paste("Blue Annotations:",input$extraStats),
                          x="", y=as.name(input$yaxis)) +
                     facet_grid(school_name ~ as.factor(substr(mo_yr_completed,1,7))) +
                     theme(strip.text.y = element_text(angle=0),
                           strip.text.x = element_text(angle=60, hjust=0.5, vjust=1),
                           # axis.text.x = element_text(angle=45, vjust=1, hjust=1, size=7),
                           legend.position="none") +
                     geom_text(aes(label = eval(as.name(input$extraStats)), y=0),
                               position = position_dodge(0.9),
                               vjust=0, hjust=0.5, color="blue", size=3)
              #END OF SCHOOL MONTH GRADE CASE
           }else{
             if(!is.na(input$dims[1]) & input$dims[1]=="mo_yr_completed" &
                !is.na(input$dims[2]) & input$dims[2]=="grade"){

               #START OF MONTH GRADE CASE
               ByMonthGrade <- dist %>%
                 group_by(mo_yr_completed, grade) %>%
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
                 #make marginals by month
                 ByMonth <- dist %>%
                   group_by(mo_yr_completed) %>%
                   summarize(grade="Grade 20",  #surely higher than 12
                             NumbersOfItems=n(),
                             NumbersOfStudents=n_distinct(student_personal_refid),
                             ItemsPerStudent=round(NumbersOfItems/NumbersOfStudents),
                             sum_item_score=sum(item_score),
                             sum_item_max_score=sum(item_max_score),
                             AvgScores=round(100*sum_item_score/sum_item_max_score),
                             sum_time_in_secs=sum(time_in_secs),
                             AvgDurations=round(mean(time_in_secs)),
                             None="")
                 #make marginals by grade
                 ByGrade <- dist %>%
                   group_by(grade) %>%
                   summarize(mo_yr_completed=mindateless1mo,
                             NumbersOfItems=n(),
                             NumbersOfStudents=n_distinct(student_personal_refid),
                             ItemsPerStudent=round(NumbersOfItems/NumbersOfStudents),
                             sum_item_score=sum(item_score),
                             sum_item_max_score=sum(item_max_score),
                             AvgScores=round(100*sum_item_score/sum_item_max_score),
                             sum_time_in_secs=sum(time_in_secs),
                             AvgDurations=round(mean(time_in_secs)),
                             None="")
                 ByGrade <- ByGrade[c(2,1,3,4,5,6,7,8,9,10,11)] #order columns to match those of ByMonthGrade
                 #make overall stats
                 Overall <- dist %>%
                   summarize(grade="Grade 20",
                             mo_yr_completed=mindateless1mo,
                             NumbersOfItems=n(),
                             NumbersOfStudents=n_distinct(student_personal_refid),
                             ItemsPerStudent=round(NumbersOfItems/NumbersOfStudents),
                             sum_item_score=sum(item_score),
                             sum_item_max_score=sum(item_max_score),
                             AvgScores=round(100*sum_item_score/sum_item_max_score),
                             sum_time_in_secs=sum(time_in_secs),
                             AvgDurations=round(mean(time_in_secs)),
                             None="")
                 #Bind all the rows into a data frame:
                 ByMonthGrade <- rbind.data.frame(ByMonthGrade, ByMonth, ByGrade, Overall)
               }
               
               #Change labels for grades so that ALL is displayed last:
               ByMonthGrade$grade <- as.factor(ByMonthGrade$grade)
               ByMonthGrade$grade <- as.character(ByMonthGrade$grade)
               ByMonthGrade <- arrange(ByMonthGrade, grade)   #to ensure that Grade 20 comes last
               ByMonthGrade$grade <- as.factor(ByMonthGrade$grade)
               levels(ByMonthGrade$grade)[levels(ByMonthGrade$grade)=="Grade 20"] <- "ALL"
               
               #Change labels for Per-Grade marginals (stats over ALL time) from Aug 2017 to ALL:
               ByMonthGrade$mo_yr_completed <- as.factor(ByMonthGrade$mo_yr_completed)
               ByMonthGrade$mo_yr_completed <- substr(as.character(ByMonthGrade$mo_yr_completed),1,7)
               ByMonthGrade$mo_yr_completed[ByMonthGrade$mo_yr_completed==mindateless1mostring] <- "ALL"
               
               #Make the graph
               ggplot(ByMonthGrade, aes(x=mo_yr_completed, y=eval(as.name(input$yaxis)))) +
                 geom_bar(stat="identity", fill=colr) +
                 scale_y_continuous(labels=comma) +
                 labs(title="Items by Month and Grade",
                      subtitle=paste("Blue Annotations:",input$extraStats),
                      x="", y=as.name(input$yaxis)) +
                 facet_grid(grade ~ .) +
                 theme(strip.text.y = element_text(angle=0),
                       legend.position="none",
                       axis.text.x = element_text(angle=45, vjust=1, hjust=1)) +
                 geom_text(aes(label = eval(as.name(input$extraStats)), y=0),
                           position = position_dodge(0.9),
                           vjust=0, hjust=0.5, color="blue")
               #END OF MONTH GRADE CASE
               
             }else{
               if(!is.na(input$dims[1]) & input$dims[1]=="school_name" &
                  !is.na(input$dims[2]) & input$dims[2]=="grade"){
                 #START OF SCHOOL GRADE CASE
                 BySchoolGrade <- dist %>%
                   group_by(school_name, grade) %>%
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
                   BySchool <- dist %>%
                     group_by(school_name) %>%
                     summarize(grade="Grade 20",  #surely higher than 12
                               NumbersOfItems=n(),
                               NumbersOfStudents=n_distinct(student_personal_refid),
                               ItemsPerStudent=round(NumbersOfItems/NumbersOfStudents),
                               sum_item_score=sum(item_score),
                               sum_item_max_score=sum(item_max_score),
                               AvgScores=round(100*sum_item_score/sum_item_max_score),
                               sum_time_in_secs=sum(time_in_secs),
                               AvgDurations=round(mean(time_in_secs)),
                               None="")
                   #make marginals by grade
                   ByGrade <- dist %>%
                     group_by(grade) %>%
                     summarize(school_name="ZZZ",    #assuming that ZZZ will be later alphabetically than any school name
                               NumbersOfItems=n(),
                               NumbersOfStudents=n_distinct(student_personal_refid),
                               ItemsPerStudent=round(NumbersOfItems/NumbersOfStudents),
                               sum_item_score=sum(item_score),
                               sum_item_max_score=sum(item_max_score),
                               AvgScores=round(100*sum_item_score/sum_item_max_score),
                               sum_time_in_secs=sum(time_in_secs),
                               AvgDurations=round(mean(time_in_secs)),
                               None="")
                   ByGrade <- ByGrade[c(2,1,3,4,5,6,7,8,9,10,11)] #order columns to match those of BySchoolGrade
                   #make overall stats
                   Overall <- dist %>%
                     summarize(school_name="ZZZ",    #assuming that ZZZ will be later alphabetically than any school name
                               grade="Grade 20",
                               NumbersOfItems=n(),
                               NumbersOfStudents=n_distinct(student_personal_refid),
                               ItemsPerStudent=round(NumbersOfItems/NumbersOfStudents),
                               sum_item_score=sum(item_score),
                               sum_item_max_score=sum(item_max_score),
                               AvgScores=round(100*sum_item_score/sum_item_max_score),
                               sum_time_in_secs=sum(time_in_secs),
                               AvgDurations=round(mean(time_in_secs)),
                               None="")
                   #Bind all the rows into a data frame:
                   BySchoolGrade <- rbind.data.frame(BySchoolGrade, BySchool, ByGrade, Overall)
                 }
                 
                 #Change labels for grades so that ALL is displayed last:
                 BySchoolGrade$grade <- as.factor(BySchoolGrade$grade)
                 BySchoolGrade$grade <- as.character(BySchoolGrade$grade)
                 BySchoolGrade <- arrange(BySchoolGrade, grade)   #to ensure that Grade 20 comes last
                 BySchoolGrade$grade <- as.factor(BySchoolGrade$grade)
                 levels(BySchoolGrade$grade)[levels(BySchoolGrade$grade)=="Grade 20"] <- "ALL"
                 
                 #Change labels for school_names so that ALL is displayed last:
                 BySchoolGrade$school_name <- as.factor(BySchoolGrade$school_name)
                 BySchoolGrade$school_name <- as.character(BySchoolGrade$school_name)
                 BySchoolGrade <- arrange(BySchoolGrade, school_name)   #to ensure that ZZZ comes last
                 BySchoolGrade$school_name <- as.factor(BySchoolGrade$school_name)
                 levels(BySchoolGrade$school_name)[levels(BySchoolGrade$school_name)=="ZZZ"] <- "ALL"
                 
                 #Make the graph
                 ggplot(BySchoolGrade, aes(x=grade, y=eval(as.name(input$yaxis)))) +
                   geom_bar(stat="identity", fill=colr) +
                   scale_y_continuous(labels=comma) +
                   labs(title="Items by School and Grade",
                        subtitle=paste("Blue Annotations:",input$extraStats),
                        x="", y=as.name(input$yaxis)) +
                   facet_grid(school_name ~ .) +
                   theme(strip.text.y = element_text(angle=0),
                         legend.position="none",
                         axis.text.x = element_text(angle=45, vjust=1, hjust=1)) +
                   geom_text(aes(label = eval(as.name(input$extraStats)), y=0),
                             position = position_dodge(0.9),
                             vjust=0, hjust=0.5, color="blue")
                 #END OF SCHOOL GRADE CASE
               }else{
                 if(!is.na(input$dims[1]) & input$dims[1]=="school_name" &
                    !is.na(input$dims[2]) & input$dims[2]=="mo_yr_completed"){
                   #START OF SCHOOL MONTH CASE
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
                      BySchool <- dist %>%
                                  group_by(school_name) %>%
                                  summarize(mo_yr_completed=mindateless1mo,
                                            NumbersOfItems=n(),
                                            NumbersOfStudents=n_distinct(student_personal_refid),
                                            ItemsPerStudent=round(NumbersOfItems/NumbersOfStudents),
                                            sum_item_score=sum(item_score),
                                            sum_item_max_score=sum(item_max_score),
                                            AvgScores=round(100*sum_item_score/sum_item_max_score),
                                            sum_time_in_secs=sum(time_in_secs),
                                            AvgDurations=round(mean(time_in_secs)),
                                            None="")
                      #make marginals by month
                      ByMonth <- dist %>%
                                  group_by(mo_yr_completed) %>%
                                  summarize(school_name="ZZZ",    #assuming that ZZZ will be later alphabetically than any school name
                                            NumbersOfItems=n(),
                                            NumbersOfStudents=n_distinct(student_personal_refid),
                                            ItemsPerStudent=round(NumbersOfItems/NumbersOfStudents),
                                            sum_item_score=sum(item_score),
                                            sum_item_max_score=sum(item_max_score),
                                            AvgScores=round(100*sum_item_score/sum_item_max_score),
                                            sum_time_in_secs=sum(time_in_secs),
                                            AvgDurations=round(mean(time_in_secs)),
                                            None="")
                      ByMonth <- ByMonth[c(2,1,3,4,5,6,7,8,9,10,11)] #order columns to match those of BySchoolMonth
                      #make overall stats
                      Overall <- dist %>%
                                 summarize(mo_yr_completed=mindateless1mo, #temporarily put stats into earliest month - 1 month category
                                           school_name="ZZZ",    #assuming that ZZZ will be later alphabetically than any school name
                                           NumbersOfItems=n(),
                                           NumbersOfStudents=n_distinct(student_personal_refid),
                                           ItemsPerStudent=round(NumbersOfItems/NumbersOfStudents),
                                           sum_item_score=sum(item_score),
                                           sum_item_max_score=sum(item_max_score),
                                           AvgScores=round(100*sum_item_score/sum_item_max_score),
                                           sum_time_in_secs=sum(time_in_secs),
                                           AvgDurations=round(mean(time_in_secs)),
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
                        geom_bar(stat="identity", fill=colr) +
                        #scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") +
                        scale_y_continuous(labels=comma) +
                        labs(title="Items by School and Month",
                             subtitle=paste("Blue Annotations:",input$extraStats),
                             x="", y=as.name(input$yaxis)) +
                             facet_grid(school_name ~ .) +
                             theme(strip.text.y = element_text(angle=0),
                                   legend.position="none",
                                   axis.text.x = element_text(angle=45, vjust=1, hjust=1)) +
                             geom_text(aes(label = eval(as.name(input$extraStats)), y=0),
                                       position = position_dodge(0.9),
                                       vjust=0, hjust=0.5, color="blue")
                           
                     #END OF SCHOOL MONTH CASE
                 }else{
                   if(!is.na(input$dims[1]) & input$dims[1]=="school_name"){
                      #START OF SCHOOL CASE
                      BySchool <- dist %>%
                        group_by(school_name) %>%
                        summarize(mo_yr_completed=mindateless1mo,
                                  NumbersOfItems=n(),
                                  NumbersOfStudents=n_distinct(student_personal_refid),
                                  ItemsPerStudent=round(NumbersOfItems/NumbersOfStudents),
                                  sum_item_score=sum(item_score),
                                  sum_item_max_score=sum(item_max_score),
                                  AvgScores=round(100*sum_item_score/sum_item_max_score),
                                  sum_time_in_secs=sum(time_in_secs),
                                  AvgDurations=round(mean(time_in_secs)),
                                  None="")
                       #Make the graph
                       ggplot(BySchool, aes(x=school_name, y=eval(as.name(input$yaxis)))) +
                         geom_bar(stat="identity", fill=colr) +
                         scale_y_continuous(labels=comma) +
                         labs(title="Items by School",
                              subtitle=paste("Blue Annotations:",input$extraStats),
                              x="", y=as.name(input$yaxis)) +
                         theme(strip.text.y = element_text(angle=0),
                               axis.text.x = element_text(angle=45, vjust=1, hjust=1),
                               legend.position="none") +
                         geom_text(aes(label = eval(as.name(input$extraStats)), y=0),
                                   position = position_dodge(0.9),
                                   vjust=0, hjust=0.5, color="blue")
                      #END OF SCHOOL CASE
                   }else{
                     if(!is.na(input$dims[1]) & input$dims[1]=="mo_yr_completed"){
                       #START OF MONTH CASE
                       ByMonth <- dist %>%
                         group_by(mo_yr_completed) %>%
                         summarize(school_name="ZZZ",    #assuming that ZZZ will be later alphabetically than any school name
                                   NumbersOfItems=n(),
                                   NumbersOfStudents=n_distinct(student_personal_refid),
                                   ItemsPerStudent=round(NumbersOfItems/NumbersOfStudents),
                                   sum_item_score=sum(item_score),
                                   sum_item_max_score=sum(item_max_score),
                                   AvgScores=round(100*sum_item_score/sum_item_max_score),
                                   sum_time_in_secs=sum(time_in_secs),
                                   AvgDurations=round(mean(time_in_secs)),
                                   None="")
                        #Make the graph
                        ggplot(ByMonth, aes(x=mo_yr_completed, y=eval(as.name(input$yaxis)))) +
                          geom_bar(stat="identity", fill=colr) +
                          scale_x_date(date_breaks = "1 month", date_labels = "%Y-%m") +
                          scale_y_continuous(labels=comma) +
                          labs(title="Items by Month",
                               subtitle=paste("Blue Annotations:",input$extraStats),
                               x="", y=as.name(input$yaxis)) +
                          theme(strip.text.y = element_text(angle=0),
                                axis.text.x = element_text(angle=45, vjust=1, hjust=1),
                                legend.position="none") +
                          geom_text(aes(label = eval(as.name(input$extraStats)), y=0),
                                    position = position_dodge(0.9),
                                    vjust=0, hjust=0.5, color="blue")
                       #END OF MONTH CASE
                     }else{
                       if(!is.na(input$dims[1]) & input$dims[1]=="grade"){
                         #START OF GRADE CASE
                         ByGrade <- dist %>%
                           group_by(grade) %>%
                           summarize(mo_yr_completed=mindateless1mo,
                                     NumbersOfItems=n(),
                                     NumbersOfStudents=n_distinct(student_personal_refid),
                                     ItemsPerStudent=round(NumbersOfItems/NumbersOfStudents),
                                     sum_item_score=sum(item_score),
                                     sum_item_max_score=sum(item_max_score),
                                     AvgScores=round(100*sum_item_score/sum_item_max_score),
                                     sum_time_in_secs=sum(time_in_secs),
                                     AvgDurations=round(mean(time_in_secs)),
                                     None="")
                         #Make the graph
                         ggplot(ByGrade, aes(x=grade, y=eval(as.name(input$yaxis)))) +
                           geom_bar(stat="identity", fill=colr) +
                           scale_y_continuous(labels=comma) +
                           labs(title="Items by Grade",
                                subtitle=paste("Blue Annotations:",input$extraStats),
                                x="", y=as.name(input$yaxis)) +
                           theme(strip.text.y = element_text(angle=0),
                                 axis.text.x = element_text(angle=45, vjust=1, hjust=1),
                                 legend.position="none") +
                           geom_text(aes(label = eval(as.name(input$extraStats)), y=0),
                                     position = position_dodge(0.9),
                                     vjust=0, hjust=0.5, color="blue")
                         #END OF GRADE CASE
                       }
                     }
                   }
                 }
               }
             }
           }
         }
       }, height=600)
}

# Run the application 
shinyApp(ui = ui, server = server)