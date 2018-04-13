# Prototype Shiny App for Ed/Learnosity Data
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#

#Load libraries
library(shiny)
library(dplyr)
library(ggplot2)
library(lubridate)
library(scales)

#Read data
setwd("data")
files.to.read <- list.files(pattern="*.zip$") #generate list of .zip filenames
filesread <- data.frame(matrix(ncol=2, nrow=0))
colnames(filesread) <- c("Input File Name", "Records Read")
f <- 0  #index for dataframe to display input file names and record counts.
for (zipfile in files.to.read){
  f <- f+1
  csvfile <- gsub(".zip$", "", zipfile)  #csv inside zip file has almost the same name as zip file.
  df <- read.csv(unz(zipfile,csvfile), colClasses = "character")
  filesread[f,1] <- zipfile
  filesread[f,2] <- nrow(df)
  if (!exists("dfall")){        #create dfall first time through
    dfall <- read.table(text = "", #create a blank dfall dataframe with the proper columns
                        colClasses = "character",
                        col.names = colnames(df))
  }
  dfall <- rbind(dfall, df) #add current df to the bottom of dfall
}
df <- dfall
rm(dfall)
setwd("..")

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
   sidebarLayout(
      sidebarPanel(
        selectInput(inputId = "district", label = strong("District"),
                    choices = sort(unique(df$district_name)),
                    selected = "Broward Co School District"),
        checkboxInput("gradesorno", "Show grade-level detail?", FALSE),
        #checkboxInput("scoresorno", "Show average scores?", FALSE)
        selectInput(inputId = "extraStats", label = strong("Show Additional Stats?"),
                    choices = c("Number of Students", "Average Scores", "Average Durations in Seconds", "None"),
                    selected = "None")
      ),
      
      # Show a faceted graph
      mainPanel(
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
            summarize(nItems=n(),
                      nStudents=n_distinct(student_personal_refid),
                      avgScore=sum(item_score)/sum(item_max_score),
                      avgDur=mean(time_in_secs))
          
            #Eliminate display of stats as user specifies
            if(input$extraStats=="None"){
              BySchoolMonth$nStudents <- NA
              BySchoolMonth$avgScore <- NA
              BySchoolMonth$avgDur <- NA
            }
            if(input$extraStats=="Number of Students"){
              BySchoolMonth$avgScore <- NA
              BySchoolMonth$avgDur <- NA
            }
            if(input$extraStats=="Average Scores"){
              BySchoolMonth$nStudents <- NA
              BySchoolMonth$avgDur <- NA
            }
            if(input$extraStats=="Average Durations in Seconds"){
              BySchoolMonth$nStudents <- NA
              BySchoolMonth$avgScore <- NA
          }

          ggplot(BySchoolMonth, aes(x=mo_yr_completed, y=nItems)) +
            geom_bar(stat="identity", fill='goldenrod') +
            scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") +
            scale_y_continuous(labels=comma) +
            labs(title="Ed Learnosity Items by School and Month",
                 subtitle=paste("Blue Annotations:",input$extraStats),
                 x="", y="Number of Items") +
            facet_grid(school_name ~ .) +
            theme(strip.text.y = element_text(angle=0),
                  legend.position="none") + 
            geom_text(aes(label = nStudents, y = 0),
                      position = position_dodge(0.9),
                      vjust=0, hjust=0.5, color="blue") +
            geom_text(aes(label = round(100*avgScore), y = 0),
                      position = position_dodge(0.9),
                      vjust=0, hjust=0.5, color="blue") +
            geom_text(aes(label = round(avgDur), y = 0),
                      position = position_dodge(0.9),
                      vjust=0, hjust=0.5, color="blue")

       }else{
         
          BySchoolGradeMonth <- dist %>%
            group_by(school_name, grade, mo_yr_completed) %>%
            summarize(nItems=n(),
                      nStudents=n_distinct(student_personal_refid),
                      avgScore=sum(item_score)/sum(item_max_score),
                      avgDur=mean(time_in_secs))
            
          ggplot(BySchoolGradeMonth, aes(x=grade, y=nItems)) +
              geom_bar(stat="identity", fill='goldenrod') +
              scale_y_continuous(labels=comma) +
              labs(title="Ed Learnosity Items by School, Month, and Grade",
                   x="", y="Number of Items") +
              facet_grid(school_name ~ as.factor(substr(mo_yr_completed,1,7))) +
              theme(strip.text.y = element_text(angle=0),
                    axis.text.x = element_text(angle=90, vjust=0.5),
                    legend.position="none")
        }
   })
}

# Run the application 
shinyApp(ui = ui, server = server)