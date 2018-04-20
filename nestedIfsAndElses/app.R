#Figure out how to use the checkBoxGroupInput function with nested if else.
#Apply this then to user choices of dimensions in prototype ed gains deck to
#draw different graphs depending on user's choice of dimensions.
#Need spearate if statement for case when no checkboxes are checked!

library(ggplot2)
  
ui <- fluidPage(
    checkboxGroupInput("dims", "Dimensions",
                       c("school" = "school_name",
                         "month" = "mo_yr_completed",
                         "grade" = "grade"),
                       inline=TRUE),
    plotOutput("bogusPlot")
)
  
server <- function(input, output, session) {
    output$bogusPlot <- renderPlot({

      if(is.null(input$dims)){
        msg <- "nothing"
      }else{
        if(!is.na(input$dims[1]) & input$dims[1]=="school_name" &
           !is.na(input$dims[2]) & input$dims[2]=="mo_yr_completed" &
           !is.na(input$dims[3]) & input$dims[3]=="grade"){
          msg <- "school, month, grade"
        }else{
          if(!is.na(input$dims[1]) & input$dims[1]=="mo_yr_completed" &
             !is.na(input$dims[2]) & input$dims[2]=="grade"){
            msg <-  "month, grade"
          }else{
            if(!is.na(input$dims[1]) & input$dims[1]=="school_name" &
               !is.na(input$dims[2]) & input$dims[2]=="grade"){
              msg <- "school, grade"
            }else{
              if(!is.na(input$dims[1]) & input$dims[1]=="school_name" &
                 !is.na(input$dims[2]) & input$dims[2]=="mo_yr_completed"){
                msg <- "school, month"
              }else{
                if(!is.na(input$dims[1]) & input$dims[1]=="school_name"){
                  msg <- "school" 
                }else{
                  if(!is.na(input$dims[1]) & input$dims[1]=="mo_yr_completed"){
                    msg <- "month" 
                  }else{
                    if(!is.na(input$dims[1]) & input$dims[1]=="grade"){
                      msg <- "grade" 
                    }
                  }
                }
              }
            }
          }
        }
      }
      ggplot(mtcars, aes(wt,wt)) +
        theme(axis.title=element_blank(),
              axis.text =element_blank(),
              axis.ticks=element_blank(),
              panel.background=element_blank()) +
        annotate("text", x=3, y=3, label=msg, size=30, color="red")
    })
}
  
shinyApp(ui, server)