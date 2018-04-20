#Figure out how to use the checkBoxGroupInput function with dplyr's case_when function.
#Apply this then to user choices of dimensions in prototype ed gains deck to
#draw different graphs depending on user's choice of dimensions.
#Need spearate if statement for case when no checkboxes are checked!

library(dplyr)
library(ggplot2)
  
ui <- fluidPage(
    checkboxGroupInput("dims", "Dimensions",
                       c("school" = "school",
                         "month" = "month",
                         "grade" = "grade"),
                       inline=TRUE),
    plotOutput("bogusPlot")
)
  
server <- function(input, output, session) {
    output$bogusPlot <- renderPlot({

      if(!is.null(input$dims)){
        
        if(!is.na(input$dims[1]) & input$dims[1]=="school" &
           !is.na(input$dims[2]) & input$dims[2]=="month" &
           !is.na(input$dims[3]) & input$dims[3]=="grade"){
        msg <- "(S,M,G) dimensions chosen!"
        ggplot(mtcars, aes(wt,wt)) +
          annotate("text", x=3, y=3, label=msg, size=20, color="red")
      
        }else{
          
          if(!is.na(input$dims[1]) & input$dims[1]=="month" &
             !is.na(input$dims[2]) & input$dims[2]=="grade"){
          msg <- "(M,G) dimensions chosen!"
          ggplot(mtcars, aes(wt,wt)) +
            annotate("text", x=3, y=3, label=msg, size=20, color="red")
          }else{
            
            if(!is.na(input$dims[1]) & input$dims[1]=="school" &
               !is.na(input$dims[2]) & input$dims[2]=="grade"){
              msg <- "(S,G) dimensions chosen!"
              ggplot(mtcars, aes(wt,wt)) +
                annotate("text", x=3, y=3, label=msg, size=20, color="red")
            }else{
              
              if(!is.na(input$dims[1]) & input$dims[1]=="school" &
                 !is.na(input$dims[2]) & input$dims[2]=="month"){
                msg <- "(S,M) dimensions chosen!"
                ggplot(mtcars, aes(wt,wt)) +
                  annotate("text", x=3, y=3, label=msg, size=20, color="red")
              }else{
                if(!is.na(input$dims[1]) & input$dims[1]=="school"){
                  msg <- "(S) dimension chosen!"
                  ggplot(mtcars, aes(wt,wt)) +
                    annotate("text", x=3, y=3, label=msg, size=20, color="red") 
                }else{
                  if(!is.na(input$dims[1]) & input$dims[1]=="month"){
                    msg <- "(M) dimension chosen!"
                    ggplot(mtcars, aes(wt,wt)) +
                      annotate("text", x=3, y=3, label=msg, size=20, color="red") 
                  }else{
                    if(!is.na(input$dims[1]) & input$dims[1]=="grade"){
                      msg <- "(G) dimension chosen!"
                      ggplot(mtcars, aes(wt,wt)) +
                        annotate("text", x=3, y=3, label=msg, size=20, color="red") 
                    }else{
                      msg <- "No dimensions chosen!"
                      ggplot(mtcars, aes(wt,wt)) +
                        annotate("text", x=3, y=3, label=msg, size=20, color="red") 
                    }
                  }
                }
              }
            }
          }
          
      }}else{
        
        msg <- "No dimensions chosen!"
        ggplot(mtcars, aes(wt,wt)) +
          annotate("text", x=3, y=3, label=msg, size=20, color="red")
      }  
        
    })
}
  
shinyApp(ui, server)