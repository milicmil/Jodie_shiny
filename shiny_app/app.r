
library(shinythemes)
library(shiny)
library(DT)
library(ggplot2)

#you must set the working directory
setwd("c:/Jodie_NN_projects/JODIE_RNN_Shiny/")


#defines data frame for prediction data
pred_data <- read.csv("87294_prediction_data.csv")

#defines data frame for test data
test_data <- read.csv("87294_test_data.csv")

#reads in the data frame that has the choices for patients
choice_table <- read.csv("choice_list.csv")

#table that staores what variables to compare
choice_table_var <- read.csv("column_choice.csv")


#how many checkboxes can be selected
#https://stackoverflow.com/questions/31139791/checkboxgroupinput-set-minimum-and-maximum-number-of-selections-ticks
my_min <- 1
my_max <- 3


#https://stackoverflow.com/questions/33105044/make-list-for-shiny-dropdown-selectinput


ui <- fluidPage(theme = shinytheme("united"),
  titlePanel( h1("Patient prognosis estimation",align = "center")),
   
                mainPanel(h3("Choose the patient ID"),
                          h3("and the variables you would like to compare"),
                          div("Red is the predicted prognosis", style = "color:red"),div("Blue is the test data", style = "color:blue"),
                          
                          
                          #https://stackoverflow.com/questions/58049336/r-shiny-selecting-multiple-variables-with-one-checkbox
                          checkboxGroupInput(inputId= "checkGroupGender", label = h3("Checkbox group"), 
                                             choices = list("Male" = 1, "Female" = 0), 
                                             selected = 1),
                          
                        
                          
                          selectInput(inputId = "selectInputo", label = "PATIENT ID",choices = choice_table$id ),
                          
                          #shows everything in one row
                          div(style="display: inline-block;vertical-align:middle; width: 100px;",selectInput(inputId = "var1", label = "VARIABLE 1",choices = choice_table_var$acronym, selected = "cog1" )),
                          div(style="display: inline-block;vertical-align:middle; width: 100px;",selectInput(inputId = "var2", label = "VARIABLE 2",choices = choice_table_var$acronym, selected = "cog2" )),
                          div(style="display: inline-block;vertical-align:middle; width: 100px;",selectInput(inputId = "var3", label = "VARIABLE 3",choices = choice_table_var$acronym, selected = "cog3" )),
                          
                          
                          fluidPage(
                            #How ro do plots side to side
                            #https://stackoverflow.com/questions/34384907/how-can-put-multiple-plots-side-by-side-in-shiny-r
                          fluidRow(splitLayout(cellWidths = c("50%", "50%","50%"),plotOutput("my_cog1_hist"),plotOutput("my_cog2_hist"),plotOutput("my_cog3_hist"))),
                          
                                   ),
                          #plotOutput("my_cog1_hist_pred"),
                          #plotOutput("my_cog1_hist_test"),
                          
                          
                          h3("Prediction Data", style = "color:red"),
                          #shows the table https://rstudio.github.io/DT/
                          #shows fluidrow https://shiny.rstudio.com/reference/shiny/1.0.2/renderDataTable.html
                          fluidRow(column(12,dataTableOutput('table_pred'))),
                          #https://shiny.rstudio.com/tutorial/written-tutorial/lesson2/
                          h3("Test Data", style = "color:blue"),
                          
                          fluidRow(column(12,dataTableOutput('table_test'))
                          ))
)


server <- shinyServer(function(input, output, session) {
  


  #https://stackoverflow.com/questions/49373540/reactive-updating-of-two-related-selectizeinput-widgets-in-shiny  
  filterData_sex <- reactive({
    #choice_table[which(choice_table$msex == ( input$gender2 | input$gender ) ),]
    choice_table[which(choice_table$msex %in% ( input$checkGroupGender ) ),]
    
  })
  
  #https://stackoverflow.com/questions/41246254/filter-renderdatatable-shiny   NOT NEEDED
  # updateSelectInput(session, "selectInputo", label = NULL, choices = mylist,
  #                  selected = NULL)
  
  
  #How to reactively update 
  #https://stackoverflow.com/questions/21465411/r-shiny-passing-reactive-to-selectinput-choices
 observe({
 updateSelectInput(session, "selectInputo", label = NULL, choices = filterData_sex(),
                   selected = NULL)
 })

  #how to filter the data reactive element prediction data
  
  
  #This is filetering on te drop down
  filterData <- reactive({
    pred_data[which(pred_data$id == input$selectInputo),]
    
  })
  
  
  #how to filter the data reactive element for the line test data
  filterData2 <- reactive({
    test_data[which(test_data$id == input$selectInputo),]
    
  })
  
  
  #pred_data filterdata() red is prediction data test_data filterdata2() blue is test data
  
  
  
  
  
  #how to plot differn
  #https://stackoverflow.com/questions/9109156/ggplot-combining-two-plots-from-different-data-frames
  #https://community.rstudio.com/t/ggplot-set-colors-separately-for-geom-point-and-geom-line-manually/13901
  
  #for the checkbox
  #output$my_cog1_hist<- renderPlot(ggplot(filterData(), aes_string(x= "period", y = input$checkGroup), ) + geom_line(size =1, color = "red") + geom_line(data= filterData2(), size =1, color = "blue") )
  
  #need to use aes_string for the variable, also change x https://stackoverflow.com/questions/49473915/r-how-do-i-use-selectinput-in-shiny-to-change-the-x-and-fill-variables-in-a-gg
  output$my_cog1_hist<- renderPlot(ggplot(filterData(), aes_string(x= "period", y = input$var1) ) + geom_line(size =1, color = "red") + geom_line(data= filterData2(), size =1, color = "blue") 
                                   + ggtitle("Variable 1 over period analysis") + theme(plot.title = element_text(hjust = 0.5)))
  
  
  output$my_cog2_hist<- renderPlot(ggplot(filterData(), aes_string(x= "period", y = input$var2)) + geom_line(size =1, color = "red") + geom_line(data= filterData2(), size =1, color = "blue") 
                                   + ggtitle("Variable 2 over period analysis") + theme(plot.title = element_text(hjust = 0.5)))
  
  output$my_cog3_hist<- renderPlot(ggplot(filterData(), aes_string(x= "period", y = input$var3)) + geom_line(size =1, color = "red") + geom_line(data= filterData2(), size =1, color = "blue") 
                                   + ggtitle("Variable 3 over period analysis") + theme(plot.title = element_text(hjust = 0.5)))

  
  
  
  output$table_pred <- DT::renderDataTable({
    DT::datatable(filterData(),selection="single",rownames = F,  options = list(searching = FALSE,pageLength =18))
  })
  
  
  
  output$table_test <- DT::renderDataTable({
    DT::datatable(filterData2(),selection="single",rownames = F, options = list(searching = FALSE,pageLength =18))
  })
  
  #rendering the datatable for rediction data
  #output$table_pred <- DT::renderDataTable(filterData(),selection="single",rownames = F,pred_data, options = list(pageLength =5))
  #output$table_pred <- DT::renderDataTable(pred_data, options = list(pageLength =5), searchBuilder.columns = pred_data$id)
  #output$table_test <- DT::renderDataTable(test_data,options = list(pageLength =10))
  
})

shinyApp(ui, server)