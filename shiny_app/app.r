
library(shinythemes)
library(shiny)
library(DT)
library(ggplot2)

#you must set the working directory
setwd("c:/Users/milos_milic/Desktop/datasets/JODIE_RNN/")


#defines data frame for prediction data
df <- read.csv("87294_prediction_data.csv")

#defines data frame for test data
df2 <- read.csv("87294_test_data.csv")

#reads in the data frame that has the choices for patients
choice_table <- read.csv("choice_list.csv")
choice_table_var <- read.csv("column_choice.csv")

#now we are just using the id column 
mylist <- choice_table$id
mylist2<- choice_table_var$acronym

 
  
#how mancy checkboxes can be selected
#https://stackoverflow.com/questions/31139791/checkboxgroupinput-set-minimum-and-maximum-number-of-selections-ticks
my_min <- 1
my_max <- 3


#https://stackoverflow.com/questions/33105044/make-list-for-shiny-dropdown-selectinput
#mylist <- as.list(choice_table$id)
# Name it
#names(mylist) <- choice_table$patient


ui <- fluidPage(theme = shinytheme("united"),
  titlePanel( strong(" Jodie RNN validation output")),
 # 
#  sidebarLayout(position = "left",
#                sidebarPanel("sidebar panel",   #the button that defines 
#                             #selectInput(inputId = "selectInputo", label = "PATIENT ID",choices = mylist ),
#                             #selectInput(inputId = "var1", label = "VARIABLE 1",choices = mylist2, selected = "cog1" ),
#                             #selectInput(inputId = "var2", label = "VARIABLE 2",choices = mylist2, selected = "cog2" ),
#                             #selectInput(inputId = "var3", label = "VARIABLE 3",choices = mylist2, selected = "cog3" )
#                             
#                             
#                             #https://gallery.shinyapps.io/069-widget-check-group/
#                             #does not really work, will use dropdown
#                             #checkboxGroupInput("checkGroup", label = h3("Checkbox group"), 
#                             #                   choices = list("Choice 1" = "geno1", "Choice 2" = "geno2", "Choice 3" = "geno3",
#                             #                                  "Choice 4" = "img1", "Choice 5" = "img2", "Choice 6" = "img3",
#                             #                                  "Choice 7" = "cog1", "Choice 8" = "cog2", "Choice 9" = "cog3",
#                             #                                  "Choice 10" = "ses"),
#                             #                   selected = 1),
#                             
#                             
#                             
#                             ),
                
               
                
                mainPanel(h3("Choose the patient ID and the variables you would like to compare"),
                          
                          selectInput(inputId = "selectInputo", label = "PATIENT ID",choices = mylist ),
                          div(style="display: inline-block;vertical-align:middle; width: 150px;",selectInput(inputId = "var1", label = "VARIABLE 1",choices = mylist2, selected = "cog1" )),
                          div(style="display: inline-block;vertical-align:middle; width: 150px;",selectInput(inputId = "var2", label = "VARIABLE 2",choices = mylist2, selected = "cog2" )),
                          div(style="display: inline-block;vertical-align:middle; width: 150px;",selectInput(inputId = "var3", label = "VARIABLE 3",choices = mylist2, selected = "cog3" )),
                          
                          
                          fluidPage(
                            #How ro do plots side to side
                            #https://stackoverflow.com/questions/34384907/how-can-put-multiple-plots-side-by-side-in-shiny-r
                          fluidRow(splitLayout(cellWidths = c("66%", "66%","66%"),plotOutput("my_cog1_hist"),plotOutput("my_cog2_hist"),plotOutput("my_cog3_hist"))),
                          
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
              # for sidebar),
  
  

  
 
  
)


server <- shinyServer(function(input, output, session) {
  
  #https://stackoverflow.com/questions/41246254/filter-renderdatatable-shiny
  updateSelectInput(session, "selectInputo", label = NULL, choices = mylist,
                    selected = NULL)
  
  #how to filter the data reactive element
  #
  filterData <- reactive({
    df[which(df$id == input$selectInputo),]
    
  })
  
  
  #how to filter the data reactive element
  filterData2 <- reactive({
    df2[which(df2$id == input$selectInputo),]
    
  })
  
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
  #output$my_cog2_hist_test<- renderPlot(ggplot(filterData2(), aes(x= period, y = cog2)) + geom_line() )
  
  #output$my_cog3_hist_pred<- renderPlot(ggplot(filterData(), aes(x= period, y = cog3)) + geom_line() )
  #output$my_cog3_hist_test<- renderPlot(ggplot(filterData2(), aes(x= period, y = cog3)) + geom_line() )
  
  
  
  
  
  output$table_pred <- DT::renderDataTable({
    DT::datatable(filterData(),selection="single",rownames = F,  options = list(searching = FALSE,pageLength =18))
  })
  
  
  
  output$table_test <- DT::renderDataTable({
    DT::datatable(filterData2(),selection="single",rownames = F, options = list(searching = FALSE,pageLength =18))
  })
  
  #rendering the datatable for rediction data
  #output$table_pred <- DT::renderDataTable(filterData(),selection="single",rownames = F,df, options = list(pageLength =5))
  #output$table_pred <- DT::renderDataTable(df, options = list(pageLength =5), searchBuilder.columns = df$id)
  #output$table_test <- DT::renderDataTable(df2,options = list(pageLength =10))
  
})

shinyApp(ui, server)