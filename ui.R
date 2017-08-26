library(shiny)
library(ggplot2)

shinyUI(
  navbarPage(strong("I journal"),
             ############################################ PAGE 1: input log
             tabPanel("I do", 
               sidebarLayout(
                 sidebarPanel(
                   titlePanel("I do"),
                   p("This is your journal log of what you've done (eaten, worked-out, etc..)."),
                   dateInput("when", "When?", value = Sys.Date(),weekstart = 1),
                   selectInput('what', 'What?', c("..Loading..")     ),
                   radioButtons('quantity', 'How much?',c("..Loading..")  ),
                   actionButton("addbutton", "Add",width = "100%")
                 ),
                 mainPanel( 
                   h3(textOutput("outputtext_food")),
                   dataTableOutput('table'),
                   plotOutput("wordcloudje")
                 )
               )
             ),
             ############################################ PAGE 2: Complaint log
             tabPanel("I feel",
                      sidebarLayout(
                        sidebarPanel(
                          titlePanel("I feel"),
                          p("This is your journal log of how you feel (energetic, stressed, etc..)."),
                          dateInput("when_complaint", "When?", value = Sys.Date(),weekstart = 1),
                          selectInput('what_complaint', 'What?', c("..Loading..")     ),
                          radioButtons('quantity_complaint', 'How much?',c("..Loading..")  ),
                          actionButton("addbutton_complaint", "Add",width = "100%")
                        ),
                        mainPanel( 
                          h3(textOutput("outputtext_food_complaint")),
                          dataTableOutput('table_complaint'),
                          plotOutput("wordcloudje_complaint")
                        )
                      )
             ),
             ############################################ PAGE 3: Graphs
             tabPanel("I learn",
                      sidebarLayout(
                        sidebarPanel(
                          titlePanel("I learn"),
                          p("In this analysis we search for patterns in your journal: what causes what you feel?"),
                          checkboxGroupInput("graph_checkbox_complaint", label = "Filter: I feel", choices = "..Loading.."),
                          checkboxGroupInput("graph_checkbox", label = "Filter: I do:", choices = "..Loading..",inline = T),
                          dateInput("daterange_from", label = "Show from date:", value = Sys.Date()-7,weekstart = 1),
                          dateInput("daterange_until", label = "Show until date:", value = Sys.Date(),weekstart = 1),
                          actionButton("show_graph", "Show graph",width = "100%")
                        ),
                        mainPanel( 
                          plotOutput("myChartGrid")
                        )
                        
                      )
             ),
             ############################################ PAGE 4: add new types of input
             tabPanel(em("Add new: 'I do'"),
               sidebarLayout(
                 sidebarPanel(
                   titlePanel("Add new: 'I do'"),
                   p("Here you can add new types of I do's and their belonging categories. For example: "),
                   em("Work-out (Sport)"),br(),
                   em("Cashew Nuts (Nuts)"),
                   p(" "),
                   textInput("input_types_name", label = "I do (Name)", value = ""),
                   textInput("input_types_group", label = "I do (Category)", value = ""),
                   actionButton("addbutton_input_types", "Add", width = "100%")
                 ),
                 mainPanel( 
                   h3(textOutput("outputtext_input_types")),
                   dataTableOutput('table_input_types')
                 )
               )
             ),
             ############################################ PAGE 5: add new types of complaints
             tabPanel(em("Add new: 'I feel'"),
               sidebarLayout(
                 sidebarPanel(
                   titlePanel("Add new: 'I feel'"),
                   p("Here you can add new types of I feel's and their belonging categories. For example: "),
                   em("Dry skin (Allergy)"),br(),
                   em("Energy level (Self being)"),
                   p(" "),
                   textInput("complaint_types_name", label = "I feel (Name)", value = ""),
                   textInput("complaint_types_group", label = "I feel (Category)", value = ""),
                   actionButton("addbutton_complaint_types", "Add", width = "100%")
                 ),
                 mainPanel( 
                   h3(textOutput("outputtext_complaint_types")),
                   dataTableOutput('table_complaint_types')
                 )
               )
             )
             ############################################## next page here below:
   
             
      ###end
  )
)