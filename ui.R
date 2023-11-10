ui <- fluidPage(
  navbarPage(title = "Scheduling Algorithm Simulator"),
  
  tabPanel(title = "Main",
           
           sidebarLayout(
             sidebarPanel(
               withMathJax(),
               useShinyjs(),
               id = "sidebar",
               
               selectizeInput(
                 inputId = "schedulingChoices",
                 label = strong("Choose Scheduling Algorithm"),
                 choices = c("", 
                             "First Come First Serve", 
                             "Shortest Job First", 
                             "Round Robin", 
                             "Multilevel Queue"),
                 options = list(
                   placeholder = 'Please select an option below'),
               ),
             ),
             
             mainPanel(
               h1("Stuff will go here")
             )
           )
  )
)