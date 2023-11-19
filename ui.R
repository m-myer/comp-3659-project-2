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
               
               conditionalPanel(
                 condition = "input.schedulingChoices == 'Shortest Job First'",
                 
                 checkboxInput(
                   inputId = "preemptive",
                   label = "Preemptive", 
                   value = FALSE
                 )
               ),
               
               conditionalPanel(
                 condition = "input.schedulingChoices == 'Round Robin'",
                 
                 numericInput(
                   inputId = "timeQuantum",
                   label = strong("Time Quantum (in Millisesconds)"),
                   value = 10, 
                   min = 1,
                   step = 1
                 )
               ), #RR Conditional Panel
               
               radioButtons(
                 inputId = "simType",
                 label = strong("Simulation Type"),
                 choices = list("Runtime",
                                "Throughput"),
                 selected = "Runtime",
                 inline = TRUE
               ),
               
               conditionalPanel(
                 condition = "input.simType == 'Runtime'",

                 numericInput(
                   inputId = "runtime",
                   label = strong("Simulation Runtime (in seconds?)"),
                   value = NULL,
                   min = 1,
                   step = 1
                 )
               ),
               
               conditionalPanel(
                 condition = "input.simType == 'Throughput'",

                 numericInput(
                   inputId = "numProcesses",
                   label = strong("Total Number of Processes"),
                   value = NULL,
                   min = 1,
                   step = 1
                 )
               )
             ),
             
             mainPanel(
               uiOutput("header")
             )
           )
  )
)