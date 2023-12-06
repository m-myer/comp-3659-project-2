ui <- fluidPage(
  navbarPage(title = "Scheduling Algorithm Simulator"),
  
  tabPanel(title = "Main",
           
           sidebarLayout( 
             sidebarPanel( # sidebar ----
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
               
               numericInput(
                 inputId = "seconds",
                 label = "Clock Speed",
                 value = 1000,
                 min = 1,
                 max = 1000,
                 step = 100
               ),
               
               conditionalPanel(
                 condition = "input.schedulingChoices == 'Shortest Job First'",
                 
                 checkboxInput(
                   inputId = "preemptive",
                   label = "Preemptive", 
                   value = FALSE
                 ),
                 
                 checkboxInput(
                   inputId = "estimateSJF",
                   label = "Use Exponential Averaging", 
                   value = FALSE
                 ),
                 
                 conditionalPanel(
                   condition = "input.estimateSJF",
                   
                   numericInput(
                     inputId = "alpha",
                     label = "Elasticity Constant (\\( \\alpha \\))",
                     value = 0.5,
                     min = 0,
                     max = 1,
                     step = 0.1
                   ),
                   
                   numericInput(
                     inputId = "defaultEst",
                     label = "Default Estimate",
                     value = 10,
                     min = 1,
                     max = 20,
                     step = 1
                   ),
                 ),
                 
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
                 inputId = "eventSkew",
                 label = strong("Distribution of IO Bursts"),
                 choices = list("Random",
                                "Even"),
                 selected = "Random",
                 inline = TRUE
               ),
               
               
               
               radioButtons(
                 inputId = "simType",
                 label = strong("Simulation Type"),
                 choices = list("Runtime",
                                "Throughput"),
                 selected = "Throughput",
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
               ),
               
               actionButton(
                 inputId = "procGen", 
                 label = "Generate New Process Batch"),
               
               actionButton(
                 inputId = "run", 
                 label = "Start"),
               
               actionButton(
                 inputId = "reset", 
                 label = "Reset"),
             ),
             
             mainPanel( # mainPanel ----
               uiOutput("header"),
               uiOutput("counter"),
               hr(),
               
               br(),
               h3("FCFS Stats"),
               tableOutput("fcfsStats"),
               br(),
               h3("RR Stats"),
               tableOutput("rrStats"),
               br(),
               h3("SJF Stats"),
               tableOutput("sjfStats"),
               br(),
               h3("SJF Exp Stats"),
               tableOutput("sjfExpStats"),
               br(),
               h3("SRTF Stats"),
               tableOutput("srtfStats"),
               br(),
               h3("SRTF Exp Stats"),
               tableOutput("srtfExpStats"),
               hr(),
               tableOutput("processTable"),
               br(),
               hr(),
               
               conditionalPanel( # FCFS ----
                 condition = "input.schedulingChoices == 'First Come First Serve'",
                 
                 titlePanel("First Come First Serve"),
                 br(),
                 hr(),
                 br(),
                 h4("New Process List"),
                 br(),
                 tableOutput("fcfsNew"),
                 br(),
                 br(),
                 h4("Ready Queue"),
                 br(),
                 tableOutput("fcfsReady"),
                 br(),
                 br(),
                 h4("Running"),
                 br(),
                 tableOutput("fcfsRunning"),
                 br(),
                 br(),
                 h4("Waiting"),
                 br(),
                 tableOutput("fcfsWaiting"),
                 br(),
                 br(),
                 h4("Terminated"),
                 br(),
                 tableOutput("fcfsTerminated"),
                 br(),
                 br(),
               ),
               
               conditionalPanel( # RR ----
                                 condition = "input.schedulingChoices == 'Round Robin'",
                                 
                                 titlePanel("Round Robin"),
                                 hr(),
                                 tableOutput("rrSlice"),
                                 br(),
                                 br(),
                                 h4("New Process List"),
                                 br(),
                                 tableOutput("rrNew"),
                                 br(),
                                 br(),
                                 h4("Ready Queue"),
                                 br(),
                                 tableOutput("rrReady"),
                                 br(),
                                 br(),
                                 h4("Running"),
                                 br(),
                                 tableOutput("rrRunning"),
                                 br(),
                                 br(),
                                 h4("Waiting"),
                                 br(),
                                 tableOutput("rrWaiting"),
                                 br(),
                                 br(),
                                 h4("Terminated"),
                                 br(),
                                 tableOutput("rrTerminated"),
                                 br(),
                                 br(),
               ),
               
               conditionalPanel( # SJF ----
                                 condition = "input.schedulingChoices == 'Shortest Job First'",
                                 
                                 titlePanel("Shortest Job First"),
                                 hr(),
                                 br(),
                                 h4("New Process List"),
                                 br(),
                                 tableOutput("sjfNew"),
                                 br(),
                                 br(),
                                 h4("Ready Queue"),
                                 br(),
                                 tableOutput("sjfReady"),
                                 br(),
                                 br(),
                                 h4("Running"),
                                 br(),
                                 tableOutput("sjfRunning"),
                                 br(),
                                 br(),
                                 h4("Waiting"),
                                 br(),
                                 tableOutput("sjfWaiting"),
                                 br(),
                                 br(),
                                 h4("Terminated"),
                                 br(),
                                 tableOutput("sjfTerminated"),
                                 br(),
                                 br(),
               ),
               
               hr(),
             )
           )
  )
)