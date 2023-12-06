ui <- fluidPage(
  navbarPage(title = "Scheduling Algorithm Simulator"),
  
  tabPanel(title = "Main",
           
           sidebarLayout( 
             sidebarPanel( # sidebar ----
               withMathJax(),
               shinyjs::useShinyjs(),
               id = "sidebar",
               
               selectizeInput(
                 inputId = "schedulingChoices",
                 label = strong("Choose Scheduling Algorithm"),
                 choices = c("", 
                             "First Come First Serve", 
                             "Shortest Job First", 
                             "Round Robin", 
                             "Multilevel Queue"),
                 multiple = TRUE,
                 options = list(
                   placeholder = 'Please select an option below'),
               ),
               
               sliderInput(
                 inputId = "seconds",
                 label = "Clock Speed",
                 value = 100,
                 min = 1,
                 max = 1000,
                 ticks = FALSE
               ),
               
               conditionalPanel(
                 condition = "input.schedulingChoices.indexOf('Shortest Job First') > -1",
                 
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
                 condition = "input.schedulingChoices.indexOf('Round Robin') > -1",
                 
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

                 sliderInput(
                   inputId = "numProcesses",
                   label = strong("Total Number of Processes"),
                   value = 10,
                   min = 1,
                   max = 50,
                   step = 1,
                   ticks = FALSE
                 ),
                 
                 numericInput(
                   inputId = "cpuLambda",
                   label = strong("CPU Burst length Distribution Mean"),
                   value = 30,
                   min = 1,
                   max = 50,
                   step = 1
                 ),
                 
                numericInput(
                  inputId = "minEvents",
                  label = strong("Minimum # of IO Bursts"),
                  value = 0,
                  min = 0,
                  max = 10,
                  step = 1
                ),
                 
                 numericInput(
                   inputId = "maxEvents",
                   label = strong("Maximum # of IO Bursts"),
                   value = 10,
                   min = 0,
                   max = 10,
                   step = 1
                 ),
              
                 
                 numericInput(
                   inputId = "ioLambda",
                   label = strong("Event Burst length Distribution Mean"),
                   value = 30,
                   min = 1,
                   max = 50,
                   step = 1
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
               
               actionButton(
                 inputId = "resetStats", 
                 label = "Reset Stats"),
               
               dropdownButton(
                 tags$h3("Graph Options"),
                 
                 selectizeInput(inputId = 'graphs',
                                label = 'Select one or more graphs',
                                multiple = TRUE,
                                choices = c("Average Wait Time",
                                            "Average Turnaround Time"),
                                selected = ""),
                 
                 circle = TRUE, status = "danger",
                 icon = icon("chart-simple"), width = "300px",
                 
                 tooltip = tooltipOptions(title = "Click to see inputs !")
               ),
             ),
             
             mainPanel( # mainPanel ----
               useShinyjs(),
               uiOutput("header"),
               uiOutput("counter"),
               
               
               tableOutput("statsTable"),
               br(),
               conditionalPanel(
                 condition = "input.graphs.indexOf('Average Wait Time') > -1",
                 
                 plotOutput("waitPlot"),
                 br(),
               ),
               
               conditionalPanel(
                 condition = "input.graphs.indexOf('Average Turnaround Time') > -1",
                 
                 plotOutput("ttPlot"),
                 br(),
               ),
               
               # conditionalPanel(
               #   condition = "input.graphs.indexOf('Average Wait Time') > -1",
               #   
               #   plotOutput("ttPlot"),
               #   br(),
               # ),

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
               
               actionButton(
                 inputId = "pause", 
                 label = "Pause"),
               
               actionButton(
                 inputId = "resume", 
                 label = "Resume"),
               
               conditionalPanel( # FCFS ----
                 condition = "input.schedulingChoices.indexOf('First Come First Serve')",
                 
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
                                 condition = "'Round Robin' %in% input.schedulingChoices",
                                 
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
                                 condition = "'Shortest Job First' %in% input.schedulingChoices",
                                 
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