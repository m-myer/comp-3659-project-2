source("./global.R")

ui <- fluidPage(
  navbarPage(title = "Scheduling Algorithm Simulator"),
  
  tabPanel(title = "Main",
           
           sidebarLayout( 
             sidebarPanel( # sidebar ----
               withMathJax(),
               shinyjs::useShinyjs(),
               id = "sidebar",
               
               h4(strong("Simulation Options")),
               hr(),
               
               selectizeInput(
                 inputId = "schedulingChoices",
                 label = strong("Choose Scheduling Algorithm"),
                 choices = c("", 
                             "First Come First Serve", 
                             "Round Robin",
                             "Shortest Job First"),
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
                
               
               sliderTextInput(
                 inputId = "ioEvents",
                 label = "Range for Number of IO Events:", 
                 choices = ioEventRange,
                 selected = range(0:5)
               ),
              
                 
                 numericInput(
                   inputId = "ioLambda",
                   label = strong("Event Burst length Distribution Mean"),
                   value = 30,
                   min = 1,
                   max = 50,
                   step = 1
                 ),

               conditionalPanel(
                 condition = "input.schedulingChoices.indexOf('Round Robin') > -1",
                 
                 br(),
                 h4(strong("Round Robin Options")),
                 hr(),
                 
                 numericInput(
                   inputId = "timeQuantum",
                   label = strong("Time Quantum (in Millisesconds)"),
                   value = 10, 
                   min = 1,
                   step = 1
                 )
               ), #RR Conditional Panel
               
               conditionalPanel(
                 condition = "input.schedulingChoices.indexOf('Shortest Job First') > -1",
                 
                 br(),
                 h4(strong("Shortest Job First Options")),
                 hr(),
                 
                 checkboxGroupButtons(
                   inputId = "sjfAlgos",
                   label = "Version",
                   choices = c("SJF (No Preemption)", 
                               "SJF with Exponential Averaging", 
                               "SRTF (with Preemption)", 
                               "SRTF with Exponential Averaging"),
                   individual = TRUE,
                   checkIcon = list(
                     yes = tags$i(class = "fa fa-circle", 
                                  style = "color: steelblue"),
                     no = tags$i(class = "fa fa-circle-o", 
                                 style = "color: steelblue"))
                 ),
                 
                 # checkboxInput(
                 #   inputId = "preemptive",
                 #   label = "Preemptive", 
                 #   value = FALSE
                 # ),
                 # 
                 # checkboxInput(
                 #   inputId = "estimateSJF",
                 #   label = "Use Exponential Averaging", 
                 #   value = FALSE
                 # ),
                 
                 conditionalPanel(
                   condition = "input.sjfAlgos.indexOf('SJF with Exponential Averaging') > -1 |
                                input.sjfAlgos.indexOf('SRTF with Exponential Averaging') > -1",
                   
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
               
               br(),
               h4(strong("Graph Options")),
               hr(),

               selectizeInput(inputId = 'graphs',
                              label = 'Select one or more graphs',
                              multiple = TRUE,
                              choices = c("Average Wait Time",
                                          "Average Turnaround Time",
                                          "CPU Utilization",
                                          "Average Response Time",
                                          "Throughput"),
                              selected = ""),
               
               hr(),
               br(),
               
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
             ),
             
             mainPanel( # mainPanel ----
               useShinyjs(),
               
               h3(strong("Simulation Controls")),
               br(),
               
               actionButton(
                 inputId = "pause", 
                 label = "Pause"),
               
               actionButton(
                 inputId = "resume", 
                 label = "Resume"),
               
               br(),
               br(),
               hr(),
               br(),
               
               tabsetPanel(id = "algoTabs",
                 tabPanel("Summary Tab",
                          uiOutput("header"),
                          uiOutput("counter"),
                          
                          h3(strong("Statistics Comparison Table")),
                          tableOutput("statsTable"),
                          br(),
                          
                          div(style = "display:flex;",
                              conditionalPanel(
                                condition = "input.graphs.indexOf('Average Wait Time') > -1",
                                
                                plotOutput("waitPlot", width = "30vw"),
                                br(),
                              ), 
                              conditionalPanel(
                                condition = "input.graphs.indexOf('Average Turnaround Time') > -1",
                                
                                plotOutput("ttPlot", width = "30vw"),
                                br(),
                              )
                          ),
                          
                          div(style = "display:flex;",
                              
                              conditionalPanel(
                                condition = "input.graphs.indexOf('CPU Utilization') > -1",
                                
                                plotOutput("cpuPlot", width = "30vw"),
                                br(),
                              ),
                              
                              conditionalPanel(
                                condition = "input.graphs.indexOf('Average Response Time') > -1",
                                
                                plotOutput("responsePlot", width = "30vw"),
                                br(),
                              )
                          ),
                          
                          div(style = "display:flex;",
                              
                              conditionalPanel(
                                condition = "input.graphs.indexOf('Throughput') > -1",
                                
                                plotOutput("throughputPlot", width = "30vw"),
                                br(),
                              )
                          ),

                          
                          conditionalPanel(
                            condition = "output.processTable",
                            h3(strong("Master Process Table"))
                          ),
                          tableOutput("processTable"),
                          
                          fluidRow(
                            column(width = 6,
                              conditionalPanel(
                                condition = "output.cpuTimesTable",
                                h3(strong("IO Event Times Table"))
                              ),
                              tableOutput("cpuTimesTable"),
                            ),

                            column(width = 6,
                              conditionalPanel(
                                condition = "output.cpuLengthsTable",
                                h3(strong("IO Event Lengths Table"))
                              ),
                              tableOutput("cpuLengthsTable")
                            ),
                          ),
                          
                          
                 ),
                 
                 tabPanel("First Come First Serve",
                          h3("FCFS Stats"),
                          tableOutput("fcfsStats"),
                          br(),
                          hr(),
                          conditionalPanel( # FCFS ----
                                            condition = "input.schedulingChoices.indexOf('First Come First Serve') > -1",
                                            
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
                 ),
                 
                 tabPanel("Round Robin",
                          h3("RR Stats"),
                          tableOutput("rrStats"),
                          br(),
                          hr(),
                          conditionalPanel( # RR ----
                                            condition = "input.schedulingChoices.indexOf('Round Robin') > -1",
                                            
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
                 ),
                 
                 tabPanel("Shortest Job First",
                          h3("SJF Stats"),
                          tableOutput("sjfStats"),
                          br(),
                          hr(),
                          conditionalPanel( # SJF ----
                                            condition = "input.schedulingChoices.indexOf('Shortest Job First') > -1",
                                            
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
                 ),
                 
                 tabPanel("Shortest Job First - Est",
                          h3("SJF Exp Stats"),
                          tableOutput("sjfExpStats"),
                          br(),
                          titlePanel("Shortest Job First - Estimate"),
                          hr(),
                          br(),
                          h4("New Process List"),
                          br(),
                          tableOutput("sjfexpNew"),
                          br(),
                          br(),
                          h4("Ready Queue"),
                          br(),
                          tableOutput("sjfexpReady"),
                          br(),
                          br(),
                          h4("Running"),
                          br(),
                          tableOutput("sjfexpRunning"),
                          br(),
                          br(),
                          h4("Waiting"),
                          br(),
                          tableOutput("sjfexpWaiting"),
                          br(),
                          br(),
                          h4("Terminated"),
                          br(),
                          tableOutput("sjfexpTerminated"),
                          br(),
                          br(),
                 ),
                 
                 tabPanel("Shortest Remaining Time First",
                          h3("SRTF Stats"),
                          tableOutput("srtfStats"),
                          br(),
                          titlePanel("Shortest Remaining Time First"),
                          hr(),
                          br(),
                          h4("New Process List"),
                          br(),
                          tableOutput("srtfNew"),
                          br(),
                          br(),
                          h4("Ready Queue"),
                          br(),
                          tableOutput("srtfReady"),
                          br(),
                          br(),
                          h4("Running"),
                          br(),
                          tableOutput("srtfRunning"),
                          br(),
                          br(),
                          h4("Waiting"),
                          br(),
                          tableOutput("srtfWaiting"),
                          br(),
                          br(),
                          h4("Terminated"),
                          br(),
                          tableOutput("srtfTerminated"),
                          br(),
                          br(),
                 ),
                 
                 tabPanel("Shortest Remaining Time First - Est",
                          h3("SRTF Exp Stats"),
                          tableOutput("srtfExpStats"),
                          hr(),
                          titlePanel("Shortest Remaining Time First - Estimate"),
                          hr(),
                          br(),
                          h4("New Process List"),
                          br(),
                          tableOutput("srtfexpNew"),
                          br(),
                          br(),
                          h4("Ready Queue"),
                          br(),
                          tableOutput("srtfexpReady"),
                          br(),
                          br(),
                          h4("Running"),
                          br(),
                          tableOutput("srtfexpRunning"),
                          br(),
                          br(),
                          h4("Waiting"),
                          br(),
                          tableOutput("srtfexpWaiting"),
                          br(),
                          br(),
                          h4("Terminated"),
                          br(),
                          tableOutput("srtfexpTerminated"),
                          br(),
                          br(),
                 )
               ),
               
               br(),
               hr(),

             )
           )
  )
)