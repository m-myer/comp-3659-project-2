server <- function(session, input, output) {
  
  # Reactives ---------------------------------------------------
  
  clock <- reactiveValues(count = 0, 
                          timer = reactiveTimer(Inf),
                          active = FALSE)
  
  df_processes <- reactiveVal()
  
  df_fcfs <- reactiveValues(processes = data.frame(),
                            readyQueue = data.frame(),
                            running = data.frame(),
                            waiting = data.frame(),
                            terminated = data.frame())
  
  
  # Observers ---------------------------------------------------
  
  observeEvent(input$run, { # start button is pressed
    req(iv$is_valid()) # validate user inputs
    
    df_processes(SpawnProcesses())
    df_fcfs$processes <- df_processes()
    clock$count <- 0
    clock$timer <- reactiveTimer(500)
    clock$active <- TRUE
    
  })
  
  observeEvent(clock$timer(), { # every clock tick
    req(clock$active == TRUE) # only if simulation is running
    
    UpdateSim(df_fcfs)
    clock$count <- clock$count + 1
  })
  
  
  # Outputs -----------------------------------------------------
  
  output$header <- renderUI({
    tagList(
      h3(paste("Selected Algorithm: ", input$schedulingChoices))
    )
  })
  
  output$counter <- renderUI({
    sprintf("Timer: %s",
            clock$count)
  })
  
  output$fcfsNew <- renderTable({
    df_fcfs$processes
  })
  
  output$fcfsReady <- renderTable({
    df_fcfs$readyQueue
  })
  
  output$fcfsRunning <- renderTable({
    df_fcfs$running
  })
  
  output$fcfsTerminated <- renderTable({
    df_fcfs$terminated
  })
  
  # output$processTable <- renderTable({
  #   df_processes()
  # })
  
  
  # Functions ---------------------------------------------------
  
  SpawnProcesses <- function(){
    
    pIDs <- 1:input$numProcesses
    arrivals <- sample(0:15, size = input$numProcesses, replace = TRUE)
    cpuBurst <- sample(2:30, size = input$numProcesses, replace = TRUE)
    ioBurst <- sample(10:40, size = input$numProcesses, replace = TRUE)
    numEvents <- sample(2:5, size = input$numProcesses, replace = TRUE)
    waitTime <- rep(0, input$numProcesses)
    turnaround <- rep(0, input$numProcesses)
    
    df_processes <- data.frame("Process ID" = pIDs,
                               "Arrival" = arrivals,
                               "CPU Burst Time" = cpuBurst,
                               "Event Burst Time" = ioBurst,
                               "Number of Event Bursts" = numEvents,
                               "Total Wait Time" = waitTime,
                               "Turnaround Time" = turnaround)
    
    df_processes <- df_processes[order(df_processes$Arrival, decreasing = FALSE),] #sort by arrival time
    
    return(df_processes)
  }
  
  
  UpdateSim <- function(df){
    if(nrow(df$processes) > 0 && df$processes[1,"Arrival"] == clock$count){ # arrival time for next process
      df_newProcs <- subset(df$processes, df$processes$Arrival == clock$count) # collect processes for current arrival time
      df$readyQueue <- rbind(df$readyQueue, df_newProcs) # add to ready queue
      df$processes <- df$processes[!(df$processes$Process.ID %in% df_newProcs$Process.ID),]  # remove from new process list
    }
    
    if(nrow(df$readyQueue) > 0){ 
      if(nrow(df$running) == 0){
        df$running <- rbind(df$running, df$readyQueue[1,])
        df$readyQueue <- df$readyQueue[-c(1),]
        
      }
      
      df$readyQueue$Total.Wait.Time <- df$readyQueue$Total.Wait.Time + 1
      df$readyQueue$Turnaround.Time <- df$readyQueue$Turnaround.Time + 1
    }
    
    if(nrow(df$running) == 1){ 
      df$running$CPU.Burst.Time <- df$running$CPU.Burst.Time - 1
      df$running$Turnaround.Time <- df$running$Turnaround.Time + 1
      
      if(df$running[1,"CPU.Burst.Time"] == 0){
        df$terminated <- rbind(df$terminated, df$running[1,])
        df$running <- df$running[-c(1),]
      }
    }
    
  }
  
  
  # Input Validation --------------------------------------------
  
  iv <- iv <- InputValidator$new()
  
  iv$add_rule("numProcesses", sv_required())
  
  iv$enable()
  
}