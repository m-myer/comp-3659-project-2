server <- function(session, input, output) {
  
  # Reactives ---------------------------------------------------
  
  clock <- reactiveValues(count = 0, 
                          timer = reactiveTimer(Inf),
                          active = FALSE)
  
  df_processes <- reactiveVal()
  
  df_fcfs <- reactiveValues(new = data.frame(),
                            readyQueue = data.frame(),
                            running = data.frame(),
                            waiting = data.frame(),
                            terminated = data.frame(),
                            cpuIdleTime = 0)
  
  df_rr <- reactiveValues(new = data.frame(),
                          readyQueue = data.frame(),
                          running = data.frame(),
                          waiting = data.frame(),
                          terminated = data.frame(),
                          cpuIdleTime = 0,
                          quantum = 0,
                          timeSlice = 0)
  
  
  
  # Observers ---------------------------------------------------
  
  observeEvent(input$run, { # start button is pressed
    req(iv$is_valid()) # validate user inputs
    
    df_processes(SpawnProcesses())
    
    if(input$schedulingChoices == "First Come First Serve"){
      df_fcfs$new <- df_processes()
    }
    
    if(input$schedulingChoices == "Round Robin"){
      df_rr$new <- df_processes()
      df_rr$quantum <- input$timeQuantum
    }
    
    clock$count <- 0
    clock$timer <- reactiveTimer(100)
    clock$active <- TRUE
    
  })
  
  
  observeEvent(req(input$schedulingChoices == "First Come First Serve" && clock$timer()), { # every clock tick
    req(clock$active == TRUE) # only if simulation is running
    
    RunFCFS(df_fcfs)
    clock$count <- clock$count + 1
    
    if(nrow(df_fcfs$terminated) == input$numProcesses){
      clock$timer <- reactiveTimer(Inf)
    }
  })
  
  
  observeEvent(req(input$schedulingChoices == "Round Robin" && clock$timer()), { # every clock tick
    req(clock$active == TRUE) # only if simulation is running
    
    RunRR(df_rr)
    clock$count <- clock$count + 1
    
    if(nrow(df_rr$terminated) == input$numProcesses){
      clock$timer <- reactiveTimer(Inf)
    }
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
    df_fcfs$new
  })
  
  output$fcfsReady <- renderTable({
    df_fcfs$readyQueue
  })
  
  output$fcfsRunning <- renderTable({
    df_fcfs$running
  })
  
  output$fcfsWaiting <- renderTable({
    df_fcfs$waiting
  })
  
  output$fcfsTerminated <- renderTable({
    df_fcfs$terminated
  })
  
  output$rrSlice <- renderTable({
    c(df_rr$timeSlice, df_rr$quantum)
  })
  
  output$rrNew <- renderTable({
    df_rr$new
  })
  
  output$rrReady <- renderTable({
    df_rr$readyQueue
  })
  
  output$rrRunning <- renderTable({
    df_rr$running
  })
  
  output$rrWaiting <- renderTable({
    df_rr$waiting
  })
  
  output$rrTerminated <- renderTable({
    df_rr$terminated
  })
  
  # output$processTable <- renderTable({
  #   df_processes()
  # })
  
  
  # Functions ---------------------------------------------------
  
  SpawnProcesses <- function(){
    
    pIDs <- 1:input$numProcesses
    arrivals <- sample(0:15, size = input$numProcesses, replace = TRUE)
    cpuBurst <- sample(2:30, size = input$numProcesses, replace = TRUE)
    numEvents <- sample(2:5, size = input$numProcesses, replace = TRUE)
    waitTime <- rep(0, input$numProcesses)
    turnaround <- rep(0, input$numProcesses)
    
    df_processes <- data.frame("Process ID" = pIDs,
                               "Arrival" = arrivals,
                               "CPU Burst Time" = cpuBurst,
                               "Number of Event Bursts" = numEvents,
                               "Next Event Time" = 0,
                               "Event Burst Time" = 0,
                               "Last CPU Burst" = 0,
                               "Total Wait Time" = waitTime,
                               "Turnaround Time" = turnaround)
    
    df_processes <- GetNextIOEvent(df_processes, input$numProcesses)
    
    df_processes <- df_processes[order(df_processes$Arrival, decreasing = FALSE),] #sort by arrival time
    
    return(df_processes)
  }
  
  
  GetNextIOEvent <- function(df, rows){
    
    for(x in 1:rows){
      if(df[x, "Number.of.Event.Bursts"] != 0){
        df[x, "Event.Burst.Time"] <- sample(10:40, size = 1)
        df[x, "Next.Event.Time"] <- sample(df[x, "Number.of.Event.Bursts"]:df[x, "CPU.Burst.Time"], size = 1)
      }
    }
    
    return(df)
  }
  
  
  RunFCFS <- function(df){
    
    CheckNewProcesses(df)
    CheckWaiting(df)
    CheckReadyQueue(df)
    
    if(nrow(df$running) == 1){ # process currently running
      if(df$running$Number.of.Event.Bursts != 0 && df$running$Next.Event.Time == df$running$CPU.Burst.Time){ #processes has an IO burst
        df$running[1,"Number.of.Event.Bursts"] <- df$running[1, "Number.of.Event.Bursts"] - 1
        ChangeState(df, "waiting", "running", c(1))
        df$waiting <- df$waiting[order(df$waiting$Event.Burst.Time, decreasing = FALSE),]
      } else{
        df$running$Last.CPU.Burst <- df$running$Last.CPU.Burst + 1
        df$running$CPU.Burst.Time <- df$running$CPU.Burst.Time - 1
        df$running$Turnaround.Time <- df$running$Turnaround.Time + 1
        
        if(df$running[1,"CPU.Burst.Time"] == 0){ # processes finishes running
          ChangeState(df, "terminated", "running", c(1))
        }
      }
    } else{ # wasted cpu time (for cpu utilization)
      df$cpuIdleTime <- df$cpuIdleTime + 1
    }
  }
  
  
  RunRR <- function(df){
    
    CheckNewProcesses(df)
    CheckWaiting(df)
    CheckReadyQueue(df)
    
    if(nrow(df$running) == 1){ # process currently running
      if(df$running$Number.of.Event.Bursts != 0 && df$running$Next.Event.Time == df$running$CPU.Burst.Time){ #processes has an IO burst
        df$running[1,"Number.of.Event.Bursts"] <- df$running[1, "Number.of.Event.Bursts"] - 1
        ChangeState(df, "waiting", "running", c(1))
        df$timeSlice <- 0
        df$waiting <- df$waiting[order(df$waiting$Event.Burst.Time, decreasing = FALSE),]
      } else{
        df$running$Last.CPU.Burst <- df$running$Last.CPU.Burst + 1
        df$running$CPU.Burst.Time <- df$running$CPU.Burst.Time - 1
        df$running$Turnaround.Time <- df$running$Turnaround.Time + 1
        df$timeSlice <- df$timeSlice + 1
        
        if(df$timeSlice == df$quantum){
          ChangeState(df, "readyQueue", "running", c(1))
          df$timeSlice <- 0
          
        } else if(df$running[1,"CPU.Burst.Time"] == 0){ # processes finishes running
          ChangeState(df, "terminated", "running", c(1))
          df$timeSlice <- 0
        }
      }
    } else{ # wasted cpu time (for cpu utilization)
      df$cpuIdleTime <- df$cpuIdleTime + 1
    }
  }
  
  
  ChangeState <- function(df, df_to, df_from, processes){
    
    df[[df_to]] <- rbind(df[[df_to]], df[[df_from]][processes,])
    df[[df_from]] <- df[[df_from]][-processes,]
  }
  
  
  CheckNewProcesses <- function(df){
    
    if(nrow(df$new) > 0 && df$new[1,"Arrival"] == clock$count){ # arrival time for next process
      newProcs <- nrow(subset(df$new, df$new$Arrival == clock$count)) # collect row # for new processes
      ChangeState(df, "readyQueue", "new", c(1:newProcs)) # added row 1 to nrow
    }
  }
  
  
  CheckReadyQueue <- function(df){
    
    if(nrow(df$readyQueue) > 0){ # processes in ready state
      if(nrow(df$running) == 0){ # no process currently running
        ChangeState(df, "running", "readyQueue", c(1))
        df$running$Last.CPU.Burst <- 0
      }
      
      df$readyQueue$Total.Wait.Time <- df$readyQueue$Total.Wait.Time + 1
      df$readyQueue$Turnaround.Time <- df$readyQueue$Turnaround.Time + 1
    }
  }
  
  
  CheckWaiting <- function(df){
    
    if(nrow(df$waiting) > 0){ # processes in waiting state
      df$waiting$Event.Burst.Time <- df$waiting$Event.Burst.Time - 1
      
      if(df$waiting[1, "Event.Burst.Time"] == 0){
        ioComplete<- nrow(subset(df$waiting, df$waiting$Event.Burst.Time == 0))
        df$waiting <- GetNextIOEvent(df$waiting, ioComplete)
        ChangeState(df, "readyQueue", "waiting", c(1:ioComplete))
      }
    }
  }
  
  # Input Validation --------------------------------------------
  
  iv <- InputValidator$new()
  
  iv$add_rule("numProcesses", sv_required())
  
  iv$enable()
  
}