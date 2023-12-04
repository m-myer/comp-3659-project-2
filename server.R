server <- function(session, input, output) {
  
  # ----------------------------------------------------------- #
  #
  #                     ---- Reactives ----
  #
  # ----------------------------------------------------------- #
  
  clock <- reactiveValues(count = 0, 
                          timer = reactiveTimer(Inf),
                          active = FALSE)
  
  df_processes <- reactiveVal()
  
  # First Come First Serve Object
  df_fcfs <- reactiveValues(new = data.frame(),
                            readyQueue = data.frame(),
                            running = data.frame(),
                            waiting = data.frame(),
                            terminated = data.frame(),
                            cpuIdleTime = 0,
                            avgWaitTime = 0,
                            avgTurnaround = 0,
                            responsive = 0,
                            cpuUtil = 0)
  
  # Round Robin Object
  df_rr <- reactiveValues(new = data.frame(),
                          readyQueue = data.frame(),
                          running = data.frame(),
                          waiting = data.frame(),
                          terminated = data.frame(),
                          quantum = 0,
                          timeSlice = 0,
                          cpuIdleTime = 0,
                          avgWaitTime = 0,
                          avgTurnaround = 0,
                          responsive = 0,
                          cpuUtil = 0)
  
  # Shortest Job First Object
  df_sjf <- reactiveValues(new = data.frame(),
                           readyQueue = data.frame(),
                           running = data.frame(),
                           waiting = data.frame(),
                           terminated = data.frame(),
                           cpuIdleTime = 0,
                           avgWaitTime = 0,
                           avgTurnaround = 0,
                           responsive = 0,
                           cpuUtil = 0)
  
  
  
  # ----------------------------------------------------------- #
  #
  #                     ---- Observers ----
  #
  # ----------------------------------------------------------- #
  
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
    
    if(input$schedulingChoices == "Shortest Job First"){
      df_sjf$new <- df_processes()
    }
    
    clock$count <- -3
    clock$timer <- reactiveTimer(1000)
    clock$active <- TRUE
    
  })
  
  
  observeEvent(clock$timer(), {
    req(clock$active == TRUE)
    
    clock$count <- clock$count + 1
  })
  
  
  observeEvent(req(input$schedulingChoices == "First Come First Serve" && clock$count), { # every clock tick
    req(clock$active) # only if simulation is running
    
    RunFCFS(df_fcfs)
    
    if(nrow(df_fcfs$terminated) == input$numProcesses){
      clock$active <- FALSE
      clock$timer <- reactiveTimer(Inf)
    }
  })
  
  
  observeEvent(req(input$schedulingChoices == "Round Robin" && clock$count), { # every clock tick
    req(clock$active) # only if simulation is running
    
    RunRR(df_rr)
    
    if(nrow(df_rr$terminated) == input$numProcesses){
      clock$active <- FALSE
      clock$timer <- reactiveTimer(Inf)
    }
  })
  
  
  observeEvent(req(input$schedulingChoices == "Shortest Job First" && clock$count), { # every clock tick
    req(clock$active) # only if simulation is running

    RunSJF(df_sjf)

    if(nrow(df_sjf$terminated) == input$numProcesses){
      clock$active <- FALSE
      clock$timer <- reactiveTimer(Inf)
    }
  })

  
  # ----------------------------------------------------------- #
  #
  #                      ---- Outputs ----
  #
  # ----------------------------------------------------------- #
  
  output$header <- renderUI({
    tagList(
      h3(paste("Selected Algorithm: ", input$schedulingChoices))
    )
  })
  
  output$counter <- renderUI({
    sprintf("Timer: %s",
            clock$count)
  })
  
  # First Come First Serve
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
  
  
  # Round Robin
  output$rrSlice <- renderTable({
    c(df_rr$timeSlice, df_rr$quantum)
  })
  
  output$rrStats <- renderTable({
    data.frame("Average Wait Time" = df_rr$avgWaitTime,
               "Average Turnaround Time" = df_rr$avgWaitTime,
               "CPU Utitization (%)" = df_rr$cpuUtil,
               "Average Responsiveness" = df_rr$responsive)
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
  
  
  # Shortest Job First
  output$sjfStats <- renderTable({
    data.frame("Average Wait Time" = df_sjf$avgWaitTime,
               "Average Turnaround Time" = df_sjf$avgWaitTime,
               "CPU Utitization (%)" = df_sjf$cpuUtil,
               "Average Responsiveness" = df_sjf$responsive)
  })
  
  output$sjfNew <- renderTable({
    df_sjf$new
  })
  
  output$sjfReady <- renderTable({
    df_sjf$readyQueue
  })
  
  output$sjfRunning <- renderTable({
    df_sjf$running
  })
  
  output$sjfWaiting <- renderTable({
    df_sjf$waiting
  })
  
  output$sjfTerminated <- renderTable({
    df_sjf$terminated
  })
  
  # output$processTable <- renderTable({
  #   df_processes()
  # })
  
  
  # ----------------------------------------------------------- #
  #
  #                     ---- Functions ----
  #
  # ----------------------------------------------------------- #
  
  SpawnProcesses <- function(){
    
    pIDs <- 1:input$numProcesses
    arrivals <- sample(1:15, size = input$numProcesses, replace = TRUE)
    cpuTime <- sample(2:30, size = input$numProcesses, replace = TRUE)
    numEvents <- sample(2:5, size = input$numProcesses, replace = TRUE)
    
    df_processes <- data.frame("Process ID" = pIDs,
                               "Arrival" = arrivals,
                               "CPU Runtime" = cpuTime,
                               "Total Event Bursts" = numEvents,
                               "Next Event Time" = 0,
                               "Event Burst Length" = 0,
                               "New CPU Burst" = FALSE,
                               "Total CPU Bursts" = numEvents + 1,
                               "Last CPU Runtime" = 0,
                               "Next CPU Runtime" = 10,
                               "Next CPU Estimate" = 10,
                               "Response Time" = 0,
                               "Total Wait Time" = 0,
                               "Turnaround Time" = 0)
    
    df_processes <- GetNextIOEvent(df_processes, input$numProcesses)
    
    df_processes <- df_processes[order(df_processes$Arrival, decreasing = FALSE),] #sort by arrival time
    
    return(df_processes)
  }
  
  
  GetNextIOEvent <- function(df, rows){
    
    for(x in 1:rows){
      if(df[x, "Total.Event.Bursts"] != 0){
        df[x, "Event.Burst.Length"] <- sample(10:40, size = 1)
        df[x, "Next.Event.Time"] <- sample(df[x, "Total.Event.Bursts"]:df[x, "CPU.Runtime"], size = 1)
        df[x, "Next.CPU.Runtime"] <- df[x, "CPU.Runtime"] - df[x, "Next.Event.Time"]
        df[x, "Next.CPU.Estimate"] <- ComputeEstimate(df[x, "Next.CPU.Estimate"], df[x, "Last.CPU.Burst"])
      }
    }
    
    return(df)
  }
  
  
  RunFCFS <- function(df){
    
    CheckNewProcesses(df)
    CheckWaiting(df)
    CheckReadyQueue(df)
    CheckFCFSRunning(df)
    
    if(nrow(df$running) == 1){ # process currently running
      if(df$running$Total.Event.Bursts != 0 && df$running$Next.Event.Time >= df$running$CPU.Runtime){ #processes has an IO burst
        HandleIOBurst(df)
      } else{
        ChangeCPUCounters(df)
        
        if(df$running[1,"CPU.Runtime"] <= 0){ # processes finishes running
          TerminateProcess(df)
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
      if(df$running$Total.Event.Bursts != 0 && df$running$Next.Event.Time >= df$running$CPU.Runtime){ #processes has an IO burst
        HandleIOBurst(df)
        df$timeSlice <- 0
      } else{
        ChangeCPUCounters(df)
        df$timeSlice <- df$timeSlice + 1
        
        if(df$timeSlice == df$quantum){
          ChangeState(df, "readyQueue", "running", c(1))
          df$timeSlice <- 0
          
        } else if(df$running[1,"CPU.Runtime"] <= 0){ # processes finishes running
          TerminateProcess(df)
          df$timeSlice <- 0
        }
      }
    } else{ # wasted cpu time (for cpu utilization)
      df$cpuIdleTime <- df$cpuIdleTime + 1
    }
  }
  
  
  RunSJF <- function(df){

    CheckNewProcesses(df)
    CheckWaiting(df)
    if(nrow(df$readyQueue) > 0){
      df$readyQueue <- df$readyQueue[order(df$readyQueue$Next.CPU.Runtime, decreasing = FALSE),]
    }
    CheckReadyQueue(df)
    
    if(nrow(df$running) == 1){ # process currently running
      if(df$running$Total.Event.Bursts != 0 && df$running$Next.Event.Time >= df$running$CPU.Runtime){ #processes has an IO burst
        HandleIOBurst(df)
      } else{
        ChangeCPUCounters(df)
        
        if(df$running[1,"CPU.Runtime"] <= 0){ # processes finishes running
          TerminateProcess(df)
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
    
    if(nrow(df$new) > 0 && df$new[1,"Arrival"] <= clock$count){ # arrival time for next process
      newProcs <- nrow(subset(df$new, df$new$Arrival == clock$count)) # collect row # for new processes
      df$new[c(1:newProcs),]$New.CPU.Burst <- TRUE
      ChangeState(df, "readyQueue", "new", c(1:newProcs)) # added row 1 to nrow
    }
  }
  
  
  CheckReadyQueue <- function(df){
    
    if(nrow(df$readyQueue) > 0){ # processes in ready state
      if(nrow(df$running) == 0){ # no process currently running
        ChangeState(df, "running", "readyQueue", c(1))
        df$running$New.CPU.Burst <- FALSE
        df$running$Last.CPU.Runtime <- 0
      }
      df$readyQueue$Response.Time[df$readyQueue$New.CPU.Burst == TRUE] <- df$readyQueue$Response.Time[df$readyQueue$New.CPU.Burst == TRUE] + 1
      df$readyQueue$Total.Wait.Time <- df$readyQueue$Total.Wait.Time + 1
      df$readyQueue$Turnaround.Time <- df$readyQueue$Turnaround.Time + 1
    }
  }
  
  
  CheckWaiting <- function(df){
    
    if(nrow(df$waiting) > 0){ # processes in waiting state
      df$waiting$Event.Burst.Length <- df$waiting$Event.Burst.Length - 1
      
      if(df$waiting[1, "Event.Burst.Length"] == 0){
        ioComplete<- nrow(subset(df$waiting, df$waiting$Event.Burst.Length == 0))
        df$waiting <- GetNextIOEvent(df$waiting, ioComplete)
        df$waiting[c(1:ioComplete),]$New.CPU.Burst <- TRUE
        ChangeState(df, "readyQueue", "waiting", c(1:ioComplete))
      }
    }
  }
  
  
  CheckFCFSRunning <- function(df){
    
  }
  
  
  ChangeCPUCounters <- function(df){
    
    df$running$Last.CPU.Runtime <- df$running$Last.CPU.Runtime + 1
    df$running$CPU.Runtime <- df$running$CPU.Runtime - 1
    df$running$Turnaround.Time <- df$running$Turnaround.Time + 1
  }
  
  
  HandleIOBurst <- function(df){
    df$running[1,"Total.Event.Bursts"] <- df$running[1, "Total.Event.Bursts"] - 1
    ChangeState(df, "waiting", "running", c(1))
    df$waiting <- df$waiting[order(df$waiting$Event.Burst.Length, decreasing = FALSE),]
  }
  
  
  TerminateProcess <- function(df){
    
    df$running$Response.Time <- df$running$Response.Time / df$running$Total.CPU.Bursts
    ChangeState(df, "terminated", "running", c(1))
    UpdateStats(df)
  }
  
  
  ComputeEstimate <- function(estimate, last){
    return(10)
  }
  
  
  SortSJF <- function(df){
    df <- df[order(df$Next.CPU.Runtime, decreasing = FALSE),]
  }
  
  
  UpdateStats <- function(df){
    df$avgWaitTime <- mean(df$terminated$Total.Wait.Time)
    df$avgTurnaround <- mean(df$terminated$Turnaround.Time)
    df$cpuUtil <- (1 - (df$cpuIdleTime / clock$count)) * 100
    df$responsive <- mean(df$terminated$Response.Time)
  }
  
  
  # ----------------------------------------------------------- #
  #
  #                 ---- Input Validation ----
  #
  # ----------------------------------------------------------- #
  
  iv <- InputValidator$new()
  
  iv$add_rule("numProcesses", sv_required())
  
  iv$enable()
  
}