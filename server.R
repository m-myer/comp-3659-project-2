server <- function(session, input, output) {
  
  # ----------------------------------------------------------- #
  #
  #                     ---- Reactives ----
  #
  # ----------------------------------------------------------- #
  
  clock <- reactiveValues(count = 0, 
                          timer = reactiveTimer(Inf),
                          active = FALSE)
  
  df_processes <- reactiveValues(processes = data.frame(),
                                 eventTimes = data.frame(),
                                 eventLengths = data.frame())
  
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
  
  df_sjfexp <- reactiveValues(new = data.frame(),
                              readyQueue = data.frame(),
                           running = data.frame(),
                           waiting = data.frame(),
                           terminated = data.frame(),
                           cpuIdleTime = 0,
                           avgWaitTime = 0,
                           avgTurnaround = 0,
                           responsive = 0,
                           cpuUtil = 0)
  
  df_srtf <- reactiveValues(new = data.frame(),
                              readyQueue = data.frame(),
                              running = data.frame(),
                              waiting = data.frame(),
                              terminated = data.frame(),
                              cpuIdleTime = 0,
                              avgWaitTime = 0,
                              avgTurnaround = 0,
                              responsive = 0,
                              cpuUtil = 0)
  
  df_srtfexp <- reactiveValues(new = data.frame(),
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
  
  observeEvent(input$procGen, {
    req(iv$is_valid())

    df_processes$processes <- SpawnProcesses(df_processes)

  })
  
  observeEvent(input$run, { # start button is pressed
    req(iv$is_valid()) # validate user inputs
    
    # df_processes(SpawnProcesses())
    
    if(input$schedulingChoices == "First Come First Serve"){
      df_fcfs$new <- df_processes$processes
    }
    
    if(input$schedulingChoices == "Round Robin"){
      df_rr$new <- df_processes$processes
      df_rr$quantum <- input$timeQuantum
    }
    
    if(input$schedulingChoices == "Shortest Job First"){
      df_sjf$new <- df_processes$processes
      df_sjfexp$new <- df_processes$processes
      df_srtf$new <- df_processes$processes
      df_srtfexp$new <- df_processes$processes
      
    }
    
    clock$count <- -3
    clock$timer <- reactiveTimer(input$seconds)
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

    RunSJF(GetSJF())
    

    if(nrow(GetSJF()$terminated) == input$numProcesses){
      clock$active <- FALSE
      clock$timer <- reactiveTimer(Inf)
    }
  })
  
  
  observeEvent(input$reset, {
    ResetSim()
    shinyjs::reset("schedulingChoices")
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
  output$fcfsStats <- renderTable({
    data.frame("Average Wait Time" = df_fcfs$avgWaitTime,
               "Average Turnaround Time" = df_fcfs$avgTurnaround,
               "CPU Utitization (%)" = df_fcfs$cpuUtil,
               "Average Responsiveness" = df_fcfs$responsive)
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
  
  
  # Round Robin
  output$rrSlice <- renderTable({
    c(df_rr$timeSlice, df_rr$quantum)
  })
  
  output$rrStats <- renderTable({
    data.frame("Average Wait Time" = df_rr$avgWaitTime,
               "Average Turnaround Time" = df_rr$avgTurnaround,
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
               "Average Turnaround Time" = df_sjf$avgTurnaround,
               "CPU Utitization (%)" = df_sjf$cpuUtil,
               "Average Responsiveness" = df_sjf$responsive)
  })
  
  output$sjfExpStats <- renderTable({
    data.frame("Average Wait Time" = df_sjfexp$avgWaitTime,
               "Average Turnaround Time" = df_sjfexp$avgTurnaround,
               "CPU Utitization (%)" = df_sjfexp$cpuUtil,
               "Average Responsiveness" = df_sjfexp$responsive)
  })
  
  output$srtfStats <- renderTable({
    data.frame("Average Wait Time" = df_srtf$avgWaitTime,
               "Average Turnaround Time" = df_srtf$avgTurnaround,
               "CPU Utitization (%)" = df_srtf$cpuUtil,
               "Average Responsiveness" = df_srtf$responsive)
  })
  
  output$srtfExpStats <- renderTable({
    data.frame("Average Wait Time" = df_srtfexp$avgWaitTime,
               "Average Turnaround Time" = df_srtfexp$avgTurnaround,
               "CPU Utitization (%)" = df_srtfexp$cpuUtil,
               "Average Responsiveness" = df_srtfexp$responsive)
  })
  
  output$sjfNew <- renderTable({
    GetSJF()$new
  })
  
  output$sjfReady <- renderTable({
    GetSJF()$readyQueue
  })
  
  output$sjfRunning <- renderTable({
    GetSJF()$running
  })
  
  output$sjfWaiting <- renderTable({
    GetSJF()$waiting
  })
  
  output$sjfTerminated <- renderTable({
    GetSJF()$terminated
  })
  
  output$processTable <- renderTable({
     df_processes$eventTimes
  })
  
  
  # ----------------------------------------------------------- #
  #
  #                     ---- Functions ----
  #
  # ----------------------------------------------------------- #
  
  SpawnProcesses <- function(df){
    
    pIDs <- 1:input$numProcesses
    arrivals <- sample(1:15, size = input$numProcesses, replace = TRUE)
    cpuTime <- sample(10:30, size = input$numProcesses, replace = TRUE)
    numEvents <- sample(0:5, size = input$numProcesses, replace = TRUE)
    
    df$processes <- data.frame("Process ID" = pIDs,
                               "Arrival" = arrivals,
                               "Total CPU Runtime" = cpuTime,
                               "Next CPU Runtime" = 0,
                               "Total Event Bursts" = numEvents,
                               "Current Event Burst" = 1,
                               "Next Event Time" = 0,
                               "Event Burst Length" = 0,
                               "New CPU Burst" = FALSE,
                               "Total CPU Bursts" = numEvents + 1,
                               "Last CPU Runtime" = 0,
                               "Next CPU Estimate" = 0,
                               "Response Time" = 0,
                               "Total Wait Time" = 0,
                               "Turnaround Time" = 0)
    
    ioCols <- max(numEvents)
    
    df_eventTimes <- data.frame(matrix(ncol = ioCols, nrow = input$numProcesses))  
    df_eventLengths <- data.frame(matrix(ncol = ioCols, nrow = input$numProcesses))
    
    divisor <- floor(cpuTime /(numEvents + 1))
    
    for(x in 1:input$numProcesses){
      if(numEvents[x] != 0){
        
        if(input$eventSkew == "even"){
          df_eventTimes[x,1] <- cpuTime[x] - divisor[x] # even
        } else {
          df_eventTimes[x,1] <- sample(numEvents[x]:cpuTime[x], size = 1) # random
        }
        
        df_eventLengths[x, 1] <- sample(10:40, size = 1)
        
        if(numEvents[x] > 1){
          for(y in 2:ioCols){
            
            if(y <= numEvents[x]){
              
              if(input$eventSkew == "even"){
                df_eventTimes[x,y] <- df_eventTimes[x,y-1] - divisor[x] # even
              } else{
                df_eventTimes[x,y] <- sample((numEvents[x] - (y - 1)):df_eventTimes[x,y-1], size = 1) # random
              }

              df_eventLengths[x, y] <- sample(10:40, size = 1)
            }
          }
        }
      }
    }
    
    df$eventTimes <- df_eventTimes
    df$eventLengths <- df_eventLengths
    df$processes <- GetNextIOEvent(df$processes, input$numProcesses)
    
    df$processes <- df$processes[order(df$processes$Arrival, decreasing = FALSE),] #sort by arrival time
    
  }
  
  
  GetNextIOEvent <- function(df, rows){
    
    for(x in 1:rows){
      if(df[x, "Total.Event.Bursts"] != 0){
        df[x, "Next.Event.Time"] <- df_processes$eventTimes[df[x, "Process.ID"], df[x, "Current.Event.Burst"]]
        df[x, "Event.Burst.Length"] <- df_processes$eventLengths[df[x, "Process.ID"], df[x, "Current.Event.Burst"]]
        df[x, "Next.CPU.Estimate"] <- ComputeEstimate(df[x, "Next.CPU.Estimate"], df[x, "Last.CPU.Runtime"])
      } else {
        df[x, "Next.Event.Time"] <- 0
      }
      
      df[x, "Next.CPU.Runtime"] <- df[x, "Total.CPU.Runtime"] - df[x, "Next.Event.Time"]
      
      
    }
    
    return(df)
  }
  
  
  RunFCFS <- function(df){
    
    CheckNewProcesses(df)
    CheckWaiting(df)
    CheckReadyQueue(df)
    CheckFCFSRunning(df)
    
    if(nrow(df$running) == 1){ # process currently running
      if(df$running$Total.Event.Bursts != 0 && df$running$Next.Event.Time >= df$running$Total.CPU.Runtime){ #processes has an IO burst
        HandleIOBurst(df)
      } else{
        ChangeCPUCounters(df)
        
        if(df$running[1,"Total.CPU.Runtime"] <= 0){ # processes finishes running
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
      if(df$running$Total.Event.Bursts != 0 && df$running$Next.Event.Time >= df$running$Total.CPU.Runtime){ #processes has an IO burst
        HandleIOBurst(df)
        df$timeSlice <- 0
      } else{
        ChangeCPUCounters(df)
        df$timeSlice <- df$timeSlice + 1
        
        if(df$timeSlice == df$quantum){
          ChangeState(df, "readyQueue", "running", c(1))
          df$timeSlice <- 0
          
        } else if(df$running[1,"Total.CPU.Runtime"] <= 0){ # processes finishes running
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
    
    if(nrow(df$readyQueue) > 0){ # sort by estimate or actual
      if(input$estimateSJF){
        df$readyQueue <- df$readyQueue[order(df$readyQueue$Next.CPU.Estimate, decreasing = FALSE),]
      } else{
        df$readyQueue <- df$readyQueue[order(df$readyQueue$Next.CPU.Runtime, decreasing = FALSE),]
      }
    }

    if(input$preemptive){
      CheckPreemptiveReadyQueue(df)
    } else{
      CheckReadyQueue(df)
    }
    
    
    if(nrow(df$running) == 1){ # process currently running
      if(df$running$Total.Event.Bursts != 0 && df$running$Next.Event.Time >= df$running$Total.CPU.Runtime){ #processes has an IO burst
        HandleIOBurst(df)
      } else{
        ChangeCPUCounters(df)
        
        if(df$running[1,"Total.CPU.Runtime"] <= 0){ # processes finishes running
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
      for(x in 1:newProcs){
        df$new[x,"Next.CPU.Runtime"] <- df$new[x,"Total.CPU.Runtime"] - df$new[x,"Next.Event.Time"]
      }
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
  
  
  CheckPreemptiveReadyQueue <- function(df){
    
    if(nrow(df$readyQueue) > 0){ # processes in ready state
      
      if(nrow(df$running) == 0){ # no process currently running
        ChangeState(df, "running", "readyQueue", c(1))
        df$running$New.CPU.Burst <- FALSE
        df$running$Last.CPU.Runtime <- 0
      
      } else if(input$estimateSJF && df$running[1,"Next.CPU.Estimate"] > df$readyQueue[1, "Next.CPU.Estimate"]){ # preempt with estimate
          ChangeState(df, "readyQueue", "running", c(1))
          ChangeState(df, "running", "readyQueue", c(1))
          df$running$New.CPU.Burst <- FALSE
          df$running$Last.CPU.Runtime <- 0

      }else if(df$running[1,"Next.CPU.Runtime"] > df$readyQueue[1, "Next.CPU.Runtime"]){ #preempt without estimate
         ChangeState(df, "readyQueue", "running", c(1))
         ChangeState(df, "running", "readyQueue", c(1))
         df$running$New.CPU.Burst <- FALSE
         df$running$Last.CPU.Runtime <- 0
      }

    }

    df$readyQueue$Response.Time[df$readyQueue$New.CPU.Burst == TRUE] <- df$readyQueue$Response.Time[df$readyQueue$New.CPU.Burst == TRUE] + 1
    df$readyQueue$Total.Wait.Time <- df$readyQueue$Total.Wait.Time + 1
    df$readyQueue$Turnaround.Time <- df$readyQueue$Turnaround.Time + 1
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
    df$running$Total.CPU.Runtime <- df$running$Total.CPU.Runtime - 1
    df$running$Turnaround.Time <- df$running$Turnaround.Time + 1
    df$running$Next.CPU.Runtime <- df$running$Next.CPU.Runtime - 1
  }
  
  
  HandleIOBurst <- function(df){
    df$running[1,"Total.Event.Bursts"] <- df$running[1, "Total.Event.Bursts"] - 1
    df$running[1,"Current.Event.Burst"] <- df$running[1, "Current.Event.Burst"] + 1
    
    ChangeState(df, "waiting", "running", c(1))
    df$waiting <- df$waiting[order(df$waiting$Event.Burst.Length, decreasing = FALSE),]
  }
  
  
  TerminateProcess <- function(df){
    
    df$running$Response.Time <- df$running$Response.Time / df$running$Total.CPU.Bursts
    ChangeState(df, "terminated", "running", c(1))
    UpdateStats(df)
  }
  
  
  ComputeEstimate <- function(estimate, last){
    
    if(estimate == 0){
      return <- input$defaultEst
    } else{
      newEstimate = (input$alpha * last) + (1 - input$alpha)*estimate

      return <- newEstimate
      
    }
    return(as.numeric(return))
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
  
  ResetSim <- function(){
    clock$count <- 0
    clock$active <- FALSE
    
    ClearQueues(df_fcfs)
    ClearQueues(df_rr)
    ClearQueues(df_sjf)
    ClearQueues(df_sjfexp)
    ClearQueues(df_srtf)
    ClearQueues(df_srtfexp)
    
  }
  
  ClearQueues <- function(df){
    
    df$new <- data.frame()
    df$readyQueue <- data.frame()
    df$running <- data.frame()
    df$waiting <- data.frame()
    df$terminated <- data.frame()
    df$cpuIdleTime <- 0
  }
  
  GetSJF <- function(){
    if(input$estimateSJF){
      if(input$preemptive){
        df_srtfexp
      } else{
        df_sjfexp
      }
    } else{
      if(input$preemptive){
        df_srtf
      } else{
        df_sjf
      }
    }
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