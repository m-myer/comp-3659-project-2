server <- function(session, input, output) {
  
  # ----------------------------------------------------------- #
  #
  #                     ---- Reactives ----
  #
  # ----------------------------------------------------------- #
  
  clock <- reactiveValues(count = 0, 
                          timer = reactiveTimer(Inf),
                          active = FALSE,
                          runningAlgos = 0)
  
  df_processes <- reactiveValues(processes = data.frame(),
                                 eventTimes = data.frame(),
                                 eventLengths = data.frame())
  
  df_stats <- reactiveValues(statsTable = matrix(0,
                                                 ncol = 7, 
                                                 nrow = 6,
                                                 dimnames = list(algorithms, 
                                                                 c("Average Wait Time",
                                                                   "Median Wait Time",
                                                                   "Average Turnaround Time",
                                                                   "Median Turnaround Time",
                                                                   "CPU Utilization",
                                                                   "Average Response Time",
                                                                   "Throughput"))))
  
  # First Come First Serve Object
  df_fcfs <- reactiveValues(new = data.frame(),
                            readyQueue = data.frame(),
                            running = data.frame(),
                            waiting = data.frame(),
                            terminated = data.frame(),
                            cpuIdleTime = 0,
                            algorithmNum = 1)
  
  # Round Robin Object
  df_rr <- reactiveValues(new = data.frame(),
                          readyQueue = data.frame(),
                          running = data.frame(),
                          waiting = data.frame(),
                          terminated = data.frame(),
                          quantum = 0,
                          timeSlice = 0,
                          cpuIdleTime = 0,
                          algorithmNum = 2)
  
  # Shortest Job First Object
  df_sjf <- reactiveValues(new = data.frame(),
                           readyQueue = data.frame(),
                           running = data.frame(),
                           waiting = data.frame(),
                           terminated = data.frame(),
                           cpuIdleTime = 0,
                           algorithmNum = 3,
                           preempt = FALSE,
                           estimate = FALSE)
  
  df_sjfexp <- reactiveValues(new = data.frame(),
                              readyQueue = data.frame(),
                              running = data.frame(),
                              waiting = data.frame(),
                              terminated = data.frame(),
                              cpuIdleTime = 0,
                              algorithmNum = 4,
                              preempt = FALSE,
                              estimate = TRUE)
  
  df_srtf <- reactiveValues(new = data.frame(),
                            readyQueue = data.frame(),
                            running = data.frame(),
                            waiting = data.frame(),
                            terminated = data.frame(),
                            cpuIdleTime = 0,
                            algorithmNum = 5,
                            preempt = TRUE,
                            estimate = FALSE)
  
  df_srtfexp <- reactiveValues(new = data.frame(),
                              readyQueue = data.frame(),
                              running = data.frame(),
                              waiting = data.frame(),
                              terminated = data.frame(),
                              cpuIdleTime = 0,
                              algorithmNum = 6,
                              preempt = TRUE,
                              estimate = TRUE)
  
  # Multilevel Queue Object
  # df_mq <- reactiveValues(queue1 = list(new = data.frame(),
  #                                       readyQueue = data.frame(),
  #                                       waiting = data.frame(),
  #                                       terminated = data.frame(),
  #                                       priority = 1),
  #                         queue2 = list(new = data.frame(),
  #                                       readyQueue = data.frame(),
  #                                       waiting = data.frame(),
  #                                       terminated = data.frame(),
  #                                       priority = 2),
  #                         queue3 = list(new = data.frame(),
  #                                       readyQueue = data.frame(),
  #                                       waiting = data.frame(),
  #                                       terminated = data.frame(),
  #                                       priority = 3),
  #                         running = data.frame(),
  #                         cpuIdleTime = 0,
  #                         algorithmNum = 7)
  
  
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
    
    if("First Come First Serve" %in% input$schedulingChoices){
      df_fcfs$new <- df_processes$processes
      clock$runningAlgos <- clock$runningAlgos + 1
    }
    
    if("Round Robin" %in% input$schedulingChoices){
      df_rr$new <- df_processes$processes
      df_rr$quantum <- input$timeQuantum
      clock$runningAlgos <- clock$runningAlgos + 1
    }
    
    if("Shortest Job First" %in% input$schedulingChoices){
      df_sjf$new <- df_processes$processes
      df_sjfexp$new <- df_processes$processes
      df_srtf$new <- df_processes$processes
      df_srtfexp$new <- df_processes$processes
      clock$runningAlgos <- clock$runningAlgos + length(input$sjfAlgos)
    }
    
    clock$count <- -1
    clock$timer <- reactiveTimer(input$seconds)
    clock$active <- TRUE
    
  })
  
  
  observeEvent(clock$timer(), {
    req(clock$active == TRUE)
    
    clock$count <- clock$count + 1
  })
  
  observeEvent(input$pause, {
    clock$active <- FALSE
  })
  
  observeEvent(input$resume, {
    clock$active <- TRUE
  })
  
  
  observeEvent(req("First Come First Serve" %in% input$schedulingChoices && clock$count), { # every clock tick
    req(clock$active) # only if simulation is running
    
    RunFCFS(df_fcfs)
    
    if(nrow(df_fcfs$terminated) == input$numProcesses){
      clock$runningAlgos <- clock$runningAlgos - 1
    }
  })
  
  
  observeEvent(req("Round Robin" %in% input$schedulingChoices && clock$count), { # every clock tick
    req(clock$active) # only if simulation is running
    
    RunRR(df_rr)
    
    if(nrow(df_rr$terminated) == input$numProcesses){
      clock$runningAlgos <- clock$runningAlgos - 1
      
    }
  })
  
  
  observeEvent(req("Shortest Job First" %in% input$schedulingChoices && clock$count), { # every clock tick
    req(clock$active) # only if simulation is running

    if("SJF (No Preemption)" %in% input$sjfAlgos){
      RunSJF(df_sjf)
      
      if(nrow(df_sjf$terminated) == input$numProcesses){
        clock$runningAlgos <- clock$runningAlgos - 1
      }
    }
    
    if("SJF with Exponential Averaging" %in% input$sjfAlgos){
      RunSJF(df_sjfexp)
      
      if(nrow(df_sjfexp$terminated) == input$numProcesses){
        clock$runningAlgos <- clock$runningAlgos - 1
      }
    }
    
    if("SRTF (with Preemption)" %in% input$sjfAlgos){
      RunSJF(df_srtf)
      
      if(nrow(df_srtf$terminated) == input$numProcesses){
        clock$runningAlgos <- clock$runningAlgos - 1
      }
    }
    
    if("SRTF with Exponential Averaging" %in% input$sjfAlgos){
      RunSJF(df_srtfexp)
      
      if(nrow(df_srtfexp$terminated) == input$numProcesses){
        clock$runningAlgos <- clock$runningAlgos - 1
      }
    }
  })
  
  observeEvent(req(clock$active && clock$runningAlgos == 0), {
    clock$active <- FALSE
    clock$timer <- reactiveTimer(Inf)
  })
  
  observeEvent(input$reset, {
    ResetSim()
    shinyjs::reset("schedulingChoices")
  })
  
  observeEvent(input$resetStats, {
    ResetStats()
  })

  
  # ----------------------------------------------------------- #
  #
   #                      ---- Outputs ----
  #
  # ----------------------------------------------------------- #
  
  output$counter <- renderUI({
    div(sprintf("Timer: %s",
            clock$count)
    )
  })
  
  output$statsTable <- renderTable(
    df_stats$statsTable,
    rownames = TRUE,
    digits = 2
  )
  
  output$waitPlot <- renderPlot({
    df <- data.frame(stat = df_stats$statsTable[,"Average Wait Time"])
    
    RenderBarChart(df, "Average Wait Time", "Average Wait Time (in clock ticks)")
  })
  
  output$ttPlot <- renderPlot({
    df <- data.frame(stat = df_stats$statsTable[,"Average Turnaround Time"])
    
    RenderBarChart(df, "Average Turnaround Time", "Average Turnaround Time (in clock ticks)")
  })
  
  output$cpuPlot <- renderPlot({
    df <- data.frame(stat = df_stats$statsTable[,"CPU Utilization"])
    
    RenderBarChart(df, "CPU Utilization", "CPU Utilization (%)")
  })
  
  output$responsePlot <- renderPlot({
    df <- data.frame(stat = df_stats$statsTable[,"Average Response Time"])
    
    RenderBarChart(df, "Average Response Time", "Average Response Time (in clock ticks)")
  })
  
  output$throughputPlot <- renderPlot({
    df <- data.frame(stat = df_stats$statsTable[,"Throughput"])
    
    RenderBarChart(df, "Throughput", "Throughput")
  })
  
  # First Come First Serve
  output$fcfsStats <- renderTable(
    df_stats$statsTable[df_fcfs$algorithmNum,],
    rownames = TRUE,
    digits = 2
  )
  
  # FCFS ----
  output$fcfsNew <- renderTable({
    GetAlgoOutput(df_fcfs$new)
  })
  
  output$fcfsReady <- renderTable({
    GetAlgoOutput(df_fcfs$readyQueue)
  })
  
  output$fcfsRunning <- renderTable({
    GetAlgoOutput(df_fcfs$running)
  })
  
  output$fcfsWaiting <- renderTable({
    GetAlgoOutput(df_fcfs$waiting)
  })
  
  output$fcfsTerminated <- renderTable({
    GetAlgoOutput(df_fcfs$terminated)
  })
  
  
  # Round Robin ----
  output$rrSlice <- renderTable({
    c(df_rr$timeSlice, df_rr$quantum)
  })
  
  output$rrStats <- renderTable(
    df_stats$statsTable[df_rr$algorithmNum,],
    rownames = TRUE,
    digits = 2
  )
  
  output$rrNew <- renderTable({
    GetAlgoOutput(df_rr$new)
  })
  
  output$rrReady <- renderTable({
    GetAlgoOutput(df_rr$readyQueue)
  })
  
  output$rrRunning <- renderTable({
    GetAlgoOutput(df_rr$running)
  })
  
  output$rrWaiting <- renderTable({
    GetAlgoOutput(df_rr$waiting)
  })
  
  output$rrTerminated <- renderTable({
    GetAlgoOutput(df_rr$terminated)
  })
  
  
  # Shortest Job First ----
  output$sjfStats <- renderTable(
    df_stats$statsTable[df_sjf$algorithmNum,],
    rownames = TRUE,
    digits = 2
  )
  
  output$sjfExpStats <- renderTable(
    df_stats$statsTable[df_sjfexp$algorithmNum,],
    rownames = TRUE,
    digits = 2
  )
  
  output$srtfStats <- renderTable(
    df_stats$statsTable[df_srtf$algorithmNum,],
    rownames = TRUE,
    digits = 2
  )
  
  output$srtfExpStats <- renderTable(
    df_stats$statsTable[df_srtfexp$algorithmNum,],
    rownames = TRUE,
    digits = 2
  )

  output$sjfNew <- renderTable(
    GetAlgoOutput(df_sjf$new)
  )

  output$sjfReady <- renderTable({
    GetAlgoOutput(df_sjf$readyQueue)
  })

  output$sjfRunning <- renderTable({
    GetAlgoOutput(df_sjf$running)

  })

  output$sjfWaiting <- renderTable({
    GetAlgoOutput(df_sjf$waiting)

  })

  output$sjfTerminated <- renderTable({
    GetAlgoOutput(df_sjf$terminated)

  })
  
  
  output$sjfexpNew <- renderTable(
    GetAlgoOutput(df_sjfexp$new)
  )
  
  output$sjfexpReady <- renderTable({
    GetAlgoOutput(df_sjfexp$readyQueue)
  })
  
  output$sjfexpRunning <- renderTable({
    GetAlgoOutput(df_sjfexp$running)
    
  })
  
  output$sjfexpWaiting <- renderTable({
    GetAlgoOutput(df_sjfexp$waiting)
    
  })
  
  output$sjfexpTerminated <- renderTable({
    GetAlgoOutput(df_sjfexp$terminated)
    
  })
  
  
  
  output$srtfNew <- renderTable(
    GetAlgoOutput(df_srtf$new)
  )
  
  output$srtfReady <- renderTable({
    GetAlgoOutput(df_srtf$readyQueue)
  })
  
  output$srtfRunning <- renderTable({
    GetAlgoOutput(df_srtf$running)
    
  })
  
  output$srtfWaiting <- renderTable({
    GetAlgoOutput(df_srtf$waiting)
    
  })
  
  output$srtfTerminated <- renderTable({
    GetAlgoOutput(df_srtf$terminated)
    
  })
  
  
  
  output$srtfexpNew <- renderTable(
    GetAlgoOutput(df_srtfexp$new)
  )
  
  output$srtfexpReady <- renderTable({
    GetAlgoOutput(df_srtfexp$readyQueue)
  })
  
  output$srtfexpRunning <- renderTable({
    GetAlgoOutput(df_srtfexp$running)
    
  })
  
  output$srtfexpWaiting <- renderTable({
    GetAlgoOutput(df_srtfexp$waiting)
    
  })
  
  output$srtfexpTerminated <- renderTable({
    GetAlgoOutput(df_srtfexp$terminated)
    
  })
  
  
  output$processTable <- renderTable(
    df_processes$processes,
    rownames = TRUE
  )
  
  output$cpuTimesTable <- renderTable(
    df_processes$eventTimes,
    rownames = TRUE
  )
  
  output$cpuLengthsTable <- renderTable(
    df_processes$eventLengths,
    rownames = TRUE
  )
  
  
  # ----------------------------------------------------------- #
  #
  #                     ---- Functions ----
  #
  # ----------------------------------------------------------- #
  
  SpawnProcesses <- function(df){
    
    pIDs <- 1:input$numProcesses
    arrivals <- sample(1:input$numProcesses, size = input$numProcesses, replace = TRUE)
    cpuTime <- sample(rpois(input$numProcesses, input$cpuLambda), size = input$numProcesses, replace = FALSE)
    numEvents <- sample(input$ioEvents[1]:input$ioEvents[2], size = input$numProcesses, replace = TRUE)
    
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
    procNames <- list()
    ioColsNames <- list()
    
    for(x in 1:ioCols){
      ioColsNames[x] <- paste("Event ", x)
    }
    
    for(x in 1:input$numProcesses){
      procNames[x] <- paste("Process ", x)
    }
    
    df_eventTimes <- data.frame(matrix(ncol = ioCols, 
                                       nrow = input$numProcesses, 
                                       dimnames = list(c(procNames),
                                                       c(ioColsNames))))  
    df_eventLengths <- data.frame(matrix(ncol = ioCols, 
                                         nrow = input$numProcesses,
                                         dimnames = list(c(procNames),
                                                         c(ioColsNames))))
    
    divisor <- floor(cpuTime /(numEvents + 1))
    
    for(x in 1:input$numProcesses){
      if(numEvents[x] != 0){
        
        if(input$eventSkew == "Even"){
          df_eventTimes[x,1] <- cpuTime[x] - divisor[x] # even
        } else {
          df_eventTimes[x,1] <- sample(numEvents[x]:cpuTime[x], size = 1) # random
        }
        
        df_eventLengths[x, 1] <- sample(rpois(input$numProcesses, input$ioLambda), size = 1)
        
        if(numEvents[x] > 1){
          for(y in 2:ioCols){
            
            if(y <= numEvents[x]){
              
              if(input$eventSkew == "Even"){
                df_eventTimes[x,y] <- df_eventTimes[x,y-1] - divisor[x] # even
              } else{
                df_eventTimes[x,y] <- sample((numEvents[x] - (y - 1)):df_eventTimes[x,y-1] - 1, size = 1) # random
              }

              df_eventLengths[x, y] <- sample(rpois(input$numProcesses, input$ioLambda), size = 1)
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
      if(df$estimate){
        df$readyQueue <- df$readyQueue[order(df$readyQueue$Next.CPU.Estimate, decreasing = FALSE),]
      } else{
        df$readyQueue <- df$readyQueue[order(df$readyQueue$Next.CPU.Runtime, decreasing = FALSE),]
      }
    }

    if(df$preempt){
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
      
      } else if(df$estimate && (df$running[1,"Next.CPU.Estimate"] > df$readyQueue[1, "Next.CPU.Estimate"])){ # preempt with estimate
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
        df$waiting[c(1:ioComplete),]$Last.CPU.Runtime <- 0
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
    UpdateStats(df, df$algorithmNum)
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
  
  
  UpdateStats <- function(df, rowNum){

    df_stats$statsTable[rowNum,"Average Wait Time"] <- mean(df$terminated$Total.Wait.Time)
    df_stats$statsTable[rowNum,"Median Wait Time"] <- median(df$terminated$Total.Wait.Time)
    df_stats$statsTable[rowNum,"Average Turnaround Time"] <- mean(df$terminated$Turnaround.Time)
    df_stats$statsTable[rowNum,"Median Turnaround Time"] <- median(df$terminated$Turnaround.Time)
    df_stats$statsTable[rowNum,"CPU Utilization"] <- (1 - (df$cpuIdleTime / clock$count)) * 100
    df_stats$statsTable[rowNum,"Average Response Time"] <- mean(df$terminated$Response.Time)
    df_stats$statsTable[rowNum,"Throughput"]<- (sum(df$terminated$Total.CPU.Bursts) / clock$count) * 100
  }
  
  ResetSim <- function(){
    clock$count <- 0
    clock$active <- FALSE
    clock$runningAlgos <- 0
    
    ClearQueues(df_fcfs)
    ClearQueues(df_rr)
    ClearQueues(df_sjf)
    ClearQueues(df_sjfexp)
    ClearQueues(df_srtf)
    ClearQueues(df_srtfexp)
    
  }
  
  ResetStats <- function(){
    
    df_stats$statsTable <- matrix(0,
                                  ncol = 7, 
                                  nrow = 6,
                                  dimnames = list(algorithms, 
                                                  c("Average Wait Time",
                                                    "Median Wait Time",
                                                    "Average Turnaround Time",
                                                    "Median Turnaround Time",
                                                    "CPU Utilization",
                                                    "Average Response Time",
                                                    "Throughput")))
  }
  
  ClearQueues <- function(df){
    
    df$new <- data.frame()
    df$readyQueue <- data.frame()
    df$running <- data.frame()
    df$waiting <- data.frame()
    df$terminated <- data.frame()
    df$cpuIdleTime <- 0
  }
  
  # ClearStats <- function(df){
  #   
  #   df$cpuIdleTime <- 0
  #   df$avgWaitTime <- 0
  #   df$medWaitTime <- 0
  #   df$avgTurnaround <- 0
  #   df$medTurnaround <- 0
  #   df$responsive <- 0
  #   df$cpuUtil <- 0
  #   df$throughput <- 0
  # }
  
  # GetSJF <- function(){
  #   
  #   if(input$estimateSJF){
  #     if(input$preemptive){
  #       df_srtfexp
  #     } else{
  #       df_sjfexp
  #     }
  #   } else{
  #     if(input$preemptive){
  #       df_srtf
  #     } else{
  #       df_sjf
  #     }
  #   }
  # }
  
  
  GetAlgoOutput <- function(df){
    
    keep <- c("Process.ID",
              "Arrival",
              "Total.CPU.Runtime",
              "Last.CPU.Runtime",
              "Next.CPU.Runtime",
              "Total.Event.Bursts",
              "Next.Event.Time",
              "Event.Burst.Length",
              "Next.CPU.Estimate",
              "Total.CPU.Bursts",
              "Response.Time",
              "Total.Wait.Time",
              "Turnaround.Time")

    df_new <- df[, (names(df) %in% keep)]
    
    
    # if(!input$estimateSJF){
    #   dropEst <- c("Next.CPU.Estimate")
    #   df_new <- df_new[, !(names(df_new) %in% dropEst)]
    # }
    
    return(df_new)
  }
  
  GetStats <- function(df){
    df_stats <- data.frame("Average Wait Time" = df$avgWaitTime,
                           "Median Wait Time" = df$medWaitTime,
                           "Average Turnaround Time" = df$avgTurnaround,
                           "Median Turnaround Time" = df$medTurnaround,
                           "CPU Utitization (%)" = as.numeric(df$cpuUtil),
                           "Average Responsiveness" = as.numeric(df$responsive),
                           "Throughput" = as.numeric(df$throughput))
    
    return(df_stats)
  }
  
  RenderBarChart <- function(df, plotTitle, yLabel){
    
    barchart <- ggplot(
      data=df, aes(x=algorithms, y=stat, fill = algorithms)) +
      geom_bar(stat="identity") +
      labs(title=plotTitle, y=yLabel, x="Algorithm") +
      theme(
        title = element_text(size=22, hjust = 0.5),
        axis.title.x=element_text(size=18, vjust=-0.4),  # X axis title
        axis.title.y=element_text(size=18),  # Y axis title
        axis.text.x=element_text(size=14),  # X axis text
        axis.text.y=element_text(size=14)) +
      coord_cartesian(clip="off")
    
    return(barchart)
  }
  
  
  # ----------------------------------------------------------- #
  #
  #                 ---- Input Validation ----
  #
  # ----------------------------------------------------------- #
  
  iv <- InputValidator$new()
  
  iv$add_rule("numProcesses", sv_required())
  # iv$add_rule("numProcesses", sv_)
  
  iv$enable()
  
}