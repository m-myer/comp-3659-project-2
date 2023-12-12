library(base)
library(DT)
library(ggplot2)
library(shiny)
library(shinydashboard)
library(shinythemes)
library(shinyjs)
library(shinyvalidate)
library(shinyWidgets)
library(xtable)

algorithms <- c("First Come First Serve",
                "Round Robin",
                "Shortest Job First",
                "SJF - Estimated",
                "Shortest Remaining Time First",
                "SRTF - Estimated")

ioEventRange <- c(0:10)

#source("R/anotherScript.R")