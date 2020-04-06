# ----------------------------------------------------- #
#
#    READi Tool;
#        Stanley Wang
#        Brennan Beal
#
# ----------------------------------------------------- #
library(shiny)
library(plotly)
library(shinyjs)
library(rintrojs)
library(shinythemes)
library(shinyWidgets)
library(scales)
library(lubridate)
library(shinyBS)
library(kableExtra)
library(ggthemes)
library(tidyverse)
library(V8)

source("Components/EvalPage/evalPage.R")
source("Components/HomePage/homePage.R")
source("Components/IdentifyPage/identifyPage.R")
source("Auxiliary/auxiliary.R")
source("Components/Authentication/authentication.R")
source("Components/Authentication/LoginDropdown/loginDropdown.R")
source("Components/RecPage/recPage.R")
source("Components/EvalHistory/evalHistory.R")
source("Components/SumPage/sumPage.R")
source("Components/UI/Loader/loader.R")
source("Components/UI/SideDrawer/sideDrawer.R")

# Define UI for application that draws a histogram
ui <- function(request){
  fluidPage(
    title = "READi",
    theme = shinytheme("lumen"),
    introjsUI(),
    useShinyjs(),
    extendShinyjs(text = toTop),
    extendShinyjs(script = "account.js"),
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"),
      tags$link(rel = "stylesheet", type = "text/css", href = "auth.css"),
      tags$link(rel = "stylesheet", type = "text/css", href = "UI.css"),
      tags$link(rel = "stylesheet", href = "https://cdnjs.cloudflare.com/ajax/libs/jquery-confirm/3.3.2/jquery-confirm.min.css"),
      tags$script(src="https://www.gstatic.com/firebasejs/7.9.2/firebase-app.js"),
      tags$script(src="https://www.gstatic.com/firebasejs/7.9.2/firebase-auth.js"),
      tags$script(src="https://cdnjs.cloudflare.com/ajax/libs/jquery-confirm/3.3.2/jquery-confirm.min.js"),
      shiny::tags$script(src="auth.js"),
      shiny::tags$script(src="UI.js")
    ),
    authenticationUI("authentication"),
    loader,
    sideDrawerUI,
    navbarPageWithBtn(div(tags$div(id = "mobile-toggle", icon("user-circle")), span(id = "nav_title", "READi Tool")),
                      id = "tabs",
                      collapsible = TRUE,
                      header = bookmarkButton(label = "Save Progress", id = "bookmark"),
                      tabPanel("Home", value = "home",
                               homePageUI),
                      
                      # ---------------------------  ----------------------------------#
                      # --------------------------- Phase 1: RWE ----------------------------------#
                      # ---------------------------  ----------------------------------#
                      tabPanel(uiOutput("title_panel_1", class = "inline"), value = "tab1", icon = icon("check-circle"),
                               identifyPageUI("identify_page")),
                      
                      # ---------------------------  ----------------------------------#
                      # ------------------------- Phase 2: Grading of Evidence ---------------------------#
                      # ---------------------------  ----------------------------------#
                      tabPanel(uiOutput("title_panel_2", class = "inline"), value = "tab2", icon = icon("check-circle"),
                               evalPageUI("eval_page")),
                      
                      # ---------------------------  ----------------------------------#
                      # --------------------------- Phase 3: Evidence-Based Rec ----------------------------#
                      # ---------------------------  ----------------------------------#
                      tabPanel(uiOutput("title_panel_3", class = "inline"), value = "tab3", icon = icon("check-circle"),
                               sumPageUI("sum_page")),
                      
                      # ---------------------------  ----------------------------------#
                      # --------------------------- Phase 4: Making an Evidence-Based Rec ----------------------------#
                      # ---------------------------  ----------------------------------#
                      tabPanel(uiOutput("title_panel_4", class = "inline"), value = "tab4", icon = icon("check-circle"),
                               recPageUI("rec_page")),
                      
                      # Evaluation history page
                      tabPanel("", value = "history", class = "always-show", evalHistory)
    ))
} # closing function (function necessary for bookmarking)
