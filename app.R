# ----------------------------------------------------- #
#
#    READi Tool;
#        Stanley Wang
#        Brennan Beal
#
# ----------------------------------------------------- #
library(shiny)
library(gt)
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
source("Components/ReviewSummary/finalSum.R")
source("Components/UI/Loader/loader.R")
source("Components/UI/SideDrawer/sideDrawer.R")

# Define UI for application that draws a histogram
ui <- function(request){
  fluidPage(
    title = "READi",
    theme = shinytheme("cerulean"),
    introjsUI(),
    useShinyjs(),
    extendShinyjs(text = toTop),
    extendShinyjs(script = "account.js"),
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"),
      tags$link(rel = "stylesheet", type = "text/css", href = "auth.css"),
      tags$link(rel = "stylesheet", type = "text/css", href = "UI.css"),
      tags$link(rel = "stylesheet", type = "text/css", href = "print.css"),
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
                      
                      # ---------------------------  ----------------------------------#
                      # --------------------------- Phase 5: Summary of Review ----------------------------#
                      # ---------------------------  ----------------------------------#
                      tabPanel(uiOutput("title_panel_5", class = "inline"), value = "tab5", icon = icon("check-circle"),
                               finalSumUI("final_sum")),
                      
                      # Evaluation history page
                      tabPanel("", value = "history", class = "always-show", evalHistory)
    ))
} # closing function (function necessary for bookmarking)


# Define server logic
server <- function(input, output, session) {
  
  # -------------------- Intro Tutorial
  observeEvent(input$intro, {
    removeModal()
    int_text <- c(
      
      # -- Welcome
      "Welcome to the READi tool - for a quick tour, click next, otherwise click 'skip' to begin! (needs to be html with actual button to continue to tour - this is placeholder)",
      
      # -- Login
      "The first thing you should do is login. Here, you can either register as a new user or continue your progress on any of your projects with your login.
        If you don't want to register, you can continue as a guest but you won't have access to your other projects!",
      
      # -- Save progress
      "You will want to make sure you are saving your progress (the process can be a lot for one sitting).
        Click this button for a link to copy and save progress.",
      
      #  -- If you want to know more
      "This process is completed in phases (which I will show you in a few seconds). If you want to learn more about the
        phases themselves before beginning, check them out here.",
      
      # -- Navigate
      "You can navigate between those phases here.",
      
      # -- To begin
      "Finally, click here when you are ready to begin!"
      
    )
    
    intro <- data.frame(
      element = c("null", "#loginToggle", "#loginToggle", "#learn", "#tabs", "#beginPhase"),
      intro = int_text,
      position =  c("bottom", "bottom", "bottom", "bottom", "bottom", "bottom"))
    
    introjs(session, options = list(steps = intro,
                                    disableInteraction = TRUE,
                                    "nextLabel" = "Next",
                                    "prevLabel" = "Prev",
                                    "doneLabel" = "Cool - let's get started!",
                                    "showProgress" = TRUE,
                                    "showStepNumbers" = FALSE,                                                                                                                             
                                    "hidePrev" = TRUE,
                                    "hideNext" = TRUE))
    
  })
  # ----- Welcoming users to site (with introduction)
  observe({
    if (length(getQueryString(session)) == 0) {
      showModal(modalDialog(
        # --- Need html file here but for now:
        title = "Important Message",
        "Some welcome message:",
        easyClose = TRUE,
        footer = tagList(
          # --- Creating intro option on main intro
          actionButton(inputId = "intro", label = "Introduction Tour!", icon = icon("info-circle"))
        )
      ))
    }
  })
 
  setBookmarkExclude(c("bookmark", "new_session"))
  
  observeEvent(input$bookmark, {
    session$doBookmark()
  })
  
  # ----- Hiding all tabs upon  entry to site
  hideTab("tabs", "history")
  hideTab(inputId = "tabs", target = "tab1")
  hideTab(inputId = "tabs", target = "tab2")
  hideTab(inputId = "tabs", target = "tab3")
  # hideTab(inputId = "tabs", target = "tab4")
  # hideTab(inputId = "tabs", target = "tab5")
  
  observeEvent(input$beginPhase,{
    if (session$userData$inSession()) {
      confirmSweetAlert(
        session,
        inputId = "new_session",
        title = "New session",
        text = "Do you want to leave your current session? Any unsaved progress will be lost.",
        type = "question",
        btn_labels = c("cancel", "continue"),
        closeOnClickOutside = TRUE,
        showCloseButton = TRUE,
        html = FALSE
      )
    } else {
      startNewSession()
    }
  })
  
  observeEvent(input$new_session, {
    if (isTRUE(input$new_session)) {
      js$newSession()
    }
  })
  
  # display the my-progress page
  showProgressPage <- function() {
    shinyjs::hide(id = "bookmark")
    updateNavbarPage(session, "tabs", "history")
    toggleDropdownButton(inputId = "account_dropdown")
    js$updateAccount(session$userData$current_user()$uid)
  }
  
  # Update state and UI and start a new session
  startNewSession <- function() {
    showTab(inputId = "tabs", target = "tab1")
    shinyjs::show(id = "bookmark")
    updateNavbarPage(session, "tabs", "tab1")
    session$userData$inSession(TRUE)
    session$userData$phase(1)
  }
  
  # eventHandler for displaying my-progress page
  observeEvent(input$my_progress, {
    showProgressPage()
  })
  
  # eventHandler for displaying my-progress page from side bar (mobile)
  observeEvent(input$my_progress_side, {
    showProgressPage()
  })
  
  callModule(authentication, "authentication")
  
  # initialize user and session data
  session$userData$current_state <- reactiveVal(NULL)
  session$userData$current_user <- reactiveVal(NULL)
  session$userData$current_session <- reactiveVal(NULL)
  session$userData$inSession <- reactiveVal(FALSE)
  session$userData$phase <- reactiveVal(NULL)
  observeEvent(input$auth_user, {
    session$userData$current_user(input$auth_user)
  }, ignoreNULL = FALSE)
  observeEvent(input$current_session, {
    session$userData$current_session(input$current_session)
  }, ignoreNULL = FALSE)
  
  # switch between auth sign in/registration and app for signed in user
  observeEvent(session$userData$current_user(), {
    current_user <- session$userData$current_user()
    if (!is.null(current_user)) {
      removeClass(selector = "#auth_panel", class = "Show")
      addClass(selector = "#login_backdrop", class = "hidden")
      cur_session <- getQueryString()$session
      js$hideSpinner()
      # get current session and update bookmark save state
      if (!is.null(cur_session)) {
        session$userData$current_state(getQueryString()[['_state_id_']])
        js$checkSession(current_user$uid, cur_session)
        session$userData$current_session(cur_session)
        session$userData$inSession(TRUE)
      }
      onBookmarked(function(url) {
        if (!is.null(session$userData$current_state())) {
          cmd <- paste0("rmdir /Q /S shiny_bookmarks\\", session$userData$current_state)
          try(shell(cmd))
        }
        session$userData$current_state(strsplit(url, "=")[[1]][2])
        js$saveState(url, current_user$uid, session$userData$current_session(), session$userData$phase())
      })
    } else {
      js$clearAccount()
      js$hideSpinner()
    }
  }, ignoreNULL = FALSE)
  
  output$loginToggle <- renderUI({
    current_user <- session$userData$current_user()
    if (is.null(current_user)) {
      return (
        tags$button(
          id = "login",
          type = "button",
          class = "Login btn DesktopOnly",
          "Log in"
        )
      )
    } else {
      return (loginDropdown)
    }
  })
  
  output$loginToggleSide <- renderUI({
    current_user <- session$userData$current_user()
    if (is.null(current_user)) {
      return (
        tags$button(
          id = "login-side",
          type = "button",
          class = "Login SideDrawerItem",
          "Log in"
        )
      )
    } else {
      return (loginDropdownSide)
    }
  })
  
  # switch between desktop and mobile navbar
  observe({
    req(input$width)
    if(input$width < 768) {
      shinyjs::hide("loginToggle")
      shinyjs::show("sidebar")
    } else {
      shinyjs::show("loginToggle")
      shinyjs::hide("sidebar")
    }
  })
  
  # hide bookarmk button on homepage
  observe({
    if (req(input$tabs) == "home") {
      shinyjs::hide(id = "bookmark")
    }
  })

  # save current phase on bookmark
  onBookmark(function(state) {
    state$values$phase <- session$userData$phase()
  })
  
  # On restored bookmark, update UI to current phase
  onRestore(function(state) {
    session$userData$inSession(TRUE)
    if (!is.null(getQueryString()$new) && getQueryString()$new == "true") {
      startNewSession()
      return()
    }
    session$userData$phase(state$values$phase)
    for (i in 1:state$values$phase) {
      if (i > 1) {
        shinyjs::show(selector = paste0("#tabs li:nth-child(", i, ") i"))
      }
      showTab(inputId = "tabs", target = paste0("tab", i))
    }
    updateNavbarPage(session, "tabs", paste0("tab", i))
  })
  
  # dynamic title for tab 1
  output$title_panel_1 = renderText({
    if (req(input$tabs) == "tab1") {
      shinyjs::show(id = "bookmark")
      hideTab(inputId = "tabs", target = "tab2")
      hideTab(inputId = "tabs", target = "tab3")
      session$userData$phase(1)
      return("Phase 1: Identify Real World Evidence")
    }
    return("Phase 1")
  })
  
  # dynamic title for tab 2
  output$title_panel_2 = renderText({
    if (req(input$tabs) == "tab2") {
      shinyjs::show(id = "bookmark")
      session$userData$phase(2)
      return("Phase 2: Reviewing and Grading of Evidence")
    }
    return("Phase 2")
  })
  
  # dynamic title for tab 3
  output$title_panel_3 = renderText({
    if (req(input$tabs) == "tab3") {
      shinyjs::show(id = "bookmark")
      session$userData$phase(3)
      return("Phase 3: Summarizing The Literature")
    }
    return("Phase 3")
  })
  
  # dynamic title for tab 4
  output$title_panel_4 = renderText({
    if (req(input$tabs) == "tab4") {
      shinyjs::show(id = "bookmark")
      session$userData$phase(4)
      return("Phase 4: Making an Evidence-Based Recommendation")
    }
    return("Phase 4")
  })
  
  # dynamic title for tab 5
  output$title_panel_5= renderText({
    if (req(input$tabs) == "tab5") {
      shinyjs::show(id = "bookmark")
      session$userData$phase(5)
      return("Phase 5: Final Summary")
    }
    return("Phase 5")
  })
  
  
  # ---------------------------  ----------------------------------#
  # --------------------------- Phase 1: RWE ----------------------------------#
  # ---------------------------  ----------------------------------#
  phase1_inputs <- callModule(identifyPage, "identify_page", session)
  
  # ---------------------------  ----------------------------------#
  # --------------------------- Phase 2: RWE ----------------------------------#
  # ---------------------------  ----------------------------------#
  bias_values <- callModule(evalPage, "eval_page", session, phase1_inputs)
  
  # ---------------------------  ----------------------------------#
  # --------------------------- Phase 3: RWE ----------------------------------#
  # ---------------------------  ----------------------------------#
  
  phase3_inputs <- callModule(sumPage, "sum_page", session, phase1_inputs, bias_values)
  
  # ---------------------------  ----------------------------------#
  # --------------------------- Phase 4: RWE ----------------------------------#
  # ---------------------------  ----------------------------------#
  
  phase4_inputs <- callModule(recPage, "rec_page", session)
  
  # ---------------------------  ----------------------------------#
  # --------------------------- Phase 5: RWE ----------------------------------#
  # ---------------------------  ----------------------------------#
  
  callModule(finalSum, "final_sum", phase1_inputs, bias_values, phase3_inputs, phase4_inputs)
  
  
}

# Run the application 
shinyApp(ui = ui, server = server, enableBookmarking = "server")

