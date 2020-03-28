# ----------------------------------------------------- #
#
#    READi Tool;
#        Stanley Wang
#        Brennan Beal
#
# ----------------------------------------------------- #
library(shiny)
library(shinyjs)
library(rintrojs)
library(shinythemes)
library(shinyWidgets)
library(lubridate)
library(shinyBS)
library(kableExtra)
library(V8)

source("Components/EvalPage/evalPage.R")
source("Components/HomePage/homePage.R")
source("Components/IdentifyPage/identifyPage.R")
source("Auxiliary/auxiliary.R")
source("Components/Authentication/authentication.R")
source("Components/Authentication/LoginDropdown/loginDropdown.R")
source("Components/EvalHistory/evalHistory.R")
source("Components/RecPage/recPage.R")
source("Components/UI/Loader/loader.R")

# Define UI for application that draws a histogram
ui <- function(request){
  fluidPage(
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
      shiny::tags$script(src="auth.js")
    ),
    authenticationUI("authentication"),
    loader,
    navbarPageWithBtn("READi Tool",
               id = "tabs",
               collapsible = TRUE,
               tabPanel("Home",
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
                        evalPageUI("eval_page")
               ),
               
               # ---------------------------  ----------------------------------#
               # --------------------------- Phase 3: Evidence-Based Rec ----------------------------#
               # ---------------------------  ----------------------------------#
               tabPanel(uiOutput("title_panel_3", class = "inline"), value = "tab3", icon = icon("check-circle"),
                        recPageUI("rec_page")),
               
               # Evaluation history page
               tabPanel("", value = "account", class = "always-show", evalHistory)
  ))
} # closing function (function necessary for bookmarking)


# Define server logic required to draw a histogram
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
        element = c("#home", "#loginToggle", "#bookmark", "#learn", "#tabs", "#beginPhase"),
        intro = int_text,
        position =  c("bottom", "bottom", "bottom", "bottom", "bottom", "bottom"))
  
      introjs(session, options = list(steps = intro,
                                      "nextLabel" = "Next",
                                      "prevLabel" = "Go Back",
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
    
    setBookmarkExclude(c("bookmark"))
    
    observeEvent(input$bookmark, {
      session$doBookmark()
    })
   
    # ----- Hiding all tabs upon  entry to site
    hideTab("tabs", "account")
    hideTab(inputId = "tabs", target = "tab1")
    hideTab(inputId = "tabs", target = "tab2")
    hideTab(inputId = "tabs", target = "tab3")

    observeEvent(input$beginPhase,{
      showTab(inputId = "tabs", target = "tab1")
      updateNavbarPage(session, "tabs", "tab1")
    })

    observeEvent(input$my_account, {
      updateNavbarPage(session, "tabs", "account")
      toggleDropdownButton(inputId = "account_dropdown")
      js$updateAccount(session$userData$current_user()$uid)
    })

    callModule(authentication, "authentication")
    
    # initialize user and session data
    session$userData$current_user <- reactiveVal(NULL)
    session$userData$current_session <- reactiveVal(NULL)
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
          session$userData$current_session(cur_session)
        }
        onBookmarked(function(url) {
          js$saveState(url, current_user$uid, session$userData$current_session())
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
            class = "btn",
            "Log in"
          )
        )
      } else {
        return (loginDropdown)
      }
    })
    
    # always show phase 1 for restored state
    onRestore(function(state) {
      showTab(inputId = "tabs", target = "tab1")
    })
    
    # dynamic title for tab 1
    output$title_panel_1 = renderText({
      if (req(input$tabs) == "tab1") {
        hideTab(inputId = "tabs", target = "tab2")
        hideTab(inputId = "tabs", target = "tab3")
        return("Phase 1: Identify Real World Evidence")
      }
      return("Phase 1")
    })
    
    # dynamic title for tab 2
    output$title_panel_2 = renderText({
      if (req(input$tabs) == "tab2") {
        return("Phase 2: Reviewing and Grading of Evidence")
      }
      return("Phase 2")
    })
    
    # dynamic title for tab 3
    output$title_panel_3 = renderText({
      if (req(input$tabs) == "tab3") {
        return("Phase 3: Making Evidence-Based Recommendations")
      }
      return("Phase 3")
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

    callModule(recPage, "rec_page", phase1_inputs, bias_values)
  }

# Run the application 
shinyApp(ui = ui, server = server, enableBookmarking = "server")

