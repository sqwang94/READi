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
                        uiOutput("t3_pt1"),
                        column(8, offset = 2,
                               wellPanel(
                                 wellPanel(
                                   htmlOutput("t3_table"))))),
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
    phase2_inputs <- callModule(evalPage, "eval_page", session, phase1_inputs)
    
           # ---------------------------  ----------------------------------#
    # --------------------------- Phase 3: RWE ----------------------------------#
           # ---------------------------  ----------------------------------#

    
    output$t3_pt1 <- renderUI({
      outcome <- callModule(identifyPageGetOutcome, "identify_page")

      # ------ Defining inputID for all inputs
      studylim <- lapply(seq_len(outcome$outcomes()), function(i){paste0("t3_studylim_", i)})
      subjects <- lapply(seq_len(outcome$outcomes()), function(i){paste0("t3_subjects_", i)})
      comparator <- lapply(seq_len(outcome$outcomes()), function(i){paste0("t3_comparator_", i)})
      consistent <- lapply(seq_len(outcome$outcomes()), function(i){paste0("t3_consistent_", i)})
      direct     <- lapply(seq_len(outcome$outcomes()), function(i){paste0("t3_direct_", i)})
      precise    <- lapply(seq_len(outcome$outcomes()), function(i){paste0("t3_precise_", i)})
      bias       <- lapply(seq_len(outcome$outcomes()), function(i){paste0("t3_bias_", i)})

      lapply(seq_len(outcome$outcomes()), function(i){
        if(i == 1){
          out <- "primary"
          type <- outcome$poutcome()
          study <- phase2_inputs$t2_n_studies
        } else {
          out <- "secondary"
          type <- outcome$soutcome()
        }
            column(8, offset = 2,
              wellPanel(strong(paste0("For your ", out, " outcome of ", type, " answer the following questions:")),
                        br(),
                        br(),
                        wellPanel(
                          selectInput(inputId = studylim[[i]],
                                      label = "Based on the rating for each study, what's the overall level of study limitation?",
                                      choices = c("High", "Moderate", "Low"),
                                      selected = character(0)),
                          textInput(inputId = subjects[[i]],
                                    label = "What is the overall number of subjects (N)?",
                                    placeholder = 50),
                          textInput(inputId = comparator[[i]],
                                    label = "What is the comparator listed in each study for this outcome?",
                                    placeholder = "Standard of Care"),
                          selectInput(inputId = consistent[[i]],
                                      label = "Are the results among the studies consistent with one another? ",
                                      choices = c("Consistent", "Unknown", "Inconsistent")),
                          selectInput(inputId = direct[[i]],
                                      label = "Are the results direct?",  # need hover tool tip for "direct"
                                      choices = c("Direct", "Indirect")),
                          selectInput(inputId = precise[[i]],
                                      label = "Are the results precise?",
                                      choices = c("Precise", "Imprecise")),
                          selectInput(inputId = bias[[i]],
                                      label = "Is there publication bias?",
                                      choices = c("Yes", "No"))),
                        bsPopover(id = direct[[i]],
                                  title = "Evidence can be indirect when:",
                                  content =  paste("i. Patients, intervention, or outcomes differ from that of interest",
                                                   "ii. Clinicians must choose between interventions that hvae not been compared in a head-to-head manner",
                                                   sep = "<br><br>"), 
                                  placement = "left", 
                                  trigger = "hover"),
                        bsPopover(id = precise[[i]],
                                  title = "Precision", content =  paste("Studies can be considered imprecise if there are few patients or and few events, thereby rendering a fairly large confidence interval"),
                                  placement = "left",
                                  trigger = "hover"),
                        bsPopover(id = bias[[i]],
                                  title = "Publication Bias:", content =  paste("A systematic over or underestimate of treatment effect due to the selective publication of studies"),
                                  placement = "left",
                                  trigger = "hover")))
            })
    })
    
    output$t3_table <- renderText({
      outcome <- callModule(identifyPageGetOutcome, "identify_page")
      domain_vec <- c("Outcome of interest", 
                      "Comparator in each study", 
                      "Number of studies and number of subjects (overall)",
                      "Level of overall study limitation",
                      "Are the results consistent",
                      "Are the results direct?",
                      "Are the results precise?",
                      "Is there publication bias?")
      if (outcome$outcomes() == 1){
        outcomes <- c(outcome$poutcome(),
                      input$t3_studylim_1, 
                      input$t3_subjects_1, 
                      input$t3_comparator_1, 
                      input$t3_consistent_1, 
                      input$t3_direct_1, 
                      input$t3_precise_1,
                      input$t3_bias_1)
        
        df <- data.frame(
          Domains           = domain_vec,
          `Primary Outcome` = outcomes
        )
        
          kable(df,
                col.names = c("Domains", "Primary Outcome"),
                "html", 
                escape = FALSE,
                align = "c") %>% 
            kable_styling(full_width = TRUE, 
                          bootstrap_options = c("striped", "hover", "condensed", "bordered")) %>% 
            row_spec(c(1,3,5,7), background = "#d1b3e6", color = "black") %>% 
            row_spec(c(2,4,6,8), background = "white", color = "black")
      } else {
          outcomes <- list(c(outcome$poutcome(),
                           input$t3_studylim_1, 
                           input$t3_subjects_1, 
                           input$t3_comparator_1, 
                           input$t3_consistent_1, 
                           input$t3_direct_1, 
                           input$t3_precise_1,
                           input$t3_bias_1),
                         c(outcome$soutcome(),
                           input$t3_studylim_2, 
                           input$t3_subjects_2, 
                           input$t3_comparator_2, 
                           input$t3_consistent_2, 
                           input$t3_direct_2, 
                           input$t3_precise_2,
                           input$t3_bias_2))
        df <- data.frame(
          Domains           = domain_vec,
          `Primary Outcome` = outcomes[[1]],
          `Secondary Outcomes` = outcomes[[2]]
          
        )
        
        kable(df,
              col.names = c("Domains", "Primary Outcome", "Secondary Outcome"),
              "html", 
              escape = FALSE,
              align = "c") %>% 
          kable_styling(full_width = TRUE, 
                        bootstrap_options = c("striped", "hover", "condensed")) %>% 
          row_spec(c(1,3,5,7), background = "#d1b3e6", color = "black") %>% 
          row_spec(c(2,4,6,8), background = "white", color = "black")
      }
      
      })
  }

# Run the application 
shinyApp(ui = ui, server = server, enableBookmarking = "server")

