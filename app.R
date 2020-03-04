library(shiny)
library(shinyjs)
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

# wrapper for navbarPage with login button
navbarPageWithBtn <- function(...) {
    navbar <- navbarPage(...)
    element <- uiOutput("loginToggle")
    btn <- tags$button(
        id = "login",
        type = "button",
        class = "btn",
        "Log in"
    )
    navbar[[3]][[1]]$children[[1]]$children[[2]] <- htmltools::tagAppendChild(
        navbar[[3]][[1]]$children[[1]]$children[[2]], element)
    navbar
}

# Define UI for application that draws a histogram
ui <- function(request){
  fluidPage(
    theme = shinytheme("lumen"),
    useShinyjs(),
    extendShinyjs(text = toTop),
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"),
      tags$link(rel = "stylesheet", type = "text/css", href = "auth.css"),
      tags$script(src="https://www.gstatic.com/firebasejs/7.9.2/firebase-app.js"),
      tags$script(src="https://www.gstatic.com/firebasejs/7.9.2/firebase-auth.js"),
      shiny::tags$script(src="auth.js")
    ),
    authenticationUI("authentication"),
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
                                   htmlOutput("t3_table")))))
  ))
} # closing function (function necessary for bookmarking)


# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
    # hideTab(inputId = "tabs", target = "tab1")
    # hideTab(inputId = "tabs", target = "tab2")
    # hideTab(inputId = "tabs", target = "tab3")
    
    callModule(authentication, "authentication")
    
    session$userData$current_user <- reactiveVal(NULL)
    
    observeEvent(input$auth_user, {
        session$userData$current_user(input$auth_user)
    }, ignoreNULL = FALSE)
    
    # switch between auth sign in/registration and app for signed in user
    observeEvent(session$userData$current_user(), {
        current_user <- session$userData$current_user()
        if (is.null(current_user)) {
          
        } else {
            removeClass(selector = "#auth_panel", class = "Show")
            addClass(selector = "#backdrop", class = "hidden")
            print(current_user$emailVerified)
            # if (current_user$emailVerified == TRUE) {
            #     shinyjs::show("main")
            # } else {
            #     shinyjs::show("verify_email_view")
            # }
          
        }
    }, ignoreNULL = FALSE)
    
    observeEvent(input$beginPhase,{
        showTab(inputId = "tabs", target = "tab1")
        updateNavbarPage(session, "tabs", "tab1")
    })
    
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
            return (
                tags$button(
                    id = "signout",
                    type = "button",
                    class = "btn",
                    "Sign out"
                )
            )
        }
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
    callModule(identifyPage, "identify_page", session)
    
           # ---------------------------  ----------------------------------#
    # --------------------------- Phase 2: RWE ----------------------------------#
           # ---------------------------  ----------------------------------#
    callModule(evalPage, "eval_page", session)
    
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
            column(8, offset = 2,
              wellPanel(strong(paste0("For your primary outcome of ", outcome$poutcome(), " answer the following questions:")),
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
          outcomes <- list(c(outcome$poutcome,
                           input$t3_studylim_1, 
                           input$t3_subjects_1, 
                           input$t3_comparator_1, 
                           input$t3_consistent_1, 
                           input$t3_direct_1, 
                           input$t3_precise_1,
                           input$t3_bias_1),
                         c(input$t1_secondary_outcome,
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

