library(shiny)
library(shinyjs)
library(shinythemes)
library(shinyWidgets)
library(lubridate)
library(shinyBS)
library(kableExtra)

source("EvalPage/evalPage.R")
source("HomePage/homePage.R")

# JS function scroll window to the study eval
jscode <- "shinyjs.toTop = function() {
    let offsetTop = document.getElementById('study_react').parentElement.offsetTop;
    window.scrollTo(0, offsetTop);
}"

choice_vec <- c("Safety and efficacy" = 1,   # chioces to later be passed to question list
            "Treatment patterns" = 2,        
            "Comparative effectiveness" = 3,
            "Economic evaluation" = 4,
            "Disease burdens" = 5,
            "Screening and surveilence" = 6)

input_validation <- function(x){                    # a function to validate inputs for submission
  logic_list <- lapply(x, isTruthy)                 # create a list of logical values
  unlisted_vec <- unlist(logic_list)                # unlist to sum
  sum(unlisted_vec) == length(unlisted_vec)         # logical testing whether there are any "non-True" values
}

# Define UI for application that draws a histogram
ui <- function(request){
  fluidPage(
    theme = shinytheme("cerulean"),
    useShinyjs(),
    extendShinyjs(text = jscode),
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
    ),
    navbarPage("READi Tool", collapsible = TRUE,
                       tabPanel("Home",
                                homePageUI),
                       
                       # ---------------------------  ----------------------------------#
                       # --------------------------- Phase 1: RWE ----------------------------------#
                       # ---------------------------  ----------------------------------#
                       tabPanel("Phase 1: Identify Real World Evidence",
                                column(2, 
                                       dropdownButton(
                                         print("This page will allow you to enter all the details of studies
                                         you are searching for. If you are receiving an error when trying to
                                         submit, make sure that all inputs available have been marked. Don't panic, it
                                         is possible to continue on with the error! The error message is a 
                                         formality to help ensure we have the most information possible. The inputs that have
                                         been marked are still being read."),
                                         icon = icon("info")
                                       )),
                                useSweetAlert(),
                                column(8, #offset = 3,
                                       # --------- Well #1
                                       wellPanel(style = "background: #d1b3e6",
                                                 selectInput("t1_int",
                                                             "1. What Type of Intervention Are You Evaluating?",
                                                             choices =  list("Pharmaceuticals" = 1,
                                                                             "Devices"            = 2,
                                                                             "Imaging"            = 3,
                                                                             "Diagnostic"         = 4,
                                                                             "Health-System"      = 5,
                                                                             "Gene Therapy"       = 6))),
                                       # ---------- Well #2
                                       wellPanel(
                                         strong("2. To define your question of interest and focus your literature search, 
                                      first answer the following questions using the PICOTS framework.
                                    We'll use your inputs for the literature search. The better refined your initial search strategy is, 
                                    the more focused the returned results will be."),
                                         br(),
                                         br(),
                                         textInput("t1_pop_interest",
                                                   "(P) What is your population of interest?",
                                                   placeholder = "Ex.) Diabetic, Geriatric, etc."),
                                         textInput("t1_int_interest",
                                                   "(I) What is your intervention of interest?",
                                                   placeholder = "Ex.) Statin, Benzodiazepines, etc."),
                                         textInput("t1_comparator",
                                                   "(C) What is the comparator?",
                                                   placeholder = "Ex.) Standard of Care"),
                                         radioButtons("t1_outcomes",
                                                      "(O) a. Do you have multiple outcomes of interest?",
                                                      choices = list("Yes" = 2,
                                                                     "No"  = 1),
                                                      selected = 1),
                                         textInput("t1_poutcome",
                                                   "(O) b. What is your primary outcome of interest?",
                                                   placeholder = "Ex.) MACE, Falls, etc."),
                                         uiOutput("multoutcomes"),
                                         # place holder for yes or no (if yes, need multiple outcomes of interest)
                                         sliderInput("t1_timeframe",
                                                     "(C) What is the time frame in years?",
                                                     min = 0.5, max = 20, step = 0.5, value = 5),
                                         textInput("t1_setting",
                                                   "(C) What is the setting of interest?",
                                                   placeholder = "Ex.) SNF, Acute Care, etc.")),
                                       # ---------- Well #3
                                       wellPanel(style = "background: #d1b3e6",
                                                 strong("3. For which topic(s) are you seeking to evaluate the literature? 
                                   More specific questions will pop up based on your selected topic(s)."),
                                                 br(),
                                                 br(),
                                                 checkboxGroupInput("t1_AOI",
                                                                    "What is your area(s) of interest?",
                                                                    choices = choice_vec,
                                                                    selected = NULL),
                                                 uiOutput("ui"),
                                                 textInput("t1_other",
                                                           "If your area of interest is not specified above,
                                      please specify it here:")
                                       ),
                                       # ------------------ Well #4
                                       wellPanel(
                                         radioButtons(
                                           "t1_studytype",
                                           "4. The types of studies that appear should be specific to each topic specified in Question 3. 
                              Based on your specific topic(s) and questions(s) about evidence, 
                              publications that use the following study designs may be the most useful to you (as a reference). 
                              You don't need to check the boxes.",
                                           choices = list("Prospective cohort study"                              = 1,
                                                          "Retrospective cohort study"                            = 2,
                                                          "Cross-sectional study for surveys"                     = 3,
                                                          "Systematic review/Meta-analysis/Network Meta-analysis" = 4,
                                                          "Pragmatic controlled trial/Large simple trial"         = 5,
                                                          "Quasi Expiremental"                                    = 6,
                                                          "Diagnostic accuracy study"                             = 7,
                                                          "Modeling (e.g. CEA, BIA, etc.)"                        = 8,
                                                          "Case-control study"                                    = 9))
                                       ),
                                       wellPanel(style = "background: #d1b3e6",
                                                 sliderInput("t1_nyears",
                                                             "5. What's the preferred time frame for your literature search (in the last N years)? 
                                        Please specify the number of years only.",
                                                             min = 0.5, max = 20, step = 0.5, value = 10)),
                                       wellPanel(
                                         radioButtons("t1_language",
                                                      "6. What is your preferred language of the literature?",
                                                      choices = list("English"                = 1,
                                                                     "Not limited to English" = 2)
                                                      
                                         )
                                       ),
                                       wellPanel(style = "background: #d1b3e6",
                                                 radioButtons("t1_limitsearch",
                                                              "Do you want to limit your literature search to key words in titles and abstracts only (recommended)?",
                                                              choices = c("Yes", "No")
                                                              
                                                 )
                                       ),
                                       br(),
                                       br(),
                                       actionBttn(
                                         inputId = "submit_1",
                                         label = "Submit Form",
                                         color = "royal",
                                         style = "minimal",
                                         icon = NULL,
                                         block = TRUE
                                       ),
                                       br(),
                                       br(),
                                       br(),
                                       br()
                                       
                                )),
                       
                       
                       
                       
                       
                       # ---------------------------  ----------------------------------#
                       # ------------------------- Phase 2: Grading of Evidence ---------------------------#
                       # ---------------------------  ----------------------------------#
                       tabPanel("Phase 2: Reviewing and Grading of Evidence",
                                column(8, offset = 2,
                                       wellPanel(
                                         radioButtons("t2_ev_available",
                                                      "Is there literature/evidence available?",
                                                      choices = c("Yes", "No")),
                                         uiOutput("study_identified")
                                       )),
                                column(8, offset = 2,
                                       uiOutput("study_react"),
                                       br(),
                                       br(),
                                       actionBttn(
                                         inputId = "submit_2",
                                         label = "Submit Form",
                                         color = "royal",
                                         style = "minimal",
                                         icon = NULL,
                                         block = TRUE
                                       ),
                                       br(),
                                       br()
                                )
                       ),
                       
                       # ---------------------------  ----------------------------------#
                       # --------------------------- Phase 3: Evidence-Based Rec ----------------------------#
                       # ---------------------------  ----------------------------------#
                       tabPanel("Phase 3: Making Evidence-Based Recommendations",
                                uiOutput("t3_pt1"),
                                column(8, offset = 2,
                                       wellPanel(
                                         wellPanel(
                                           htmlOutput("t3_table")))))
                       
  ))
} # closing function (function necessary for bookmarking)


# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
         # ---------------------------  ----------------------------------#
  # --------------------------- Phase 1: RWE ----------------------------------#
         # ---------------------------  ----------------------------------#
  
  # The following renders questions for Phase1, Question 2 (O)
  output$multoutcomes <- renderUI({
    
    if(is.null(input$t1_outcomes))
      return()
    
    if(input$t1_outcomes == 2){
      textInput("t1_secondary_outcome",
                "(O) c. What is your secondary outcome of interest?")
    }
    
  })
  
  # The following renders questions for Phase1, Question 3
  output$ui <- renderUI({        # renders questions based on checkbox
    if (is.null(input$t1_AOI))
      return()
    
    c_vec <- c()
    if(1 %in% input$t1_AOI){
      c_vec <- c(c_vec, c("To what degree is the treatment safe? (Safety and efficacy)",
                          "To what degree is the treatment effective? (Safety and efficacy)"))
    } 
    
    if(2 %in% input$t1_AOI){
      c_vec <- c(c_vec, c("What are the current treatment patterns (switching/cycling/adherence/persistence) for this disease? (Treatment patterns)",
                          "What is the heterogeneity of the treatment effect among my subpopulation? (Treatment patterns)"))
    } 
    
    if(3 %in% input$t1_AOI){
      c_vec <- c(c_vec, c("What is the comparative effectiveness of intervention on clinical outcomes? (Comparative effectiveness)",
                          "What is the comparative effectiveness of intervention on patient-centered outcomes? (Comparative effectiveness)"))
    } 
    
    if(4 %in% input$t1_AOI){
      c_vec <- c(c_vec, c("What is the current value of this intervention compared to the next best alternative? (Can include HRQoL) (Economic evaluation)",
                          "What is the budget impact? (Economic evaluation)"))
    } 
    
    if(5 %in% input$t1_AOI){
      c_vec <- c(c_vec, c("What is the natural history of the disease without treatment? (Disease burdens)",
                          "What is the clinical burden of the disease? (Disease burdens)",
                          "What is the economic burden of the disease? (Disease burdens)"))
    } 
    
    if(6 %in% input$t1_AOI){
      c_vec <- c(c_vec, c("What current screening strategies are in place to detect early disease or risk factors for disease in large numbers of apparently healthy individuals? (Screening and surveilence)",
                          "What current surveillance strategies are in place to assess the safety/efficacy/prevention of disease recurrence of my intervention? (Screening and surveilence)"))
    } 
    
    
    checkboxGroupInput(label = "What is your specific question(s) about evidence?", # add all questions above to final choice vector and checkbox
                       "dynamic",
                       choices = c_vec)
  })
  
  t1_inputs <- reactive({                           # Creating list to check if all inputs are valid (not NULL)
    list(Pop_interest = input$t1_pop_interest,
         int_interest = input$t1_int_interest,
         comparator = input$t1_comparator,
         primary_outcomes = input$t1_poutcome,
         setting = input$t1_setting, 
         a_interest = input$t1_AOI)
    
  })
  
  
  # --- Need to create search string:
  # ----- 
  search_string <- reactive({
    pop <- input$t1_pop_interest
    int <- input$t1_int_interest
    comparator <- input$t1_comparator
    outcome1 <- input$t1_poutcome
    time_frame <- input$t1_timeframe
    search <- paste("https://www.ncbi.nlm.nih.gov/pubmed/?term=([",pop,"]%20AND%20[",int,"]%20AND%20[",outcome1,"]%20AND%20(\"last ",time_frame," years\"[PDat])%20AND%20English[lang])%20NOT%20(Randomized%20Controlled%20Trial%5Bptyp%5D%20NOT%20(Meta-analysis%5Bptyp%5D%20OR%20 Systematic%20Review%5Bptyp%5D%20OR%20\"meta-analysis%20as%20topic\"%5BMeSH%20Terms%5D %20OR%20\"pragmatic%20clinical%20trials%20as%20topic\"%5BMeSH%20Terms%5D))&cmd=DetailsSearch")
    search
    
  })
  
  observeEvent(input$submit_1,
               if(input_validation(t1_inputs()))
                 {
                 search_string()
                 text <- paste("Take a look at your custom PubMed search <a href=\'", search_string(),"\' target=\"_blank\">here</a>. You'll use the studies identified here for grading in phase 2!")
                   sendSweetAlert(        # if all inputs are valid, submission successful
                     session = session,
                     title = "Submitted!", 
                     text = HTML(text),
                     html = TRUE,
                     type = "success",
                     btn_labels = c("Great")
                     ) 
                 
                 # ----- Need to add code here to also add all inputs to a data frame/however they should be stored
               } else {
                 sendSweetAlert(         # add error message if user needs more information
                   session = session,
                   title = "Oops!",
                   text = "It looks like you may not have answered all the questions!",
                   type = "error",
                   btn_labels = c("Go back")
                 )
                 
               }
  )
  
           # ---------------------------  ----------------------------------#
      # --------------------------- Phase 2: RWE ----------------------------------#
           # ---------------------------  ----------------------------------#
  
  
                 
  output$study_identified <- renderUI({ # Rendering UI based on whether or not studies are available
    if(input$t2_ev_available == "No"){
      return()
    } else {
      sliderInput("t2_n_studies",
                  "How many studies have you found?",
                  min = 0, max = 50, step = 1, value = 0)
    }
  })
  
  # ------ Creating reactionary wellPanel based on how many studies selected ------- # 
  output$study_react <- renderUI({    # goal: create panels of questions in response to "How many studies did you find?"
    num_studies <- input$t2_n_studies  # defining number of studies
    poi <- input$t1_poutcome           # defining outcome of interest to paste into radio button
    well_style <- "background: #d1b3e6"
    
    if(input$t2_ev_available == "No"){
      return(" If no relevant literature can be found, please click `Submit` below and proceed to Phase 3.")
    } else if(num_studies == 0){
      return("Please select the number of studies identified above!")
    } else {
      our_ui <- evalPageUI("review", num_studies)
      callModule(evalPage, "review", num_studies)
    }
    our_ui
  })
  callModule(evalPageFunction, "review")
  
          # ---------------------------  ----------------------------------#
      # --------------------------- Phase 3: RWE ----------------------------------#
          # ---------------------------  ----------------------------------#

    output$t3_pt1 <- renderUI({
      
      
      # ------ Defining inputID for all inputs
      studylim <- lapply(seq_len(input$t1_outcomes), function(i){paste0("t3_studylim_", i)})
      subjects <- lapply(seq_len(input$t1_outcomes), function(i){paste0("t3_subjects_", i)})
      comparator <- lapply(seq_len(input$t1_outcomes), function(i){paste0("t3_comparator_", i)})
      consistent <- lapply(seq_len(input$t1_outcomes), function(i){paste0("t3_consistent_", i)})
      direct     <- lapply(seq_len(input$t1_outcomes), function(i){paste0("t3_direct_", i)})
      precise    <- lapply(seq_len(input$t1_outcomes), function(i){paste0("t3_precise_", i)})
      bias       <- lapply(seq_len(input$t1_outcomes), function(i){paste0("t3_bias_", i)})

      lapply(seq_len(input$t1_outcomes), function(i){
            column(8, offset = 2,
              wellPanel(strong(paste0("For your primary outcome of ", input$t1_poutcome, " answer the following questions:")),
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

      domain_vec <- c("Outcome of interest", 
                      "Comparator in each study", 
                      "Number of studies and number of subjects (overall)",
                      "Level of overall study limitation",
                      "Are the results consistent",
                      "Are the results direct?",
                      "Are the results precise?",
                      "Is there publication bias?")
      if(input$t1_outcomes == 1){
        outcomes <- c(input$t1_poutcome,
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
        outcomes <- list(c(input$t1_poutcome,
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

