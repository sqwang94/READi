# UI function for phase 1 identifying evidence page
identifyPageUI <- function(id) {
    ns <- NS(id)
    choice_vec <- c("Safety and efficacy" = 1,   # chioces to later be passed to question list
                    "Treatment patterns" = 2,        
                    "Comparative effectiveness" = 3,
                    "Economic evaluation" = 4,
                    "Disease burdens" = 5,
                    "Screening and surveilence" = 6)
    fluidPage(
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
                   textInput(ns("t1_pop_interest"),
                             "(P) What is your population of interest?",
                             placeholder = "Ex.) Diabetic, Geriatric, etc."),
                   textInput(ns("t1_int_interest"),
                             "(I) What is your intervention of interest?",
                             placeholder = "Ex.) Statin, Benzodiazepines, etc."),
                   textInput(ns("t1_comparator"),
                             "(C) What is the comparator?",
                             placeholder = "Ex.) Standard of Care"),
                   radioButtons(ns("t1_outcomes"),
                                "(O) a. Do you have multiple outcomes of interest?",
                                choices = list("Yes" = 2,
                                               "No"  = 1),
                                selected = 1),
                   textInput(ns("t1_poutcome"),
                             "(O) b. What is your primary outcome of interest?",
                             placeholder = "Ex.) MACE, Falls, etc."),
                   uiOutput(ns("multoutcomes")),
                   # place holder for yes or no (if yes, need multiple outcomes of interest)
                   sliderInput(ns("t1_timeframe"),
                               "(C) What is the time frame in years?",
                               min = 0.5, max = 20, step = 0.5, value = 5),
                   textInput(ns("t1_setting"),
                             "(C) What is the setting of interest?",
                             placeholder = "Ex.) SNF, Acute Care, etc.")),
               # ---------- Well #3
               wellPanel(style = "background: #d1b3e6",
                         strong("3. For which topic(s) are you seeking to evaluate the literature? 
                                   More specific questions will pop up based on your selected topic(s)."),
                         br(),
                         br(),
                         checkboxGroupInput(ns("t1_AOI"),
                                            "What is your area(s) of interest?",
                                            choices = choice_vec,
                                            selected = NULL),
                         uiOutput(ns("ui")),
                         textInput(ns("t1_other"),
                                   "If your area of interest is not specified above,
                                      please specify it here:")
               ),
               
               # ------------------ Well #4
               wellPanel(
                   radioButtons(
                       ns("t1_studytype"),
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
                         sliderInput(ns("t1_nyears"),
                                     "5. What's the preferred time frame for your literature search (in the last N years)? 
                                        Please specify the number of years only.",
                                     min = 0.5, max = 20, step = 0.5, value = 10)),
               wellPanel(
                   radioButtons(ns("t1_language"),
                                "6. What is your preferred language of the literature?",
                                choices = list("English"                = 1,
                                               "Not limited to English" = 2)
                                
                   )
               ),
               wellPanel(style = "background: #d1b3e6",
                         radioButtons(ns("t1_limitsearch"),
                                      "Do you want to limit your literature search to key words in titles and abstracts only (recommended)?",
                                      choices = c("Yes", "No")
                                      
                         )
               ),
               br(),
               br(),
               actionBttn(
                   inputId = ns("submit_1"),
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
        ))
}

# server function for phase 1 identification of evidence page
identifyPage <- function(input, output, session, parentSession) {
    
    # The following renders questions for Phase1, Question 2 (O)
    output$multoutcomes <- renderUI({
         
        if (is.null(input$t1_outcomes))
            return()
        
        if (input$t1_outcomes == 2){
            textInput("t1_secondary_outcome",
                      "(O) c. What is your secondary outcome of interest?")
        }
        
    })
    
    # The following renders questions for Phase1, Question 3
    output$ui <- renderUI({        # renders questions based on checkbox
        if (is.null(input$t1_AOI))
            return()
        
        c_vec <- c()
        if (1 %in% input$t1_AOI){
            c_vec <- c(c_vec, c("To what degree is the treatment safe? (Safety and efficacy)",
                                "To what degree is the treatment effective? (Safety and efficacy)"))
        } 
        
        if (2 %in% input$t1_AOI){
            c_vec <- c(c_vec, c("What are the current treatment patterns (switching/cycling/adherence/persistence) for this disease? (Treatment patterns)",
                                "What is the heterogeneity of the treatment effect among my subpopulation? (Treatment patterns)"))
        } 
        
        if (3 %in% input$t1_AOI){
            c_vec <- c(c_vec, c("What is the comparative effectiveness of intervention on clinical outcomes? (Comparative effectiveness)",
                                "What is the comparative effectiveness of intervention on patient-centered outcomes? (Comparative effectiveness)"))
        } 
        
        if (4 %in% input$t1_AOI){
            c_vec <- c(c_vec, c("What is the current value of this intervention compared to the next best alternative? (Can include HRQoL) (Economic evaluation)",
                                "What is the budget impact? (Economic evaluation)"))
        } 
        
        if (5 %in% input$t1_AOI){
            c_vec <- c(c_vec, c("What is the natural history of the disease without treatment? (Disease burdens)",
                                "What is the clinical burden of the disease? (Disease burdens)",
                                "What is the economic burden of the disease? (Disease burdens)"))
        } 
        
        if (6 %in% input$t1_AOI){
            c_vec <- c(c_vec, c("What current screening strategies are in place to detect early disease or risk factors for disease in large numbers of apparently healthy individuals? (Screening and surveilence)",
                                "What current surveillance strategies are in place to assess the safety/efficacy/prevention of disease recurrence of my intervention? (Screening and surveilence)"))
        } 
        
        
        checkboxGroupInput(label = "What is your specific question(s) about evidence?", # add all questions above to final choice vector and checkbox
                           "dynamic",
                           choices = c_vec)
    })
    
    # Creating list to check if all inputs are valid (not NULL)
    t1_inputs <- reactive({                           
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
    
    input_validation <- function(x){                    # a function to validate inputs for submission
        logic_list <- lapply(x, isTruthy)                 # create a list of logical values
        unlisted_vec <- unlist(logic_list)                # unlist to sum
        sum(unlisted_vec) == length(unlisted_vec)         # logical testing whether there are any "non-True" values
    }
    
    observeEvent(input$submit_1,
                 if (input_validation(t1_inputs())) {
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
                     showTab(session = parentSession, inputId = "tabs", target = "tab2")
                     showTab(session = parentSession, inputId = "tabs", target = "tab3")
                     updateNavbarPage(parentSession, "tabs", "tab2")
                     
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
}

# returns the number of outcomes and the primary outcome input from phase 1
identifyPageGetOutcome <- function(input, output, session) {
    return (
        list(
            outcomes = reactive({input$t1_outcomes}),
            poutcome = reactive({input$t1_poutcome})
        )
    )
}