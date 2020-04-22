# UI function for phase 1 identifying evidence page
identifyPageUI <- function(id) {
    ns <- NS(id)
    
    fluidPage(
        column(2, 
               dropdownButton(inputId = "infoDropDown",
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
               wellPanel(selectInput(ns("t1_int"),
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
                               "(T) What is the time frame in years?",
                               min = 1, max = 20, step = 1, value = 5),
                   textInput(ns("t1_setting"),
                             "(S) What is the setting of interest?",
                             placeholder = "Ex.) SNF, Acute Care, etc.")),
               # ---------- Well #3
               wellPanel(strong("3. For which topic(s) are you seeking to evaluate the literature? 
                                   More specific questions will pop up based on your selected topic(s)."),
                         br(),
                         br(),
                         checkboxGroupInput(ns("t1_AOI"),
                                            "What is your area(s) of interest?",
                                            choices = c("Safety and efficacy",
                                                        "Treatment patterns",        
                                                        "Effectiveness",
                                                        "Value",
                                                        "Disease burdens",
                                                        "Screening and surveilence"), 
                                            selected = NULL),
                         textInput(ns("t1_other"),
                                   "If your area of interest is not specified above,
                                      please specify it here:")
               ),
               
               # ------------------ Well #4
               wellPanel(
                   pickerInput(
                       ns("t1_studytype"), # the "=" will give the appropriate string filter for each study selected
                       "4. Select the type of studies that you are interested in:",
                       choices = list("Systematic Review" = '("systematic review"[ptyp])',
                                      "Meta-Analysis" = '("meta-analysis"[ptyp])',
                                      "Comparative Study" = '("comparative study"[ptyp])',
                                      "Observational Study" = '("observational study"[ptyp])',
                                      "Pragmatic controlled trial/Large simple trial" = '("pragmatic clinical trial"[ptyp])',
                                      "Cost-Benefit Anaylsis" = '("Cost-Benefit Analysis"[Mesh] OR "Quality-Adjusted Life Years"[Mesh] OR ("Computer Simulation/economics"[Mesh] OR "Computer Simulation/statistics and numerical data"[Mesh]) OR "Discrete event"[tiab])',
                                      "Budget Impact" = '("Budgets"[Mesh] OR "Cost Control"[Mesh])',
                                      "Discrete Choice Experiment" = '("Patient Preference"[Mesh] or "Choice Behavior"[Mesh])',
                                      "Multi-Criteria Decision Analysis" = '("Decision Support Techniques/economics"[Mesh])'
                                      ),
                       multiple = TRUE,
                       options =  pickerOptions(actionsBox = TRUE))
               ),
               wellPanel(
                   radioButtons(ns("t1_language"),
                                "6. What is your preferred language of the literature?",
                                choices = list("English"                = 1,
                                               "Not limited to English" = 2)
                                
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
               br(),
               
               # ----------------------------------- Creating the popover to indicate that some things will not be going into the search string
               lapply(1:3, function(i){
                 popover_vec <- c("t1_int", "t1_AOI", "t1_setting") # create vector of ids to place this popover
                 bsPopover(id = ns(popover_vec[i]),                 # insert ID into standard popover
                           title = "Notice:",
                           content =  paste("This entry will not go directly into your search. This question is meant only assist your thought process."), 
                           placement = "left", 
                           trigger = "hover")
               })
               
        ))
}

# server function for phase 1 identification of evidence page
identifyPage <- function(input, output, session, parentSession) {
    ns <- session$ns
    setBookmarkExclude(c("submit_1"))

    
    # The following renders questions for Phase1, Question 2 (O)
    output$multoutcomes <- renderUI({
        
        if (is.null(input$t1_outcomes))
            return()
        
        if (input$t1_outcomes == 2){
            textInput(ns("t1_soutcome"),
                      "(O) c. What is your secondary outcome of interest?",
                      placeholder = "Ex.) HBA1c, mortality, etc.")
        }
    })
    
    outputOptions(output, "multoutcomes", suspendWhenHidden=FALSE)
    
    # Creating list to check if all inputs are valid (not NULL)
    t1_inputs <- reactive({ 
        inputs <- list()
        inputs[[ns("t1_pop_interest")]] = input$t1_pop_interest
        inputs[[ns("t1_int_interest")]] = input$t1_int_interest
        inputs[[ns("t1_comparator")]] = input$t1_comparator
        inputs[[ns("t1_poutcome")]] = input$t1_poutcome
        if (!is.null(input$t1_outcomes) && input$t1_outcomes == 2) {
            inputs[[ns("t1_soutcome")]] = input$t1_soutcome
        }
        inputs[[ns("t1_setting")]] = input$t1_setting
        aoi <- input$t1_AOI
        if (is.null(input$t1_AOI)) {
            aoi <- ""
        }
        inputs[[ns("t1_AOI")]] = aoi
        return(inputs)
    })
    
    # --- Need to create search string:
    search_string <- reactive({
        pop <- input$t1_pop_interest
        int <- input$t1_int_interest
        study_type <- input$t1_studytype
        comparator <- input$t1_comparator
        outcome1 <- input$t1_poutcome
        outcome2 <- input$t1_soutcome
        time_frame <- paste0("&filter=years.", year(Sys.Date())-input$t1_timeframe,"-",year(Sys.Date()))
        if(is.null(outcome2)){
            # -- begin string 
            search <- paste0("https://pubmed.ncbi.nlm.nih.gov/?term=((",pop,"[tiab])+AND+(",int,"[tiab])+AND+(",comparator,"[tiab])+AND+(",outcome1,"[tiab]))")
        } else {
            # -- begin string but add second outcome
            search <- paste0("https://pubmed.ncbi.nlm.nih.gov/?term=((",pop,"[tiab])+AND+(",int,"[tiab])+AND+(",comparator,"[tiab])+AND+(",outcome1,"[tiab])+AND+(",outcome2,"[tiab]))")
        }
        search <- paste0(search, "+NOT+(randomized+controlled+trial[ptyp])+AND+(")
        # -- add selected study types and then exclude Randomized Controlled Trials
        for (type in 1:length(study_type)) {
            if (type != length(study_type)) {
                search <- paste0(search, study_type[[type]], "+OR+")
            } else {
                search <- paste0(search, study_type[[type]], ")")
            }
        }
        search <- paste0(search, time_frame)
        search
    })
    
    # event listener for phase 1 submit button, including input validation
    observeEvent(input$submit_1, {
        inputs <- t1_inputs()
        toggleErrorInputHandler(inputs)
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
            shinyjs::show(selector = "#tabs li:nth-child(2) i")
            updateNavbarPage(parentSession, "tabs", "tab2")
            parentSession$userData$phase(2)
            js$toWindowTop()
        } else {
            sendSweetAlert(         # add error message if user needs more information
                session = session,
                title = "Oops!",
                text = "It looks like you may not have answered all the questions!",
                type = "error",
                btn_labels = c("Go back")
            )
            hideTab(session = parentSession, inputId = "tabs", target = "tab2")
            hideTab(session = parentSession, inputId = "tabs", target = "tab3")
            shinyjs::hide(selector = "#tabs li:nth-child(2) i")
        }
    })
    return(input)
}