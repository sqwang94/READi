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
                   pickerInput(
                       ns("t1_studytype"), # the "=" will give the appropriate string filter for each study selected
                       "4. Select the type of studies that you are interested in:",
                       choices = list("Systematic Review"                        = "systematicreviews[Filter]",
                                      "Meta-Analysis"                            = "meta-analysis[Filter]",
                                      "Comparative Study"                        = "comparativestudy[Filter]",
                                      "Observational Study (pro/retrospective cohort/case-control/cross-sectional)" = "observationalstudy[Filter]",
                                      "Pragmatic controlled trial/Large simple trial"         = "pragmaticclinicaltrial[Filter]"),
                       multiple = TRUE,
                       options =  pickerOptions(actionsBox = TRUE))
               ),
               textOutput(ns("st")),
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
    ns <- session$ns
    setBookmarkExclude(c("submit_1"))
    
    output$st <- renderText({input$t1_studytype})
    
    # The following renders questions for Phase1, Question 2 (O)
    output$multoutcomes <- renderUI({
        
        if (is.null(input$t1_outcomes))
            return()
        
        if (input$t1_outcomes == 2){
            textInput(ns("t1_soutcome"),
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
          search <- paste0("https://pubmed.ncbi.nlm.nih.gov/?term=(",pop,"[tiab]+",int,"[tiab]+",comparator,"[tiab]+",outcome1,"[tiab](")
        } else {
          # -- begin string but add second outcome
          search <- paste0("https://pubmed.ncbi.nlm.nih.gov/?term=(",pop,"[tiab]+",int,"[tiab]+",comparator,"[tiab]+",outcome1,"[tiab]+",outcome2,"[tiab](")
        }
          # -- add selected study types and then exclude Randomized Controlled Trials
        for (type in 1:length(study_type)) {
            # -- loop through all the types; if it isn't the last type, add +OR+, else complete with closed brackets and remove RCTs
          if(type != length(study_type)){
            search <- paste0(search, study_type[[type]],"+OR+")
          } else {
            search <- paste0(search, study_type[[type]], "))+NOT+(Randomized+Controlled+Trial)")
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
            
            # ----- Need to add code here to also add all inputs to a data frame/however they should be stored
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