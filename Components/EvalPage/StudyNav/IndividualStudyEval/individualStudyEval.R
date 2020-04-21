source("Components/EvalPage/StudyNav/IndividualStudyEval/Robins/robins.R")
source("Components/EvalPage/StudyNav/IndividualStudyEval/AMSTAR2/amstar.R")
source("Components/EvalPage/StudyNav/IndividualStudyEval/GRACE/grace.R")
source("Components/EvalPage/StudyNav/IndividualStudyEval/QHES/qhes.R")


# UI function for individual study eval
individualStudyEvalUI <- function(id, studyId) {
  ns <- NS(id)
  wellPanel(strong(paste("Answer the following questions about study #", studyId)),
            wellPanel("Basic Information",
                      br(),
                      textInput(ns("author"),
                                paste("The first author of study #", studyId," is (last name only)")),
                      sliderInput(ns("pub_year"), 
                                  "The year of publication is: ",
                                  min = 1950, max = year(Sys.Date()), value = 2015, sep = ""),
                      
                      uiOutput(ns("poutcome")),
                      uiOutput(ns("soutcome"))
            ),
            br(),
            wellPanel(
              radioButtons(ns("study_design"),
                           strong("Question 1A: Please select the study design"),
                           choices = c("Pragmatic controlled trial/Large simple trial",
                                       "Quasi experimental",
                                       "Prospective cohort study",
                                       "Retrospective cohort study",
                                       "Case-control study",
                                       "Systematic review/Meta-analysis/Network Meta-analysis",
                                       "Cost-Benefit Analysis",
                                       "Budget Impact Model",
                                       "Discrete Choice Experiment",
                                       "Multi-criteria Decision Analysis",
                                       "Comparative Study",
                                       "None of the above"),
                           selected = "None of the above")),
            br(),
            uiOutput(ns("radio_random"))
            
  )
}

# server function for individual study eval
individualStudyEval <- function(input, output, session, phase1_inputs) {
  ns <- session$ns
  
  output$poutcome <- renderUI({
    radioButtons(ns("outcome1"),
                 paste0("Did this study address your primary outcome of interest (", phase1_inputs$t1_poutcome, ") ?"),
                 choices = c("Yes", "No"))
  })
  
  output$soutcome <- renderUI({
    if (!is.null(phase1_inputs$t1_outcomes) && phase1_inputs$t1_outcomes == 2) {
      return(radioButtons(ns("outcome2"),
                          paste0("Did this study address your secondary outcome of interest (", phase1_inputs$t1_soutcome, ") ?"),
                          choices = c("Yes", "No")))
    }
    return()
  })
  
  # create well panels based on the study type
  output$radio_random <- renderUI({
    
    # ---- Creating a standard bias question to go at the end of all questions, regardless of study type
    standard_bias_question <- radioButtons(inputId = ns("standard_bias"), label = strong("1C. Please rate the overall risk of bias for the study evaluated above"),
                                           choices = c("Low Risk of Bias", "Moderate Risk of Bias", "High Risk of Bias", "Unclear Reporting"),
                                           selected = character(0))
    
    # Pragmatci Controlled Trial
    if (input$study_design == "Pragmatic controlled trial/Large simple trial"){
      list(wellPanel(strong("1B. Please rate the quality of the study using the Revised Cochrane risk of bias tool for randomized trials (RoB 2)"),
                     br(),
                     tagList(a("RoB 2", href="https://www.riskofbias.info/welcome/rob-2-0-tool/current-version-of-rob-2", target = "_blank")),
                     br()),
           wellPanel(standard_bias_question))

      
      # BIM
    } else if (input$study_design == "Budget Impact Model"){
      list(wellPanel(strong("1B. Please use the following ISPOR Good Research Practices to inform your review"),
                     br(),
                     tagList(a("ISPOR Good Research Practices for BIM II", href = "https://www.ispor.org/heor-resources/good-practices-for-outcomes-research/article/principles-of-good-practice-for-budget-impact-analysis-ii", 
                               target = "_blank"))),
           wellPanel(standard_bias_question))
      
      # DCE
    } else if (input$study_design == "Discrete Choice Experiment"){
      list(wellPanel(strong("1B. Please use the following ISPOR Good Research Practices to inform your review"),
                     br(),
                     tagList(a("Constructing Experimental Designs for Discrete-Choice Experiments ", href = "https://www.ispor.org/heor-resources/good-practices-for-outcomes-research/article/constructing-experimental-designs-for-discrete-choice-experiments", 
                               target = "_blank"))),
           wellPanel(standard_bias_question))
      
      # MCDA
    } else if (input$study_design == "Multi-criteria Decision Analysis"){
      list(wellPanel(strong("1B. Please use the following ISPOR Good Research Practices to inform your review"),
                     br(),
                     tagList(a("Multiple Criteria Decision Analysis for Health Care Decision Making - Emerging Good Practices: Report 2 ", href = "https://www.ispor.org/heor-resources/good-practices-for-outcomes-research/article/multiple-criteria-decision-analysis-for-health-care-decision-making---emerging-good-practices", 
                               target = "_blank"))),
           wellPanel(standard_bias_question))
      
      # Observational
    } else if (input$study_design %in% c("Prospective cohort study", "Retrospective cohort study", "Case-control study", "Quasi experimental")) {
      list(robinsUI(ns("robins")),
           wellPanel(standard_bias_question))
      
      # CBA
    } else if (input$study_design == c("Cost-Benefit Analysis")) {
      list(qhesUI(ns("qhes")),
           wellPanel(standard_bias_question))
      
      # Systematic Reviews
    } else if (input$study_design == "Systematic review/Meta-analysis/Network Meta-analysis") {
      list(amstar2UI(ns("amstar")),
           wellPanel(standard_bias_question))
      
      # Compartive Study
    } else if (input$study_design == "Comparative Study"){
      list(graceUI(ns("grace")),
           wellPanel(standard_bias_question))
      
      
    } else if (input$study_design == "None of the above"){
      list(wellPanel(strong("We're sorry your study design is not an option below; please do your best to assess the risk bias independently and select a value below!",
                            br(),
                            br(),
                            standard_bias_question)))
    } else {
      return()
    }
    
    
  })
  
  
  
  callModule(amstar2, "amstar")
  callModule(robinsServer, "robins")
  callModule(grace, "grace")
  return(input)
  
  
}


# returns all inputs in the individual study as a list for input validation. Adds Error class to invalid input.
individualStudyInputValidation <- function(input, output, session) {
  ns <- session$ns
  reactive({
    inputs <- list()
    inputs[[ns("author")]] <- input$author
    if (input$study_design %in% c("Pragmatic controlled trial/Large simple trial", "Quasi experimental")) {
      for (i in seq(8)) {
        selected <- input[[paste0("input", i)]]
        if (is.null(input[[paste0("input", i)]])) {
          selected <- ""
        }
        inputs[[ns(paste0("input", i))]] <- selected
      }
      eval <- input$input
      if (is.null(input$input)) {
        eval <- ""
      }
      inputs[[ns("input")]] <- eval
    }
    bias <- input$standard_bias
    if (input$study_design != "None of the above" && is.null(input$standard_bias)) {
      bias <- ""
    }
    inputs[[ns("standard_bias")]] <- bias
    toggleErrorInputHandler(inputs)
    return(inputs)
  })
}







