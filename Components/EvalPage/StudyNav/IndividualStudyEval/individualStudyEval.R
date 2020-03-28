source("Components/EvalPage/StudyNav/IndividualStudyEval/Robins/robins.R")

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
                          paste0("Did this study address your secondary outcome of interest (", phase1_inputs$t1_secondary_outcome, ") ?"),
                          choices = c("Yes", "No")))
    }
    return()
  })
  
  # create well panels based on the study type
  output$radio_random <- renderUI({
    
    # ---- Creating a standard bias question to go at the end of all questions, regardless of 
    standard_bias_question <- radioButtons(inputId = ns("standard_bias"), label = strong("1X. THIS IS WHERE THE STANDARD RATING OF BIAS WILL GO, 
                                                                                           please give a general rating of the risk of bias for this study. 
                                                                                           (Low risk: Low risk of bias for all key domains; Unclear risk: Unclear risk of bias for one or more key domains;
                                                                                           High risk: High risk of bias for one or more key domains)"),
                                           choices = c("Low Risk", "Unclear Risk", "High Risk"),
                                           selected = character(0))
    
    if (input$study_design == "Pragmatic controlled trial/Large simple trial"){
      choice_prag <- c("Low Risk", "Unclear Risk", "High Risk")
      labels_prag <- c("Random sequence generation (selection bias)",
                       "Allocation concealment (selection bias)",
                       "Blinding of participants and personnel (performance bias)",
                       "Blinding of outcome assessment (detection bias) (patient-reported outcomes)",
                       "Blinding of outcome assessment (detection bias) (Mortality)",
                       "Incomplete outcome data addressed (attrition bias) (Short-term outcomes (2-6 weeks))",
                       "Incomplete outcome data addressed (attrition bias) (Longer-term outcomes (>6 weeks))",
                       "Selective reporting (reporting bias)")
      list(wellPanel(strong("1B. Please rate the quality of the study using the Cochrane risk of bias tool below to assess the risk of bias"),
                     br(),
                     tagList(a("Cochrane", href="http://handbook-5-1.cochrane.org/", target = "_blank")),
                     br(),
                     br(),
                     lapply(seq_len(8),
                            function(i){radioButtons(inputId = ns(paste0("input", i)), 
                                                     label = labels_prag[i], 
                                                     choices = choice_prag,
                                                     inline = TRUE,
                                                     selected = character(0))}),
                     br(),
                     br(),
                     radioButtons(inputId = ns("input"), label = strong("1C. Overall: Based on the rating for each domain, 
                                                                          please give a general rating of the risk of bias for this study. 
                                                                          (Low risk: Low risk of bias for all key domains; Unclear risk: Unclear risk of bias for one or more key domains;
                                                                          High risk: High risk of bias for one or more key domains)"),
                                  choices = choice_prag,
                                  selected = character(0))),
           wellPanel(standard_bias_question))
    } else if (input$study_design == "Quasi experimental"){
      choice_quasi <- c("Yes", "No", "Other")
      labels_quasi <- c("Was the study question or objective clearly stated?",
                        "Were eligibility/selection criteria for the study population prespecified and clearly described?",
                        "Were the participants in the study representative of those who would be eligible for the test/service/intervention in the general or clinical population of interest?",
                        "Were all eligible participants that met the prespecified entry criteria enrolled?",
                        "Was the sample size sufficiently large to provide confidence in the findings?",
                        "Was the test/service/intervention clearly described and delivered consistently across the study population?",
                        "Were the outcome measures prespecified, clearly defined, valid, reliable, and assessed consistently across all study participants?",
                        "Were the people assessing the outcomes blinded to the participants' exposures/interventions?",
                        "Was the loss to follow-up after baseline 20% or less? Were those lost to follow-up accounted for in the analysis?",
                        "Did the statistical methods examine changes in outcome measures from before to after the intervention? Were statistical tests done that provided p values for the pre-to-post changes?",
                        "Were outcome measures of interest taken multiple times before the intervention and multiple times after the intervention (i.e., did they use an interrupted time-series design)?",
                        "If the intervention was conducted at a group level (e.g., a whole hospital, a community, etc.) did the statistical analysis take into account the use of individual-level data to determine effects at the group level?")
      list(wellPanel(strong("1B. Please rate the quality of this study using the quality assessment tool from NHLBI"),
                     br(),
                     tagList(a("NHLBI Tool", href="https://www.nhlbi.nih.gov/health-topics/study-quality-assessment-tools", target = "_blank")),
                     br(),
                     br(),
                     lapply(seq_len(8),
                            function(i){radioButtons(inputId = ns(paste0("input", i)),
                                                     label = labels_quasi[i],
                                                     choices = choice_quasi,
                                                     inline = TRUE,
                                                     selected = character(0))}),
                     br(),
                     br(),
                     radioButtons(inputId = ns("input"),
                                  label = strong("1C. Overall: Based on the rating for each question, please give a general rating of the quality of this study. (Good; Fair; Poor)"),
                                  choices = choice_quasi,
                                  selected = character(0))),
           wellPanel(standard_bias_question)
      )
    } else if (input$study_design %in% c("Prospective cohort study", "Retrospective cohort study", "Case-control study")) {
      list(robinsUI(ns("robins")),
           wellPanel(standard_bias_question))
    } else if (input$study_design == "Systematic review/Meta-analysis/Network Meta-analysis") {
      list(wellPanel(strong("1B. Please rate the quality of the study using the AMSTAR 2 Checklist."),
                     br(),
                     tagList(a("Amstar", href="https://amstar.ca/", target = "_blank"))),
           wellPanel(standard_bias_question))
    } else {
      return()
    }
  })
  callModule(robinsServer, "robins")
  return(input)
}


# returns all inputs in the individual study as a list for input validation. Adds Error class to invalid input.
individualStudyInputValidation <- function(input, output, session) {
  ns <- session$ns
  reactive({
    inputs <- list()
    inputs[[ns("author")]] = input$author
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







