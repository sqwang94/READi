source("Components/EvalPage/StudyNav/studyNav.R")

# UI function for phase 2 evaluation of evidence page
evalPageUI <- function(id) {
    ns <- NS(id)
    fluidPage(
        column(8, offset = 2,
               wellPanel(
                   radioButtons(ns("t2_ev_available"),
                                "Is there literature/evidence available?",
                                choices = c("Yes", "No")),
                   uiOutput(ns("study_identified"))
               )),
        column(8, offset = 2,
               uiOutput(ns("study_react")),
               br(),
               br(),
               actionBttn(
                   inputId = ns("submit_2"),
                   label = "Submit Form",
                   color = "royal",
                   style = "minimal",
                   icon = NULL,
                   block = TRUE
               ),
               br(),
               br()
        )
    )
}

# server function for phase 2 evaluation of evidence page
evalPage <- function(input, output, session, parentSession, phase1_inputs) {
    ns <- session$ns
    
    output$study_identified <- renderUI({ # Rendering UI based on whether or not studies are available
        if(input$t2_ev_available == "No"){
            return()
        } else {
            sliderInput(ns("t2_n_studies"),
                        "How many studies have you found?",
                        min = 0, max = 50, step = 1, value = 0)
        }
    })
    
    bias_values <- reactiveVal(reactiveValues())
    # ------ Creating reactionary wellPanel based on how many studies selected ------- # 
    output$study_react <- renderUI({    # goal: create panels of questions in response to "How many studies did you find?"
        bias_values(NULL)
        if(input$t2_ev_available == "No"){
            return(" If no relevant literature can be found, please click `Submit` below and proceed to Phase 3.")
        }
        if (!is.null(input$t2_n_studies)) {
            num_studies <- input$t2_n_studies  # defining number of studies
            if(num_studies == 0){
                return("Please select the number of studies identified above!")
            } else {
                our_ui <- studyNavUI(ns("study_nav"), num_studies)
                bias_values(callModule(studyNav, "study_nav", num_studies, phase1_inputs))
            }
            our_ui
        }
    })
    callModule(studyNavGlobal, "study_nav", phase1_inputs, bias_values)

    # input validation for all studies in studies navigation
    observeEvent(input$submit_2, {
        inputs <- callModule(studyNavValidation, "study_nav")
        if (input$t2_ev_available == "Yes" && (input$t2_n_studies == 0 || length(inputs()) == 0)) {
            return()    # does not allow submission if study identified but no study is filled 
        }
        if (input$t2_ev_available == "No" || input_validation(inputs())) {
            sendSweetAlert(        # if all inputs are valid, submission successful
                session = session,
                title = "Submitted!", 
                text = "Please move on to next phase!",
                type = "success",
                btn_labels = c("Great")
            )
            shinyjs::show(selector = "#tabs li:nth-child(3) i")
            showTab(session = parentSession, inputId = "tabs", target = "tab3")
            updateNavbarPage(parentSession, "tabs", "tab3")
        } else {
            sendSweetAlert(     # add error message if user needs more information
                session = session,
                title = "Oops!",
                text = "It looks like you may not have answered all the questions!",
                type = "error",
                btn_labels = c("Go back")
            )
            shinyjs::hide(selector = "#tabs li:nth-child(3) i")
        }
    })
    return(bias_values)
}




