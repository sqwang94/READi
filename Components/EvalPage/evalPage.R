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
evalPage <- function(input, output, session) {
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
    
    # ------ Creating reactionary wellPanel based on how many studies selected ------- # 
    output$study_react <- renderUI({    # goal: create panels of questions in response to "How many studies did you find?"
        num_studies <- input$t2_n_studies  # defining number of studies

        if(input$t2_ev_available == "No"){
            return(" If no relevant literature can be found, please click `Submit` below and proceed to Phase 3.")
        } else if(num_studies == 0){
            return("Please select the number of studies identified above!")
        } else {
            our_ui <- studyNavUI(ns("study_nav"), num_studies)
            callModule(studyNav, "study_nav", num_studies)
        }
        our_ui
    })
    callModule(studyNavGlobal, "study_nav")
}