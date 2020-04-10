source("Components/RecPage/considerations.R")
# --- Recommendation Module

recPageUI <- function(id){
  ns <- NS(id)
  fluidPage(
    column(8, offset = 2,
           wellPanel(strong("Reviewing the Total Body of Evidence"),
                     br(),
                     br(),
                     wellPanel(
                     p("Based on your results after the literature review, please consider whether the evidence is
                     sufficient and applicable for your own population and question(s) of interest."),
             radioButtons(ns("t4_ev_available"),
                          "1. Was there any literature/evidence available?",
                          choices = c("Yes", "No"),
                          selected = "No"),
             uiOutput(ns("lit_available")))
           )),
    uiOutput(ns("rec")),
    br(),
    br(),
    column(8, offset = 2,
    actionBttn(
      inputId = ns("submit_rec"),
      label = "Submit Form",
      color = "royal",
      style = "minimal",
      icon = NULL,
      block = TRUE
    ),
    br(),
    br(),
    br(),
    br())
  )
}


recPage <- function(input, output, session){
  ns <- session$ns
  
  output$lit_available <- renderUI({
    if(input$t4_ev_available == "Yes"){
      list(radioButtons(ns("applicable"),
                   "2. Is current evidence applicable to answer your research question(s)? (generalizable to your population)",
                   choices = c("Yes", "No"),
                   selected = "No"),
           uiOutput(ns("lit_applicable")))
    } else {
      return()
    }
  })
  
  output$lit_applicable <- renderUI({
    if(input$applicable == "Yes"){
      list(radioButtons(ns("sufficient"),
                   "3. Was current evidence sufficient to answer your research question(s)? Please consider your rating from Phase 3 (especially the body of evidence).",
                   choices = c("Yes","No"),
                   selected = "No"),
           uiOutput(ns("lit_sufficient")))
    } else {
      return()
    }
  })
  
  output$rec <- renderUI({
    if(is.null(input$sufficient)){
      return()
    } else if(input$sufficient == "Yes"){
      list(
        column(8, offset = 2,
               wellPanel(strong("Making an Evidence-Based Recommendation"),
                         br(),
                         br(),
                         wellPanel(recpageDIV),
                         wellPanel(
               radioButtons(ns("recommendation"),
                            "4. Please select the recommendations from the list below; if your planned recommendation is not listed,
                            please select 'Other' and proceed to the next question.",
                            choices = c("No coverage",
                                        "Performance-based risk-sharing arrangements (PBRSA)",
                                        "Coverage with guidelines",
                                        "Coverage with prior authorization criteria (e.g. step therapy)",
                                        "Coverage with benefit or product contractual rules (in the actual beneficial context, e.g. we are not going to pay for weight loss)",
                                        "Other")),
               uiOutput(ns("rec_output"))))))
    }
  })
  
  output$rec_output <- renderUI({
    if(input$recommendation == "Other"){
      textInput(ns("other"), 
                label = "5. What other recommendation are you considering?")
    } else if (input$recommendation == "Performance-based risk-sharing arrangements (PBRSA)"){
      pickerInput(
        ns("t1_studytype"), # the "=" will give the appropriate string filter for each study selected
        "5. Select the type of studies that you are interested in:",
        choices = list("CED (coverage with evidence development)",
                       "CTC (conditional treatment continuation)",
                       "FU (financial or utilization)",
                       "PLR (performance-linked reimbursement)"),
        multiple = TRUE,
        options =  pickerOptions(actionsBox = TRUE))
    } else {
      return()
    }
  })
  
  
}
