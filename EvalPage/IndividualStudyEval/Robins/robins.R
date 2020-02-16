# UI function for robins tool
robinsUI <- function(id) {
    ns <- NS(id)
    responses <- c("Yes", "Probably Yes", "Probably No" , "No", "No Information")
    wellPanel(strong("1B. Please rate the risk of bias of this study using the ROBINS-I assessment tool"),
              br(),
              tagList(a("Robins-I", href="https://sites.google.com/site/riskofbiastool/welcome/home", target = "_blank"),
                      wellPanel(strong("1. Bias due to Confounding:"),
                                radioButtons(label = "1.1 Is there potential for confounding of the effect of intervention in this study?",
                                             inputId = ns("bias_input1_1"),
                                             choices = responses,
                                             inline = TRUE,
                                             selected = "No Information"),
                                uiOutput(ns("rob_output1_1"))), # This first question is responsive - there will be no need to answer parts 1.2-1.6 if No or probably no
                      wellPanel(strong("2. Bias in Selection of Participants into the Study"))
              )
    )
}


# server function for robins tool 
robinsServer <- function(input, output, session) {
    ns <- session$ns
    responses <- c("Yes", "Probably Yes", "Probably No" , "No", "No Information")
    
    # ---- for quesstion response to 1.1
    output$rob_output1_1 <- renderUI({
        if (input$bias_input1_1 %in% c("No", "Probably No")) {
            "This study is considered to have low risk of bias due to confounding - please move on to question 2."
        } else {
            list(radioButtons(label = "1.2 Was the analysis based on splitting participants' follow up time according to intervention received?",
                              inputId = ns("bias_input1_2"),
                              choices = responses,
                              inline = TRUE,
                              selected = "No Information"),
                 uiOutput(ns("rob_output1_2")))
        }
            
    })
    
    # ---- for quesstion response to 1.2
    output$rob_output1_2 <- renderUI({
        if (input$bias_input1_2 %in% c("Yes", "Probably Yes")) {
            list(radioButtons(label = "1.3 Were the intervention discontinuations or switches likely to be related to factors that  are prognostic for the outcome?",
                              inputId = ns("bias_input1_3"),
                              inline = TRUE,
                              choices = responses,
                              selected = "No Information"),
                 uiOutput(ns("rob_output1_3")))
        } else {
            list(radioButtons(label = "1.4 Did the authors use an appropriate analysis method that controlled for all the important confounding domains?",
                              inputId = ns("bias_input1_4"),
                              inline = TRUE,
                              choices = responses,
                              selected = "No Information"),
                 radioButtons(label = "1.5 Were confounding domains that were controlled for measured reliably and validly by the variables available in this study?",
                              inputId = ns("bias_input1_5"),
                              inline = TRUE,
                              choices = responses,
                              selected = "No Information"),
                 radioButtons(label = "1.6 Did the authors control for any post-intervention variables that could have been affected by the intervention?",
                              inputId = ns("bias_input1_6"),
                              inline = TRUE,
                              choices = responses,
                              selected = "No Information"),
                 uiOutput(ns("rob_output1_4")))
        }
    })
    
    # ---- for quesstion response to 1.3
    output$rob_output1_3 <- renderUI({
        if (input$bias_input1_3 %in% c("No", "Probably No")) {
            list(radioButtons(label = "1.4 Did the authors use an appropriate analysis method that controlled for all the important confounding domains?",
                              inputId = ns("bias_input1_4"),
                              inline = TRUE,
                              choices = responses,
                              selected = "No Information"),
                 radioButtons(label = "1.5 Were confounding domains that were controlled for measured reliably and validly by the variables available in this study?",
                              inputId = ns("bias_input1_5"),
                              inline = TRUE,
                              choices = responses,
                              selected = "No Information"),
                 radioButtons(label = "1.6 Did the authors control for any post-intervention variables that could have been affected by the intervention?",
                              inputId = ns("bias_input1_6"),
                              inline = TRUE,
                              choices = responses,
                              selected = "No Information"))
        } else {
            list(radioButtons(label = "1.7 Did the authors use an appropriate analysis method that adjusted for all the important confounding domains and for time-varying confounding?",
                              inputId = ns("bias_input1_7"),
                              inline = TRUE,
                              choices = responses,
                              selected = "No Information"),
                 uiOutput(ns("rob_output1_7")))
        }
    })
}