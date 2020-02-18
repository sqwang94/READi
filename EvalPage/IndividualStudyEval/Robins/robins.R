# UI function for robins tool
robinsUI <- function(id) {
    ns <- NS(id)
    responses <- c("Yes", "Probably Yes", "Probably No" , "No", "No Information")
    wellPanel(strong("1B. Please rate the risk of bias of this study using the ROBINS-I assessment tool"),
              br(),
              tagList(a("Robins-I", href="https://sites.google.com/site/riskofbiastool/welcome/home", target = "_blank"),
                      
                      # ------ Question 1 from Robins
                      wellPanel(strong("1. Bias due to Confounding:"),
                                br(),
                                radioButtons(label = "1.1 Is there potential for confounding of the effect of intervention in this study?",
                                             inputId = ns("bias_input1_1"),
                                             choices = responses,
                                             inline = TRUE,
                                             selected = "No Information"),
                                uiOutput(ns("rob_output1_1"))), # This first question is responsive - there will be no need to answer parts 1.2-1.6 if No or probably no
                      
                      # ------ Question 2 from Robins
                      wellPanel(strong("2. Bias in Selection of Participants into the Study"),
                                br(),
                                radioButtons(label = "2.1. Was selection of participants into the study (or into the analysis) based on participant characteristics observed after the start of intervention?",
                                             inputId = ns("bias_input2_1"),
                                             choices = responses,
                                             inline = TRUE,
                                             selected = "No Information"),
                                uiOutput(ns("rob_output2_1")),
                                radioButtons(label = "2.4 Do start of follow-up and start of intervention coincide for most participants?",
                                             inputId = ns("bias_input2_4"),
                                             inline = TRUE,
                                             choices = responses,
                                             selected = "No Information"),
                                uiOutput(ns("rob_output2_5"))),
                      
                      # ------ Question 3 from Robins
                      wellPanel(strong("3. Bias in Classification of Interventions"),
                                br(),
                                radioButtons(label = "3.1 Were intervention groups clearly defined?",
                                             inputId = ns("bias_input3_1"),
                                             choices = responses,
                                             inline = TRUE,
                                             selected = "No Information"),
                                radioButtons(label = "3.2 Was information used to define intervention groups recorded at the start of the intervention?",
                                             inputId = ns("bias_input3_2"),
                                             choices = responses,
                                             inline = TRUE,
                                             selected = "No Information"),
                                radioButtons(label = "3.3 Could classification of intervention status have been affected by knowledge of the outcome or risk of the outcome?",
                                             inputId = ns("bias_input3_3"),
                                             choices = responses,
                                             inline = TRUE,
                                             selected = "No Information")),
                      
                      # ------ Question 4 from Robins
                      wellPanel(strong("4. Bias in Classification of Interventions"),
                                br(),
                                strong("If your aim for this study is to assess the effect of assignment to intervention, answer questions 4.1 and 4.2"),
                                br(),
                                radioButtons(label = "4.1. Were there deviations from the intended intervention beyond what would be expected in usual practice?",
                                             inputId = ns("bias_input4_1"),
                                             choices = responses,
                                             inline = TRUE,
                                             selected = "No Information"),
                                uiOutput(ns("rob_output4_1")),
                                br(),
                                strong("If your aim for this study is to assess the effect of starting and adhering to intervention, answer questions 4.3 to 4.6"),
                                radioButtons(label = "4.3. Were important co-interventions balanced across intervention groups?",
                                             inputId = ns("bias_input4_3"),
                                             choices = responses,
                                             inline = TRUE,
                                             selected = "No Information"),
                                radioButtons(label = "4.4. Was the intervention implemented successfully for most participants?",
                                             inputId = ns("bias_input4_4"),
                                             choices = responses,
                                             inline = TRUE,
                                             selected = "No Information"),
                                radioButtons(label = "4.5. Did study participants adhere to the assigned intervention regimen?",
                                             inputId = ns("bias_input4_5"),
                                             choices = responses,
                                             inline = TRUE,
                                             selected = "No Information"),
                                uiOutput(ns("rob_output4_6"))),
                      
                      # ------ Question 6 from Robins
                      wellPanel(strong("6. Bias in Measurement of Outcome"),
                                br(),
                                radioButtons(label = "6.1 Could the outcome measure have been influenced by knowledge of the intervention received?",
                                             inputId = ns("bias_input6_1"),
                                             choices = responses,
                                             inline = TRUE,
                                             selected = "No Information"),
                                radioButtons(label = "6.2. Were outcome assessors aware of the intervention received by study participants?",
                                             inputId = ns("bias_input6_2"),
                                             choices = responses,
                                             inline = TRUE,
                                             selected = "No Information"),
                                radioButtons(label = "6.3 Were the methods of outcome assessment comparable across intervention groups?",
                                             inputId = ns("bias_input6_3"),
                                             choices = responses,
                                             inline = TRUE,
                                             selected = "No Information"),
                                radioButtons(label = "6.4 Were any systematic errors in measurement of the outcome related to intervention received?",
                                             inputId = ns("bias_input6_4"),
                                             choices = responses,
                                             inline = TRUE,
                                             selected = "No Information")
                                ),
                      
                      # ------ Question 7 from Robins
                      wellPanel(strong("7. Bias in Selection of the Reported Result"),
                                br(),
                                radioButtons(label = "7.1 Is the reported effect estimate likely to be selected, on the basis of the results, from multiple outcome measurements within the outcome domain?",
                                             inputId = ns("bias_input7_1"),
                                             choices = responses,
                                             inline = TRUE,
                                             selected = "No Information"),
                                radioButtons(label = "7.2 Is the reported effect estimate likely to be selected, on the basis of the results, from multiple analyses of the intervention-outcome relationship?",
                                             inputId = ns("bias_input7_2"),
                                             choices = responses,
                                             inline = TRUE,
                                             selected = "No Information"),
                                radioButtons(label = "7.3 Is the reported effect estimate likely to be selected, on the basis of the results, from different subgroups?",
                                             inputId = ns("bias_input7_3"),
                                             choices = responses,
                                             inline = TRUE,
                                             selected = "No Information")
                      )
              )
    )
}


# server function for robins tool 
robinsServer <- function(input, output, session) {
    ns <- session$ns
    responses <- c("Yes", "Probably Yes", "Probably No" , "No", "No Information")
    
    # ------------------------------------------------------------------------------------------------------------------- #
    # -------------------------------------  For all of Part 1; Bias due to Confounding --------------------------------- #
    # ------------------------------------------------------------------------------------------------------------------- #
    
    # ---- for question response to 1.1
    output$rob_output1_1 <- renderUI({
        if (input$bias_input1_1 %in% c("No", "Probably No", "No Information")) {
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
        if (input$bias_input1_2 == "No Information"){
          return()
        } else if (input$bias_input1_2 %in% c("Yes", "Probably Yes")) {
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
                 uiOutput(ns("rob_output1_4")))
        }
    })
    
    # ---- for question response to 1.3
    output$rob_output1_3 <- renderUI({
        if(input$bias_input1_3 == "No Information"){
          return()
        } else if (input$bias_input1_3 %in% c("No", "Probably No")) {
            list(radioButtons(label = "1.4 Did the authors use an appropriate analysis method that controlled for all the important confounding domains?",
                              inputId = ns("bias_input1_4"),
                              inline = TRUE,
                              choices = responses,
                              selected = "No Information"),
                 uiOutput(ns("rob_output1_4")))
        } else {
            list(radioButtons(label = "1.7 Did the authors use an appropriate analysis method that adjusted for all the important confounding domains and for time-varying confounding?",
                              inputId = ns("bias_input1_7"),
                              inline = TRUE,
                              choices = responses,
                              selected = "No Information"),
                 uiOutput(ns("rob_output1_7")))
        }
    })
    
    # ---- for question response to 1.4
    output$rob_output1_4 <- renderUI({
      if(input$bias_input1_4 == "No Information"){
        return()
      } else if(input$bias_input1_4 %in% c("Yes", "Probably Yes")){
        list(radioButtons(label = "1.5 Were confounding domains that were controlled for measured reliably and validly by the variables available in this study?",
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
        radioButtons(label = "1.6 Did the authors control for any post-intervention variables that could have been affected by the intervention?",
                     inputId = ns("bias_input1_6"),
                     inline = TRUE,
                     choices = responses,
                     selected = "No Information")
      }
    })
    
    
    output$rob_output1_7 <- renderUI({
      if(input$bias_input1_7 %in% c("No", "Probably No", "No Information")){
        return()
      } else if(input$bias_input1_7 %in% c("Yes", "Probably Yes")){
        radioButtons(label = "1.8 Were confounding domains that were adjusted for measured validly and reliably by the variables available in this study?",
                     inputId = ns("bias_input1_8"),
                     inline = TRUE,
                     choices = responses,
                     selected = "No Information")
      }
    })
    
    
    # ------------------------------------------------------------------------------------------------------------------- #
    # -------------------------------------  For all of Part 2; Bias due to Confounding --------------------------------- #
    # ------------------------------------------------------------------------------------------------------------------- #
    
    output$rob_output2_1 <- renderUI({
      if(input$bias_input2_1 %in% c("No Information", "No", "Probably No")){
        return()
      } else {
        list(radioButtons(label = "2.2 Were the post-intervention variables that influenced selection likely to be associated with intervention?",
                     inputId = ns("bias_input2_2"),
                     inline = TRUE,
                     choices = responses,
                     selected = "No Information"),
             uiOutput(ns("rob_output2_2")))
      }
    })
     # ------- Question 2.2 not functional for some reason? 
    output$rob_output2_2 <- renderUI({
      if(input$bias_input2_2 %in% c("Yes", "Probably Yes")){
        radioButtons(label = "2.3 Were the post-intervention variables that influenced selection likely to be influenced by the outcome or a cause of the outcome?",
                     inputId = ns("bias_input2_3"),
                     inline = TRUE,
                     choices = responses,
                     selected = "No Information")
      } else {
        return()
      }
      
    })
    
    output$rob_output2_5 <- renderUI({
      if(input$bias_input2_4 %in% c("No", "Probably No")){
        radioButtons(label = "2.5 Were adjustment techniques used that are likely to correct for the presence of selection biases?",
                     inputId = ns("bias_input2_5"),
                     inline = TRUE,
                     choices = responses,
                     selected = "No Information")
      } else if(input$bias_input2_2 & input$bias_input2_3 %in% c("Yes", "Probably Yes")){
        radioButtons(label = "2.5 Were adjustment techniques used that are likely to correct for the presence of selection biases?",
                     inputId = ns("bias_input2_5"),
                     inline = TRUE,
                     choices = responses,
                     selected = "No Information")
      } else {
        return()
      }
    })
    

    # ------------------------------------------------------------------------------------------------------------------- #
    # -------------------------------------  For all of Part 4; Bias due to Confounding --------------------------------- #
    # ------------------------------------------------------------------------------------------------------------------- #

    output$rob_output4_1 <- renderUI({
      if(input$bias_input4_1 %in% c("No", "Probably No", "No Information")){
        return()
      } else {
        radioButtons(label = "4.2 Were these deviations from intended intervention unbalanced between groups and likely to have affected the outcome??",
                     inputId = ns("bias_input4_2"),
                     inline = TRUE,
                     choices = responses,
                     selected = "No Information")
      }
    })
    
    output$rob_output4_6 <- renderUI({
      input_vec <- c(input$bias_input4_3, input$bias_input4_4, input$bias_input4_5)
      if(isTruthy(c("No", "Probably No") %in% input_vec)){
        radioButtons(label = "4.6 Was an appropriate analysis used to estimate the effect of starting and adhering to the intervention?",
                     inputId = ns("bias_input4_6"),
                     inline = TRUE,
                     choices = responses,
                     selected = "No Information")
      }
    })
    
    
    
}