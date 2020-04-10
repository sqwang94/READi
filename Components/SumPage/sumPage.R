source("Components/SumPage/biasPlot/biasPlot.R")

sumPageUI <- function(id) {
    ns <- NS(id)
    fluidPage(
        column(8, offset = 2,
               wellPanel(strong(paste0("Below is a summary of your responses for your outcome(s). Use this to think about the questions below.")),
                         br(),
                         br(),
                         wellPanel(style = "background: #FFFFFF",
                           plotlyOutput(ns("summaryoutcomes"))))),
        uiOutput(ns("t3_pt1")),
        column(8, offset = 2,
               wellPanel(style = "background: #FFFFFF",
                   gt_output(ns("t3_table"))))
    )
}

sumPage <- function(input, output, session, phase1_inputs, bias_values) {
    ns <- session$ns
    
    output$summaryoutcomes <- renderPlotly({
      biasPlotFunction(phase1_inputs, bias_values)
    })

    output$t3_pt1 <- renderUI({
      study_count <- 0
      if (!is.null(bias_values())) {
        study_count <- reactiveValuesToList(bias_values())$count
      }
      
        # ------ Defining inputID for all inputs
        studylim <- lapply(seq_len(phase1_inputs$t1_outcomes), function(i){paste0("t3_studylim_", i)})
        subjects <- lapply(seq_len(phase1_inputs$t1_outcomes), function(i){paste0("t3_subjects_", i)})
        comparator <- lapply(seq_len(phase1_inputs$t1_outcomes), function(i){paste0("t3_comparator_", i)})
        consistent <- lapply(seq_len(phase1_inputs$t1_outcomes), function(i){paste0("t3_consistent_", i)})
        direct     <- lapply(seq_len(phase1_inputs$t1_outcomes), function(i){paste0("t3_direct_", i)})
        precise    <- lapply(seq_len(phase1_inputs$t1_outcomes), function(i){paste0("t3_precise_", i)})
        bias       <- lapply(seq_len(phase1_inputs$t1_outcomes), function(i){paste0("t3_bias_", i)})
        
        lapply(seq_len(phase1_inputs$t1_outcomes), function(i){
            if(i == 1){
                out <- "primary"
                type <- phase1_inputs$t1_poutcome
                study <- study_count
            } else {
                out <- "secondary"
                type <- phase1_inputs$t1_soutcome
            }
            column(8, offset = 2,
                   wellPanel(strong(paste0("For your ", out, " outcome of ", type, " answer the following questions:")),
                             br(),
                             br(),
                             wellPanel(
                                 selectInput(inputId = ns(studylim[[i]]),
                                             label = "Based on the rating for each study, what's the overall level of study limitation?",
                                             choices = c("High", "Moderate", "Low"),
                                             selected = character(0)),
                                 textInput(inputId = ns(subjects[[i]]),
                                           label = "What is the overall number of subjects (N)?",
                                           placeholder = 50),
                                 textInput(inputId = ns(comparator[[i]]),
                                           label = "What is the comparator listed in each study for this outcome?",
                                           placeholder = "Standard of Care"),
                                 selectInput(inputId = ns(consistent[[i]]),
                                             label = "Are the results among the studies consistent with one another? ",
                                             choices = c("Consistent", "Unknown", "Inconsistent")),
                                 selectInput(inputId = ns(direct[[i]]),
                                             label = "Are the results direct?",  # need hover tool tip for "direct"
                                             choices = c("Direct", "Indirect")),
                                 selectInput(inputId = ns(precise[[i]]),
                                             label = "Are the results precise?",
                                             choices = c("Precise", "Imprecise")),
                                 selectInput(inputId = ns(bias[[i]]),
                                             label = "Is there publication bias?",
                                             choices = c("Yes", "No"))),
                             bsPopover(id = ns(direct[[i]]),
                                       title = "Evidence can be indirect when:",
                                       content =  paste("i. Patients, intervention, or outcomes differ from that of interest",
                                                        "ii. Clinicians must choose between interventions that hvae not been compared in a head-to-head manner",
                                                        sep = "<br><br>"), 
                                       placement = "left", 
                                       trigger = "hover"),
                             bsPopover(id = ns(precise[[i]]),
                                       title = "Precision", content =  paste("Studies can be considered imprecise if there are few patients or and few events, thereby rendering a fairly large confidence interval"),
                                       placement = "left",
                                       trigger = "hover"),
                             bsPopover(id = ns(bias[[i]]),
                                       title = "Publication Bias:", content =  paste("A systematic over or underestimate of treatment effect due to the selective publication of studies"),
                                       placement = "left",
                                       trigger = "hover")))
        })
    })
    
    output$t3_table <- render_gt({
        domain_vec <- c("Comparator in each study", 
                        "Number of studies and number of subjects (overall)",
                        "Level of overall study limitation",
                        "Are the results consistent",
                        "Are the results direct?",
                        "Are the results precise?",
                        "Is there publication bias?")
        
        if (phase1_inputs$t1_outcomes == 1){
            outcomes <- c(input$t3_comparator_1, 
                          input$t3_subjects_1, 
                          input$t3_studylim_1,
                          input$t3_consistent_1, 
                          input$t3_direct_1, 
                          input$t3_precise_1,
                          input$t3_bias_1)
            
            df <- data.frame(
                Domains   = domain_vec,
                Responses = outcomes,
                type      = rep(phase1_inputs$t1_poutcome, 7)) %>% 
              group_by(type) %>% 
              gt(rowname_col = "Domains") %>% 
              tab_header(title = "Whole Body of Evidence Review",
                         subtitle = md("*For each outcome of interest*")) %>% 
              tab_options(
                row_group.background.color = "#FFEFDB80",
                heading.background.color = "#E8DCF0"
              )
        } else {
            outcomes <- c(input$t3_studylim_1, 
                               input$t3_subjects_1, 
                               input$t3_comparator_1, 
                               input$t3_consistent_1, 
                               input$t3_direct_1, 
                               input$t3_precise_1,
                               input$t3_bias_1,
                               input$t3_studylim_2, 
                               input$t3_subjects_2, 
                               input$t3_comparator_2, 
                               input$t3_consistent_2, 
                               input$t3_direct_2, 
                               input$t3_precise_2,
                               input$t3_bias_2)
           df <- data.frame(
                Domains   = rep(domain_vec,2),
                Responses = outcomes,
                type = c(rep(phase1_inputs$t1_poutcome, 7),rep(phase1_inputs$t1_soutcome,7))) %>% 
              group_by(type) %>% 
              gt(rowname_col = "Domains") %>% 
              tab_header(title = "Whole Body of Evidence Review",
                         subtitle = md("*For each outcome of interest*")) %>% 
              tab_options(
                row_group.background.color = "#FFEFDB80",
                heading.background.color = "#E8DCF0"
              )
        }
        return(df)
        
    })
    
}