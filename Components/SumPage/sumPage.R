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
               wellPanel(
                   htmlOutput(ns("t3_table"))))
    )
}

sumPage <- function(input, output, session, phase1_inputs, bias_values) {
    ns <- session$ns
    
    output$summaryoutcomes <- renderPlotly({
      if (is.null(bias_values())){
        return()
      }
      
      # -- creating dummy df to merge to primary or secondary outcomes (to account for 0 values in count)
      df_dummy <- data.frame(     # using variable convention of table() to bind and then account for 0s (table won't show 0 count values, obviously)
        responses =  c("High Risk", "Low Risk", "Unclear Risk"),
        Freq = c(rep(0,3))
      )
      
      bias <- reactiveValuesToList(bias_values())
      
      # -- Answers to the Standard Question at the end of all individual study evals
      x <- unlist(lapply(seq_len(length(bias)), function(i){bias[[as.character(i)]]$standard_bias})) # unlisting answers from individualStudyEval (found in bias <- reactiveValuesToList(bias_values()))
      
      # -- "Does this apply to your primary outcome"
      y <- unlist(lapply(seq_len(length(bias)), function(i){bias[[as.character(i)]]$outcome1}))
      
      if (phase1_inputs$t1_outcomes == 1){
      
        # -- we want all values to show up regardless if they have been selected, so we need to count them here and give 0 to all without values
        # -- additionally, we only want the responses applicable to outcome 1 (if y = "Yes", from above)
        df_responses <- data.frame(x = x, y = y) 
        responses <- df_responses$x[df_responses$y == "Yes"]
        
        x <- data.frame(table(responses))   # counting all responses in x

        
        df_full <- rbind(x, df_dummy) %>% # create and sum df_full - all counts should be there regardless of 0 or non-zero
          mutate(total_count = sum(Freq)) %>% 
          group_by(responses) %>% 
          summarise(pct = sum(Freq)/max(total_count))

        
        plot_ly(df_full,
                x = ~responses,
                y = ~pct,
                type = "bar",
                marker = list(color = c('rgba(51,0,111,0.8)', 'rgba(232,211,162,0.8)',
                                        'rgba(216,217,218,1)')),
                hovertemplate = paste("Response: %{x} <br> Share of Responses: %{y}")) %>% 
          layout(title = "Summary of Responses for the Primary Outcome of",
                 yaxis = list(title = 'Percent', tickformat = '.0%',range = c(0,1)),
                 xaxis = list(title = '')) %>% 
          config(displayModeBar = FALSE, displaylogo = FALSE)
        
      } else {
        
        z <- unlist(lapply(seq_len(length(bias)), function(i){bias[[as.character(i)]]$outcome2}))
          
        # -- gathering all the primary data
        df_responses_primary <- data.frame(x = x, y = y)
        responses_primary <- df_responses_primary$x[df_responses_primary$y == "Yes"]
        df_primary <- data.frame(table(responses_primary)) %>% 
          select(responses = responses_primary, Freq)
        
        df_first <- rbind(df_primary, df_dummy) %>% 
          mutate(total_count = sum(Freq)) %>% 
          group_by(responses) %>% 
          summarise(pct1 = sum(Freq)/max(total_count))
        
        # -- gathering all the secondary data
        df_responses_secondary <- data.frame(x = x, y = z)
        responses_secondary <- df_responses_secondary$x[df_responses_secondary$y == "Yes"]
        df_secondary <- data.frame(table(responses_secondary)) %>% 
          select(responses = responses_secondary, Freq)
        
        df_second<- rbind(df_secondary, df_dummy) %>% 
          mutate(total_count = sum(Freq)) %>% 
          group_by(responses) %>% 
          summarise(pct2 = sum(Freq)/max(total_count))
        
        final <- merge(df_first, df_second)
        
        
        plot_ly(final,
                x = ~responses,
                y = ~pct1,
                type = "bar",
                marker = list(color = c('rgba(51,0,111,0.8)')),
                name = "Primary",
                hovertemplate = paste("Outcome: Primary <br> Response: %{x} <br> Share of Responses: %{y}")) %>% 
          layout(title = "Summary of Responses for the Primary Outcome of",
                 yaxis = list(title = 'Percent', tickformat = '.0%',range = c(0,1)),
                 xaxis = list(title = '')) %>% 
          add_trace(
            x = ~responses,
            y = ~pct2,
            type = "bar",
            hovertemplate = paste("Outcome: Secondary <br> Response: %{x} <br> Share of Responses: %{y}"),
            marker = list(color = c('rgba(232,211,162,0.8)')),
            name = "Secondary"
          ) %>% 
          config(displayModeBar = FALSE, displaylogo = FALSE)
        
      }
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
    
    output$t3_table <- renderText({
        domain_vec <- c("Outcome of interest", 
                        "Comparator in each study", 
                        "Number of studies and number of subjects (overall)",
                        "Level of overall study limitation",
                        "Are the results consistent",
                        "Are the results direct?",
                        "Are the results precise?",
                        "Is there publication bias?")
        if (phase1_inputs$t1_outcomes == 1){
            outcomes <- c(phase1_inputs$t1_poutcome,
                          input$t3_studylim_1, 
                          input$t3_subjects_1, 
                          input$t3_comparator_1, 
                          input$t3_consistent_1, 
                          input$t3_direct_1, 
                          input$t3_precise_1,
                          input$t3_bias_1)
            
            df <- data.frame(
                Domains           = domain_vec,
                `Primary Outcome` = outcomes
            )
            
            kable(df,
                  col.names = c("Domains", "Primary Outcome"),
                  "html", 
                  escape = FALSE,
                  align = "c") %>% 
                kable_styling(full_width = TRUE, 
                              bootstrap_options = c("striped", "hover", "condensed", "bordered")) %>% 
                row_spec(c(1,3,5,7), background = "#d1b3e6", color = "black") %>% 
                row_spec(c(2,4,6,8), background = "white", color = "black")
        } else {
            outcomes <- list(c(phase1_inputs$t1_poutcome,
                               input$t3_studylim_1, 
                               input$t3_subjects_1, 
                               input$t3_comparator_1, 
                               input$t3_consistent_1, 
                               input$t3_direct_1, 
                               input$t3_precise_1,
                               input$t3_bias_1),
                             c(phase1_inputs$t1_soutcome,
                               input$t3_studylim_2, 
                               input$t3_subjects_2, 
                               input$t3_comparator_2, 
                               input$t3_consistent_2, 
                               input$t3_direct_2, 
                               input$t3_precise_2,
                               input$t3_bias_2))
            df <- data.frame(
                Domains           = domain_vec,
                `Primary Outcome` = outcomes[[1]],
                `Secondary Outcomes` = outcomes[[2]]
                
            )
            
            kable(df,
                  col.names = c("Domains", "Primary Outcome", "Secondary Outcome"),
                  "html", 
                  escape = FALSE,
                  align = "c") %>% 
                kable_styling(full_width = TRUE, 
                              bootstrap_options = c("striped", "hover", "condensed")) %>% 
                row_spec(c(1,3,5,7), background = "#d1b3e6", color = "black") %>% 
                row_spec(c(2,4,6,8), background = "white", color = "black")
        }
        
    })
    
}