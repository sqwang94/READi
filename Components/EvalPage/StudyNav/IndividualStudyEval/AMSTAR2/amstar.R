amstar2UI <- function(id) {
  ns <- NS(id)
  yesnoVec   <- c("Yes", "No")
  partialVec <- c("Yes", "Partial Yes", "No")
  noMAvec    <- c("Yes", "No", "No meta-analyis conducted")
  
  
  wellPanel(strong("1B. Please rate the quality of the study using the AMSTAR 2 Checklist."),
            br(),
            tagList(a("Amstar", href="https://amstar.ca/", target = "_blank"),

                    # ------ Question 1 from AMSTAR
                    wellPanel(strong("1. Did the research questions and inclusion criteria for the review include the components of PICO?"),
                              br(),
                              br(),
                              fluidRow(
                                column(4, 
                                checkboxGroupInput(inputId = ns("q1_yes"), 
                                                   label = "For Yes: ", 
                                                   choices = c("Population", "Intervention", "Comparator group", "Outcome"))),
                                column(4,
                                checkboxGroupInput(inputId = ns("q1_optional"), 
                                                   label = "Optional (recommended): ", 
                                                   choices = "Timeframe for follow-up")),
                                column(4,
                                prettyRadioButtons(inputId = ns("q1_choice"),
                                                   label = "Summary", thick = TRUE,
                                                   choices = yesnoVec,
                                                   animation = "pulse",
                                                   status = "info",
                                                   icon = icon("check"))))),
                    
                    # ------ Question 2 from AMSTAR
                    wellPanel(strong("2. Did the report of the review contain an explicit statement that the review methods were established prior to the conduct of the review and did the report justify any significant deviations from the protocol?"),
                              br(),
                              br(),
                              fluidRow(
                              column(4,
                                checkboxGroupInput(inputId = ns("q2_partialyes"),
                                                   label = "For Partial Yes (The authors state that they 
                                                   had a written protocol or guide that included all of 
                                                   the following): ", 
                                                   choices = c("Review question(s)", "A search strategy", "Inclusion/exclusion criteria", "A risk of bias assessment"))),
                              column(4,
                                checkboxGroupInput(inputId = ns("q2_yes"), 
                                                   label = "For Yes, as for partial yes, 
                                                   plus the protocol should be registered 
                                                   and should also have specified: ", 
                                                   choices = c("A meta-analysis/synthesis plan, if appropriate, and", "A plan for investigating causes of heterogeneity", "Justification for any deviations from the protocol"))),
                              column(4,
                                prettyRadioButtons(inputId = ns("q2_choice"),
                                                   label = "Summary", thick = TRUE,
                                                   choices = partialVec,
                                                   animation = "pulse",
                                                   status = "info",
                                                   icon = icon("check"))))),
                    
                    # ------ Question 3 from AMSTAR
                    wellPanel(strong("3. Did the review authors explain their selection of the study designs for inclusion in the review?"),
                              br(),
                              br(),
                              fluidRow(
                                column(8,
                                       checkboxGroupInput(inputId = ns("q3_yes"),
                                                          label = "For Yes, the review should satisfy ONE of the following:", 
                                                          choices = c("Explanation for including only RCTs", "OR explanation for including only NRSI", "OR explanation for including both RCTs and NRSI"))),
                                column(4,
                                       prettyRadioButtons(inputId = ns("q3_choice"),
                                                          label = "Summary", thick = TRUE,
                                                          choices = yesnoVec,
                                                          animation = "pulse",
                                                          status = "info",
                                                          icon = icon("check"))))),
                    
                    # ------ Question 4 from AMSTAR
                    wellPanel(strong("4. Did the review authors use a comprehensive literature search strategy?"),
                              br(),
                              br(),
                              fluidRow(
                                column(4,
                                       checkboxGroupInput(inputId = ns("q4_partialyes"),
                                                          label = "For Partial Yes (all of the following):", 
                                                          choices = c("Searched at least 2 databases (relevant to search question)", "Provided key word and/or search strategy", 
                                                                      "Justified publication restrictions (e.g. language)"))),
                                column(4,
                                       checkboxGroupInput(inputId = ns("q4_yes"),
                                                          label = "For Yes, should also have(all the following):",
                                                          choices = c("Searched the reference lists / bibliographies of included studies", 
                                                                      "Searched trial/study registries", 
                                                                      "Included/consulted content experts in the field", 
                                                                      "Where relevant, searched for grey literature",
                                                                      "Conducted search within 24 months of completion of the review"))),
                                column(4,
                                       prettyRadioButtons(inputId = ns("q4_choice"),
                                                          label = "Summary", thick = TRUE,
                                                          choices = partialVec,
                                                          animation = "pulse",
                                                          status = "info",
                                                          icon = icon("check"))))),
                    
                    # ------ Question 5 from AMSTAR
                    wellPanel(strong("5. Did the review authors perform study selection in duplicate?"),
                              br(),
                              br(),
                              fluidRow(
                                column(8,
                                       checkboxGroupInput(inputId = ns("q5_yes"),
                                                          label = "For Yes, either ONE of the following:", 
                                                          choices = c("At least two reviewers independently agreed on selection of eligible studies and achieved consensus on which studies to include", 
                                                                      "OR two reviewers selected a sample of eligible studies and achieved good agreement (at least 80 percent), with the remainder selected by one reviewer."))),
                                column(4,
                                       prettyRadioButtons(inputId = ns("q5_choice"),
                                                          label = "Summary", thick = TRUE,
                                                          choices = yesnoVec,
                                                          animation = "pulse",
                                                          status = "info",
                                                          icon = icon("check"))))),
                    
                    # ------ Question 6 from AMSTAR
                    wellPanel(strong("6. Did the review authors perform data extraction in duplicate?"),
                              br(),
                              br(),
                              fluidRow(
                                column(8,
                                       checkboxGroupInput(inputId = ns("q6_yes"),
                                                          label = "For Yes, either ONE of the following:", 
                                                          choices = c("At least two reviewers achieved consensus on which data to extract from included studies", 
                                                                      "OR two reviewers extracted data from a sample of eligible studies and achieved good agreement  (at least 80 percent), with the remainder extracted by one reviewer"))),
                                column(4,
                                       prettyRadioButtons(inputId = ns("q6_choice"),
                                                          label = "Summary", thick = TRUE,
                                                          choices = yesnoVec,
                                                          animation = "pulse",
                                                          status = "info",
                                                          icon = icon("check"))))),
                    # ------ Question 7 from AMSTAR
                    wellPanel(strong("7. Did the review authors provide a list of excluded studies and justify the exclusions?"),
                              br(),
                              br(),
                              fluidRow(
                                column(4,
                                       checkboxGroupInput(inputId = ns("q7_partialyes"),
                                                          label = "For Partial Yes:", 
                                                          choices = c("provided a list of all potentially relevant studies that were read in full-text form but excluded from the review"))),
                                column(4,
                                       checkboxGroupInput(inputId = ns("q7_yes"),
                                                          label = "For Yes, must also have:", 
                                                          choices = c("Justified the exclusion from the review of each potentially relevant study"))),
                                
                                column(4,
                                       prettyRadioButtons(inputId = ns("q7_choice"),
                                                          label = "Summary", thick = TRUE,
                                                          choices = partialVec,
                                                          animation = "pulse",
                                                          status = "info",
                                                          icon = icon("check"))))),
                    
                    # ------ Question 8 from AMSTAR
                    wellPanel(strong("8. Did the review authors describe the included studies in adequate detail?"),
                              br(),
                              br(),
                              fluidRow(
                                column(4,
                                       checkboxGroupInput(inputId = ns("q8_partialyes"),
                                                          label = "For Partial Yes (ALL of the following):", 
                                                          choices = c("Described populations",
                                                                      "Described interventions",
                                                                      "Described comparators",
                                                                      "Described outcomes",
                                                                      "Described research designs"))),
                                column(4,
                                       checkboxGroupInput(inputId = ns("q8_yes"),
                                                          label = "For Yes, should also have ALL the following:", 
                                                          choices = c("Described population in detail",
                                                                      "Described intervention in detail (including doses where relevant)",
                                                                      "Described comparator in detail (including doses where relevant)",
                                                                      "Described study's setting",
                                                                      "Timeframe for follow-up"))),
                                
                                column(4,
                                       prettyRadioButtons(inputId = ns("q8_choice"),
                                                          label = "Summary", thick = TRUE,
                                                          choices = partialVec,
                                                          animation = "pulse",
                                                          status = "info",
                                                          icon = icon("check"))))),
                    
                    # ------ Question 9 from AMSTAR
                    wellPanel(strong("9. Did the review authors use a satisfactory technique for assessing the risk of bias (RoB) in individual studies that were included in the review?"),
                              br(),
                              br(),
                              checkboxGroupButtons(inputId = ns("RCTvNRSI_1"), 
                                                   label = "Select the study design",
                                                   choices = c("RCTs", "NRSI")),
                              uiOutput(ns("rctnrsi_1"))),
                    
                    
                    # ------ Question 10 from AMSTAR
                    wellPanel(strong("10. Did the review authors report on the sources of funding for the studies included in the review?"),
                              br(),
                              br(),
                              fluidRow(
                                column(8,
                                       checkboxGroupInput(inputId = ns("q10_yes"),
                                                          label = "For Yes:", 
                                                          choices = c("Must have reported on the sources of funding for individual studies included in the review. Note: Reporting that the reviewers looked for this information but it was not reported by study authors also qualifies"))),
                                column(4,
                                       prettyRadioButtons(inputId = ns("q10_choice"),
                                                          label = "Summary", thick = TRUE,
                                                          choices = yesnoVec,
                                                          animation = "pulse",
                                                          status = "info",
                                                          icon = icon("check"))))),
                    
                    
                    
                    # ------ Question 11 from AMSTAR
                    wellPanel(strong("11. If meta-analysis was performed did the review authors use appropriate methods for statistical combination of results?"),
                              br(),
                              br(),
                              checkboxGroupButtons(inputId = ns("RCTvNRSI_2"), 
                                                   label = "Select the study design",
                                                   choices = c("RCTs", "NRSI")),
                              uiOutput(ns("rctnrsi_2"))),
                    
                    
                    # ------ Question 12 from AMSTAR
                    wellPanel(strong("12. If meta-analysis was performed, did the review authors assess the potential impact of RoB in individual studies on the results of the meta-analysis or other evidence synthesis?"),
                              br(),
                              br(),
                              fluidRow(
                                column(8,
                                       checkboxGroupInput(inputId = ns("q12_yes"),
                                                          label = "For Yes:", 
                                                          choices = c("Included only low risk of bias RCTs", 
                                                                      "OR, if the pooled estimate was based on RCTs and/or NRSI at variable RoB, the authors performed analyses to investigate possible impact of RoB on summary estimates of effect."))),
                                column(4,
                                       prettyRadioButtons(inputId = ns("q12_choice"),
                                                          label = "Summary", thick = TRUE,
                                                          choices = noMAvec,
                                                          animation = "pulse",
                                                          status = "info",
                                                          icon = icon("check"))))),
                    
                    
                    # ------ Question 13 from AMSTAR
                    wellPanel(strong("13. Did the review authors account for RoB in individual studies when interpreting/discussing the results of the review?"),
                              br(),
                              br(),
                              fluidRow(
                                column(8,
                                       checkboxGroupInput(inputId = ns("q13_yes"),
                                                          label = "For Yes:", 
                                                          choices = c("Included only low risk of bias RCTs", 
                                                                      "OR, if RCTs with moderate or high RoB, or NRSI were included the review provided a discussion of the likely impact of RoB on the results"))),
                                column(4,
                                       prettyRadioButtons(inputId = ns("q13_choice"),
                                                          label = "Summary", thick = TRUE,
                                                          choices = yesnoVec,
                                                          animation = "pulse",
                                                          status = "info",
                                                          icon = icon("check"))))),
                    
                    # ------ Question 14 from AMSTAR
                    wellPanel(strong("14. Did the review authors provide a satisfactory explanation for, and discussion of, any heterogeneity observed in the results of the review?"),
                              br(),
                              br(),
                              fluidRow(
                                column(8,
                                       checkboxGroupInput(inputId = ns("q14_yes"),
                                                          label = "For Yes:", 
                                                          choices = c("There was no significant heterogeneity in the results", 
                                                                      "OR if heterogeneity was present the authors performed an investigation of sources of any heterogeneity in the results and discussed the impact of this on the results of the review"))),
                                column(4,
                                       prettyRadioButtons(inputId = ns("q14_choice"),
                                                          label = "Summary", thick = TRUE,
                                                          choices = yesnoVec,
                                                          animation = "pulse",
                                                          status = "info",
                                                          icon = icon("check"))))),
                    
                    # ------ Question 15 from AMSTAR
                    wellPanel(strong("15. If they performed quantitative synthesis did the review authors carry out an adequate investigation of publication bias (small study bias) and discuss its likely impact on the results of the review?"),
                              br(),
                              br(),
                              fluidRow(
                                column(8,
                                       checkboxGroupInput(inputId = ns("q15_yes"),
                                                          label = "For Yes:", 
                                                          choices = c("performed graphical or statistical tests for publication bias and discussed the likelihood and magnitude of impact of publication bias"))),
                                column(4,
                                       prettyRadioButtons(inputId = ns("q15_choice"),
                                                          label = "Summary", thick = TRUE,
                                                          choices = noMAvec,
                                                          animation = "pulse",
                                                          status = "info",
                                                          icon = icon("check"))))),
                    
                    # ------ Question 16 from AMSTAR
                    wellPanel(strong("16. Did the review authors report any potential sources of conflict of interest, including any funding they received for conducting the review?"),
                              br(),
                              br(),
                              fluidRow(
                                column(8,
                                       checkboxGroupInput(inputId = ns("q16_yes"),
                                                          label = "For Yes:", 
                                                          choices = c("The authors reported no competing interests OR",
                                                                      "The authors described their funding sources and how they managed potential conflicts of interest"))),
                                column(4,
                                       prettyRadioButtons(inputId = ns("q16_choice"),
                                                          label = "Summary", thick = TRUE,
                                                          choices = yesnoVec,
                                                          animation = "pulse",
                                                          status = "info",
                                                          icon = icon("check")))))
            )
  )
  
  
}




amstar2 <- function(input, output, session){
  ns <- session$ns
  yesnoVec   <- c("Yes", "No")
  partialVec <- c("Yes", "Partial Yes", "No")
  noMAvec    <- c("Yes", "No", "No meta-analyis conducted")
  
  
  
  output$rctnrsi_1 <- renderUI({
    
    if(is.null(input$RCTvNRSI_1)){
      return()
    }
    
    if(length(input$RCTvNRSI_1) == 2){
      list(
        fluidRow(
        column(4, "RCTs",
               checkboxGroupInput(inputId = ns("q9_partialyes_1"),
                                  label = "For Partial Yes, must have assessed RoB from:",
                                  choices = c("Unconcealed allocation, and",
                                              "Lack of blinding of patients and assessors when assessing outcomes (unnecessary for objective outcomes such as all-cause mortality)"))),
        column(4,
               checkboxGroupInput(inputId = ns("q9_yes_1"),
                                  label = "For Yes, must also have assessed RoB from:",
                                  choices = c("Allocation sequence that was not truly random, and",
                                              "Selection of the reported result form among multiple measurments or analyses of a specified outcome"))),
        column(4,
               prettyRadioButtons(inputId = ns("q9_choice_1"),
                                  label = "Summary", thick = TRUE,
                                  choices = c("Yes", "Partial Yes", "No", "Includes only NSRI"),
                                  animation = "pulse",
                                  status = "info",
                                  icon = icon("check")))),
        fluidRow(
          column(4, "NRSI",
                 checkboxGroupInput(inputId = ns("q9_partialyes"),
                                    label = "For Partial Yes, must have assessed RoB:",
                                    choices = c("from confounding, and",
                                                "from selection bias"))),
          column(4,
                 checkboxGroupInput(inputId = ns("q9_yes"),
                                    label = "For Yes, must also have assessed RoB:",
                                    choices = c("Methods used to ascertain exposures and outcomes, and",
                                                "Selection of the reported result from among multiple measurements or analyses of a specified outcome"))),
          
          column(4,
                 prettyRadioButtons(inputId = ns("q9_choice"),
                                    label = "Summary", thick = TRUE,
                                    choices = c("Yes", "Partial Yes", "No", "Includes only RCTs"),
                                    animation = "pulse",
                                    status = "info",
                                    icon = icon("check")))))
    } else if(input$RCTvNRSI_1 == "NRSI") {
      list(
        fluidRow(
        column(4, "NRSI",
               checkboxGroupInput(inputId = ns("q9_partialyes"),
                                  label = "For Partial Yes, must have assessed RoB:",
                                  choices = c("from confounding, and",
                                              "from selection bias"))),
        column(4,
               checkboxGroupInput(inputId = ns("q9_yes"),
                                  label = "For Yes, must also have assessed RoB:",
                                  choices = c("Methods used to ascertain exposures and outcomes, and",
                                              "Selection of the reported result from among multiple measurements or analyses of a specified outcome"))),

        column(4,
               prettyRadioButtons(inputId = ns("q9_choice"),
                                  label = "Summary", thick = TRUE,
                                  choices = c("Yes", "Partial Yes", "No", "Includes only RCTs"),
                                  animation = "pulse",
                                  status = "info",
                                  icon = icon("check")))))
    } else {
      list(
        fluidRow(
          column(4, "RCTs",
                 checkboxGroupInput(inputId = ns("q9_partialyes"),
                                    label = "For Partial Yes, must have assessed RoB from:",
                                    choices = c("Unconcealed allocation, and",
                                                "Lack of blinding of patients and assessors when assessing outcomes (unnecessary for objective outcomes such as all-cause mortality)"))),
          column(4,
                 checkboxGroupInput(inputId = ns("q9_yes"),
                                    label = "For Yes, must also have assessed RoB from:",
                                    choices = c("Allocation sequence that was not truly random, and",
                                                "Selection of the reported result form among multiple measurments or analyses of a specified outcome"))),
          column(4,
                 prettyRadioButtons(inputId = ns("q9_choice"),
                                    label = "Summary", thick = TRUE,
                                    choices = c("Yes", "Partial Yes", "No", "Includes only NSRI"),
                                    animation = "pulse",
                                    status = "info",
                                    icon = icon("check")))))
    }
  })
  
  
  output$rctnrsi_2 <- renderUI({
    if(is.null(input$RCTvNRSI_2)){
      return()
      }
    
    if(length(input$RCTvNRSI_2) == 2){
     list(
        fluidRow(
        column(8, "NRSI",
             checkboxGroupInput(inputId = ns("q11_yes"),
                                label = "For Yes:", 
                                choices = c("The authors justified combining the data in a meta-analysis",
                                            "AND they uesd an appropriate weighted technique to combine study results and adjusted for heterogeneity in present.",
                                            "AND investigated thee causes of any heterogeneity"))),
      
        column(4,
             prettyRadioButtons(inputId = ns("q11_choice"),
                                label = "Summary", thick = TRUE,
                                choices = noMAvec,
                                animation = "pulse",
                                status = "info",
                                icon = icon("check")))),
        fluidRow(
          column(8, "RCTs",
                 checkboxGroupInput(inputId = ns("q11_yes_2"),
                                    label = "For Yes:", 
                                    choices = c("The authors justified combining the data in a meta-analysis",
                                                "AND they used an appropriate weighted technique to combine study results, adjusting for heterogeneity if present.",
                                                "AND they statistically combined effect estimates from NRSI that were adjusted for confounding, rather than combining raw data, or justified combining raw data when adjusted effect estimates were not available",
                                                "AND they reported separate summary esstimates for RCTs and NSRI separately when both were included in the review"))),
          
          column(4,
                 prettyRadioButtons(inputId = ns("q11_choice_2"),
                                    label = "Summary", thick = TRUE,
                                    choices = noMAvec,
                                    animation = "pulse",
                                    status = "info",
                                    icon = icon("check")))))
    } else if(input$RCTvNRSI_2 == "NRSI"){
      list(
        fluidRow(
        column(8, "NRSI",
               checkboxGroupInput(inputId = ns("q11_yes"),
                                  label = "For Yes:", 
                                  choices = c("The authors justified combining the data in a meta-analysis",
                                              "AND they uesd an appropriate weighted technique to combine study results and adjusted for heterogeneity in present.",
                                              "AND investigated thee causes of any heterogeneity"))),
        column(4,
               prettyRadioButtons(inputId = ns("q11_choice"),
                                  label = "Summary", thick = TRUE,
                                  choices = noMAvec,
                                  animation = "pulse",
                                  status = "info",
                                  icon = icon("check")))))
    } else {
      list(
        fluidRow(
          column(8, "RCTs",
                 checkboxGroupInput(inputId = ns("q11_yes"),
                                    label = "For Yes:", 
                                    choices = c("The authors justified combining the data in a meta-analysis",
                                                "AND they used an appropriate weighted technique to combine study results, adjusting for heterogeneity if present.",
                                                "AND they statistically combined effect estimates from NRSI that were adjusted for confounding, rather than combining raw data, or justified combining raw data when adjusted effect estimates were not available",
                                                "AND they reported separate summary esstimates for RCTs and NSRI separately when both were included in the review"))),
          
          column(4,
                 prettyRadioButtons(inputId = ns("q11_choice"),
                                    label = "Summary", thick = TRUE,
                                    choices = noMAvec,
                                    animation = "pulse",
                                    status = "info",
                                    icon = icon("check")))))
    }
    
  })
  
  
  
  
}

