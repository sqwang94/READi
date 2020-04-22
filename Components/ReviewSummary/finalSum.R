source("Components/SumPage/biasPlot/biasPlot.R")
source("Components/SumPage/tableFunction/tableFunction.R")


finalSumUI <- function(id){
  ns <- NS(id)
  
  fluidPage(
    id = "final-container",
    div(id = "print",
        icon("print")),
    HTML(paste("<center><b><i><h1>READi</h1></b>
             <h6><u>R</u>eal-World <u>E</u>vidence <u>A</u>ssessments and Needs Guidance</h6></i></center>")),
    fluidRow(
      class = "final-row",
        column(6,
               class = "final-col",
               id = "final-picot",
               wellPanel(class = "final-well", htmlOutput(ns("picot"))))),
    fluidRow(
      column(6,
             class = "final-col",
             id = "final-bias",
             wellPanel(class = "final-well", "We asked you to grade each of your identified articles. Your summary is below: ",
                       br(),
                       br(),
                       wellPanel(id = "final-plot", style = "background: #FFFFFF",
                                 plotlyOutput(ns("biasplot_final"))))),
      class = "final-row",
      wellPanel(id = "final-table", class = "final-well", "We asked you to assess the full body of information for each outcome. Your summary is below: ",
               br(),
               br(),
               wellPanel(style = "background: #FFFFFF",
                         id = "final-table-content",
                         gt_output(ns("final_summary_table"))))),
      fluidRow( class = "final-row",
                wellPanel(id = "final-rec", class = "final-well", h3("Making an Evidence-Based Recommendation"),
                          hr(),
                          div(id = "final-rec-content", 
                              uiOutput(ns("final_rec")))
                          )
                )
  )
}



finalSum <- function(input, output, session, phase1_inputs, bias_values, phase3_inputs, phase4_inputs){
  ns <- session$ns
  
  output$picot <- renderUI({
    if (phase1_inputs$t1_outcomes == 1){
      
      bootstrapPage(
        HTML("<b><h3><font color='#8A2BE2'><u>P</u>opulation: </font></b>", phase1_inputs$t1_pop_interest,
                "<br/><b><font color='#8A2BE2'><u>I</u>ntervention: </font></b>", phase1_inputs$t1_int_interest,
                "<br/><b><font color='#8A2BE2'><u>C</u>omparator: </font></b>", phase1_inputs$t1_comparator,
                "<br/><b><font color='#8A2BE2'><u>O</u>utcome: </font></b>", phase1_inputs$t1_poutcome,
                "<br/><b><font color='#8A2BE2'><u>T</u>ime: </font></b>", phase1_inputs$t1_timeframe,
                "<br/><b><font color='#8A2BE2'><u>S</u>etting: </font></b>", phase1_inputs$t1_setting, "</h3>")
      
      )
    }  else {
      HTML(paste("<b><h3><font color='#8A2BE2'>Population: </font></b>", phase1_inputs$t1_pop_interest,
                 "<br/><b><font color='#8A2BE2'>Intervention: </font></b>", phase1_inputs$t1_int_interest,
                 "<br/><b><font color='#8A2BE2'>Comparator: </font></b>", phase1_inputs$t1_comparator,
                 "<br/><b><font color='#8A2BE2'>Outcomes:</font></b>", phase1_inputs$t1_poutcome, "and", phase1_inputs$t1_soutcome,
                 "<br/><b><font color='#8A2BE2'>Time: </font></b>", phase1_inputs$t1_timeframe,
                 "<br/><b><font color='#8A2BE2'>Setting: </font></b> ", phase1_inputs$t1_setting, "</h3>"))
    }
  
  })
  
  output$biasplot_final <- renderPlotly({
    biasPlotFunction(phase1_inputs, bias_values, TRUE)
  })
  
  shinyjs::delay(500, {
    output$final_summary_table <- render_gt({
      table_function(phase1_inputs, phase3_inputs)
    })
  })
  
  output$final_rec <- renderUI({
    if (!is.null(phase4_inputs$recommendation)) {
      if (phase4_inputs$recommendation == "Other") {
        div(id = "final-rec-content", h4(phase4_inputs$other))
      } else {
        div(id = "final-rec-content", h4(phase4_inputs$recommendation), uiOutput(ns("final_rec_aux")))
      }
    }
  })
  
  shinyjs::delay(500, {
    output$final_rec_aux <- renderUI({
      if (phase4_inputs$recommendation == "Performance-based risk-sharing arrangements (PBRSA)") {
        lapply(0:length(phase4_inputs$t1_studytype), function(i) {
          if (i == 0) {
            h5("Study types interested in:")
          } else {
            tags$li(phase4_inputs$t1_studytype[i])
          }
        })
      } else {
        return()
      }
    })
  })
}


