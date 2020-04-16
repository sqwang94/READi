source("Components/SumPage/biasPlot/biasPlot.R")
source("Components/SumPage/tableFunction/tableFunction.R")


finalSumUI <- function(id){
  ns <- NS(id)
  
  fluidPage(
    fluidRow(
      column(12,
             HTML("<center><h4>Congratulations, you have completed the  READi (Real-World Evidence Assessments and Needs Guidance) Tool!</h4></center>"))),
    fluidRow(
      column(6, 
           wellPanel(htmlOutput(ns("picot")))),
      column(6,
           wellPanel("We asked you to grade each of your identified articles. Your summary is below: ",
                     br(),
                     br(),
                     wellPanel(style = "background: #FFFFFF",
                               plotlyOutput(ns("biasplot_final")))))),
    fluidRow(
      column(6, offset = 6,
           wellPanel("We asked you to assess the full body of information for each outcome. Your summary is below: ",
                     br(),
                     br(),
                     wellPanel(style = "background: #FFFFFF",
                               gt_output(ns("final_summary_table")))))),
    br(),
    br(),
    br(),
    column(8, offset =2, 
           wellPanel(strong("If you'd like to generate a PDF of your report, you may do so here. Note that we recommend creating an account and saving your progress as well."),
                     br(),
                     br(),
                     wellPanel(style = "background: #FFFFFF",
                       textInput(inputId = ns("filename"), "Name your file here", placeholder = "MyFile.PDF"),
                       downloadButton(outputId = ns("dwnld"), label = "Click here to Download PDF"))
           )
    )
  )
  
}



finalSum <- function(input, output, session, phase1_inputs, bias_values, phase3_inputs){
  ns <- session$ns
  
  
  output$picot <- renderUI({
    if (phase1_inputs$t1_outcomes == 1){
      
      bootstrapPage(
        HTML(paste("<center><b><i><h1>READi</h1></b>
             <h6><u>R</u>eal-World <u>E</u>vidence <u>A</u>ssessments and Needs Guidance</h6></i></center>")),
        hr(),
        br(),
      HTML("<b><h3><font color='#8A2BE2'><u>P</u>opulation: </font></b>", phase1_inputs$t1_pop_interest,
                "<br/><b><font color='#8A2BE2'><u>I</u>ntervention: </font></b>", phase1_inputs$t1_int_interest,
                "<br/><b><font color='#8A2BE2'><u>C</u>omparator: </font></b>", phase1_inputs$t1_comparator,
                "<br/><b><font color='#8A2BE2'><u>O</u>utcome: </font></b>", phase1_inputs$t1_poutcome,
                "<br/><b><font color='#8A2BE2'><u>T</u>ime: </font></b>", phase1_inputs$t1_timeframe,
                "<br/><b><font color='#8A2BE2'><u>S</u>etting: </font></b>", phase1_inputs$t1_setting, "</h3>")
      
      )
    }  else {
      HTML(paste("<center><b><font color='#8A2BE2'>Population: </font></b>", phase1_inputs$t1_pop_interest,
                 "<br/><b><font color='#8A2BE2'>Intervention: </font></b>", phase1_inputs$t1_int_interest,
                 "<br/><b><font color='#8A2BE2'>Comparator: </font></b>", phase1_inputs$t1_comparator,
                 "<br/><b><font color='#8A2BE2'>Outcomes:</font></b>", phase1_inputs$t1_poutcome, "and", phase1_inputs$t1_soutcome,
                 "<br/><b><font color='#8A2BE2'>Time: </font></b>", phase1_inputs$t1_timeframe,
                 "<br/><b><font color='#8A2BE2'>Setting: </font></b> ", phase1_inputs$t1_setting, "</center>"))
    }
  
  })
  
  output$biasplot_final <- renderPlotly({
    biasPlotFunction(phase1_inputs, bias_values)
  })
  
  output$final_summary_table <- render_gt({
    table_function(phase1_inputs, phase3_inputs)
  })
  
  
  output$dwnld <- downloadHandler(

    filename = function(){
      name <- input$filename
      pdf_check <- str_detect(tolower(name), ".pdf")
      
      if(is.null(name)){
        return(print("Please give your pdf file a name (above)"))
      } else if(!pdf_check){
        print("Please save your file as '.pdf'. For example, if you'd like to make your title 'BrennanIsCool', please type 'BrennanIsCool.pdf")
      } else {
        paste0(name, ".pdf")
      }
    },
    
    content = function(file){
      hist(cars$speed)
    }
    
    
    
    
  )
  
  
  
  
  
  

  
}


