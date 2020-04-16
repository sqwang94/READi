

qhesUI <- function(id){
  ns <- NS(id)
  
  # --- creating standard choices
  choices <- c("Yes", "No")
  # --- putting all labels here to be looped through
  label_list <- list(
    "1. Was  the  study  objective  presented  in  a  clear,  specific,  and  measurable manner?",
    "2. Were  the  perspective  of  the  analysis  (societal,  third-party  payer,etc.)  and reasons for its selection stated?",
    "3. Were variable estimates used in the analysis from the best available source (i.e., randomized control trial -best, expert opinion -worst)?",
    "4. If estimates  came from a subgroup analysis, were  thegroups pre-specified at the beginning of the study?",
    "5. Was  uncertainty  handled  by  (1)  statistical  analysis  to  address  random events, (2) sensitivity analysis to cover a range of assumptions?",
    "6. Was  incremental  analysis  performed  between alternatives  for  resources and costs?",
    "7. Was  the  methodology  for  data  abstraction  (including  the  value  of  health states and other benefits) stated?",
    "8. Did   the   analytic   horizon   allow   time   for   all   relevant   and   important outcomes?  Were  benefits and  costs  that  went  beyond  1  year  discounted (3% to 5%) and justification given for the discount rate?",
    "9. Was  the  measurement  of  costs  appropriate  and  the  methodology  for  the estimation of quantities and unit costs clearly described?",
    "10. Were the primary outcome measure(s) for the economic evaluation clearly stated  and  did  they  include  the major  short-term, long-term,  and  negative outcomes?",
    "11. Were the health outcomes measures/scales valid and reliable? If previously tested  valid  and  reliable  measures  were  not  available,  was  justification given for the measures/scales used?",
    "12. Were   the   economic   model   (including   structure),   study   methods   and analysis, and the components of the numerator and denominator displayed in a clear, transparent manner?",
    "13. Were the  choice  of economic model, main assumptions, and limitations of the study stated and justified?",
    "14. Did  the  author(s)  explicitly  discuss  direction  and  magnitude  of  potential biases?",
    "15. Were the conclusions/recommendations of the study justified and based on the study results?",
    "16. Was there a statement disclosing the source of funding for the study?"
  )
  
  
  
  
  wellPanel(strong("1B. Please rate the quality of the study using the QHES instrument"),
            br(),
            tagList(a("QHES: Quality of Health Economic Analysis", href="https://pubmed.ncbi.nlm.nih.gov/14613362/", target = "_blank"),
                    br(),
                    br(),
                    wellPanel(
                      br(),
                      lapply(seq_len(length(label_list)), function(i){
                        prettyRadioButtons(inputId = ns(paste0("qhes",i)), 
                                           label = label_list[[i]], 
                                           animation = "pulse",
                                           status = "info",
                                           width = "100%",
                                           icon = icon("check"),
                                           choices = choices)
                      }))


                    
                    
                    ))
                  
  
  
  
  
}