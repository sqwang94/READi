# --- Recommendation Module

recPageUI <- function(id){
  ns <- NS(id)
  fluidPage(
    column(8, offset = 2,
           wellPanel("Based on your results after the literature review, please consider whethre the evidence is
                     sufficient and applicable for your own population and question(s) of interest.",
             radioButtons(ns("t4_ev_available"),
                          "Was there any literature/evidence available?",
                          choices = c("Yes", "No")),
             radioButtons(ns("applicable"),
                          "Is current evidence applicable to answer your research question(s)? (generalizable to your population)",
                          choices = c("Yes", "No")),
             radioButtons(ns("sufficient"),
                          "Was current evidence sufficient to answer your research question(s)? Please consider your rating from Phase 3 (especially the body of evidence).",
                          choices = c("Yes","No")),
             
             uiOutput(ns("study_identified"))
           )),
    column(8, offset = 2,
           radioButtons(ns("recommendation"),
                        "Please select the recommendations from the list below; if your planned recommendation is not listed,
                        please select 'Other' and proceed to the next question.",
                        choices = c("No coverage",
                                    "Performance-based risk-sharing arrangements (PBRSA)",
                                    "Coverage with guidelines",
                                    "Coverage with prior authorization criteria (e.g. step therapy)",
                                    "Coverage with benefit or product contractual rules (in the actual beneficial context, e.g. we are not going to pay for weight loss)",
                                    "Other"))
    )
  )
}