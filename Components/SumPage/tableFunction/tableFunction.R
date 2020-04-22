# create the summary table based on phase 1 and phase 3 inputs
table_function <- function(phase1_inputs, phase3_inputs){
  domain_vec <- c("Overall risk of bias",
                  "Are the results consistent",
                  "Are the results direct?",
                  "Are the results precise?",
                  "Is there publication bias?")
    if (phase1_inputs$t1_outcomes == 1){
      outcomes <- c(phase3_inputs$t3_studylim_1, # note this is now related to "Overall Risk of Bias"
                    phase3_inputs$t3_consistent_1, 
                    phase3_inputs$t3_direct_1, 
                    phase3_inputs$t3_precise_1,
                    phase3_inputs$t3_bias_1)
      
      df <- data.frame(
        Domains   = domain_vec,
        Responses = outcomes,
        type      = rep(phase1_inputs$t1_poutcome, 5)) %>% 
        group_by(type) %>% 
        gt(rowname_col = "Domains") %>% 
        tab_header(title = "Whole Body of Evidence Review",
                   subtitle = md("*For each outcome of interest*")) %>% 
        tab_options(
          table.width = "100%",
          row_group.background.color = "#FFEFDB80",
          heading.background.color = "#E8DCF0"
        )
    } else {
      outcomes <- c(phase3_inputs$t3_studylim_1, 
                    phase3_inputs$t3_consistent_1, 
                    phase3_inputs$t3_direct_1, 
                    phase3_inputs$t3_precise_1,
                    phase3_inputs$t3_bias_1,
                    phase3_inputs$t3_studylim_2, 
                    phase3_inputs$t3_consistent_2, 
                    phase3_inputs$t3_direct_2, 
                    phase3_inputs$t3_precise_2,
                    phase3_inputs$t3_bias_2)
      df <- data.frame(
        Domains   = rep(domain_vec,2),
        Responses = outcomes,
        type = c(rep(phase1_inputs$t1_poutcome, 5),rep(phase1_inputs$t1_soutcome,5))) %>% 
        group_by(type) %>% 
        gt(rowname_col = "Domains") %>% 
        tab_header(title = "Whole Body of Evidence Review",
                   subtitle = md("*For each outcome of interest*")) %>% 
        tab_options(
          table.width = "100%",
          row_group.background.color = "#FFEFDB80",
          heading.background.color = "#E8DCF0"
        )
    }
    return(df)
}