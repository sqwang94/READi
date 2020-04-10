

biasPlotFunction <- function(phase1_inputs, bias_values){
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
    
    
    bias_plot <- plot_ly(df_full,
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
    
    return(bias_plot)
    
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
    
    
    bias_plot <- plot_ly(final,
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
    
    return(bias_plot)
    
  
  
}

