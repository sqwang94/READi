# a function to validate inputs for submission
input_validation <- function(x){                    
    logic_list <- lapply(x, isTruthy)                 # create a list of logical values
    unlisted_vec <- unlist(logic_list)                # unlist to sum
    sum(unlisted_vec) == length(unlisted_vec)         # logical testing whether there are any "non-True" values
}

# JS function scroll window to the study eval
toTop <- "shinyjs.toTop = function() {
    let offsetTop = document.getElementById('eval_page-study_react').parentElement.offsetTop;
    window.scrollTo(0, offsetTop);
}"
