# a function to validate inputs for submission
input_validation <- function(x){                    
    logic_list <- lapply(x, isTruthy)                 # create a list of logical values
    unlisted_vec <- unlist(logic_list)                # unlist to sum
    sum(unlisted_vec) == length(unlisted_vec)         # logical testing whether there are any "non-True" values
}

# check and add Error class for visual effects on missing input fields
toggleErrorInputHandler <- function(inputs) {
    for (key in names(inputs)) {
        if (inputs[key] == "") {
            addClass(selector = paste0("#", key), class="Error")
        } else {
            removeClass(selector = paste0("#", key), class="Error")
        }
    }
}

# wrapper for navbarPage with login button
navbarPageWithBtn <- function(...) {
    navbar <- navbarPage(...)
    element <- uiOutput("loginToggle")
    btn <- tags$button(
        id = "login",
        type = "button",
        class = "btn",
        "Log in"
    )
    navbar[[3]][[1]]$children[[1]]$children[[2]] <- htmltools::tagAppendChild(
        navbar[[3]][[1]]$children[[1]]$children[[2]], element)
    navbar
}

# JS function scroll window to the study eval
toTop <- "shinyjs.toTop = function() {
    $('html, body').animate(
        {
            scrollTop: $('#eval_page-study_react').offset().top,
        },
        200,
        'linear'
    )
}"

toWindowTop <- "shinyjs.toWindowTop = function() {
    $('html, body').scrollTop(0)
}"