# UI function for previous and next button components in study eval navbarPage
studyNavButtonButton <- function(id) {
    ns <- NS(id)
    return(fluidRow(
        column(6, align="left", actionButton(ns("prevButton"), label = icon("angle-left"), class = "PrevButton")),
        column(6, align="right", actionButton(ns("nextButton"), label = icon("angle-right"), class = "NextButton"))
    ))
}

# Server function for previous and next buttons in each study eval component
studyNavButton <- function(input, output, session, parentSession, page) {
    observeEvent(input$prevButton, {
        updateNavbarPage(parentSession, "navBar", selected=toString(page - 1));
        js$toTop();
    })
    observeEvent(input$nextButton, {
        updateNavbarPage(parentSession, "navBar", selected=toString(page + 1));
        js$toTop();
    })
}