source("Components/EvalPage/StudyNav/IndividualStudyEval/individualStudyEval.R")
source("Components/EvalPage/StudyNav/StudyNavButton/studyNavButton.R")
source("Components/EvalPage/StudyNav/FillerContainer/fillerContainer.R")

# UI function for studies navigation component. Uses "First" and "Last" class to show/hide Prev/Next button.
studyNavUI <- function(id, numOfStudy) {
    ns <- NS(id)
    if (numOfStudy == 1) {
        panels <- list(tabPanel(class = "First Last", title = "1", value = "1", individualStudyEvalUI(ns("study1"), 1), studyNavButtonButton(ns("buttons1"))))
    }
    else {
        panels <- list(tabPanel(class = "First", title = "1", value = "1", individualStudyEvalUI(ns("study1"), 1), studyNavButtonButton(ns("buttons1"))))
    }
    if (numOfStudy > 2) {
        for (i in 2:(numOfStudy - 1)) {
            panels[[i]] <- tabPanel(title = toString(i), value = toString(i), individualStudyEvalUI(ns(paste0("study", i)), i), studyNavButtonButton(ns(paste0("buttons", i))))
        }
    }
    if (numOfStudy > 1) {
        panels[[numOfStudy]] <- tabPanel(class = "Last", title = toString(numOfStudy), value=toString(numOfStudy), individualStudyEvalUI(ns(paste0("study", numOfStudy)), numOfStudy), studyNavButtonButton(ns(paste0("buttons", numOfStudy))))
    }
    panels[[numOfStudy + 1]] <- tabPanel(title = NULL, value = "Add", icon=icon("plus"), fillerContainerUI("filler-add"))
    panels[[numOfStudy + 2]] <- tabPanel(title = NULL, value = "Remove", icon=icon("trash-alt"), fillerContainerUI("filler-remove"), div("Please add a study"))
    return(div(do.call(navbarPage, c(title=NULL, title="Literature", id=ns("navBar"), collapsible = TRUE, panels)), div(class = "hidden", numericInput(ns("studyCounter"), NULL, value = numOfStudy))))
}

# server function for studies navigation component
studyNav <- function(input, output, session, numOfStudy, phase1_inputs) {
    lapply(1:numOfStudy, function(i) {
        callModule(studyNavButton, paste0("buttons", i), session, i)
        callModule(individualStudyEval, paste0("study", i), phase1_inputs)
    })
    js$toTop()
}

# global add and remove functionality for the studies navigation
studyNavGlobal <- function(input, output, session, phase1_inputs) {
    ns <- session$ns
    
    # add a new page to the studies navigation
    addPageHandler <- function(tabId) {
        selector <- paste0("#eval_page-study_react ul li:nth-child(", tabId, ")>a")
        runjs(paste0("document.querySelector('", selector, "').blur()"))
        updateNumericInput(session, "studyCounter", value = tabId)
        removeClass(selector = ".Last", class="Last")
        removeClass(selector = "#filler-remove", class="Hidden")
        class <- "Last"
        if (tabId == 1) {
            class <- "First Last"
        }
        insertTab("navBar",
                  tabPanel(class = class, title = toString(tabId), value = toString(tabId), individualStudyEvalUI(ns(paste0("study", tabId)), tabId), studyNavButtonButton(ns(paste0("buttons", tabId)))),
                  target = "Add", select = TRUE, position = "before")
        callModule(studyNavButton, paste0("buttons", tabId), session, tabId)
        callModule(individualStudyEval, paste0("study", tabId), phase1_inputs)
    }
    
    # remove the last page from the studies navigation
    removePageHandler <- function(tabId) {
        if (tabId > 0) {
            selector <- paste0("#eval_page-study_react ul li:nth-child(", tabId + 2, ")>a")
            runjs(paste0("document.querySelector('", selector, "').blur()"))
            updateNavbarPage(session, "navBar", "1")
            updateNumericInput(session, "studyCounter", value = tabId - 1)
            removeTab("navBar", toString(tabId))
            addClass(selector = paste0("#eval_page-study_react .tab-content .tab-pane:nth-child(", tabId - 1, ")"), class="Last")
            if (tabId == 1) {
                addClass(selector = "#filler-remove", class="Hidden")
            }
        }
    }
    
    # event listener for add and remove
    observe({
        # add functionallity for the studies navigation
        if (req(input$navBar) == "Add") {
            newTabId <- input$studyCounter + 1
            addPageHandler(newTabId)
        }
        
        # remove functionallity for the studies navigation
        if (req(input$navBar) == "Remove") {
            removeTabId <- input$studyCounter
            removePageHandler(removeTabId)
        }
        js$toTop()
    })
}

# returns list of inputs from all studies from studies navigation for input validation
studyNavValidation <- function(input, output, session) {
    return(
        reactive({
            numStudy <- input$studyCounter
            inputs <- list()
            if (!is.null(numStudy) && numStudy > 0) {
                for (i in seq(numStudy)) {
                    input <- callModule(individualStudyInputValidation, paste0("study", i))
                    inputs <- append(inputs, input())
                }
            }
            return(inputs)
        })
    )
}
