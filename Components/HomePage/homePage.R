homePageUI <- bootstrapPage(
    bookmarkButton(),
    div(id = "home", class = "jumbotron",
        img(src = "CHOICE.png", id = "choice-image"),
        h2("Welcome to the READi (Real-World Evidence Assessments and Needs Guidance) Tool"),
        p(id = "author", "presented by The CHOICE Institute"),
        hr(),
        p("The READi Tool is a comprehensive online tool to guide literature review and coverage decision-making"),
        p("Once you have decided you need real world evidence to answer your question of interest, the READi tool 
                                        will guide you through a 3-Phase processs of evaluating the literature to inform your adoption decision."),
        a(class = "btn btn-primary btn-lg", `data-toggle` = "collapse", href ="#collapse-detail", role = "button", "Learn more"),
        div(class = "collapse", id = "collapse-detail",
            bsCollapse(id = "accordion",
                       bsCollapsePanel("Phase 1: ", div(class = "card-body", "Identifying Real World Evidence (RWE) Needs")),
                       bsCollapsePanel("Phase 2: ", div(class = "card-body",
                                                        "Reviewing and Grading Evidence. This phase assumes you have conducted your 
                                                                                      literature search and have identified your studies of interest from Phase 1.",
                                                        tags$ul(tags$li("Phase 2.1: Rate the quality of each study."),
                                                                tags$li("Phase 2.2: Rate the quality of the body of evidence for each outcome of interest."))
                       )),
                       bsCollapsePanel("phase 3: ", div(class = "card-body", "Making evidenced-based Recommendations."))
            )),
        hr(),
        actionButton(inputId = "beginPhase", label = "Click here when you're ready to begin!")
    )
)