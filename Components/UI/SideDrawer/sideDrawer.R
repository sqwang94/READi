sideDrawerUI <- div(
    div(id = "sidebar", class = "SideDrawer Close",
        div(class = "sideDrawer-header", h2("READi")),
        tags$hr(),
        uiOutput("loginToggleSide")),
    div(id = "side_backdrop", class = "backdrop hidden")
)