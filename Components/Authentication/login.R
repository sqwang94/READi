loginUI <- function(id) {
    ns <- NS(id)
    return(
        div(
            id = "sign_in_panel",
            class = "auth_panel",
            h1("Sign In"),
            div(
                class = "form-group",
                tags$label(
                    tagList(icon("envelope"), "Email"),
                    `for` = "email"
                ),
                tags$input(
                    id = "email",
                    type = "text",
                    class = "form-control auth-input",
                    value = "",
                    placeholder = "Please enter your email"
                ),
                div(class = "hidden error_message", 
                    icon("exclamation-circle"),
                    span(class = "error_message", "Please enter a valid Email")
                )
            ),
            div(
                class = "form-group",
                tags$label(
                    tagList(icon("unlock-alt"), "Password"),
                    `for` = "password"
                ),
                tags$input(
                    id = "password",
                    type = "password",
                    class = "form-control auth-input",
                    value = "",
                    placeholder = "Please enter your password"
                ),
                div(class = "hidden error_message", 
                    icon("exclamation-circle"),
                    span(class = "error_message", "Please enter a valid password")
                )
            ),
            br(),
            div(class = "auth-submit",
                tags$button(
                    id = "submit_sign_in",
                    type = "button",
                    class = "btn btn-primary btn-lg",
                    "Sign In"
                ),
                br(),
                hr(),
                actionLink(
                    ns("go_to_register"),
                    "Not a member? Register!"
                ),
                br(),
                br(),
                tags$a(
                    id = "reset_password",
                    href = "#",
                    "Forgot your password?"
                ),
                br(),
                actionLink(
                    ns("guest"),
                    "Continue as guest"
                ),
            )
        )
    )
}

login <- function(input, output, session) {
    observeEvent(input$go_to_register, {
        shinyjs::show(selector = "#register_panel", anim = TRUE, animType = "fade", time = 0.05)
        shinyjs::hide(selector = "#sign_in_panel")
    }, ignoreInit = TRUE)
}