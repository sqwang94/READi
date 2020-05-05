# UI function for login panel
loginUI <- function(id) {
    ns <- NS(id)
    return(
        div(id = "sign_in_panel",
            h1("Sign In"),
            div(id = "login_error", class = "error_message hidden"),
            div(class = "form-group",
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
            div(class = "form-group",
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
                )
            )
        )
    )
}

# server function for login panel
login <- function(input, output, session) {
    observeEvent(input$go_to_register, {
        removeClass(selector = "#register_panel", class = "hidden")
        addClass(selector = "#sign_in_panel", class = "hidden")
    }, ignoreInit = TRUE)
}