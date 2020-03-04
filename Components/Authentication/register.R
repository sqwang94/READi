# UI function for register panel
registerUI <- function(id) {
    ns <- NS(id)
    return(
        div(id = "register_panel",
            h1("Register"),
            div(class = "form-group",
                tags$label(
                    tagList(icon("envelope"), "Email"),
                    `for` = "register_email"
                ),
                tags$input(
                    id = "register_email",
                    type = "text",
                    class = "form-control auth-input",
                    value = "",
                    placeholder = "example@domain.com"
                ),
                div(class = "hidden error_message", 
                    icon("exclamation-circle"),
                    span(class = "error_message", "Please enter a valid Email")
                )
            ),
            div(class = "form-group",
                tags$label(
                    tagList(icon("unlock-alt"), "Password"),
                    `for` = "register_password"
                ),
                tags$input(
                    id = "register_password",
                    type = "password",
                    class = "form-control auth-input",
                    value = "",
                    placeholder = "Please enter your password"
                ),
                div(class = "hidden error_message", 
                    icon("exclamation-circle"),
                    span(class = "error_message", "Please enter a password of at least 8 characters")
                )
            ),
            div(class = "form-group",
                tags$label(
                    tagList(icon("unlock-alt"), "verify password"),
                    `for` = "register_password_verify"
                ),
                tags$input(
                    id = "register_password_verify",
                    type = "password",
                    class = "form-control auth-input",
                    value = "",
                    placeholder = "Please confirm your password"
                ),
                div(class = "hidden error_message", 
                    icon("exclamation-circle"),
                    span(class = "error_message", "Passwords do not match")
                )
            ),
            br(),
            div(class = "auth-submit",
                tags$button(id = "submit_register",
                            type = "button",
                            class = "btn btn-primary btn-lg",
                            "Register"
                ),
                br(),
                hr(),
                actionLink(
                    ns("go_to_sign_in"),
                    "Already a member? Sign in!"
                )
            )
        )
    )
}
  
# server function for register panel
register <- function(input, output, session) {
    observeEvent(input$go_to_sign_in, {
        addClass(selector = "#register_panel", class = "hidden")
        removeClass(selector = "#sign_in_panel", class = "hidden")
    }, ignoreInit = TRUE)
}