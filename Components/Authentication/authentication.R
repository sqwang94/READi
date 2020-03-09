source("Components/Authentication/login.R")
source("Components/Authentication/register.R")

# UI function for authentication modal
authenticationUI <- function(id) {
    ns <- NS(id)
    return(
        div(
            div(
              id = "auth_panel",
                loginUI(ns("login")),
                registerUI(ns("register"))
            ),
            div(
              id = "backdrop", class = "modal-backdrop hidden"))
        )
    
}

# server function for authentication modal
authentication <- function(input, output, session) {
    callModule(login, "login")
    callModule(register, "register")
    
    
}