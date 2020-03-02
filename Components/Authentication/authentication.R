
source("Components/Authentication/login.R")
source("Components/Authentication/register.R")

authenticationUI <- function(id) {
    ns <- NS(id)
    return(
        div(
            loginUI(ns("login")),
            registerUI(ns("register")),
            div(id = "backdrop", class = "modal-backdrop hidden")
        )
    )
}

authentication <- function(input, output, session) {
    callModule(login, "login")
    callModule(register, "register")
    
    
    
    ##### Switch Views ------------------
    # if user click link to register, go to register view
    
    
  
    
    # # switch between auth sign in/registration and app for signed in user
    # observeEvent(session$userData$current_user(), {
    #     current_user <- session$userData$current_user()
    #     
    #     if (is.null(current_user)) {
    #         shinyjs::show("sign_in_panel")
    #         shinyjs::hide("main")
    #         shinyjs::hide("verify_email_view")
    #     } else {
    #         shinyjs::hide("sign_in_panel")
    #         shinyjs::hide("register_panel")
    #         
    #         if (current_user$emailVerified == TRUE) {
    #             shinyjs::show("main")
    #         } else {
    #             shinyjs::show("verify_email_view")
    #         }
    #         
    #     }
    #     
    # }, ignoreNULL = FALSE)
}