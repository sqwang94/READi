authentication <- function(input, output, session) {
    
    ##### Switch Views ------------------
    # if user click link to register, go to register view
    observeEvent(input$go_to_register, {
        shinyjs::show("register_panel", anim = TRUE, animType = "fade")
        shinyjs::hide("sign_in_panel")
    }, ignoreInit = TRUE)
    
    observeEvent(input$go_to_sign_in, {
        shinyjs::hide("register_panel")
        shinyjs::show("sign_in_panel", anim = TRUE, animType = "fade")
    }, ignoreInit = TRUE)
    
    # switch between auth sign in/registration and app for signed in user
    observeEvent(session$userData$current_user(), {
        current_user <- session$userData$current_user()
        
        if (is.null(current_user)) {
            shinyjs::show("sign_in_panel")
            shinyjs::hide("main")
            shinyjs::hide("verify_email_view")
        } else {
            shinyjs::hide("sign_in_panel")
            shinyjs::hide("register_panel")
            
            if (current_user$emailVerified == TRUE) {
                shinyjs::show("main")
            } else {
                shinyjs::show("verify_email_view")
            }
            
        }
        
    }, ignoreNULL = FALSE)