# dropdown button UI for logged in user
loginDropdown <- dropdownButton(
    inputId = "account_dropdown",
    circle = FALSE,
    right = TRUE,
    label = "account",
    actionButton(
        inputId = "my_account",
        class = "login_dropdown_item",
        "My account"
    ),
    actionButton(
        inputId = "my_progress",
        type = "button",
        class = "login_dropdown_item",
        "My progress"
    ),
    tags$button(
        id = "signout",
        type = "button",
        class = "login_dropdown_item",
        "Sign out"
    )
)
