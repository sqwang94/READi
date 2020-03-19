# dropdown button UI for logged in user
loginDropdown <- dropdownButton(
    inputId = "account_dropdown",
    circle = FALSE,
    right = TRUE,
    label = "account",
    tags$button(
        id = "my_account",
        type = "button",
        class = "login_dropdown_item",
        "My account"
    ),
    tags$button(
        id = "my_progress",
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