# dropdown button UI for logged in user
loginDropdown <- dropdownButton(
    inputId = "account_dropdown",
    circle = FALSE,
    right = TRUE,
    label = "account",
    actionButton(
        inputId = "my_account",
        class = "Account login_dropdown_item",
        "My account"
    ),
    actionButton(
        inputId = "my_progress",
        type = "button",
        class = "Progress login_dropdown_item",
        "My progress"
    ),
    tags$button(
        id = "signout",
        type = "button",
        class = "Logout login_dropdown_item",
        "Sign out"
    )
)

# sidebar login dropdown UI for logged in user
loginDropdownSide <- div(
    actionButton(
        inputId = "my_account-side",
        class = "Account SideDrawerItem",
        "My account"
    ),
    actionButton(
        inputId = "my_progress_side",
        type = "button",
        class = "Progress SideDrawerItem",
        "My progress"
    ),
    tags$button(
        id = "signout-side",
        type = "button",
        class = "Logout SideDrawerItem",
        "Sign out"
    )
)
