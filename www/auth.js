$(document).on("click", "#submit_sign_in", () => {
  const email = $("#email").val()
  const password = $("#password").val()
  validateFormLogin(email, password)
})

$(document).on("click", "#submit_register", () => {
  const email = $("#register_email").val()
  const password = $("#register_password").val()
  const confirm = $("#register_password_verify").val()
  validateFormRegister(email, password, confirm)
})

$(document).on("click", "#login", () => {
  $("#backdrop").removeClass("hidden")
  $("#sign_in_panel").addClass("Show")
  $("#register_panel").addClass("Show")
})

$(document).on("click", "#backdrop", () => {
  $("#backdrop").addClass("hidden")
  $("#sign_in_panel").removeClass("Show")
  $("#register_panel").removeClass("Show")
})

/**
 * Validate the inputs of the login form. Displays error if invalid input.
 * @param {string} email - email input
 * @param {string} password - password input
 * @returns {boolean} true if login form input is valid
 */
function validateFormLogin(email, password) {
  removeWarnings()
  let valid = true
  if (!email.match("^[^@]+@[^@]+\.[^@]+$")) {
    $("#email").addClass("invalid")
    $("#email").next().removeClass("hidden")
    valid = false
  }
  if (password.length < 8) {
    $("#password").addClass("invalid")
    $("#password").next().removeClass("hidden")
    valid = false;
  }
  return valid
}

/**
 * Validate the inputs of the register form. Displays error if invalid input.
 * @param {string} email - email input
 * @param {string} password - password input
 * @param {string} confirm - confirm password input
 * @returns {boolean} true if register form input is valid
 */
function validateFormRegister(email, password, confirm) {
  removeWarnings()
  let valid = true
  if (!email.match("^[^@]+@[^@]+\.[^@]+$")) {
    $("#register_email").addClass("invalid")
    $("#register_email").next().removeClass("hidden")
    valid = false
  }
  if (password.length < 8) {
    $("#register_password").addClass("invalid")
    $("#register_password").next().removeClass("hidden")
    valid = false;
  }
  if (confirm != password) {
    $("#register_password_verify").addClass("invalid")
    $("#register_password_verify").next().removeClass("hidden")
    valid = false;
  }
  return valid
}

//** Removes all warning messages on the input fields. */
function removeWarnings() {
  $(".auth-input").each(function() {
    $(this).removeClass("invalid")
    $(this).next().addClass("hidden")
  })
}
