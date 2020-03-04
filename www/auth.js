const config = {
  apiKey: "AIzaSyAhPODofJbMMXB3EmBg53l9T9klQPjpje4",
  authDomain: "readi-dcf98.firebaseapp.com",
  projectId: "readi-dcf98"
}

firebase.initializeApp(config)
const auth = firebase.auth()

$(document).on("click", "#submit_sign_in", () => {
  const email = $("#email").val()
  const password = $("#password").val()
  if (validateFormLogin(email, password)) {
    auth.signInWithEmailAndPassword(email, password)
    .catch((error) => {
      let errorText = "Something went wrong."
      if (error.code === "auth/user-not-found") {
        errorText = "The username and password you entered did not match our records. Please double-check and try again."
      }
      $("#login_error").text(errorText)
      $("#login_error").removeClass("hidden")
    })
  }
})

/**
 * when user signs in or out send the info about that user to Shiny as
 * a Shiny input `input$auth_user`
 */
auth.onAuthStateChanged((user) => {
  Shiny.setInputValue('auth_user', user);
  console.log(user)
})

$(document).on("click", "#submit_register", (e) => {
  e.stopPropagation();
  e.preventDefault();
  const email = $("#register_email").val()
  const password = $("#register_password").val()
  const confirm = $("#register_password_verify").val()
  validateFormRegister(email, password, confirm)
})

$(document).on("click", "#login", (e) => {
  e.stopPropagation();
  e.preventDefault();
  console.log("clicked login")
  $("#backdrop").removeClass("hidden")
  $("#sign_in_panel").removeClass("hidden")
  $("#auth_panel").addClass("Show")
  $("#register_panel").addClass("hidden")
})

$(document).on("mouseup", "#signout", (e) => {
  e.stopPropagation();
  e.preventDefault();
  auth.signOut()
})

$(document).on("click", "#backdrop", (e) => {
  e.stopPropagation();
  e.preventDefault();
  removeWarnings()
  resetValues()
  $("#backdrop").addClass("hidden")
  $("#auth_panel").removeClass("Show")
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
  $("login_error").addClass("hidden")
  $("#login_error").text("")
}

//** Reset all values of input fields. */
function resetValues() {
  $(".auth-input").each(function() {
    $(this).val("")
  })
}
