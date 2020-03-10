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
    .then(user => {
      if(!user.user.emailVerified) {
        $("#login_error").html("<p>Your email address has not been verified. Click <a id='resend_verification' href='#'>here</a> to resend a verification email.")
        $(document).off('click', '#resend_verification');
        $(document).on("click", "#resend_verification", () => {
          user.user.sendEmailVerification()
          .then(() => {
            $.alert({
                title: 'Please check your email',
                content: 'A verifiation email has been sent to ' + email,
            });
          })
          .catch((error) => {
            if (error.code === "auth/too-many-requests") {
              $("#login_error").text("we recently sent your a verification email. Please check your email.")
            }
          })
        })
        auth.signOut()
        $("#login_error").removeClass("hidden")
      }
    })
    .catch((error) => {
      let errorText = "Something went wrong."
      if (error.code === "auth/user-not-found" || error.code === "auth/wrong-password") {
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
  if (!user || user.emailVerified) {
    Shiny.setInputValue('auth_user', user);
  }
})

$(document).on("click", "#reset_password", (e) => {
  $.confirm({
    icon: "fas fa-exclamation-triangle",
    title: 'Resetting your password',
    content: '' +
    '<form action="">' +
    '<div class="form-group">' +
    '<label>Please Enter Your Email</label>' +
    '<input type="text" placeholder="example@domain.com" class="reset_email form-control" required />' +
    '<p class="hidden error_message">Please enter a valid Email address</p>' +
    '</div>' +
    '</form>',
    buttons: {
        formSubmit: {
            text: 'Submit',
            btnClass: 'btn-blue',
            action: function () {
                let email = this.$content.find('.reset_email').val();
                if (!email.match("^[^@]+@[^@]+\.[^@]+$")) {
                  this.$content.find('.error_message').removeClass("hidden")
                  this.$content.find('.reset_email').addClass("invalid")
                  return false;
                }
                auth.sendPasswordResetEmail(email).then(() => {
                  $.alert("Please check your email.");
                }).catch((error) => {
                  $.alert({
                    title: 'Error',
                    content: 'Error sending password reset email',
                    type: "orange"
                  });
                })
            }
        },
        cancel: function () {
            //close
        },
    },
    onContentReady: function () {
        // bind to events
        var jc = this;
        this.$content.find('form').on('submit', function (e) {
            // if the user submits the form by pressing enter in the field.
            e.preventDefault();
            jc.$$formSubmit.trigger('click'); // reference the button and click it
        });
    }
  });
})

$(document).on("click", "#submit_register", (e) => {
  e.stopPropagation();
  e.preventDefault();
  const email = $("#register_email").val()
  const password = $("#register_password").val()
  const confirm = $("#register_password_verify").val()
  if (validateFormRegister(email, password, confirm)) {
    auth.createUserWithEmailAndPassword(email, password).then((user) => {
      user.user.sendEmailVerification()
      .then(response => {
        $.alert({
            title: 'Success!',
            content: 'You have successfully registered! Please verify your email address.',
        });
        $("#sign_in_panel").removeClass("hidden")
        $("#register_panel").addClass("hidden")
      })
      .catch((error) => {
        $("#register_error").text("We couldn't send verification email to the email address")
        $("#register_error").removeClass("hidden")
      })
    })
    .catch((error) => {
      let errorText = "Something went wrong."
      if (error.code === "auth/email-already-in-use") {
        errorText = error.message
      }
      $("#register_error").text(errorText)
      $("#register_error").removeClass("hidden")
    })
  }
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
  $("register_error").addClass("hidden")
  $("#register_error").text("")
}

//** Reset all values of input fields. */
function resetValues() {
  $(".auth-input").each(function() {
    $(this).val("")
  })
}
