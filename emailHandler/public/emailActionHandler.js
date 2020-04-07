const homeURL = "54.213.15.223:3838/READi/"

document.addEventListener('DOMContentLoaded', function() {
  let mode = getParameterByName('mode');
  let actionCode = getParameterByName('oobCode');
  let continueUrl = getParameterByName('continueUrl');
  var lang = getParameterByName('lang') || 'en';

  const config = {
    apiKey: "AIzaSyAhPODofJbMMXB3EmBg53l9T9klQPjpje4",
    authDomain: "readi-dcf98.firebaseapp.com",
    projectId: "readi-dcf98"
  };

  const app = firebase.initializeApp(config);
  const auth = app.auth();

  // Handle the user management action.
  switch (mode) {
    case 'resetPassword':
      // Display reset password handler and UI.
      handleResetPassword(auth, actionCode);
      break;
    case 'verifyEmail':
      // Display email verification handler and UI.
      handleVerifyEmail(auth, actionCode);
      break;
    default:
      $("#error").show();
  }
}, false);

/**
 * Validate the inputs of the password reset form. Displays error if invalid input.
 * @param {string} password - password input
 * @param {string} confirm - confirm password input
 * @returns {boolean} true if reset form input is valid
 */
function validateFormReset(password, confirm) {
  removeWarnings()
  let valid = true
  if (password.length < 8) {
    $("#password").addClass("invalid")
    $("#password").next().removeClass("hidden")
    valid = false
  } else if (confirm != password) {
    $("#confirm_password").addClass("invalid")
    $("#confirm_password").next().removeClass("hidden")
    valid = false
  }
  return valid
}

//** Removes all warning messages on the input fields. */
function removeWarnings() {
  $("input").each(function() {
    $(this).removeClass("invalid")
    $(this).next().addClass("hidden")
  })
  $("error").text("")
}

/**
 * Retrieve the query parameter from the URL.
 * @param {string} name - query name
 * @returns {string} query value
 */
function getParameterByName(name) {
  const urlParams = new URLSearchParams(window.location.search)
  return urlParams.get(name) ? urlParams.get(name) : ""
}

function handleVerifyEmail(auth, actionCode) {
  // Try to apply the email verification code.
  auth.applyActionCode(actionCode).then(function(resp) {
    $("#title").text("Verify email address")
    $("#email_verified").removeClass("hidden")
    $(document).on("click", "#redirect", () => {
      window.location.replace(homeURL);
    })
  }).catch(function(error) {
    $("#error").show();
  });
}

function handleResetPassword(auth, actionCode) {
  // Verify the password reset code is valid.
  auth.verifyPasswordResetCode(actionCode).then(function(email) {
    let accountEmail = email
    $("#title").text("Resetting password")
    $("#reset_password").removeClass("hidden")
    $(document).on("click", "#submit_reset", (e) => {
      e.stopPropagation();
      e.preventDefault();
      const password = $("#password").val()
      const confirm = $("#confirm_password").val()
      if (validateFormReset(password, confirm)) {
        auth.confirmPasswordReset(actionCode, password).then(function(resp) {
          // Password reset has been confirmed and new password updated.
          $(document).on("click", "#redirect_reset", () => {
            window.location.replace(homeURL);
          })
          $("#resetSuccess").modal("show")
          $("#title").text("")
          $("#reset_password").addClass("hidden")
        }).catch(function(error) {
          $("#error_reset").text("Something went wrong. Please try again.")
          $("#error_reset").removeClass("hidden")
        });
      }
    })
  }).catch(function(error) {
    $("#error").show();
  });
}
