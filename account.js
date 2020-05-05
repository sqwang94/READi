var entryPerPage = 3
var databaseURL = 'https://uw-readi.firebaseio.com/'
var homeURL = 'http://34.210.151.228:3838/READi/'
var phases = ["Phase 1: Identify Real World Evidence",
              "Phase 2: Reviewing and Grading of Evidence",
              "Phase 3: Summarizing The Literature",
              "Phase 4: Making an Evidence-Based Recommendation",
              "Phase 5: Final Summary"]



/**
 * Retrieve eval history data from firebase and update the eval history page
 * @param {Array/String} uid - data of the user and entry indexes
 */
shinyjs.updateAccount = function(uid) {
  shinyjs.showSpinner()
  auth.currentUser.getIdToken(true).then(function(idToken) {
    $.get(databaseURL + uid + '.json?auth=' + idToken).done(function(data) {
      update(uid, data, 0)
      shinyjs.hideSpinner()
    }).fail(function(error) {
      shinyjs.hideSpinner()
      console.log(error)
    })
  })
}

/** Clear the evalutaion history page. */
shinyjs.clearAccount = function() {
  $("#history").empty()
  $("#history-nav").empty()
}

/**
 * Check if the current session is expired. Redirect to homepage if true.
 * @param {Array} state - data of the state, including user id and session id
 */
shinyjs.checkSession = function(state) {
  auth.currentUser.getIdToken(true).then(function(idToken) {
    $.get(databaseURL + state[0] + "/" + state[1] + '.json?auth=' + idToken).done(function(data) {
      if (!data) {
        $.alert({
          title: 'Session expired',
          content: 'You will be redirected to homepage',
          autoClose: 'OK|5000',
          draggable: false,
          buttons: {
            OK: {
              action: function () {
                window.location.replace(homeURL)
              }
            }
          }
        });
      }
    }).fail(function(error) {
      console.log(error)
    })
  })
}

/**
 * Save the current state to firebase and replace the previous state if exists
 * @param {Array} stateData - data of the saved state
 */
shinyjs.saveState = function(stateData) {
  var session = stateData[2]
  var d = new Date($.now())
  var time = (d.getMonth() + 1) + "-" + d.getDate() + "-" + d.getFullYear() + " " + d.getHours() + ":" + d.getMinutes() + ":" + (d.getSeconds() < 9 ? "0" : "") + d.getSeconds()
  if (session) {
    shinyjs.showSpinner()
    var data = {
      url: stateData[0],
      time: time,
      phase: stateData[3]
    }
    auth.currentUser.getIdToken(true).then(function(idToken) {
      $.ajax({
        url: databaseURL + stateData[1] + '/' + session + '.json?auth=' + idToken,
        type: 'PATCH',
        data: JSON.stringify(data)
      }).done(function(data){
        shinyjs.hideSpinner()
      }).fail(function(error) {
        console.log(error)
      })
    })
  } else {
    $.confirm({
      title: 'Save your progress',
      content: '' +
      '<form action="" class="formName">' +
      '<div class="form-group">' +
      '<label>Please enter a name</label>' +
      '<input type="text" placeholder="Enter your project name" class="name form-control" required />' +
      '<p id="project-name-error" class="error_message Fixed hidden">Project name is required</p>' +
      '</div>' +
      '</form>',
      buttons: {
          formSubmit: {
              text: 'Submit',
              btnClass: 'btn-blue',
              action: function () {
                  var name = this.$content.find('.name').val();
                  if(!name){
                      $('#project-name-error').removeClass("hidden")
                      this.$content.find('.name').addClass("invalid")
                      return false;
                  }
                  shinyjs.showSpinner()
                  var data = {
                    name: name,
                    url: stateData[0],
                    time: time,
                    phase: stateData[3]
                  }
                  // Add the new saved state
                  auth.currentUser.getIdToken(true).then(function(idToken) {
                    $.post(databaseURL + stateData[1] + '.json?auth=' + idToken, JSON.stringify(data)).done(function(data) {
                      Shiny.setInputValue('current_session', data.name);
                      shinyjs.hideSpinner()
                    }).fail(function(error) {
                      console.log(error)
                    })
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
  }
}

/**
 * Update the Evaluation history page.
 * @param {String} uid - id of the user
 * @param {Object} data - data retrieved from firebase
 * @param {number} start - start index of the entries
 */
function update(uid, data, start) {
  shinyjs.clearAccount()
  data = data == null ? {} : data
  var keys = Object.keys(data)
  if (!jQuery.isEmptyObject(data)) {
    keys.forEach(function(key, index, array) {
      if (index >= start && index < start + entryPerPage) {
        makeEvalEntry(uid, array[index], data[key])
      }
    })
    if (keys.length > entryPerPage) {
      makeEvalNav(uid, data, start)
    }
  } else {
    $("#history").text("No previous evaluation")
  }
}

// Create a callback function as onclick listener for eval nav item
function createCallbackNav(uid, data, i) {
  return function() {
    update(uid, data, i)
  }
}

/**
 * Create the eval history navigatio div
 * @param {String} uid - id of the user
 * @param {Object} data - data retrieved from firebase
 * @param {number} start - start index of the entries
 */
function makeEvalNav(uid, data, start) {
  var length = Object.keys(data).length
  $("#history-nav").append("<ul/>")
  $("#history-nav ul").append(
    $('<li/>', {"class": start === 0 ? "eval-nav-item-disabled" : "eval-nav-item", click: function() {
      if (start > 0) {
        update(uid, data, start - entryPerPage)
      }
    }}).append(
      $("<a/>", {text: "prev"})
    )
  )
  for (i = 0; i < Math.ceil(length / entryPerPage); i++) {
    var className = "eval-nav-item"
    if (i === Math.floor(start / entryPerPage)) {
      className += " Active"
    }
    $("#history-nav ul").append(
      $('<li/>', {"class": className, click: createCallbackNav(uid, data, i * entryPerPage)}).append(
        $("<a/>", {text: i + 1})
      )
    )
  }
  $("#history-nav ul").append(
    $('<li/>', {"class": start + entryPerPage >= length ? "eval-nav-item-disabled" : "eval-nav-item", click: function() {
      if (start + entryPerPage < length) {
        update(uid, data, start + entryPerPage)
      }
    }}).append(
      $("<a/>", {text: "next"})
    )
  )
}

/**
 * Create a nav button in the eval history navigation
 * @param {String} text - text of the button
 */
function makeNavButton(text) {
  return($('<li/>').append(
    $("<a/>", {text: text})
  ))
}

/**
 * Create an evaluation entry in the eval history page
 * @param {String} uid - id of the user
 * @param {String} key - id of the entry
 * @param {Object} data - data corresponding to the entry
 */
function makeEvalEntry(uid, key, data) {
  $("#history").append(
    $('<div/>',
      {'class': 'evalEntry'}
    ).append(
      $('<div/>',
        {"class": "evalHeader"}
      ).append(
        $('<h3/>', {text: data.name})
      ).append(
        $('<i/>', {"class" : "fa fa-edit", click: function() {
          $.confirm({
            title: 'Edit Name',
            content: '' +
            '<form action="" class="formName">' +
            '<div class="form-group">' +
            '<label>Please enter a new name</label>' +
            '<input type="text" placeholder="Enter your project name" class="name form-control" required />' +
            '<p id="project-name-error" class="error_message Fixed hidden">Project name is required</p>' +
            '</div>' +
            '</form>',
            buttons: {
                formSubmit: {
                    text: 'Submit',
                    btnClass: 'btn-blue',
                    action: function () {
                      var name = this.$content.find('.name').val();
                      if(!name){
                        $('#project-name-error').removeClass("hidden")
                        this.$content.find('.name').addClass("invalid")
                        return false;
                      }
                      editName(uid, key, name)
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
        }})
      )
    ).append(
      $('<hr>')
    ).append(
      $('<div/>',
        {'class': 'evalInfo'}
      ).append(
        $('<p/>'
        ).append(
          $('<strong/>', {text: "Progress: "})
        ).append(
          $('<span/>', {text: phases[data.phase - 1]})
        )
      ).append(
        $('<p/>'
        ).append(
          $('<strong/>', {text: "Last modified: "})
        ).append(
          $('<span/>', {text: data.time})
        )
      ).append(
        $('<div/>',
          {'class': 'evalActions'}
        ).append(
          $('<a/>', {'href': data.url + '&session=' + key, text: "Click Here to Continue"})
        ).append(
          $('<button/>', {'class': 'btn', text: "delete"}).click(function() {
            $.confirm({
              icon: "fas fa-exclamation-triangle",
              title: 'Are you sure?',
              content: 'Do you really want to delete this entry? This process cannot be undone.',
              buttons: {
                cancel: function () {
                },
                confirm: function () {
                  shinyjs.showSpinner()
                  deleteEntry(uid, key)
                },
              }
            })
          })
        )
      )
    )
  )
}

/** Show spinner UI. */
shinyjs.showSpinner = function() {
  $("#spinner").removeClass("hidden")
  $("#spinner_backdrop").removeClass("hidden")
}

/** Hide spinner UI. */
shinyjs.hideSpinner = function() {
  $("#spinner").addClass("hidden")
  $("#spinner_backdrop").addClass("hidden")
}

/** Refresh the page and start a new session. */
shinyjs.newSession = function() {
  window.location.replace(homeURL + "?new=true")
}

/**
 * Delete an evaluation entry in the eval history page
 * @param {String} uid - id of the user
 * @param {String} id - id of the entry
 */
function deleteEntry(uid, id) {
  auth.currentUser.getIdToken(true).then(function(idToken) {
    $.ajax({
      url: databaseURL + uid + '/' + id + '.json?auth=' + idToken,
      type: 'DELETE'
    }).done(function(data) {
      shinyjs.updateAccount(uid)
    }).fail(function(error) {
      shinyjs.hideSpinner()
    })
  })
}

/**
 * Edit the name of the evaluation entry.
 * @param {String} uid - id of the user
 * @param {String} id - id of the entry
 * @param {String} name - new name of the entry
 */
function editName(uid, id, name) {
  shinyjs.showSpinner()
  var data = {
    name: name
  }
  auth.currentUser.getIdToken(true).then(function(idToken) {
    $.ajax({
      url: databaseURL + uid + '/' + id + '.json?auth=' + idToken,
      type: 'PATCH',
      data: JSON.stringify(data)
    }).done(function(data){
      shinyjs.updateAccount(uid)
    }).fail(function(error) {
      console.log(error)
    })
  })
}
