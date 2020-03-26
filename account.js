/**
 * Retrieve eval history data from firebase and update the eval history page
 * @param {String} uid - id of the user
 */
shinyjs.updateAccount = function(uid) {
  shinyjs.showSpinner()
  $("#history").empty()
  $.get('https://readi-dcf98.firebaseio.com/' + uid + '.json').done(function(data) {
    update(uid, data)
    shinyjs.hideSpinner()
  }).fail(function(error) {
    shinyjs.hideSpinner()
    console.log(error)
  })
}

/** Clear the evalutaion history page. */
shinyjs.clearAccount = function() {
  $("#history").empty()
}

/**
 * Save the current state to firebase and replace the previous state if exists
 * @param {Array} stateData - data of the saved state
 */
shinyjs.saveState = function(stateData) {
  shinyjs.showSpinner()
  var session = stateData[2]
  // Delete the previous state if session exists
  if (session) {
    $.ajax({
    url: 'https://readi-dcf98.firebaseio.com/' + stateData[1] + '/' + session + '.json',
    type: 'DELETE'
    });
  }
  var d = new Date($.now());
  var time = (d.getMonth() + 1) + "-" + d.getDate() + "-" + d.getFullYear() + " " + d.getHours() + ":" + d.getMinutes()+ ":" + d.getSeconds()
  var data = {
    url: stateData[0],
    time: time
  }
  // Add the new saved state
  $.post('https://readi-dcf98.firebaseio.com/' + stateData[1] + '.json', JSON.stringify(data)).done(function(data) {
    Shiny.setInputValue('current_session', data.name);
    shinyjs.hideSpinner()
  }).fail(function(error) {
    console.log(error)
  })
}

/**
 * Update the Evaluation history page.
 * @param {Object} data - data retrieved from firebase
 */
function update(uid, data) {
  if (!jQuery.isEmptyObject(data)) {
    $.each(data, function(key, value) {
      makeEvalEntry(uid, key, value)
    })
  } else {
    $("#history").text("No previous evaluation")
  }
}

/**
 * Create an evaluation entry in the eval history page
 * @param {String} key - id of the entry
 * @param {Object} data - data corresponding to the entry
 */
function makeEvalEntry(uid, key, data) {
  $("#history").append(
    $('<div/>',
      {'class': 'evalEntry'}
    ).append(
      $('<a/>', {'class': 'link', 'href': data.url + '&session=' + key, text: "Click Here to Continue"})
    ).append(
      $('<span/>', {text: data.url})
    ).append(
      $('<span/>', {text: "Last modified: " + data.time})
    ).append(
      $('<button/>', {text: "delete"}).click(function() {
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

function deleteEntry(uid, id) {
  $.ajax({
    url: 'https://readi-dcf98.firebaseio.com/' + uid + '/' + id + '.json',
    type: 'DELETE'
  }).done(function(data) {
    shinyjs.updateAccount(uid)
  }).fail(function(error) {
    shinyjs.hideSpinner()
  })
}
