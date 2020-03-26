let session = null

shinyjs.updateAccount = function(uid) {
  $("#history").empty()
  $.get('https://readi-dcf98.firebaseio.com/' + uid + '.json').done(function(data) {
    update(data)
  }).fail(function(error) {
    console.log(error)
  })
}

shinyjs.clearAccount = function() {
  $("#history").empty()
}

shinyjs.saveState = function(stateData) {
  console.log(stateData)
  if (session) {
    $.ajax({
    url: 'https://readi-dcf98.firebaseio.com/' + stateData[1] + '/' + session + '.json',
    type: 'DELETE'
    });
  }
  const data = {
    url: stateData[0]
  }
  $.post('https://readi-dcf98.firebaseio.com/' + stateData[1] + '.json', JSON.stringify(data)).done(function(data) {
    session = data.name
  }).fail(function(error) {
    console.log(error)
  })
}

function update(data) {
  if (data != null && Object.keys(data).length > 0) {
    let keys = Object.keys(data)
    for (i = 0; i < keys.length; i++) {
      makeEvalEntry(keys[i], data[keys[i]])
    }
  }else {
    $("#history").text("No previous evaluation")
  }
}

function makeEvalEntry(key, data) {
  $("#history").append(
    $('<div/>', {'class': 'evalEntry'}).append(
      $('<a/>', {'class': 'link', 'id': key, 'href': data.url + '&session=' + key, text: "Click Here to Continue"})
    ).append(
      $('<span/>', {text: data.url})
    )
  )
}
