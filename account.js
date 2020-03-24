shinyjs.updateAccount = function() {
  $.get('https://readi-dcf98.firebaseio.com/test1.json').done(function(data) {
    console.log(data)
    update(Object.values(data))
  }).fail(function(error) {
    console.log(error)
  })
}

shinyjs.clearAccount = function() {
  console.log("account cleared")
}

function update(data) {
  let numOfEval = data.length
  if (numOfEval === 0) {
    $("#history").text("No previous evaluation")
  } else {
    for (i = 0; i < numOfEval; i++) {
      console.log(data[i])
      $("#history").append(makeEvalEntry(data[i]))
    }
  }
}

function makeEvalEntry(data) {
  let entry = $("<div></div>");
  entry.addClass("evalEntry")
  return entry
}
