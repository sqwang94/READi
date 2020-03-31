var width = 0;
$(document).on("shiny:connected", function(e) {
  width = window.innerWidth
  Shiny.onInputChange("width", width);
})

$(window).resize(function(e) {
  width = window.innerWidth;
  Shiny.onInputChange("width", width);
})

$(document).on("click", "#mobile-toggle", function() {
  $("#sidebar").toggleClass("Show Close")
  $("#side_backdrop").toggleClass("hidden")
})

$(document).on("click", "#side_backdrop", function() {
  hideSideBar()
})

$(document).on("click", "#login", function() {
  hideSideBar()
})

function hideSideBar() {
  $("#sidebar").addClass("Close")
  $("#sidebar").removeClass("Show")
  $("#side_backdrop").toggleClass("hidden")
}
