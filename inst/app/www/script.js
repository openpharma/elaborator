$(document).on("shiny:connected", function(e) {
    Shiny.onInputChange("innerWidth", window.innerWidth);
});

$(window).resize(function(e) {
    Shiny.onInputChange("innerWidth", window.innerWidth);
});

$('body').addClass('sidebar-mini');

$(document).ready(function() {
    $("header").find("nav").append('<h4> A New Perspective on Laboratory Data </h4>');
})