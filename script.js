$(document).ready(function() {
  $('#main_title').click(function() {
    Shiny.setInputValue('main_title_click', Math.random());
  });
});
