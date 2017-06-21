$(document).ready(function(){

  var button = $('.timeToggle a');
  var chart = $('.figure');
  var yearChart = $('.year');
  var monthChart = $('.month');
  var weekChart = $('.week');

  var toggle = function(){
    if($('#yearButton.active').length > 0){
      yearChart.show();
    }
    if($('#monthButton.active').length > 0){
      monthChart.show();
    }
    if($('#weekButton.active').length > 0){
      weekChart.show();
    }
  };

  button.on('click', function(){
    button.removeClass('active');
    chart.hide();
    $(this).addClass('active');
    toggle();
  });

  toggle();

});

