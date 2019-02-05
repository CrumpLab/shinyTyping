js_typing_times = [];
d= new Date();

update_typing_times = function(){
  Shiny.setInputValue("typing_times",js_typing_times);
};


$(document).ready(function(){
 $('#input_typing').mouseup(function() { d= new Date(); }); 
});

$(function(){
		/*
		 * this swallows backspace keys on any non-input element.
		 * stops backspace -> back
		 */
		var rx = /INPUT|SELECT|TEXTAREA/i;
	
		$(document).bind({
			keypress: function(e){
			  last_keystroke = d.getTime();
			  d= new Date();
			  js_typing_times.push(d.getTime()-last_keystroke);
			}
			
		});
});