/*input_typing is the distinct label for the typing area, I need to make the keypress function 
* happen only in the inputtyping labeled element so that characters and 
*/
  
  js_typing_times = [];
  
  js_chars = [];
  
  update_typing_times = function(){
    Shiny.setInputValue("typing_times",js_typing_times);
  };
  
  update_chars = function(){
    Shiny.setInputValue("chars",js_chars);
  };
  update_finalTimeStamp = function(){
    Shiny.setInputValue("finalTimeStamp",lastTimeStamp);
  };
  /*
    this function will only set a new date when user clicks the text area but, also will 
  have set a new date at the opening of window and gotten another date when right user types a key in the
  username box
  */
    $(document).ready(function(){
      
      $('#input_typing').mouseup(function() {
        d= new Date();
       
      }); 
       $('#input_typing').keypress(function(){
        last_keystroke = d.getTime();
        d= new Date();
         js_typing_times.push(d.getTime()-last_keystroke);
        if (event.which === null)
          char= String.fromCharCode(event.keyCode);    
        else if (event.which !== 0 && event.charCode !== 0)
          char= String.fromCharCode(event.which);
        
        js_chars.push(char);
        lastTimeStamp=d.getTime();
        });
      
    });
  