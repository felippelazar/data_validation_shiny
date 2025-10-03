var tooltipTriggerList = [].slice.call(document.querySelectorAll('[data-bs-toggle=\"tooltip\"]'))
    var tooltipList = tooltipTriggerList.map(function (tooltipTriggerEl) {
      return new bootstrap.Tooltip(tooltipTriggerEl)
    })
    
$(document).on('keyup', function(e) {
  if (e.key === 'ArrowUp') {
    var tabEl = $('a[data-value="Edit"]');
    
    if (!tabEl.hasClass('active')) {
      // Tab is not active → show it
      var tab = new bootstrap.Tab(tabEl[0]);
      tab.show();
      
      // Show message to user
      if ($('#arrowup-msg').length === 0) {
        $('body').append('<div id="arrowup-msg" style="position:fixed;top:10px;left:50%;transform:translateX(-50%);background:#ffc; padding:10px 20px; border:1px solid #cc9; border-radius:5px; z-index:9999;">Press again the ArrowUp key to validate (you need to be in the Edit tab)</div>');
        setTimeout(function() {
          $('#arrowup-msg').fadeOut(500, function() { $(this).remove(); });
        }, 3000); // disappears after 3 seconds
      }
      
    } else {
      // Tab is active → trigger Shiny input + button
      Shiny.setInputValue('data_validated_input', true, {priority: 'event'});
      $('#save_edit').click();
    }
  }
});

$(document).on('keyup', function(e) {
  if (e.key === 'ArrowDown') {
    var tabEl = $('a[data-value="Edit"]');
    
    if (!tabEl.hasClass('active')) {
      // Tab is not active → show it
      var tab = new bootstrap.Tab(tabEl[0]);
      tab.show();
      
      // Show message to user
      if ($('#arrowup-msg').length === 0) {
        $('body').append('<div id="arrowup-msg" style="position:fixed;top:10px;left:50%;transform:translateX(-50%);background:#ffc; padding:10px 20px; border:1px solid #cc9; border-radius:5px; z-index:9999;">Press again the ArrowUp key to validate (you need to be in the Edit tab)</div>');
        setTimeout(function() {
          $('#arrowup-msg').fadeOut(500, function() { $(this).remove(); });
        }, 3000); // disappears after 3 seconds
      }
      
    } else {
      // Tab is active → trigger Shiny input + button
      Shiny.setInputValue('data_validated_input', false, {priority: 'event'});
      $('#save_edit').click();
    }
  }
});

$(document).on('keyup', function(e) {
  // ArrowLeft → click the previous 1 button
  if (e.key === 'ArrowLeft') {
    $('#btprevious1').click();
  }

  // ArrowRight → click the next 1 button
  if (e.key === 'ArrowRight') {
    $('#btnext1').click();
  }
});