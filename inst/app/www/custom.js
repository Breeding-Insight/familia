$(document).ready(function() {
  // Use event delegation for dynamically created elements
  $(document).on('click', '.card-header', function(e) {
    // Don't trigger if clicking on the actual collapse button
    if (!$(e.target).closest('.card-tools').length) {
      // Find the collapse button in this header and trigger click
      var collapseBtn = $(this).find('[data-card-widget="collapse"]');
      if (collapseBtn.length > 0) {
        collapseBtn.trigger('click');
      }
    }
  });
  
  // Function to apply styles to card headers (both existing and new ones)
  function styleCardHeaders() {
    $('.card-header').css('cursor', 'pointer');
    $('.card-tools').css('cursor', 'default');
  }
  
  // Apply styles initially
  styleCardHeaders();
  
  // Watch for new elements and apply styles
  var observer = new MutationObserver(function(mutations) {
    mutations.forEach(function(mutation) {
      if (mutation.addedNodes.length > 0) {
        styleCardHeaders();
      }
    });
  });
  
  // Start observing
  observer.observe(document.body, {
    childList: true,
    subtree: true
  });
});