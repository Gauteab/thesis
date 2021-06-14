window.addEventListener('load', function () {
    Reveal.configure({
      keyboard: {
        13: 'next', // go to the next slide when the ENTER key is pressed
        27: function() {}, // do something custom when ESC is pressed
        32: null // don't do anything when SPACE is pressed (i.e. disable a reveal.js default binding)
      }
    });
})

