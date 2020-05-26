function setColorTheme(theme) {
  if (theme == 'dark') {
    document.body.classList.add('dark');
  } else {
    document.body.classList.remove('dark');
  }

  // Swap the button text
  let other = (theme == 'dark') ? 'Light' : 'Dark';
  document.getElementsByClassName('dark-mode')[0].innerHTML = other + ' Mode';

  // Persist the theme
  window.localStorage.setItem('color-theme', theme);
}


function swapDarkLight() {
  if (document.body.classList.contains('dark')) {
    setColorTheme('light');
  } else {
    setColorTheme('dark');
  }
}

// Restore theme from session
setColorTheme(window.localStorage.getItem('color-theme'));
