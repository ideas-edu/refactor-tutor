#!/bin/sh

echo "content-type: text/html"

if [ "$LOGIN_ENABLED" = "true" ]; then
  echo "location: login.html"
  echo "status: 302"
  echo ""
  echo "You should be redirected. If not, click <a href='login.html'>here</a>."
else
  echo ""
  cat <<HTML
  <script>
    localStorage.setItem('rpt:userid', '0000');
    window.location = 'rpt.html';
  </script>
  You should be redirected.
  <noscript>Make sure Javascript is enabled.</noscript>
HTML
fi