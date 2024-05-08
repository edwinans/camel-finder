#!/usr/bin/bash

index='_build/default/bin/index.html'

if command -v google-chrome &> /dev/null; then
  google-chrome "$index"
elif command -v firefox &> /dev/null; then
  firefox "$index"
else
  if command -v xdg-open &> /dev/null; then
    xdg-open "$index"
  else
    echo "Neither Google Chrome, Firefox, nor a default browser is installed. Please install a browser to view the HTML file."
  fi
fi
