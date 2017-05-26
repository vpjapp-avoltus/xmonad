#!/bin/bash
if [ ! -f "/tmp/fedset" ]; then
  feh --bg-fill /home/vpjapp/.xmonad/background.jpg
  echo "Set bg"
fi
touch /tmp/fedset
