#!/bin/bash

switch_workplace() {
  sleep 0.4
  ~/.xmonad/bin/switch_workplace $1
}

shift_win_left() {
  sleep 0.4
  xdotool key alt+shift+h
}

shift_win_right() {
  sleep 0.4
  xdotool key alt+shift+l
}
