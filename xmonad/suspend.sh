#!/bin/sh

if ifconfig | grep bnep; then
  xmessage "Remember to shut down bluetooth"
else
  dbus-send --system --print-reply --dest="org.freedesktop.UPower" /org/freedesktop/UPower org.freedesktop.UPower.Suspend
fi
