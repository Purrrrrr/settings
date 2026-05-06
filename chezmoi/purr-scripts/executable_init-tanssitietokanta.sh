#!/bin/bash
. $(dirname $(readlink -f "$BASH_SOURCE"))/start-term.sh
. $(dirname $(readlink -f "$BASH_SOURCE"))/xmonad-ctrl.sh

DIR=~/Projects/personal/Tanssiaistietokanta

echo "Starting frontend terminals"
switch_workplace 6
start-term $DIR/frontend
start-term $DIR/frontend
start-term $DIR/frontend
shift_win_right
start-term $DIR/frontend 'ALLOW_ALL_HOSTS=1 npm run start'

echo "Start backend terminals?"
switch_workplace 6B
start-term $DIR/backend
start-term $DIR/backend 'npm run dev'
start-term $DIR/backend
shift_win_right
start-term $DIR/backend
