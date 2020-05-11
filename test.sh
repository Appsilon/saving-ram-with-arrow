#!/usr/bin/env bash

while true; do awk '/Mem:/ {print $3}' <(free -m) >> memory; sleep 1; done &
while true; do tail memory -n 1; sleep 1; done | feedgnuplot --stream 1 --ymin=0 --exit --lines &
R -e "profvis::profvis('shiny::runApp(\"app.R\", port = 5757)')"

