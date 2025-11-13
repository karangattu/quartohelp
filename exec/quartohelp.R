#!/usr/bin/env Rapp

#| description: open a browser
open <- TRUE

if (open) {
  options("shiny.launch.browser" = TRUE)
}
library("methods")
library("shiny")

#| description: Optional string to send as the first user turn.
question <- NULL

quartohelp::ask(question)
