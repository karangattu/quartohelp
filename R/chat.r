.require_api_key <- function() {
  if (!nzchar(Sys.getenv("OPENAI_API_KEY"))) {
    stop("No OPENAI_API_KEY found in the environment.", call. = FALSE)
  }
}

.normalize_question <- function(question) {
  asNamespace("rlang")$check_string(question, allow_null = TRUE)
  if (is.null(question)) {
    return(NULL)
  }
  question <- trimws(question)
  if (!nzchar(question)) {
    return(NULL)
  }
  question
}

#' Launch the Quarto Help chat app
#'
#' Starts an interactive chat interface backed by the Quarto knowledge store.
#' The app uses [shinychat::chat_mod_ui()] and [shinychat::chat_mod_server()] to
#' wire a single chat instance and an embedded documentation browser. By
#' default a fresh chat is created with [quartohelp::chat_quartohelp()].
#'
#' @param initial_question Optional character string to send as the first user turn.
#' @param new_chat A function that returns a fresh `ellmer::Chat` instance. Used
#'   whenever the conversation is reset. Defaults to [quartohelp::chat_quartohelp()].
#' @param initial_chat Optional `ellmer::Chat` instance seeded into the app. Defaults
#'   to `new_chat()` so a fresh chat is created when none is supplied.
#'
#' @return Invisibly returns the chat instance used by the app.
#' @seealso [quartohelp::chat_quartohelp()], [quartohelp::as_quartohelp_chat()]
#' @export
#' @examples
#' if (interactive() && nzchar(Sys.getenv("OPENAI_API_KEY"))) {
#'   quartohelp::launch_app()
#' }
launch_app <- function(
  initial_question = NULL,
  initial_chat = new_chat(),
  new_chat = chat_quartohelp
) {
  .require_api_key()

  if (!is.function(new_chat)) {
    stop("`new_chat` must be a function that returns a 'Chat'.", call. = FALSE)
  }
  question <- .normalize_question(initial_question)

  chat <- initial_chat
  if (!is.null(chat) && !inherits(chat, "Chat")) {
    stop("`initial_chat` must inherit from 'Chat'.", call. = FALSE)
  }
  if (is.null(chat)) {
    chat <- new_chat()
    if (!inherits(chat, "Chat")) {
      stop(
        "`new_chat()` must return an object that inherits from 'Chat'.",
        call. = FALSE
      )
    }
  }

  app <- shiny::shinyApp(
    ui = quartohelp_app_ui(),
    server = quartohelp_app_server(
      initial_chat = chat,
      new_chat = new_chat,
      initial_question = question
    )
  )

  # runApp() blocks until the window is closed.
  shiny::runApp(app)
  invisible(chat)
}

#' Launch a simple Quarto Help chat app
#'
#' Provides a bare-bones Shiny example that wires [as_quartohelp_chat()] into the
#' [`shinychat`](https://rstudio.github.io/shinychat/) UI module. The app mirrors
#' just the chat pane from [launch_app()] without the documentation browser or
#' any advanced controls.
#'
#' @inheritParams launch_app
#'
#' @return Invisibly returns the chat instance used by the app.
#' @export
#' @examples
#' if (interactive() && nzchar(Sys.getenv("OPENAI_API_KEY"))) {
#'   quartohelp::launch_app_simple()
#' }
launch_app_simple <- function(
  initial_question = NULL,
  initial_chat = new_chat(),
  new_chat = chat_quartohelp
) {
  .require_api_key()

  if (!is.function(new_chat)) {
    stop("`new_chat` must be a function that returns a 'Chat'.", call. = FALSE)
  }
  question <- .normalize_question(initial_question)

  chat <- initial_chat
  if (!is.null(chat) && !inherits(chat, "Chat")) {
    stop("`initial_chat` must inherit from 'Chat'.", call. = FALSE)
  }
  if (is.null(chat)) {
    chat <- new_chat()
    if (!inherits(chat, "Chat")) {
      stop(
        "`new_chat()` must return an object that inherits from 'Chat'.",
        call. = FALSE
      )
    }
  }

  ui <- bslib::page_fillable(
    title = "Quarto Help (Simple Chat)",
    theme = bslib::bs_theme(version = 5),
    bslib::card(
      fill = TRUE,
      bslib::card_header(
        shiny::div(
          class = "d-flex align-items-center justify-content-between gap-2",
          shiny::tags$span("Chat"),
          shiny::actionButton(
            inputId = "clear_chat",
            label = "Clear",
            icon = shiny::icon("broom"),
            class = "btn btn-sm btn-outline-secondary"
          )
        )
      ),
      bslib::card_body(
        fill = TRUE,
        shiny::div(
          class = "h-100",
          shiny::uiOutput("chat_panel")
        )
      )
    )
  )

  server <- quartohelp_app_server(
    initial_chat = chat,
    new_chat = new_chat,
    initial_question = question
  )

  shiny::runApp(shiny::shinyApp(ui = ui, server = server))
  invisible(chat)
}

#' Ask a single question with Quarto Help
#'
#' Convenience wrapper that either launches [launch_app()] (the default) or
#' runs a single turn against the provided chat when `interactive = FALSE`.
#'
#' @inheritParams launch_app
#' @param question Optional character string to send as the first user turn.
#' @param ... Reserved for future use. Must be empty.
#' @param interactive When `FALSE`, return the result of `chat$chat(question)`
#'   instead of launching the app. If `question` is `NULL`, the chat object used
#'   for the interaction is returned so you can continue the conversation manually.
#' @return When `interactive = TRUE`, invisibly returns the chat instance used by
#'   the app. Otherwise returns either the chat response or the chat object.
#' @export
#' @examples
#'
#' if (interactive() && nzchar(Sys.getenv("OPENAI_API_KEY"))) {
#'   # Launch the app
#'   quartohelp::ask()
#'
#'   # Or answer a question programmatically
#'   quartohelp::ask("How can I make a two column layout?", interactive = FALSE)
#' }
ask <- function(
  question = NULL,
  ...,
  initial_chat = NULL,
  new_chat = chat_quartohelp,
  interactive = TRUE
) {
  rlang::check_dots_empty()

  if (!interactive) {
    .require_api_key()
    if (!is.function(new_chat)) {
      stop(
        "`new_chat` must be a function that returns a 'Chat'.",
        call. = FALSE
      )
    }

    question <- .normalize_question(question)
    chat <- initial_chat
    if (is.null(chat)) {
      chat <- new_chat()
    }
    if (!inherits(chat, "Chat")) {
      stop("`initial_chat` must inherit from 'Chat'.", call. = FALSE)
    }

    if (is.null(question)) {
      return(chat)
    }
    return(chat$chat(question))
  }

  launch_app(
    initial_question = question,
    initial_chat = initial_chat,
    new_chat = new_chat
  )
}


#' Install the `quartohelp` cli launcher
#'
#' @inheritDotParams Rapp::install_pkg_cli_apps -package -lib.loc
#'
#' @details
#'
#' From R, run:
#'
#' ```r
#'   quartohelp::install_quartohelp_cli()
#' ```
#'
#' Then, from terminal, call quartohelp
#'
#' ```bash
#'   quartohelp
#'   quartohelp --help
#'   quartohelp 'how to make a two-column layout?'
#' ```
#'
#' @export
install_quartohelp_cli <- function(...) {
  Rapp::install_pkg_cli_apps("quartohelp", lib.loc = NULL, ...)
}
