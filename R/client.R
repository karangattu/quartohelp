#' Configure a chat object for Quarto Help
#'
#' Attaches the Quarto knowledge store retrieval tool and system prompt to a
#' chat instance. By default this creates a fresh OpenAI chat via
#' [ellmer::chat_openai_responses()] and registers
#' [ragnar::ragnar_register_tool_retrieve] so that every response can cite
#' relevant Quarto documentation.
#'
#' @param proto An `ellmer::Chat` object to configure. When `NULL`, a default OpenAI
#'   chat is created. You can also set a global prototype via the
#'   `quartohelp.proto_chat` option, which may be either a function returning a chat or
#'   an existing `ellmer::Chat` instance.
#' @param top_k Number of excerpts to request from the knowledge store for each
#'   retrieval.
#' @param store A `ragnar::RagnarStore` object (for example, from
#'   `quartohelp_ragnar_store()`).
#' @return The configured `chat` object.
#' @keywords internal
as_quartohelp_chat <- function(
  proto = getOption("quartohelp.proto_chat"),
  top_k = 8,
  store = quartohelp_ragnar_store()
) {
  if (inherits(proto, "Chat")) {
    proto$set_turns(list())
    proto$set_tools(list())
    chat <- proto
  } else if (is.null(proto)) {
    chat <- ellmer::chat_openai(
      model = "gpt-5",
      api_args = list(list(
        reasoning = list(effort = "low"),
        verbosity = "low"
      )),
      # params = ellmer::params(
      #   text = list(verbosity = "low"),
      #   reasoning_effort = "low"
      # ),
      echo = "none"
    )
  } else if (is.function(proto)) {
    chat <- proto()
    if (!inherits(chat, "Chat")) {
      stop(
        "`proto()` must return an object that inherits from 'Chat'.",
        call. = FALSE
      )
    }
  } else {
    stop(
      "`proto` must be NULL, a function returning a 'Chat', or a 'Chat' object.",
      call. = FALSE
    )
  }

  stopifnot(inherits(chat, "Chat"))
  chat$set_system_prompt(c(
    chat$get_system_prompt(),
    glue::trim(
      "
      You are an expert in Quarto documentation. You are concise.
      Always perform a search of the Quarto knowledge store for each user request.
      Every response must include links to official documentation sources.
      If the request is ambiguous, search first, then ask a clarifying question.
      If docs are unavailable, or if search fails, or if docs do not contain an answer
      to the question, inform the user and do NOT answer the question.

      Always give answers that include a minimal fully self-contained quarto document.

      To display Quarto code blocks, use oversized markdown fences, like this:

      ````` markdown
      PROSE HERE
      ```{r}
      CODE HERE
      ```
      ```{python}
      CODE HERE
      ```
      `````
      "
    )
  ))

  ragnar::ragnar_register_tool_retrieve(chat, store, top_k = top_k)

  chat
}

#' Create a Quarto Help chat
#'
#' This is a convenience wrapper around [as_quartohelp_chat()] that returns a fresh
#' `ellmer::Chat` instance configured with the Quarto knowledge store.
#'
#' @inheritParams as_quartohelp_chat
#' @return A configured `ellmer::Chat` object.
#' @export
chat_quartohelp <- function(
  top_k = 8,
  store = quartohelp_ragnar_store(),
  proto = NULL
) {
  as_quartohelp_chat(
    proto = proto,
    top_k = top_k,
    store = store
  )
}
