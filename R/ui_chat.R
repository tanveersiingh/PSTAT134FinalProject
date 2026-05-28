# Chat UI fragment — dynamic transcript + composer.

chat_ui <- function() {
  tagList(
    div(
      class = "chat-app",
      tags$header(
        class = "chat-header",
        tags$span(class = "chat-header-icon", HTML("&#127916;")),
        tags$div(
          class = "chat-header-text",
          tags$h1(class = "chat-header-title", "Movie Recommender"),
          tags$p(class = "chat-header-subtitle", "Enter a title — get similar picks")
        )
      ),
      div(
        class = "chat-thread",
        id = "chat_thread_container",
        uiOutput("chat_thread")
      ),
      div(
        class = "chat-composer",
        div(
          class = "chat-composer-actions",
          div(
            style = "flex: 1; min-width: 0;",
            tags$textarea(
              id = "chat_input",
              class = "form-control",
              placeholder = "e.g. Toy Story, The Matrix, or movies similar to James Bond\u2026",
              rows = "2",
              style = "width: 100%;",
              onkeydown = HTML(
                "if((event.key==='Enter'||event.keyCode===13)&&!event.shiftKey&&!event.isComposing){event.preventDefault();document.getElementById('send').click();return false;}"
              )
            )
          ),
          actionButton("send", "Send", class = "btn-primary")
        ),
        helpText(
          "Enter to send \u00b7 Shift+Enter for a new line \u00b7 Powered by bigram TF-IDF",
          class = "chat-composer-hint"
        )
      ),
      tags$script(src = "chat.js")
    )
  )
}
