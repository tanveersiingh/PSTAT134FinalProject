# Shiny server logic. Runs after global.R and ui.R.

server <- function(input, output, session) {
  messages <- reactiveVal(list(
    list(
      role = "assistant",
      type = "text",
      text = paste(
        "Enter a movie you like and I'll suggest similar titles —",
        "for example: \"Toy Story\", \"The Matrix\",",
        "or \"movies similar to James Bond\"."
      )
    )
  ))

  output$chat_thread <- renderUI({
    tagList(lapply(messages(), render_chat_message))
  })

  observe({
    messages()
    session$onFlushed(function() {
      session$sendCustomMessage("scrollChatToBottom", list())
    }, once = TRUE)
  })

  observeEvent(input$send, {
    query <- trimws(input$chat_input)
    if (!nzchar(query)) {
      return()
    }

    msgs <- messages()
    msgs[[length(msgs) + 1]] <- list(role = "user", type = "text", text = query)

    assistant_msg <- tryCatch(
      {
        recs <- recommend_for_chat(query, n = 5)
        list(
          role = "assistant",
          type = "recommendations",
          query = query,
          recs = recs
        )
      },
      error = function(e) {
        list(
          role = "assistant",
          type = "text",
          text = format_query_error(query, conditionMessage(e))
        )
      }
    )

    msgs[[length(msgs) + 1]] <- assistant_msg
    messages(msgs)
    updateTextAreaInput(session, "chat_input", value = "")
  }, ignoreInit = TRUE)
}
