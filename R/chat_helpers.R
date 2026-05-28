# Chat UI helpers: message bubbles and visual recommendation cards.

movie_page_url <- function(movie_id) {
  id <- as.character(movie_id)
  if (length(id) != 1 || is.na(id) || !nzchar(id)) {
    return(NULL)
  }
  paste0("https://www.themoviedb.org/movie/", id)
}

format_query_error <- function(query, error_msg) {
  if (grepl("multiple possible matches", error_msg, fixed = TRUE)) {
    return(error_msg)
  }
  if (nzchar(error_msg) && !grepl("matching terms", error_msg, fixed = TRUE)) {
    return(error_msg)
  }
  paste(
    "I couldn't find movies similar to that request.",
    "Try a known movie title or phrasing like",
    "\"Toy Story\", \"The Matrix\", or \"movies similar to James Bond\".",
    sep = "\n"
  )
}

render_recommendation_message <- function(query, recs) {
  cards <- lapply(seq_len(nrow(recs)), function(i) {
    row <- recs[i, , drop = FALSE]
    year <- if (!is.na(row$release_year)) as.character(row$release_year) else "—"
    rating <- if (!is.na(row$tmdb_vote_average)) {
      sprintf("\u2605 %.1f", as.numeric(row$tmdb_vote_average))
    } else {
      "\u2014"
    }

    card_body <- tagList(
      tags$span(class = "chat-rec-rank", paste0("#", i)),
      tags$div(
        class = "chat-rec-body",
        tags$div(class = "chat-rec-title", row$title),
        tags$div(
          class = "chat-rec-meta",
          tags$span(year),
          tags$span(class = "chat-rec-meta-sep", "\u00b7"),
          tags$span(class = "chat-rec-rating", rating)
        )
      )
    )

    movie_url <- movie_page_url(row$id)
    if (!is.null(movie_url)) {
      tags$a(
        href = movie_url,
        target = "_blank",
        rel = "noopener noreferrer",
        class = "chat-rec-card",
        title = paste("View", row$title, "on TMDB"),
        card_body
      )
    } else {
      tags$div(class = "chat-rec-card", card_body)
    }
  })

  tagList(
    tags$div(class = "chat-rec-intro", "Here are your top picks:"),
    tags$div(class = "chat-rec-query", paste0("\u201c", query, "\u201d")),
    tags$div(class = "chat-rec-cards", cards)
  )
}

render_chat_message <- function(msg) {
  role <- msg$role
  role_class <- if (role == "user") "chat-message--user" else "chat-message--assistant"
  label <- if (role == "user") "You" else "Assistant"

  body <- if (identical(msg$type, "recommendations")) {
    render_recommendation_message(msg$query, msg$recs)
  } else {
    msg$text
  }

  tags$div(
    class = paste("chat-message", role_class),
    tags$div(class = "chat-message-label", label),
    tags$div(class = "chat-message-body", body)
  )
}
