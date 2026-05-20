# ggplot helpers for EDA sections (matches 134FinalProjectWriteUp.Rmd styling).

plot_rating_distribution <- function(data, binwidth = 0.5) {
  ggplot(data, aes(tmdb_vote_average)) +
    geom_histogram(
      binwidth = binwidth,
      fill = "blue",
      color = "white"
    ) +
    labs(
      title = "Ratings Distribution",
      x = "Voting Average",
      y = "Number of Obs"
    ) +
    theme_minimal()
}

plot_vote_count_distribution <- function(data, bins = 40) {
  ggplot(data, aes(x = tmdb_vote_count)) +
    geom_histogram(
      bins = bins,
      fill = "#4C78A8",
      color = "white",
      alpha = 0.9
    ) +
    scale_x_log10(labels = scales::comma) +
    labs(
      title = "Distribution of TMDB Vote Counts",
      x = "Vote Count (log scale)",
      y = "Number of Movies"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(face = "bold"),
      plot.subtitle = element_text(color = "gray40"),
      axis.title = element_text(face = "bold"),
      panel.grid.minor = element_blank()
    )
}

plot_movies_per_year <- function(movies_clean) {
  movies_clean %>%
    count(release_year) %>%
    ggplot(aes(x = release_year, y = n)) +
    geom_line(
      color = "#4C78A8",
      linewidth = 1.2
    ) +
    geom_point(
      color = "#4C78A8",
      size = 1.5
    ) +
    labs(
      title = "Number of Movies Released Per Year",
      x = "Release Year",
      y = "Number of Movies"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(face = "bold"),
      plot.subtitle = element_text(color = "gray40"),
      axis.title = element_text(face = "bold")
    )
}

plot_movies_by_genre <- function(data, top_n = NULL) {
  movies_genres_long <- data %>%
    mutate(
      genres_parsed = map(genres, ~ {
        genres <- str_extract_all(.x, "(?<='name': ')[^']+")[[1]]
        trimws(genres)
      })
    ) %>%
    unnest(genres_parsed)
  
  genre_counts <- movies_genres_long %>%
    count(genres_parsed, sort = TRUE)
  
  if (!is.null(top_n)) {
    genre_counts <- genre_counts %>%
      slice_max(n, n = top_n)
  }
  
  ggplot(genre_counts, aes(x = reorder(genres_parsed, n), y = n)) +
    geom_col(
      fill = "#4C78A8",
      alpha = 0.9
    ) +
    coord_flip() +
    scale_y_continuous(labels = scales::comma) +
    labs(
      title = "Movies by Genre",
      x = "Genre",
      y = "Number of Movies"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(face = "bold"),
      axis.title = element_text(face = "bold"),
      panel.grid.minor = element_blank()
    )
}

plot_genre_average_rating <- function(data) {
  
  movies_genres_long <- data %>%
    mutate(
      genres_parsed = map(genres, ~ {
        str_extract_all(.x, "(?<='name': ')[^']+")[[1]]
      })
    ) %>%
    unnest(genres_parsed)
  
  movies_genres_long %>%
    group_by(genres_parsed) %>%
    summarise(
      avg_rating = mean(tmdb_vote_average, na.rm = TRUE)
    ) %>%
    ggplot(aes(
      x = reorder(genres_parsed, avg_rating),
      y = avg_rating
    )) +
    geom_col(fill = "lightblue") +
    coord_flip(ylim = c(0, 10)) +
    labs(
      title = "Genre vs. Average Rating",
      x = "Genre",
      y = "Average Rating"
    ) +
    theme_minimal()
}

plot_correlation_matrix <- function(data) {
  
  numeric_df <- data %>%
    select(where(is.numeric)) %>%
    select(-id) %>%
    drop_na()
  
  corrplot(
    cor(numeric_df),
    method = "color",
    type = "upper",
    tl.cex = 0.7,
    tl.col = "black"
  )
}

plot_runtime_distribution <- function(data, max_runtime = 300, bins = 40) {
    outliers <- data |>
    filter(runtime_minutes > max_runtime) |>
    select(title, runtime_minutes) |>
    arrange(desc(runtime_minutes))
  
  print(outliers)
  data |>
    filter(runtime_minutes <= max_runtime) |>
    ggplot(aes(x = runtime_minutes)) +
    geom_histogram(
      bins = bins,
      fill = "lightblue",
      color = "white",
      alpha = 0.9
    ) +
    scale_x_log10() +
    labs(
      title = "Runtime Distribution",
      x = "Runtime (minutes, log scale)",
      y = "Number of Movies"
    ) +
    theme_minimal()
}

plot_popularity_vs_rating <- function(data,
                                      point_alpha = 0.2,
                                      point_size = 0.5,
                                      log_scale = TRUE) {
  
  plot_data <- data |>
    filter(!is.na(tmdb_popularity),
           !is.na(tmdb_vote_average))
  
  if (log_scale) {
    plot_data <- plot_data |>
      filter(tmdb_popularity > 0)
  }
  
  p <- ggplot(
    plot_data,
    aes(x = tmdb_popularity, y = tmdb_vote_average)
  ) +
    geom_point(
      alpha = point_alpha,
      size = point_size,
      color = "lightblue"
    ) +
    labs(
      title = "Popularity vs. Rating",
      x = ifelse(log_scale,
                 "Popularity (log scale)",
                 "Popularity"),
      y = "Average Rating"
    ) +
    theme_minimal()
  
  if (log_scale) {
    p <- p + scale_x_log10()
  }
  
  return(p)
}

plot_bigram_graph <- function(data,
                              text_column,
                              min_count = 20,
                              max_edges = 50) {
  
  text_column <- enquo(text_column)
  
  bigram_counts <- data %>%
    filter(!is.na(!!text_column)) %>%
    select(!!text_column) %>%
    unnest_tokens(
      bigram,
      !!text_column,
      token = "ngrams",
      n = 2
    ) %>%
    separate(bigram, into = c("word1", "word2"), sep = " ") %>%
    filter(
      !is.na(word1),
      !is.na(word2),
      word1 != "",
      word2 != "",
      !word1 %in% stop_words$word,
      !word2 %in% stop_words$word
    ) %>%
    count(word1, word2, sort = TRUE) %>%
    filter(n >= min_count) %>%
    slice_max(n, n = max_edges)
  
  bigram_graph <- bigram_counts %>%
    graph_from_data_frame()
  
  ggraph(bigram_graph, layout = "fr") +
    geom_edge_link(
      aes(edge_alpha = n, edge_width = n),
      show.legend = FALSE
    ) +
    geom_node_point(
      color = "lightblue",
      size = 5
    ) +
    geom_node_text(
      aes(label = name),
      repel = TRUE,
      size = 4
    ) +
    theme_void() +
    labs(title = "Bigram Network Graph")
}