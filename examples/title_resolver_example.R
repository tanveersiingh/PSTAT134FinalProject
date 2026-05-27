source("R/title_resolver.R")

build_fallback_catalog <- function() {
  data.frame(
    id = c(603, 604, 605, 550, 27205),
    title = c(
      "The Matrix",
      "The Matrix Reloaded",
      "The Matrix Revolutions",
      "Fight Club",
      "Inception"
    ),
    tmdb_popularity = c(80, 70, 65, 75, 85),
    stringsAsFactors = FALSE
  )
}

load_catalog_for_example <- function() {
  csv_path <- "data/movies/movies_metadata.csv"
  if (!file.exists(csv_path)) {
    message("Dataset not found at data/movies/movies_metadata.csv. Using fallback sample catalog.")
    return(build_fallback_catalog())
  }

  if (!requireNamespace("readr", quietly = TRUE)) {
    message("Package 'readr' is unavailable. Using fallback sample catalog.")
    return(build_fallback_catalog())
  }

  cols <- names(readr::read_csv(csv_path, n_max = 0, show_col_types = FALSE))
  selected_cols <- intersect(c("id", "title", "popularity"), cols)
  if (!all(c("id", "title") %in% selected_cols)) {
    message("Required columns not found in dataset. Using fallback sample catalog.")
    return(build_fallback_catalog())
  }

  # The Kaggle metadata file has known mixed-type rows; we only need id/title/popularity.
  movies <- suppressWarnings(
    readr::read_csv(
      csv_path,
      col_select = tidyselect::all_of(selected_cols),
      show_col_types = FALSE
    )
  )

  if ("popularity" %in% names(movies) && !"tmdb_popularity" %in% names(movies)) {
    names(movies)[names(movies) == "popularity"] <- "tmdb_popularity"
  }

  as.data.frame(movies, stringsAsFactors = FALSE)
}

catalog <- load_catalog_for_example()

queries <- c(
  "What would you reccommend if I like the Matrix",
  "What would you recommend if I liked \"Fight Club\"?",
  "Can you recommend movies similar to The Godfather?",
  "I liked Pulp Fiction what should I watch?",
  "Something like The Dark Knight please",
  "what is something similar to inception",
  "recommend me a movie like matrix reloaded",
  "what if i liked toy story",
  "Can you reccomend something like Spirited Away?",
  "recommend something like batman",
  "recommend something like it",
  "I loved Avatar",
  "what should I watch after Top Gun",
  "Can you suggest qwertyuiop movie"
)

for (query in queries) {
  cat("\nQuery:", query, "\n")
  result <- resolve_movie_title_from_text(query, catalog, top_k = 5)
  cat("Status:", result$status, "\n")
  cat("Resolved title:", ifelse(is.na(result$resolved_title), "<none>", result$resolved_title), "\n")
  if (nzchar(result$user_prompt)) {
    cat("Prompt:", result$user_prompt, "\n")
  }
}
