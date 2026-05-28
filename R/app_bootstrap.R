# One-time data load and recommender model build for the Shiny app.

load_movie_raw_csvs <- function() {
  list(
    keywords = read_csv("data/movies/keywords.csv"),
    links = read_csv("data/movies/links.csv"),
    ratings = read_csv("data/movies/ratings.csv"),
    movies = read_csv("data/movies/movies_metadata.csv"),
    credits = read_csv("data/movies/credits.csv")
  )
}

bootstrap_recommender <- function() {
  message("Loading raw CSVs...")
  raw <- load_movie_raw_csvs()

  message("Cleaning and merging datasets...")
  movies_clean <- trim_movies_metadata(raw$movies)
  movies_full <- merge_movies_keywords_credits(
    movies_clean,
    raw$keywords,
    raw$credits
  )
  ratings_clean <- clean_ratings(raw$ratings)
  ratings_join_out <- map_ratings_to_tmdb_summaries(ratings_clean, raw$links)
  movies_final <- join_rating_summaries(
    movies_full,
    ratings_join_out$ratings_summary
  )
  movies_final <- map_original_language_to_names(movies_final)
  movies_final <- rename_tmdb_vote_columns(movies_final)

  dedupe_out <- dedupe_movies_final_with_summary(movies_final)
  movies_final <- dedupe_out$movies_final
  movies_clean <- impute_and_drop_for_modeling(movies_final)
  movies_clean <- filter_movies_released_through_year(movies_clean, 2017L)

  message("Building NLP features (bigram TF-IDF)...")
  movies_text <- make_movies_text(movies_clean)
  movie_bigrams <- make_bigram_tokens(movies_text)
  movies_bigrams_clean <- clean_bigram_tokens(movie_bigrams)
  bigram_tf_idf <- make_bigram_tf_idf(movies_bigrams_clean)
  bigram_tf_idf_top <- clean_bigram_tf_idf(bigram_tf_idf)
  bigram_tf_idf_clean <- bigram_tf_idf %>%
    semi_join(bigram_tf_idf_top, by = "bigram")

  message("Creating feature matrix...")
  movies_feature_matrix <<- create_feature_matrix(bigram_tf_idf_clean)

  message("Preparing recommender lookup...")
  prepared <- prepare_movie_search(movies_text, movies_feature_matrix)
  movie_search <<- prepared$movie_search
  movies_feature_matrix <<- prepared$movies_feature_matrix
  movie_row_norms <<- matrix_row_norms(movies_feature_matrix)
  bigram_idf_lookup <<- bigram_tf_idf_clean %>%
    distinct(bigram, idf) %>%
    mutate(bigram = as.character(bigram))

  invisible(list(
    movies_feature_matrix = movies_feature_matrix,
    movie_search = movie_search,
    movie_row_norms = movie_row_norms,
    bigram_idf_lookup = bigram_idf_lookup
  ))
}
