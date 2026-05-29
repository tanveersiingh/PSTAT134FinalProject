# Recommender process for modularization
prepare_movie_search <- function(movies_text, movies_feature_matrix) {
  
  movie_search <- movies_text %>%
    distinct(id, title, release_year, tmdb_vote_average, tmdb_popularity) %>%
    mutate(
      id = as.character(id),
      title = as.character(title),
      release_year = suppressWarnings(as.integer(release_year)),
      tmdb_vote_average = suppressWarnings(as.numeric(tmdb_vote_average)),
      tmdb_popularity = suppressWarnings(as.numeric(tmdb_popularity))
    )
  
  if (!inherits(movies_feature_matrix, "sparseMatrix")) {
    movies_feature_matrix <- as(movies_feature_matrix, "dgCMatrix")
  }
  
  rownames(movies_feature_matrix) <- as.character(rownames(movies_feature_matrix))
  
  movie_search <- movie_search %>%
    filter(id %in% rownames(movies_feature_matrix))
  
  movies_feature_matrix <- movies_feature_matrix[movie_search$id, ]
  
  return(list(
    movie_search = movie_search,
    movies_feature_matrix = movies_feature_matrix
  ))
}

rescale_01 <- function(x) {
  x <- suppressWarnings(as.numeric(x))
  
  if (all(is.na(x))) {
    return(rep(0, length(x)))
  }
  
  min_x <- min(x, na.rm = TRUE)
  max_x <- max(x, na.rm = TRUE)
  
  if (max_x == min_x) {
    return(rep(0, length(x)))
  }
  
  (x - min_x) / (max_x - min_x)
}

create_recommender_summary <- function(movies_feature_matrix) {
  
  recommender_summary <- tibble(
    movies = nrow(movies_feature_matrix),
    features = ncol(movies_feature_matrix),
    nonzero_values = length(movies_feature_matrix@x),
    sparsity = 1 - (
      length(movies_feature_matrix@x) /
        (nrow(movies_feature_matrix) * ncol(movies_feature_matrix))
    )
  )
  
  recommender_summary %>%
    kable(
      digits = 4,
      caption = "Sparse Feature Matrix Summary"
    )
}

matrix_row_norms <- function(mat) {
  sqrt(Matrix::rowSums(mat ^ 2))
}

cosine_against_one_movie <- function(movie_id, feature_matrix, row_norms = NULL) {
  
  movie_id <- as.character(movie_id)
  
  if (!(movie_id %in% rownames(feature_matrix))) {
    stop("Movie ID was not found in the feature matrix.")
  }
  
  if (is.null(row_norms)) {
    row_norms <- matrix_row_norms(feature_matrix)
  }
  
  movie_vec <- feature_matrix[movie_id, , drop = FALSE]
  
  movie_norm <- row_norms[
    match(movie_id, rownames(feature_matrix))
  ]
  
  raw_scores <- as.numeric(
    feature_matrix %*% Matrix::t(movie_vec)
  )
  
  denom <- row_norms * movie_norm
  
  similarities <- ifelse(
    denom == 0,
    0,
    raw_scores / denom
  )
  
  similarities[is.na(similarities)] <- 0
  
  return(similarities)
}


find_movie_id <- function(title_query, movie_lookup) {
  
  title_query_clean <- str_to_lower(
    str_squish(title_query)
  )
  
  exact_match <- movie_lookup %>%
    filter(
      str_to_lower(title) == title_query_clean
    ) %>%
    arrange(
      desc(tmdb_popularity),
      desc(tmdb_vote_average)
    )
  
  if (nrow(exact_match) > 0) {
    return(exact_match$id[1])
  }
  
  partial_match <- movie_lookup %>%
    filter(
      str_detect(
        str_to_lower(title),
        fixed(title_query_clean)
      )
    ) %>%
    arrange(
      desc(tmdb_popularity),
      desc(tmdb_vote_average)
    )
  
  if (nrow(partial_match) > 0) {
    return(partial_match$id[1])
  }
  
  stop(
    "No matching movie title was found. Try a simpler or more exact title."
  )
}

recommend_similar_movies <- function(title_query,
                                     n = 10,
                                     feature_matrix = movies_feature_matrix,
                                     movie_lookup = movie_search,
                                     row_norms = movie_row_norms,
                                     similarity_weight = 0.80,
                                     popularity_weight = 0.10,
                                     rating_weight = 0.10) {
  
  selected_id <- find_movie_id(title_query, movie_lookup)
  
  similarities <- cosine_against_one_movie(
    movie_id = selected_id,
    feature_matrix = feature_matrix,
    row_norms = row_norms
  )
  
  score_table <- tibble(
    id = rownames(feature_matrix),
    content_similarity = similarities
  ) %>%
    left_join(movie_lookup, by = "id") %>%
    mutate(
      popularity_scaled = rescale_01(tmdb_popularity),
      rating_scaled = rescale_01(tmdb_vote_average),
      final_score = similarity_weight * content_similarity +
        popularity_weight * popularity_scaled +
        rating_weight * rating_scaled
    ) %>%
    filter(id != selected_id) %>%
    arrange(desc(final_score), desc(content_similarity)) %>%
    slice_head(n = n)
  
  selected_movie <- movie_lookup %>%
    filter(id == selected_id) %>%
    slice(1)
  
  return(list(
    input_movie = selected_movie,
    recommendations = score_table
  ))
}


make_query_vector <- function(query,
                              feature_terms = colnames(movies_feature_matrix),
                              idf_lookup = bigram_idf_lookup) {
  
  query_tbl <- tibble(query_id = "query", text = query)
  
  query_bigrams <- query_tbl %>%
    unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%
    separate(bigram, into = c("word1", "word2"), sep = " ", remove = FALSE) %>%
    filter(
      !(word1 %in% stop_words$word & word2 %in% stop_words$word)
    ) %>%
    select(query_id, bigram)
  
  if (nrow(query_bigrams) == 0) {
    fallback_words <- query_tbl %>%
      unnest_tokens(word, text) %>%
      filter(!word %in% stop_words$word, nchar(word) >= 3) %>%
      pull(word) %>%
      unique()
    
    if (length(fallback_words) > 0) {
      matched_bigrams <- feature_terms[
        vapply(
          feature_terms,
          function(bigram) {
            any(vapply(
              fallback_words,
              function(w) grepl(w, bigram, fixed = TRUE),
              logical(1)
            ))
          },
          logical(1)
        )
      ]
      if (length(matched_bigrams) > 0) {
        query_bigrams <- tibble(query_id = "query", bigram = matched_bigrams)
      }
    }
  }
  
  if (nrow(query_bigrams) == 0) {
    return(Matrix(0, nrow = 1, ncol = length(feature_terms),
                  dimnames = list("query", feature_terms), sparse = TRUE))
  }
  
  query_tf <- query_bigrams %>%
    count(query_id, bigram, name = "n") %>%
    group_by(query_id) %>%
    mutate(tf = n / sum(n)) %>%
    ungroup() %>%
    left_join(idf_lookup, by = "bigram") %>%
    mutate(
      idf = replace_na(idf, 0),
      tf_idf = tf * idf
    ) %>%
    filter(bigram %in% feature_terms)
  
  query_vec <- Matrix(0,
                      nrow = 1,
                      ncol = length(feature_terms),
                      dimnames = list("query", feature_terms),
                      sparse = TRUE)
  
  if (nrow(query_tf) > 0) {
    matched_cols <- match(query_tf$bigram, feature_terms)
    query_vec[1, matched_cols] <- query_tf$tf_idf
  }
  
  query_vec
}

recommend_from_query <- function(query,
                                 n = 10,
                                 feature_matrix = movies_feature_matrix,
                                 movie_lookup = movie_search,
                                 row_norms = movie_row_norms,
                                 similarity_weight = 0.80,
                                 popularity_weight = 0.10,
                                 rating_weight = 0.10) {
  
  query_vec <- make_query_vector(
    query = query,
    feature_terms = colnames(feature_matrix)
  )
  
  query_norm <- sqrt(sum(query_vec ^ 2))
  
  if (query_norm == 0) {
    title_result <- tryCatch(
      recommend_similar_movies(
        query = query,
        n = n,
        feature_matrix = feature_matrix,
        movie_lookup = movie_lookup,
        row_norms = row_norms,
        similarity_weight = similarity_weight,
        popularity_weight = popularity_weight,
        rating_weight = rating_weight
      ),
      error = function(e) NULL
    )
    if (!is.null(title_result) && nrow(title_result$recommendations) > 0) {
      return(title_result$recommendations)
    }
    stop("The query did not contain enough matching terms. Try a movie title or a short description.")
  }
  
  raw_scores <- as.numeric(feature_matrix %*% Matrix::t(query_vec))
  denom <- row_norms * query_norm
  
  similarities <- ifelse(denom == 0, 0, raw_scores / denom)
  similarities[is.na(similarities)] <- 0
  
  score_table <- tibble(
    id = rownames(feature_matrix),
    query_similarity = similarities
  ) %>%
    left_join(movie_lookup, by = "id") %>%
    mutate(
      popularity_scaled = rescale_01(tmdb_popularity),
      rating_scaled = rescale_01(tmdb_vote_average),
      final_score = similarity_weight * query_similarity +
        popularity_weight * popularity_scaled +
        rating_weight * rating_scaled
    ) %>%
    arrange(desc(final_score), desc(query_similarity)) %>%
    slice_head(n = n)
  
  score_table
}

plot_query_recommendations <- function(query, n = 10) {
  recs <- recommend_from_query(query, n = n) %>%
    mutate(title = str_trunc(title, width = 35)) %>%
    arrange(final_score) %>%
    mutate(title = factor(title, levels = title))
  
  ggplot(recs, aes(x = final_score, y = title)) +
    geom_col() +
    labs(
      title = paste("Top Movie Recommendations for:", query),
      x = "Final Recommendation Score",
      y = "Movie"
    ) +
    theme_minimal()
}

recommend_from_query_similarity_only <- function(query,
                                                 n = 10,
                                                 feature_matrix = movies_feature_matrix,
                                                 movie_lookup = movie_search,
                                                 row_norms = movie_row_norms) {
  
  recommend_from_query(
    query = query,
    n = n,
    feature_matrix = feature_matrix,
    movie_lookup = movie_lookup,
    row_norms = row_norms,
    similarity_weight = 1,
    popularity_weight = 0,
    rating_weight = 0
  )
}

looks_like_description_query <- function(text) {
  x <- trimws(tolower(as.character(text)))
  if (!nzchar(x)) return(FALSE)
  if (nchar(x) >= 35) return(TRUE)
  
  # If it contains common "description intent" words, treat as description
  if (grepl("\\babout\\b|\\bwith\\b|\\bset\\s+in\\b|\\bstarring\\b|\\bwhere
            \\b|\\bthat\\b|\\bwhich\\b|\\brelationship(s)?\\b|\\blove\\b|
            \\bromantic\\b|\\bcomedy\\b|\\bdrama\\b|\\bthriller\\b|\\bhorror\\b|
            \\bsci[- ]?fi\\b", x)) {
    return(TRUE)
  }
  
  # If it contains lots of spaces, it's more likely a phrase than a title
  if (length(strsplit(x, "\\s+")[[1]]) >= 6) return(TRUE)
  
  FALSE
}

recommend_for_chat <- function(query,
                               n = 5,
                               feature_matrix = movies_feature_matrix,
                               movie_lookup = movie_search,
                               row_norms = movie_row_norms) {
  query <- trimws(as.character(query))
  if (!nzchar(query)) {
    stop("Please enter a movie title or description.")
  }
  
  # NEW: if it looks like a free-text description, skip title resolution.
  if (looks_like_description_query(query)) {
    return(recommend_from_query(
      query = query,
      n = n,
      feature_matrix = feature_matrix,
      movie_lookup = movie_lookup,
      row_norms = row_norms
    ))
  }
  
  # Otherwise, keep existing "title-first" behavior.
  resolved <- resolve_movie_title_from_text(
    user_text = query,
    movies_catalog = movie_lookup,
    top_k = 5L,
    min_confidence = 0.35
  )
  
  if (resolved$status == "resolved") {
    out <- recommend_similar_movies(
      title_query = resolved$resolved_title,
      n = n,
      feature_matrix = feature_matrix,
      movie_lookup = movie_lookup,
      row_norms = row_norms
    )
    return(out$recommendations)
  }
  
  if (resolved$status == "ambiguous") {
    stop(resolved$user_prompt)
  }
  
  title_result <- tryCatch(
    recommend_similar_movies(
      title_query = query,
      n = n,
      feature_matrix = feature_matrix,
      movie_lookup = movie_lookup,
      row_norms = row_norms
    ),
    error = function(e) NULL
  )
  if (!is.null(title_result) && nrow(title_result$recommendations) > 0) {
    return(title_result$recommendations)
  }
  
  cleaned <- strip_query_boilerplate(query)
  if (nzchar(cleaned) && !identical(cleaned, query)) {
    title_result <- tryCatch(
      recommend_similar_movies(
        title_query = cleaned,
        n = n,
        feature_matrix = feature_matrix,
        movie_lookup = movie_lookup,
        row_norms = row_norms
      ),
      error = function(e) NULL
    )
    if (!is.null(title_result) && nrow(title_result$recommendations) > 0) {
      return(title_result$recommendations)
    }
  }
  
  recommend_from_query(
    query = query,
    n = n,
    feature_matrix = feature_matrix,
    movie_lookup = movie_lookup,
    row_norms = row_norms
  )
}