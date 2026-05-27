# Natural-language movie title resolver.
# Public API: resolve_movie_title_from_text()

normalize_title <- function(x) {
  x <- ifelse(is.na(x), "", x)
  x <- as.character(x)
  x <- iconv(x, from = "", to = "UTF-8", sub = " ")
  x <- tolower(x)
  x <- gsub("[^[:alnum:]' ]+", " ", x)
  x <- gsub("\\s+", " ", x)
  trimws(x)
}

extract_quoted_phrase <- function(text) {
  if (!nzchar(text)) {
    return("")
  }

  double_match <- regexec('"([^"]+)"', text)
  double_value <- regmatches(text, double_match)[[1]]
  if (length(double_value) > 1) {
    return(trimws(double_value[2]))
  }

  single_match <- regexec("'([^']+)'", text)
  single_value <- regmatches(text, single_match)[[1]]
  if (length(single_value) > 1) {
    return(trimws(single_value[2]))
  }

  ""
}

strip_query_boilerplate <- function(text) {
  x <- tolower(text)
  patterns <- c(
    "what\\s+is\\s+something\\s+similar\\s+to",
    "what\\s+would\\s+you\\s+recommend\\s+if\\s+i\\s+liked",
    "what\\s+would\\s+you\\s+recommend\\s+if\\s+i\\s+like",
    "what\\s+would\\s+you\\s+reccommend\\s+if\\s+i\\s+liked",
    "what\\s+would\\s+you\\s+reccommend\\s+if\\s+i\\s+like",
    "what\\s+would\\s+you\\s+reccomend\\s+if\\s+i\\s+liked",
    "what\\s+would\\s+you\\s+reccomend\\s+if\\s+i\\s+like",
    "what\\s+would\\s+you\\s+recommend\\s+if\\s+i\\s+loved",
    "can\\s+you\\s+recommend",
    "can\\s+you\\s+reccommend",
    "can\\s+you\\s+reccomend",
    "can\\s+you\\s+suggest",
    "could\\s+you\\s+recommend",
    "could\\s+you\\s+reccommend",
    "could\\s+you\\s+reccomend",
    "could\\s+you\\s+suggest",
    "recommend\\s+me\\s+(a\\s+)?movie\\s+(like|similar\\s+to)",
    "reccommend\\s+me\\s+(a\\s+)?movie\\s+(like|similar\\s+to)",
    "reccomend\\s+me\\s+(a\\s+)?movie\\s+(like|similar\\s+to)",
    "recommend\\s+me\\s+(a\\s+)?movie",
    "reccommend\\s+me\\s+(a\\s+)?movie",
    "reccomend\\s+me\\s+(a\\s+)?movie",
    "recommend\\s+something\\s+like",
    "reccommend\\s+something\\s+like",
    "reccomend\\s+something\\s+like",
    "movies?\\s+similar\\s+to",
    "movies?\\s+like",
    "similar\\s+to",
    "if\\s+i\\s+liked",
    "if\\s+i\\s+like",
    "if\\s+i\\s+loved",
    "i\\s+liked",
    "i\\s+like",
    "i\\s+loved",
    "what\\s+should\\s+i\\s+watch\\s+after",
    "what\\s+should\\s+i\\s+watch",
    "show\\s+me\\s+movies?\\s+like",
    "please",
    "recommend",
    "reccommend",
    "reccomend",
    "suggest"
  )

  for (pattern in patterns) {
    x <- gsub(pattern, " ", x, perl = TRUE)
  }

  x <- gsub("[?!.:,;]+", " ", x)
  x <- gsub("\\s+", " ", x)
  trimws(x)
}

tokenize_title <- function(text) {
  normalized <- normalize_title(text)
  if (!nzchar(normalized)) {
    return(character(0))
  }
  tokens <- strsplit(normalized, "\\s+")[[1]]
  tokens[tokens != ""]
}

make_ranked_candidates <- function(candidate_text, movies_catalog, top_k) {
  stopifnot(is.data.frame(movies_catalog))

  catalog <- movies_catalog
  catalog$title <- as.character(catalog$title)
  catalog <- catalog[!is.na(catalog$title) & nzchar(trimws(catalog$title)), , drop = FALSE]
  if (nrow(catalog) == 0) {
    return(data.frame(id = character(0), title = character(0), score = numeric(0), reason = character(0)))
  }

  title_norm <- normalize_title(catalog$title)
  candidate_norm <- normalize_title(candidate_text)

  if (!nzchar(candidate_norm)) {
    return(data.frame(id = character(0), title = character(0), score = numeric(0), reason = character(0)))
  }

  candidate_tokens <- tokenize_title(candidate_norm)
  stop_tokens <- c(
    "the", "a", "an", "movie", "film", "to", "for", "of", "and", "is", "it",
    "what", "would", "you", "if", "i", "like", "liked", "loved",
    "can", "could", "should", "watch",
    "recommend", "reccommend", "reccomend", "suggest", "something", "similar"
  )
  candidate_tokens <- setdiff(candidate_tokens, stop_tokens)
  candidate_tokens <- unique(candidate_tokens)

  # Track the best score and reason seen for each row index.
  best_score <- rep(0, nrow(catalog))
  best_reason <- rep("", nrow(catalog))
  reason_priority <- c("fuzzy" = 1L, "token_overlap" = 2L, "substring" = 3L, "exact" = 4L)

  apply_score <- function(indices, scores, reason_name) {
    if (length(indices) == 0) {
      return(invisible(NULL))
    }
    for (j in seq_along(indices)) {
      i <- indices[j]
      score <- scores[j]
      if (is.na(score) || score <= 0) {
        next
      }
      is_better <- score > best_score[i]
      same_score <- !is_better && abs(score - best_score[i]) < 1e-9
      higher_reason_priority <- same_score &&
        reason_priority[[reason_name]] > ifelse(best_reason[i] == "", 0L, reason_priority[[best_reason[i]]])
      if (is_better || higher_reason_priority) {
        best_score[i] <<- score
        best_reason[i] <<- reason_name
      }
    }
    invisible(NULL)
  }

  # 1) Exact / normalized exact.
  exact_idx <- which(title_norm == candidate_norm)
  if (length(exact_idx) > 0) {
    apply_score(exact_idx, rep(1, length(exact_idx)), "exact")
  }

  # 2) Whole-phrase substring containment in either direction.
  if (nchar(candidate_norm) >= 3) {
    contains_candidate <- which(grepl(candidate_norm, title_norm, fixed = TRUE))
    min_contained_len <- max(4L, floor(0.55 * nchar(candidate_norm)))
    contained_in_candidate <- which(
      vapply(
        title_norm,
        function(t) nchar(t) >= min_contained_len && grepl(t, candidate_norm, fixed = TRUE),
        logical(1),
        USE.NAMES = FALSE
      )
    )
    sub_idx <- sort(unique(c(contains_candidate, contained_in_candidate)))
    if (length(sub_idx) > 0) {
      len_ratio <- pmin(nchar(candidate_norm), nchar(title_norm[sub_idx])) /
        pmax(nchar(candidate_norm), nchar(title_norm[sub_idx]))
      apply_score(sub_idx, 0.72 + 0.20 * len_ratio, "substring")
    }
  }

  # 3) Token overlap rescue, guarded to avoid weak short-token false positives.
  if (length(candidate_tokens) >= 2 || any(nchar(candidate_tokens) >= 4)) {
    hit_count <- vapply(
      title_norm,
      function(t) {
        sum(vapply(candidate_tokens, function(tok) grepl(paste0("\\b", tok, "\\b"), t), logical(1), USE.NAMES = FALSE))
      },
      numeric(1),
      USE.NAMES = FALSE
    )
    min_required_hits <- if (length(candidate_tokens) >= 2) 2 else 1
    token_idx <- which(hit_count >= min_required_hits)
    if (length(token_idx) > 0 && length(candidate_tokens) > 0) {
      token_score <- 0.35 + 0.45 * (hit_count[token_idx] / length(candidate_tokens))
      apply_score(token_idx, token_score, "token_overlap")
    }
  }

  # 4) Restricted fuzzy matching.
  fuzzy_pool <- integer(0)
  if (length(exact_idx) == 0 && length(candidate_tokens) > 0) {
    longest_token <- candidate_tokens[which.max(nchar(candidate_tokens))]
    if (nchar(longest_token) >= 3) {
      fuzzy_pool <- which(grepl(paste0("\\b", longest_token, "\\b"), title_norm))
    }
  }
  if (length(fuzzy_pool) > 0) {
    distances <- adist(candidate_norm, title_norm[fuzzy_pool], ignore.case = TRUE)
    distance_scale <- pmax(nchar(candidate_norm), nchar(title_norm[fuzzy_pool]))
    similarity <- 1 - (as.numeric(distances) / distance_scale)
    fuzzy_idx <- fuzzy_pool[similarity >= 0.72]
    if (length(fuzzy_idx) > 0) {
      fuzzy_scores <- 0.40 + 0.45 * similarity[similarity >= 0.72]
      apply_score(fuzzy_idx, fuzzy_scores, "fuzzy")
    }
  }

  matched_idx <- which(best_score > 0)
  if (length(matched_idx) == 0) {
    return(data.frame(id = character(0), title = character(0), score = numeric(0), reason = character(0)))
  }

  popularity <- if ("tmdb_popularity" %in% names(catalog)) {
    suppressWarnings(as.numeric(catalog$tmdb_popularity))
  } else {
    rep(NA_real_, nrow(catalog))
  }
  popularity[is.na(popularity)] <- -Inf

  ranked <- data.frame(
    id = catalog$id[matched_idx],
    title = catalog$title[matched_idx],
    score = round(best_score[matched_idx], 4),
    reason = best_reason[matched_idx],
    popularity = popularity[matched_idx],
    title_len = nchar(catalog$title[matched_idx]),
    stringsAsFactors = FALSE
  )

  ranked <- ranked[order(-ranked$score, -ranked$popularity, ranked$title_len, ranked$title), , drop = FALSE]
  ranked <- head(ranked, top_k)
  ranked[, c("id", "title", "score", "reason"), drop = FALSE]
}

build_ambiguity_prompt <- function(candidates) {
  lines <- vapply(
    seq_len(nrow(candidates)),
    function(i) {
      paste0(i, ". ", candidates$title[i], " (id: ", candidates$id[i], ")")
    },
    character(1)
  )
  paste(
    "I found multiple possible matches. Please pick the exact title:",
    paste(lines, collapse = "\n"),
    sep = "\n"
  )
}

resolve_movie_title_from_text <- function(
    user_text,
    movies_catalog,
    top_k = 5L,
    ambiguity_margin = 0.08,
    min_confidence = 0.45
) {
  tryCatch({
    if (!is.character(user_text) || length(user_text) != 1 || is.na(user_text)) {
      return(list(
        status = "invalid_input",
        resolved_id = NA,
        resolved_title = NA_character_,
        candidates = data.frame(id = character(0), title = character(0), score = numeric(0), reason = character(0)),
        user_prompt = "Please provide a movie request as text (for example: \"recommend movies like The Matrix\")."
      ))
    }

    if (!is.data.frame(movies_catalog) || !all(c("id", "title") %in% names(movies_catalog))) {
      stop("movies_catalog must be a data.frame with columns: id, title")
    }

    cleaned_input <- trimws(user_text)
    if (!nzchar(cleaned_input)) {
      return(list(
        status = "invalid_input",
        resolved_id = NA,
        resolved_title = NA_character_,
        candidates = data.frame(id = character(0), title = character(0), score = numeric(0), reason = character(0)),
        user_prompt = "Please enter a movie title or a sentence that includes one."
      ))
    }

    quoted_candidate <- extract_quoted_phrase(cleaned_input)
    candidate_text <- if (nzchar(quoted_candidate)) quoted_candidate else strip_query_boilerplate(cleaned_input)

    candidates <- make_ranked_candidates(candidate_text, movies_catalog, top_k = top_k)
    if (nrow(candidates) == 0) {
      return(list(
        status = "not_found",
        resolved_id = NA,
        resolved_title = NA_character_,
        candidates = candidates,
        user_prompt = "I could not confidently match a movie title. Please provide the exact movie title in quotes."
      ))
    }

    top_candidate <- candidates[1, , drop = FALSE]
    second_score <- if (nrow(candidates) >= 2) candidates$score[2] else -Inf
    score_gap <- top_candidate$score[1] - second_score

    if (top_candidate$score[1] < min_confidence) {
      return(list(
        status = "not_found",
        resolved_id = NA,
        resolved_title = NA_character_,
        candidates = candidates,
        user_prompt = "I found weak matches only. Please provide the exact movie title, ideally in quotes."
      ))
    }

    if (nrow(candidates) >= 2 && score_gap <= ambiguity_margin) {
      return(list(
        status = "ambiguous",
        resolved_id = NA,
        resolved_title = NA_character_,
        candidates = candidates,
        user_prompt = build_ambiguity_prompt(candidates)
      ))
    }

    list(
      status = "resolved",
      resolved_id = top_candidate$id[1],
      resolved_title = top_candidate$title[1],
      candidates = candidates,
      user_prompt = ""
    )
  }, error = function(e) {
    list(
      status = "error",
      resolved_id = NA,
      resolved_title = NA_character_,
      candidates = data.frame(id = character(0), title = character(0), score = numeric(0), reason = character(0)),
      user_prompt = "I hit an internal error while matching the movie title. Please try again with an exact title.",
      error = conditionMessage(e)
    )
  })
}
