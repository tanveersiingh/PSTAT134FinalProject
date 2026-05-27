# Run from project root:
#   Rscript tests/test_title_resolver.R

source("R/title_resolver.R")

toy_catalog <- data.frame(
  id = c(603, 604, 605, 606, 607, 608),
  title = c(
    "The Matrix",
    "The Matrix Reloaded",
    "The Matrix Revolutions",
    "Matrix",
    "The Lion King",
    "Inception"
  ),
  tmdb_popularity = c(80, 70, 65, 20, 75, 85),
  stringsAsFactors = FALSE
)

resolved <- resolve_movie_title_from_text(
  "What would you recommend if I liked The Matrix?",
  toy_catalog
)
stopifnot(resolved$status == "resolved")
stopifnot(identical(resolved$resolved_title, "The Matrix"))

resolved_typo <- resolve_movie_title_from_text(
  "What would you reccommend if I like the Matrix",
  toy_catalog
)
stopifnot(resolved_typo$status == "resolved")
stopifnot(identical(resolved_typo$resolved_title, "The Matrix"))

resolved_quoted <- resolve_movie_title_from_text(
  "What is something similar to \"The Matrix Reloaded\"?",
  toy_catalog
)
stopifnot(resolved_quoted$status == "resolved")
stopifnot(identical(resolved_quoted$resolved_title, "The Matrix Reloaded"))

ambiguous <- resolve_movie_title_from_text(
  "recommend something like matrix",
  toy_catalog,
  ambiguity_margin = 0.25
)
stopifnot(ambiguous$status == "ambiguous")
stopifnot(nrow(ambiguous$candidates) >= 2)
stopifnot(nzchar(ambiguous$user_prompt))

not_found <- resolve_movie_title_from_text(
  "Can you recommend something like qwertyuiop?",
  toy_catalog
)
stopifnot(not_found$status == "not_found")

invalid <- resolve_movie_title_from_text(
  "",
  toy_catalog
)
stopifnot(invalid$status == "invalid_input")

cat("All title resolver tests passed.\n")
