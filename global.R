# Runs once when the Shiny app process starts (before ui.R and server.R).

source("R/app_packages.R", local = FALSE)
source("R/03_clean_and_merge.R", local = FALSE)
source("R/06_NLP.R", local = FALSE)
source("R/title_resolver.R", local = FALSE)
source("R/05_recommender.R", local = FALSE)
source("R/app_bootstrap.R", local = FALSE)
source("R/chat_helpers.R", local = FALSE)
source("R/ui_chat.R", local = FALSE)

APP_TITLE <- "PSTAT 134 — Movie Recommender"
APP_VERSION <- "0.1.0"

message("Building recommender model (this may take a few minutes)...")
bootstrap_recommender()
message("Recommender ready.")
