# Shiny user interface. Runs after global.R.

app_theme <- bs_theme(
  version = 5,
  bg = "#f8fafc",
  fg = "#0f172a",
  primary = "#4f46e5",
  secondary = "#64748b",
  success = "#059669",
  base_font = font_google("Inter"),
  heading_font = font_google("Inter"),
  "border-radius" = "0.625rem",
  "btn-border-radius" = "0.625rem"
)

ui <- page_fillable(
  title = APP_TITLE,
  theme = app_theme,
  padding = 0,
  gap = 0,
  fillable_mobile = TRUE,
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "chat.css")
  ),
  chat_ui()
)
