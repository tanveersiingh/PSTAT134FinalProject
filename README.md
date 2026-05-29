# PSTAT 134 Final Project — Movie Recommender (Shiny)

A Shiny app + accompanying write-up that builds a **content-based movie recommender** using **NLP features (bigram TF–IDF)** from movie metadata.

## What this project does

- Loads and cleans the TMDB/MovieLens metadata datasets.
- Builds NLP features from movie text fields.
- Creates a feature matrix and recommends similar movies using vector similarity.
- Provides a simple **chat-style Shiny interface** where users can type a movie they like (or a free-form query) and receive recommendations.

## Running the app locally

### 1) Get the dataset

Download **“The Movies Dataset”** from [Kaggle](https://www.kaggle.com/datasets/rounakbanik/the-movies-dataset) and place the CSVs into the expected folder structure.

The app expects the following files to exist:

- `data/movies/movies_metadata.csv`
- `data/movies/ratings.csv`
- `data/movies/keywords.csv`
- `data/movies/credits.csv`
- `data/movies/links.csv`

### 2) Install R packages

Packages are loaded in `R/app_packages.R`:

- shiny
- bslib
- dplyr
- readr
- tidyverse
- ISOcodes
- tidytext
- Matrix
- htmltools

### 3) Start the app

From the repository root in R/RStudio:

```r
shiny::runApp()
```

On startup, `global.R` calls `bootstrap_recommender()`, which:

- loads the raw CSVs
- cleans/merges datasets
- builds the bigram TF–IDF representation
- creates the feature matrix used for recommendations

The first run may take a few minutes depending on your machine.

## Project structure (high-level)

```text
.
├── R/                      # Data cleaning, NLP, recommender + Shiny helpers
├── www/                    # App static assets (CSS, etc.)
├── examples/               # Example files / artifacts
├── tests/                  # Tests (if present)
├── ProjectMemo/            # Course deliverables
├── 134FinalProjectWriteUp.*
├── global.R
├── server.R
└── ui.R
```

## Notes / troubleshooting

- If you see file-not-found errors, double-check the dataset path is `data/movies/...` and filenames match exactly.
- If you run into memory/time issues when building the model, try running in a fresh R session and ensure you have enough RAM available.

## Authors

Eric Livshiz, Dylan Crookes, Tanveer Singh, Jake Vurpillat, Samuel Erlikhman

University of California, Santa Barbara
