{app <- function() {
    source(here::here('src', 'Initialiser.R'))

    initialise_app()

    run_mapper()
}

app()}

# ===== MANUAL IMPLEMENTATION OF THE APP =======================================
# Un-comment the following code to start a manual run of the mapper

# source(here::here('src', 'Initialiser.R'))
#
# # ----- Initialise the app -----------------------------------------------------
# # Imports all functions, verifies output folders, and sets up fonts and DPI
#
# initialise_app()
#
# # ----- Loading the saved data instead of processing it again ------------------
# texts <- extract('Test')
#
# # You can tidy the extracted texts
# tidy <- tidify(texts,
#                token='sentences',
#                low_lim = 0.65,
#                up_lim = 0.7,
#                export_json = TRUE,
#                version_name = 'PADs')
# # OR --------------------------------- or Using previously pre-processed results
# tidy <- from_saves('PADs')
#
# # ----- Importing the classification model -------------------------------------
# classifier <- readRDS("Settings/Model/model_SDGs.Rds")
#
# # ----- Classifying the texts --------------------------------------------------
# a <- map_texts(tidy, 'SDGs')
#
