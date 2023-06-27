source(here::here('src', 'Initialiser.R'))

# ===== Initialise the app =====================================================
# Imports all functions, verifies output folders, and sets up fonts and DPI

initialise_app()

# ===== Loading the saved data instead of processing it again ==================
texts <- extract('Test')

# You can tidy the extracted texts
tidy <- tidify(texts,
               token='sentences',
               low_lim = 0.65,
               up_lim = 0.7,
               export_json = TRUE,
               version_name = 'PADs')
# --------------------------- or Using previously pre-processed results
tidy <- from_saves('PADs')


# ===== Machine learning model =================================================

# ----- Data set-up for the classification model ----------

# Import the data used for training the model
# Data set-up for the classification model
# Connect to the training database
conn <- dbConnect(RSQLite::SQLite(),
                  here("Settings", "Model", "training_data.db"))

# Import the data used for training the model
data_training <- dbGetQuery(conn, glue("SELECT * FROM SDGs"))

dbDisconnect(conn)

# Combine the training data set with the extracted texts
data_complete <- rbind(data_training,
                       tidy)

data_complete <- data_complete %>%
    mutate(Text = str_replace_all(data_complete$Text, '[^A-Za-z ]', ''))

# Capturing the data sets' length
nrows_data_t <- nrow(data_training)
nrows_data_c <- nrow(data_complete)

rm(data_training)

# Create a Document Term Matrix (DTM)
dtm <- corpus_dtm(data_complete)

# Create the working data set
working_dataset <- codify(dtm, data_complete)
rm(dtm)


# ----- Create the random forest model -----------

# Set random seed
set.seed(612)

# Select training data
x_train <- working_dataset[1:nrows_data_t,]
x_train$Target <- factor(x_train$Target)

# Select test data
x_test <- working_dataset[nrows_data_t:nrows_data_c,]
x_test <- x_test %>% slice(-1)
x_test$Target <- factor(x_test$Target)

rm(working_dataset)

ncols_dtm <- dim(x_train)[2]

# Train the random forest model using the training data set
if ("model.Rdata" %in% list.files(here("Settings", "Model"))) {
    load(here("Settings", "Model", "model.Rdata"))
} else {
    t0 <- Sys.time()
    classifier <- randomForest(x = x_train[, -ncols_dtm],
                               y = x_train$Target,
                               ntree = 51)
    rm(x_train)
}

# Classify the input data -- Map the projects to the SDGs
y_pred <- predict(classifier, newdata = x_test[, -ncols_dtm])
rm(x_test)

# Save the results in a character vector
classified <- character()
for (goal in y_pred) {classified = c(classified, goal)}

# Subset the data frame and paste the classified data
results <- data_complete %>%
    slice(nrows_data_t+1: nrows_data_c)

rm(data_complete)

results$Target <- classified





results <- identify_SDGs(results)

results <- results %>% dplyr::filter(SDG != "No map")

results <- as_tibble(results)


# ====== Summaries =============================================================

# Total matches by project -------------------------------> can feed a histogram
matches_T <- count_matches(results,
                           by = 'total_matches',
                           sorted = 'Frequency')

# Total matches by Project and SDG ---------------------> can feed a column plot
matches_SDG <- count_matches(results,
                             by = 'SDG',
                             sorted = 'Frequency',
                             collapse_projects = TRUE)

# Total matches by Project and Target ------------------> can be exported as csv
matches_tgt <- count_matches(results,
                             by = 'Target',
                             sorted = 'Frequency',
                             collapse_projects = TRUE)

# Binary occurrence results by SDG ---------------------> can be exported as csv
occurrence_SDG <- count_occurrence(results,
                                   by = 'SDG',
                                   collapse_projects = TRUE)


# Identify the main SDG in each project. From binary == TRUE identifies the main
# SDG with the sum of the binary frequencies (presence or absence of a target)
# of all targets in a SDG.
# -------------------------------> can feed a column plot and be exported to csv
main_SDGs <- get_main_SDG(results,
                          from_binary = FALSE,
                          collapse_SDG = TRUE)

SDGs_proj <- get_SDGs_proj(results)


# Results as matrix ------------------------------------------------------------

matrix_relative <- results_matrix(results,
                                  relative_freqs = TRUE,
                                  with_main_SDG = TRUE)

matrix_absolute <- results_matrix(results,
                                  relative_freqs = FALSE,
                                  with_main_SDG = TRUE)


# Plotting =====================================================================

plt <- occurrence_SDG %>% plot_results(
    title = 'Testing',
    xlabel = 'SDG',
    ylabel = 'Number of projects',
    fontsize_barlabs = 9,
    fontsize_title = 50,
    fontsize_axis = 30,
    scale = 1,
    dpi = 300)

export_plot('SDGs', plt, 'occurrence', scale = 3)

matches_SDG %>% plot_results(
    title = 'Matches of the SDGs',
    subtitle = '(Number of times each SDG was mapped)',
    xlabel ='SDG',
    ylabel ='Number of matches',
    fontsize_barlabs = 5,
    scale = 2)

main_SDGs %>% plot_results(
    title = 'Predominant SDGs',
    subtitle = 'Across the portfolio',
    xlabel = 'SDG',
    ylabel = 'Number of projects',
    scale = 1)

results %>% plot_SDG_distribution(
    binwidth = 2,
    title = "Distribution of the SDGs",
    subtitle = "By the number of projects they map to",
    test = FALSE)


# Network ======================================================================

# Generate network from results --------------------
net <- generate_network(results)

graph <- plot_network(results,
                      fontsize_base = 23,
                      fontsize_title = 40,
                      fontsize_subt = 30,
                      dpi = 96,
                      scale = 1)

graph


# ===== Prompts ================================================================

prompt_export_plot('SDGs',
                   occurrence_SDG,
                   "Test",
                   'Testing the test',
                   'SDG',
                   'Count',
                   fontsize_title = 50,
                   fontsize_subt = 35,
                   fontsize_barlabs = 11,
                   fontsize_axis = 30,
                   dpi = 300,
                   figname = 'Testingg')

prompt_export_plot('SDGs',
                   matches_SDG,
                   "Test",
                   'Testing the test',
                   xlabel = 'SDG',
                   ylabel = 'Count',
                   fontsize_title = 50,
                   fontsize_subt = 35,
                   fontsize_barlabs = 11,
                   fontsize_axis = 30,
                   dpi = 300,
                   figname = 'Testingg')

prompt_export_plot('SDGs',
                   main_SDGs,
                   "Test",
                   'Testing the test',
                   xlabel = 'SDG',
                   ylabel = 'Count',
                   fontsize_title = 50,
                   fontsize_subt = 35,
                   fontsize_barlabs = 11,
                   fontsize_axis = 30,
                   dpi = 300,
                   figname = 'Testingg')

prompt_export_histogram(analysis_mode = 'SDGs',
                        data = results,
                        binwidth = 2,
                        title = "Distribution",
                        subt = "WB Portfolio")


testing <- function(analysis_mode,
                    data,
                    title = NULL,
                    subtitle = NULL,
                    xlabel,
                    ylabel,
                    font = "Roboto Condensed",
                    fontsize_barlabs = 14,
                    fontsize_title = 20,
                    fontsize_subt = 16,
                    fontsize_axis = 15,
                    figname = NULL,
                    dpi = 96,
                    scale = 1,
                    transparent_bg = FALSE) {
    folder <- here(outputs_folder(analysis_mode, 'img'))

    plot <- plot_SDG_distribution(analysis_mode,
                                  data,
                                  2,
                                  title,
                                  subtitle,
                                  xlabel,
                                  ylabel,
                                  font,
                                  fontsize_title = 20,
                                  fontsize_subt = 16,
                                  fontsize_axis = 15,
                                  dpi = 96,
                                  scale = 1)

    print(plot)

    cli_h3("Do you wish to save this plot?")
    cli_alert_info(paste0(
        "Save plot: press 'y' or 'Y' and hit Enter\n",
        "Discard: press 'n' or 'N' and hit Enter\n\n")
    )

    answer <- invisible(readline())

    if ((answer == 'y') || (answer == 'Y')) {
        plot <- plot_SDG_distribution(analysis_mode,
                                      data,
                                      2,
                                      title,
                                      subtitle,
                                      xlabel,
                                      ylabel,
                                      font,
                                      fontsize_title,
                                      fontsize_subt,
                                      fontsize_axis,
                                      dpi = dpi,
                                      scale = 1)

        export_plot(analysis_mode,
                    plot,
                    figname = figname,
                    dpi = dpi,
                    scale = scale)
    } else if ((answer == 'n') || (answer == 'N')) {
        cli_alert_warning("Plot '{figname}.png' will not be saved")
    } else {
        cli_alert_warning(
            glue("You should introduce either 'y' / 'Y' or 'n' / 'N' ",
                 "but introduced {answer}. Try again"))
        prompt_export_plot(data = data,
                           title = title,
                           subtitle = subtitle,
                           xlabel = xlabel,
                           ylabel = ylabel,
                           font = font,
                           fontsize_title = fontsize_title,
                           fontsize_subt = fontsize_subt,
                           fontsize_axis = fontsize_axis,
                           figname = figname,
                           dpi = dpi,
                           scale = scale)
    }
}

testing("SDGs", results, xlabel = "X-axis", ylabel = "Y-axis", figname  = "aaa")

}
}