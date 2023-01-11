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
                  here("Settings", "training_data.db"))

# Import the data used for training the model
data_training <- dbGetQuery(conn, glue("SELECT * FROM EUT"))

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
t0 <- Sys.time()
classifier <- randomForest(x = x_train[, -ncols_dtm],
                           y = x_train$Target,
                           ntree = 51)
rm(x_train)

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

results$Goal <- classified

results <- results %>% dplyr::filter(Goal != "No map")

results <- as_tibble(results)


# ====== Summaries =============================================================

# Total matches by project -------------------------------> can feed a histogram
matches <- count_matches(results,
                         by = 'Goal',
                         sorted = 'Frequency')


# Results as matrix ------------------------------------------------------------

matrix_relative <- results_matrix(results,
                                  'Goal',
                                  relative_freqs = TRUE,
                                  with_main_SDG = FALSE)

matrix_absolute <- results_matrix(results,
                                  'Goal',
                                  relative_freqs = TRUE,
                                  with_main_SDG = FALSE)


# ===== Plotting ===============================================================

plot_EUT_results(results, xlabel='Goal', ylabel='Frequency')


# ===== Network ================================================================

# Generate network from results --------------------
net <- generate_network(results)

graph <- plot_network('EUT',
                      results,
                      fontsize_base = 15,
                      fontsize_title = 20,
                      fontsize_subt = 15,
                      dpi = 96,
                      scale = 1,
                      title = "Interactions between the EU Taxonomy Goals",
                      subtitle = "In the World Bank's portfolio")

graph
