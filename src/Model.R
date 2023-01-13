load_training_data <- function(analysis_mode) {
    # ----- Data set-up for the classification model ----------

    # Import the data used for training the model
    # Data set-up for the classification model
    # Connect to the training database

    if (file.exists(here("Settings", "Model", "training_data.db"))) {
        conn <- dbConnect(RSQLite::SQLite(),
                          here("Settings", "Model", "training_data.db"))
    } else {
        cli_alert_warning(
            glue(
                "No training database found. Please, make sure that there ",
                "is a database file named {col_red('training_data.db')} at ",
                "{col_red(here('Settings', 'Model'))}"
            )
        )
    }
    # Import the data used for training the model
    data_training <- dbGetQuery(conn, glue("SELECT * FROM {analysis_mode}"))

    dbDisconnect(conn)

    # Create a Document Term Matrix (DTM)
    dtm <- corpus_dtm(data_training)

    # Create the working data set
    x_train <- codify(dtm, data_training)
    x_train <- x_train %>% slice(-1)

    return(x_train)
}

train_model <- function(analysis_mode) {
    set.seed(612)

    cli_h2("Training model")
    cli_text("Please wait, this could take several minutes.")

    x_train <- load_training_data(analysis_mode)

    x_train$Target <- factor(x_train$Target)

    ncols <- dim(x_train)[2]

    if (analysis_mode == 'SDGs') {
        classifier <- randomForest(x = x_train[, -ncols],
                                   y = x_train$Target,
                                   ntree = 51)

        saveRDS(classifier, file = here("Settings",
                                     "Model",
                                     "model_SDGs.Rds"))

        cli_alert_success(glue(
            "Model successfully exported to the path ",
            style_underline(style_italic(col_br_red(here(
                "Settings", "Model", "model_SDGs.Rds"
            ))))
        ))
    } else if (analysis_mode == 'EUT') {
        classifier <- randomForest(x = x_train[, -ncols],
                                   y = x_train$Target,
                                   ntree = 51)

        save(classifier, file = here("Settings",
                                     "Model",
                                     "model_EUT.Rds"))

        cli_alert_success(glue(
            "Model successfully exported to the path ",
            style_underline(style_italic(col_br_red(here(
                "Settings", "Model", "model_EUT.Rdata"
            ))))
        ))
    } else {cli_alert_warning("Analysis mode should be 'SDGs' or 'EUT'.")}

    write.csv(colnames(x_train),
              here(
                  "Settings",
                  "Model",
                  glue("cols_training_{analysis_mode}.csv")
              ),
              row.names = FALSE,
              fileEncoding = 'UTF-8')
}


load_model <- function(analysis_mode) {
    if (file.exists(
        here("Settings", "Model", glue("model_{analysis_mode}.Rds"))
    )) {
        loadRDS(here("Settings", "Model", glue("model_{analysis_mode}.Rds")))
    } else {
        cli_alert_warning(
            glue("There is no trained model in the folder. ",
                 "Press ", col_green("ENTER"), " to train it.")
        )

        invisible(readline())

        train_model(analysis_mode)
    }
}

analysis_mode = 'SDGs'

map_texts <- function(tidy_texts, analysis_mode) {
    # Create a Document Term Matrix (DTM)
    dtm <- corpus_dtm(tidy)

    # Create the working data set
    x_test <- codify(dtm, tidy)
    # x_test <- x_test %>% slice(-1)

    col <- read.csv(here(
        "Settings", "Model", glue("cols_training_{analysis_mode}.csv")))[[1]]

    x_test <- x_test[, intersect(colnames(x_test), col)]

    missing <- setdiff(col, colnames(x_test))

    mat_zero <- setNames(as.data.frame(matrix(0,
                                              ncol = length(missing),
                                              nrow = nrow(x_test))),
                         missing)

    x_test <- cbind(mat_zero, x_test)

    x_test <- x_test[col]

    x_train <- load_training_data(analysis_mode)

    working_dataset <- rbind(x_test, x_train)

    working_dataset$Target <- factor(working_dataset$Target)

    test_row <- nrow(x_test)
    train_row <- nrow(x_train)

    x_test <- working_dataset[1:test_row, ]

    # Predict the goals that each text maps to
    y_pred <- predict(classifier, newdata = x_test[, -ncol(x_test)])

    # Save the results in a character vector
    classified <- character()
    for (goal in y_pred) {classified = c(classified, goal)}

    # Subset the data frame and paste the classified data
    results <- tidy_texts

    results$Target <- classified

    return(results)
}

# =====

# classifier <- readRDS("Settings/Model/model_SDGs.Rds")
#
# train_model('SDGs')
#
# load_model('SDGs')
#
# a <- map_texts(tidy, 'SDGs')

# =====



