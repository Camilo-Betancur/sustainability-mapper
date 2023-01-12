load_training_data <- function(analysis_mode) {
    # ----- Data set-up for the classification model ----------

    # Import the data used for training the model
    # Data set-up for the classification model
    # Connect to the training database
    conn <- dbConnect(RSQLite::SQLite(),
                      here("Settings", "Model", "training_data.db"))

    # Import the data used for training the model
    data_training <- dbGetQuery(conn, glue("SELECT * FROM {analysis_mode}"))

    dbDisconnect(conn)

    # Create a Document Term Matrix (DTM)
    dtm <- corpus_dtm(data_training)

    # Create the working data set
    working_dataset <- codify(dtm, data_training)

    return(working_dataset)
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

        save(classifier, file = here("Settings",
                                     "Model",
                                     "model_SDGs.Rdata"))

        cli_alert_success(glue(
            "Model successfully exported to the path ",
            style_underline(style_italic(col_br_red(here(
                "Settings", "Model", "model_SDGs.Rdata"
            ))))
        ))
    } else if (analysis_mode == 'EUT') {
        classifier <- randomForest(x = x_train[, -ncols],
                                   y = x_train$Target,
                                   ntree = 51)

        save(classifier, file = here("Settings",
                                     "Model",
                                     "model_EUT.Rdata"))

        cli_alert_success(glue(
            "Model successfully exported to the path ",
            style_underline(style_italic(col_br_red(here(
                "Settings", "Model", "model_EUT.Rdata"
            ))))
        ))
    } else {cli_alert_warning("Analysis mode should be 'SDGs' or 'EUT'.")}
}


load_model <- function(analysis_mode) {
    if (file.exists(
        here("Settings", "Model", glue("model_{analysis_mode}.Rdata"))
    )) {
        load(here("Settings", "Model", glue("model_{analysis_mode}.Rdata")))
    } else {
        train_model(analysis_mode)
    }
}


map_texts <- function(tidy_texts, analysis_mode) {
    # Create a Document Term Matrix (DTM)
    dtm <- corpus_dtm(tidy_texts)

    # Create the working data set
    x_test <- codify(dtm, tidy_texts)
    x_test <- x_test %>% slice(-1)
    x_test$Target <- factor(x_test$Target)

    # Load the classification model
    load()

    # Predict the goals that each text maps to
    y_pred <- predict(classifier, newdata = x_test[, -ncols_dtm])

    # Save the results in a character vector
    classified <- character()
    for (goal in y_pred) {classified = c(classified, goal)}

    # Subset the data frame and paste the classified data
    results <- tidy_texts

    results$Target <- classified
}


load_model('EUT')

train_model('EUT')
