map_texts <- function(classifier, analysis_mode, tidy_texts) {
    if (analysis_mode == "EUT") {
        mode_aux <- "European Taxonomy"
    } else if (analysis_mode == "SDGs") {
        mode_aux <- "Sustainable Development Goals"
    } else {cli_abort("The analysis mode should be either EUT or SDGs.")}

    cli_h1("Mapping texts to the {mode_aux}")
    cli_h2("Preparing the test data")
    # Create a Document Term Matrix (DTM)
    dtm <- corpus_dtm(tidy_texts)

    # Create the raw complete data set
    x_test <- codify(dtm, tidy_texts)
    # x_test <- x_test %>% slice(-1)

    # Remove the columns of the test data set that are not in the columns of
    # the training data set.
    cols_training <- read.csv(here(
        "Settings", "Model", glue("cols_training_{analysis_mode}.csv")))[[1]]

    x_test <- x_test[, intersect(colnames(x_test), cols_training)]

    # Identifying the columns of the training data set that are missing from the
    # test data set.
    missing_cols <- base::setdiff(cols_training, colnames(x_test))

    # Creating temporal matrix filled with zeroes. Column names are the
    # missing_cols vector.
    mat_zero <- setNames(as.data.frame(matrix(0,
                                              ncol = length(missing_cols),
                                              nrow = nrow(x_test))),
                         missing_cols)

    # Cleaning test data set. This makes the training and test data sets have
    # the same number of columns, with exactly the same column names.
    x_test <- cbind(mat_zero, x_test)
    x_test <- x_test[cols_training]

    # Loads the training data.
    cli_h2("Matching the test data with training data")
    x_train <- load_training_data(analysis_mode)

    # Creates the working data set (union of training and test data sets).
    working_dataset <- rbind(x_test, x_train)

    working_dataset$Target <- factor(working_dataset$Target)

    # Identifies the row ids to slice the data sets into training and test rows.
    test_row <- nrow(x_test)
    train_row <- nrow(x_train)

    # Slices the working data set to extract the data to be predicted.
    x_test <- working_dataset[1:test_row, ]

    # Predict the goals that each text maps to.
    y_pred <- predict(classifier, newdata = x_test[, -ncol(x_test)])

    # Save the results in a character vector
    classified <- character()
    for (goal in y_pred) {classified = c(classified, goal)}

    # Subset the data frame and add the classified data as a new column
    results <- tidy_texts

    results$Target <- classified

    cli_alert_success("Text classification done.")

    return(results)
}