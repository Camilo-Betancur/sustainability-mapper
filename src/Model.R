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
            style_path(here(
                "Settings", "Model", "model_SDGs.Rds"
            )),
            "."
        ))
    } else if (analysis_mode == 'EUT') {
        classifier <- randomForest(x = x_train[, -ncols],
                                   y = x_train$Target,
                                   ntree = 51)

        saveRDS(classifier, file = here("Settings",
                                        "Model",
                                        "model_EUT.Rds"))

        cli_alert_success(glue(
            "Model successfully exported to the path ",
            style_path(here(
                "Settings", "Model", "model_SDGs.Rds"
            )),
            "."
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


# Loads a previously trained Random Forest Model. Allows to choose between the
# SDGs and EU Taxonomy models.
load_model <- function(mode) {
    if (mode == 'SDGs') {
        model_path <- here("Settings",
                           "Model",
                           "model_SDGs.Rds")
    } else if (mode == 'EUT') {
        model_path <- here("Settings",
                           "Model",
                           "model_EUT.Rds")
    }

    if (file.exists(model_path)) {
        model <- readRDS(model_path)
    } else {
        cli_alert_warning(
            glue("There is no trained model in the folder. ",
                 "Press ", style_option("ENTER"), " to train it.")
        )

        invisible(readline())

        train_model(analysis_mode)
    }
    return(model)
}
