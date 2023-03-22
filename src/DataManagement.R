# Checks that the folders for results, saves, and other required folders exist
# while performing the analyses. If they do not exist, the function creates them
dir_checker <- function(silent = TRUE) {
    required <- scan(here("Settings", "dirs_required.txt"),
                     what = 'character',
                     sep = '\n',
                     quiet = TRUE)
    required <- here(required)

    for (dir in required) {
        if (!dir %in% list.dirs(here())) {
            dir.create(dir)
            cli_text("Created ",
                     style_path("'{here(dir)}'"),
                     " successfully.")
        } else {
            if (silent == FALSE) {
                cli_alert_success("{(dir)} exists.")
            }
        }
    }
}


# Auxiliary function that helps create_project_folder().
# Verifies that the project folder in /Output contains the required sub-folders.
# The folder structure is the following:
#
# root
# └ Output
#   └ project_name
#     ├ EU Taxonomy
#     | ├ data
#     | └ img
#     └ SDGs
#       ├ data
#       └ img
output_structure_checker <- function(project_name) {
    types_analysis <- c("EUT", "SDGs")
    types_folder <- c("data", "img")

    for (type_a in types_analysis) {
        if (!dir.exists(here("Output", project_name, type_a))) {
            dir.create(here("Output", project_name, type_a))
        }

        for (type_f in types_folder) {
            if (!dir.exists(here("Output", project_name, type_a, type_f))) {
                dir.create(here("Output", project_name, type_a, type_f))
            }
        }
    }
}


# Creates the folder for each project in the /Output folder
create_project_folder <- function(project_name) {
    if (!dir.exists(here("Output", project_name))) {
        dir.create(here("Output", project_name))

        output_structure_checker(project_name)

        name <- project_name
    } else {
        cli_alert_warning(glue(
            "There is an analysis using the name ", emphasize("{project_name}"),
            ", do you wish to overwrite it?\n"))

        cli_ul(c(
            glue("Press ", style_option("Y"), " or ", style_option("y"),
                 " to overwrite the Outputs folder."),
            glue("Press ", style_option("N"), " or ", style_option("n"),
                 " to use the same folder for another analysis."),
            glue("Press any other key to create a folder copy.")
        ))

        answer <- invisible(readline())

        if ((answer == 'y') | (answer == 'Y')) {
            unlink(here("Output", project_name),
                   recursive = TRUE)

            dir.create(here("Output", project_name), showWarnings = FALSE)

            output_structure_checker(project_name)

            name <- project_name

            cli_alert_warning(glue(
                "Folder ", style_path("Output/{project_name}"),
                " has been successfully ", "overwritten."
                )
            )
        } else if ((answer == 'n') | (answer == 'N')) {
            name <- project_name
        } else {
            datetime <- now() %>%
                str_remove_all(" -5") %>%
                str_replace_all(" ", "_") %>%
                str_replace_all(":", "-")

            name <- paste(project_name, datetime, sep = "_")

            dir.create(here("Output", name))

            output_structure_checker(name)

            cli_alert_warning(glue(
                "Created the project folder ",
                style_path("Output/{name}"), "."
            ))
        }
    }

    return(name)
}


# Auxiliary function for the exporter functions (graphs and data).
# It returns the proper folder in which to save the graphic and tabular results
output_folder_selector <- function(analysis_mode, project_name, kind) {
    return(here("Output", project_name, analysis_mode, kind))
}


# Saves tidy results from pre-processing
save_tidy_results <- function(project_name, analysis_mode, tidy_texts) {
    write(jsonlite::toJSON(tidy_texts),
          file = here(glue("Saves/{project_name}.json")))

    cli_alert_success(glue(
        "Successfully saved the tidy results to the path ",
        style_path("Saves/{project_name}.json\'")
    ))

    cli_text("")

    cli_alert_info(glue(
        "Remember that you can import previously-saved data ",
        "by running the function ",
        emphasize("\"from_saves(\'{project_name}\')\"")
    ))

    cli_text("")
}


# Reads previously saved tidy results from pre-processing
from_saves <- function(json_name) {
    path = here(glue('Saves/{json_name}.json'))

    json_file <- as_tibble(jsonlite::fromJSON(path))

    cli_alert_success(glue(
        "Successfully imported the saves file ",
        style_path(glue('Saves/{json_name}.json')),
        ".\n "
    ))

    return(json_file)
}


# ===== Loading functions
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


# ===== Exporter functions =====================================================

# Export summary (tabular) data
export_summary <- function(analysis_mode, project_name, summary, filename) {
    folder <- output_folder_selector(analysis_mode, project_name, 'data')

    write.csv(summary,
              glue("{folder}/{filename}.csv"),
              row.names = FALSE)
    cli_alert_success(glue(
        "Data successfully exported to the path ",
        style_path(glue("{folder}/{filename}.png"))
    ))
}


export_plot <- function(analysis_mode,
                        project_name,
                        plot,
                        figname,
                        transparent_bg = FALSE,
                        dpi = 96,
                        scale = 1,
                        plot_width = 15.5) {
    folder <- here(output_folder_selector(analysis_mode, project_name, 'img'))

    if (isSingleString(figname)) {
        if (transparent_bg == FALSE){
            ggsave(
                here(glue('{folder}/{figname}.png')),
                plot = plot,
                device = 'png',
                width = plot_width,
                height = plot_width * 0.618,
                unit = 'cm',
                scale = scale,
                dpi = dpi,
                bg = 'white')

            cli_alert_success(glue(
                "Plot successfully exported to the path ",
                style_path(glue("{folder}/{figname}.png"))
            ))
        } else if (transparent_bg == TRUE) {
            ggsave(
                here(glue('{folder}/{figname}.png')),
                plot = plot,
                device = 'png',
                width = plot_width,
                height = plot_width * 0.618,
                unit = 'cm',
                scale = scale,
                dpi = dpi)

            cli_alert_success(glue(
                "Plot successfully exported to the path ",
                style_path(glue("{folder}/{figname}.png"))
            ))
        } else {
            cli_abort(paste0("The argument 'transparent_bg' must be ",
                             "either TRUE or FALSE"))
        }
    } else {
        cli_abort("The argument 'figname' must be a single string")
    }
}


export_graph <- function(analysis_mode,
                         project_name,
                         plot,
                         figname,
                         transparent_bg = FALSE,
                         dpi = 96,
                         scale = 1,
                         plot_width = 15.5) {
    folder <- here(output_folder_selector(analysis_mode, project_name, 'img'))

    if (isSingleString(figname)) {
        if (transparent_bg == FALSE){
            ggsave(
                here(glue('{folder}/{figname}.png')),
                plot = plot,
                device = 'png',
                width = plot_width,
                height = plot_width * 0.618,
                unit = 'cm',
                scale = scale,
                dpi = dpi,
                bg = 'white')

            cli_alert_success(glue(
                "Plot successfully exported to the path ",
                style_path(glue("{folder}/{figname}.png"))
            ))
        } else if (transparent_bg == TRUE) {
            ggsave(
                here(glue('{folder}/{figname}.png')),
                plot = plot,
                device = 'png',
                width = plot_width,
                height = plot_width * 0.618,
                unit = 'cm',
                scale = scale,
                dpi = dpi)

            cli_alert_success(glue(
                "Plot successfully exported to the path ",
                style_path(glue("{folder}/{figname}.png"))
            ))
        } else {
            cli_abort(paste0("The argument 'transparent_bg' must be ",
                             "either TRUE or FALSE"))
        }
    } else {
        cli_abort("The argument 'figname' must be a single string")
    }
}
