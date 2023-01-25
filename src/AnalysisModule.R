mode_selector <- function() {
    cli({
        cli_h2("Analysis mode")
        cli_text("Please, select the agenda that you want to map ",
                 "your projects to: \n\n")
        cli_text("")

        cli_text(glue(col_green("1) "), "Sustainable Development Goals."))
        cli_text(glue(
            col_green("2) "),
            "European Taxonomy for Sustainable Activities."
        ))
    })

    mode <- invisible(readline())

    if (mode == 1) {
        mode <- 'SDGs'
    } else if (mode == 2) {
        mode <- 'EUT'
    } else {mode_selector()}
    return(mode)
}


run_mapper <- function() {
    cli_h1("Sustainability Mapper Tool")
    cli_text("")

    cli_text(glue(
        "Welcome to the {col_red('Sustainability Mapper Tool')}, a tool ",
        "developed by the {col_red('Stockholm Environment Institute')} for ",
        "mapping the World Bank's portfolio to the {col_red('Sustainable')} ",
        "{col_red('Development Goals (SDGs)')} and the {col_red('European')} ",
        "{col_red('Taxonomy for Sustainable Activities')}. This application ",
        "will allow you to identify which targets and goals from any of the ",
        "agendas correspond to a set of PDF documents."
    ))

    cli_text("")
    cli_text("Please, press {col_green('ENTER')} to continue.")
    invisible(readline())

    cli_h2("Folder verification")
    cli_alert_info("\n\nVerifying folders")
    dir_checker()
    cli_alert_success("Folders checked\n\n")
    cli_text("")

    analysis_mode <- mode_selector()

    cli({
        cli_h2("Data source")
        cli_text("Please, select the source of the projects you want to ",
                 "analyze:")
        cli_text("")

        cli_text(glue(col_green("1) "), "Start analysis from scratch."))
        cli_text(glue(col_green("2) "), "Read saved data."))
        cli_text(glue(col_green("3) "), "Train the classification model."))
    })

    option <- as.character(invisible(readline()))

    # ===== Text pre-processing ================================================
    if (option == '1') {
        repeat {
        cli_text("")
        cli_text(glue(
            "Write the name of the folder that contains the PDF files and ",
            "press {col_green('ENTER')} to continue:\n\n"))

        name <- as.character(invisible(readline()))

        if (dir.exists(here('PDF', name))) {break}
        cli_text(glue(
            "The folder {col_red(here('PDF', name))} does not exist.",
            " Please try again.\n\n"))
        }

        # Text extraction
        texts <- extract(name)

        # Text tidying
        tidy <- tidify(
            texts,
            token = 'sentences',
            low_lim = 0.55,
            up_lim = 0.65)

    } else if (option == '2') {
        # Retrieving previously saved data
        repeat{
            cli_text("")
            cli_text(glue(
                "Write the name of the saved file and press {col_green('ENTER')} ",
                "to continue:"))

            name <- as.character(invisible(readline()))

            if (file.exists(here("Saves", glue(name, ".json")))) {
                break
            } else {cli_alert_warning(glue(
                "You must write the name of a {col_red(.json)} file inside the ",
                col_red(here("Saves")),
                " folder."))
            }
        }

        tidy <- from_saves(name)

    } else if (option == '3') {
        train_model(analysis_mode = analysis_mode)

        run_mapper()
    }

    # ===== Classification model ===============================================
    if (file.exists(here("Settings",
                         "Model",
                         glue("model_{analysis_mode}.Rds")))) {
        # ----- Importing the classification model -----------------------------
        classifier <- load_model(analysis_mode)
    } else {
        # ----- Train the model and save it for future use ---------------------
        train_model(analysis_mode = analysis_mode)

        run_mapper()
    }

    # ----- Classifying the texts ----------------------------------------------
    results <- map_texts(classifier = classifier,
                         analysis_mode = analysis_mode,
                         tidy_texts = tidy)

    # ----- Cleaning results for generating the reports ------------------------

    results <- identify_SDGs(results)
    results <- results %>% dplyr::filter(SDG != "No map")
    results <- as_tibble(results)


    # ====== Summaries =========================================================
    if (analysis_mode == 'SDGs') {
        # ----- Summaries for the SDGs analysis mode ---------------------------

        # Total matches by project
        matches_T <- count_matches(results,
                                   by = 'total_matches',
                                   sorted = 'Frequency',)

        # Total matches by SDG
        matches_SDG <- count_matches(results,
                                     by = 'SDG',
                                     sorted = 'Frequency',
                                     collapse_projects = TRUE)

        # Total matches by Target
        matches_tgt <- count_matches(results,
                                     by = 'Target',
                                     sorted = 'Frequency',
                                     collapse_projects = TRUE)

        # Binary occurrence results by SDG
        occurrence_SDG <- count_occurrence(results,
                                           by = 'SDG',
                                           collapse_projects = TRUE)

        # Total matches by Project and SDG
        matches_SDG_proj <- count_matches(results,
                                          by = 'SDG',
                                          sorted = 'Frequency',
                                          collapse_projects = FALSE)

        # Total matches by Project and Target
        matches_tgt_proj <- count_matches(results,
                                          by = 'Target',
                                          sorted = 'Frequency',
                                          collapse_projects = FALSE)

        # Binary occurrence results by Project and SDG
        occurrence_SDG_proj <- count_occurrence(results,
                                                by = 'SDG',
                                                collapse_projects = FALSE)

        # Aggregate of the main SDGs -- Number of times an SDG was identified as
        # the main SDG across the projects
        main_SDGs <- get_main_SDG(results,
                                  from_binary = FALSE,
                                  collapse_SDG = TRUE)

        # Aggregate of the main SDGs -- Number of times an SDG was identified as
        # the main SDG in each project
        main_SDGs_proj <- get_main_SDG(results,
                                       from_binary = FALSE,
                                       collapse_SDG = TRUE)

        # Number of SDGs identified in each project
        SDGs_proj <- get_SDGs_proj(results)

        # ----- Results as a matrix --------------------------------------------

        matrix_relative <- results_matrix(results,
                                          relative_freqs = TRUE,
                                          with_main_SDG = TRUE)

        matrix_absolute <- results_matrix(results,
                                          relative_freqs = FALSE,
                                          with_main_SDG = TRUE)
    } else if (analysis_mode == 'EUT') {
        # ----- Summaries for European Taxonomy mode ---------------------------

        # Total matches by project
        matches_T <- count_matches(results,
                                   by = 'SDG',
                                   sorted = 'Frequency')

        # Total matches by EUT Goal
        occurrence_EUT <- count_occurrence_EUT(results)

        # Results as matrix ----------------------------------------------------

        matrix_relative <- results_matrix(results,
                                          relative_freqs = TRUE,
                                          with_main_SDG = FALSE)

        matrix_absolute <- results_matrix(results,
                                          relative_freqs = TRUE,
                                          with_main_SDG = FALSE)

    }
    # ===== Export results =====================================================

    export_data_files <- prompt_export_summary()

    if ((export_data_files == 'y') || (export_data_files == 'Y')) {

        if (analysis_mode == 'SDGs' ) {
            export_summary(analysis_mode,
                           matches_T,
                           'matches_project_total')
            export_summary(analysis_mode,
                           matches_SDG,
                           'matches_SDG_total')
            export_summary(analysis_mode,
                           matches_tgt,
                           'matches_tgt_total')
            export_summary(analysis_mode,
                           occurrence_SDG,
                           'occurrence_SDG_total')
            export_summary(analysis_mode,
                           matches_SDG_proj,
                           'matches_SDG_proj')
            export_summary(analysis_mode,
                           matches_tgt_proj,
                           'matches_tgt_proj')
            export_summary(analysis_mode,
                           occurrence_SDG_proj,
                           'occurrence_SDG_proj')
            export_summary(analysis_mode,
                           main_SDGs,
                           'main_SDGs_total')
            export_summary(analysis_mode,
                           main_SDGs_proj,
                           'main_SDGs_proj')
            export_summary(analysis_mode,
                           SDGs_proj,
                           'SDGs_by_project')
            export_summary(analysis_mode,
                           matrix_absolute,
                           'results_matrix_abs')
            export_summary(analysis_mode,
                           matrix_relative,
                           'results_matrix_rel')
        } else if (analysis_mode == 'EUT') {
            export_summary(analysis_mode,
                           matches_T,
                           'matches_T')
            export_summary(analysis_mode,
                           occurrence_EUT,
                           'occurrence_EUT')
            export_summary(analysis_mode,
                           matrix_absolute,
                           'results_matrix_abs')
            export_summary(analysis_mode,
                           matrix_relative,
                           'results_matrix_rel')
        }
    }

    # ===== Prompting results to user ==========================================

    cli_h2("Results plotting")

    cli_text(glue(
        "Press ", col_green("ENTER"), " to start viewing the plots"
    ))

    invisible(readline())

    if (analysis_mode == 'SDGs') {
        prompt_export_plot_SDGs(analysis_mode,
                                occurrence_SDG,
                                title = 'Occurrence of the SDGs',
                                subtitle = paste("Number of projects in",
                                                 " which each SDG appears"),
                                xlabel = 'SDG',
                                ylabel = 'Number of projects',
                                figname = 'occurrence')

        prompt_export_plot_SDGs(analysis_mode,
                                matches_SDG,
                                title = "Matches of the SDGs",
                                subtitle = "Total sentences mapped to each SDG",
                                xlabel = "SDG",
                                ylabel = "Number of matches",
                                figname = 'matches')

        prompt_export_plot_SDGs(analysis_mode,
                                main_SDGs,
                                title = 'Main SDG',
                                subtitle = paste("Most dominant SDGs across",
                                                 " the portfolio"),
                                xlabel = 'SDG',
                                ylabel = 'Number of projects',
                                figname = 'main_SDG')

        prompt_export_histogram(analysis_mode,
                                results,
                                binwidth = 2,
                                title = "Distribution of the number of SDGs",
                                subtitle = "mapped by project",
                                figname = "SDGs_by_project")

        write.csv(results, "results_debug_network.csv")

        prompt_export_graph(analysis_mode,
                            results,
                            figname = 'SDG_connections',
                            title = "Connections between the SDGs",
                            subtitle = "How often the SDGs coexist")
    } else if (analysis_mode == 'EUT') {
        prompt_export_plot_EUT(analysis_mode,
                               occurrence_EUT,
                               title = "Occurrence of the EU Taxonomy Goals",
                               subtitle = paste("Number of projects in which",
                                                " each Goal appears"),
                               xlabel = 'EU Taxonomy Goal',
                               ylabel = 'Number of projects',
                               figname = 'occurrence')

        prompt_export_graph(analysis_mode,
                            results,
                            figname = 'EUT_connections',
                            title = "Connections between the EU Taxonomy Goals",
                            subtitle = "How often the Goals coexist")
    }
}
