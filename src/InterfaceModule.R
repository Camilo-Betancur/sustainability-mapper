mode_selector <- function() {
    cli({
        cli_h2("Analysis mode")
        cli_text("Please, select the agenda that you want to map ",
                 "your projects to: \n\n")
        cli_text("")
        cli_text(glue(
            style_option("1) "), "Sustainable Development Goals (",
            emphasize("SDGs"), ")."))
        cli_text(glue(
            style_option("2) "),
            "European Taxonomy for Sustainable Activities (", emphasize("EUT"),
            ")."
        ))
        cli_text("")
        cli_text(glue(
            style_option("9) "),"See credits."
        ))
    })

    mode <- invisible(readline())

    if (mode == 1) {
        mode <- 'SDGs'
    } else if (mode == 2) {
        mode <- 'EUT'
    } else if (mode == 9) {
        mode <- as.integer(mode)

        credits()

        cli_text("")
        cli_text(glue(
            "To start a new analysis, press", style_option(" Y "), "or",
            style_option(" y"), ". ", "To quit, press any other key."
        ))

        quit_app <- invisible(readline())

        if ((quit_app == "Y") | (quit_app == "y")) {
            mode <- as.integer(8)
        }
    } else {mode_selector()}
    return(mode)
}

run_mapper <- function() {
    app_name <- list(
        name = emphasize_t("Sustainability mApper for Planning and InvestmENT"),
        accronym = glue(emphasize_t("SAPIENT")))

    cli_h1("{app_name$accronym} - {app_name$name}")
    cli_text("")
    cli_text(glue(
        "Welcome to ", emphasize_light("SAPIENT"), ", a tool developed by the ",
        emphasize_light('Stockholm Environment Institute'),
        " for mapping projects and policy documents to the ",
        emphasize_light('Sustainable Development Goals (SDGs)'), " and the ",
        emphasize_light('European Taxonomy for Sustainable Activities'), ". ",
        "This application will allow you to identify which targets and goals ",
        "from any of the agendas correspond to a set of PDF documents."
    ))
    cli_text("")
    cli_text("For instructions and further information, visit: ")
    print(glue(github_link()))
    cli_text("")
    cli_text("Please, press {style_option('ENTER')} to continue.")
    invisible(readline())

    cli_h2("Folder verification")
    cli_alert_info("\n\nVerifying folders")
    dir_checker()
    cli_alert_success("Folders checked\n\n")
    cli_text("")

    analysis_mode <- mode_selector()

    if (analysis_mode == 8) {
        run_mapper()
    } else if (analysis_mode == 9) {
        return(cli_abort("Sesion ended.", call = NULL))
    }

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
            "press {style_option('ENTER')} to continue:\n\n"))

        name <- as.character(invisible(readline()))

        if (dir.exists(here('PDF', name))) {
            break
        }

        cli_text(glue(
            "The folder {style_path(here('PDF', name))} does not exist.",
            " Please try again.\n\n"))
        }

        # Text extraction
        texts <- extract(name)

        # Text tidying
        tidy <- tidify(texts,
                       token = 'sentences',
                       low_lim = 0.55,
                       up_lim = 0.65)

        cli_alert_success(glue(
            "Press {style_option('Y')} or {style_option('y')} and ",
            "{style_option('ENTER')} to {emphasize('save the tidy texts')}. ",
            "Press any other key to discard them."
        ))

        export_tidy_json <- invisible(readline())

        # Saves the tidy results to a JSON file
        if ((export_tidy_json == 'Y') | (export_tidy_json == 'y')) {

            cli_text("")

            save_tidy_results(name, analysis_mode, tidy)
        }

    } else if (option == '2') {
        # Retrieving previously saved data
        repeat{
            cli_text("")
            cli_text(glue(
                "Write the name of the saved file and press ",
                "{style_option('ENTER')} ", "to continue:"))

            name <- as.character(invisible(readline()))

            if (file.exists(here("Saves", glue(name, ".json")))) {
                break
            } else {cli_alert_warning(glue(
                "You must write the name of a {emphasize(.json)} file inside ",
                "the ", col_red(here("Saves")), " folder."))
            }
        }

        tidy <- from_saves(name)

    } else if (option == '3') {
        train_model(analysis_mode = analysis_mode)

        run_mapper()
    }

    # Create the project folder in the /Output folder
    name <- create_project_folder(name)

    Sys.sleep(1)

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
        matches_proj <- count_matches(results,
                                      by = 'SDG',
                                      sorted = 'Frequency')

        matches_EUT <- results %>%
            count_matches(by = 'SDG', sorted = 'Frequency') %>%
            group_by(SDG) %>%
            summarise(Frequency = sum(Frequency))


        # Total matches by EUT Goal
        occurrence_EUT <- count_occurrence_EUT(results,
                                               collapse_projects = TRUE)

        occurrence_proj <- count_occurrence_EUT(results,
                                                collapse_projects = FALSE)

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
                           name,
                           results,
                           'raw_results')
            export_summary(analysis_mode,
                           name,
                           matches_T,
                           'matches_project_total')
            export_summary(analysis_mode,
                           name,
                           matches_SDG,
                           'matches_SDG_total')
            export_summary(analysis_mode,
                           name,
                           matches_tgt,
                           'matches_tgt_total')
            export_summary(analysis_mode,
                           name,
                           occurrence_SDG,
                           'occurrence_SDG_total')
            export_summary(analysis_mode,
                           name,
                           matches_SDG_proj,
                           'matches_SDG_proj')
            export_summary(analysis_mode,
                           name,
                           matches_tgt_proj,
                           'matches_tgt_proj')
            export_summary(analysis_mode,
                           name,
                           occurrence_SDG_proj,
                           'occurrence_SDG_proj')
            export_summary(analysis_mode,
                           name,
                           main_SDGs,
                           'main_SDGs_total')
            export_summary(analysis_mode,
                           name,
                           main_SDGs_proj,
                           'main_SDGs_proj')
            export_summary(analysis_mode,
                           name,
                           SDGs_proj,
                           'SDGs_by_project')
            export_summary(analysis_mode,
                           name,
                           matrix_absolute,
                           'results_matrix_abs')
            export_summary(analysis_mode,
                           name,
                           matrix_relative,
                           'results_matrix_rel')
        } else if (analysis_mode == 'EUT') {
            export_summary(analysis_mode,
                           name,
                           results,
                           'EUT_raw_results')
            export_summary(analysis_mode,
                           name,
                           matches_proj,
                           'EUT_matches_proj')
            export_summary(analysis_mode,
                           name,
                           matches_EUT,
                           'EUT_matches_total')
            export_summary(analysis_mode,
                           name,
                           occurrence_proj,
                           'EUT_occurrence_proj')
            export_summary(analysis_mode,
                           name,
                           occurrence_EUT,
                           'EUT_occurrence_total')
            export_summary(analysis_mode,
                           name,
                           matrix_absolute,
                           'EUT_results_matrix_abs')
            export_summary(analysis_mode,
                           name,
                           matrix_relative,
                           'EUT_results_matrix_rel')
        }
    }

    # ===== Prompting results to user ==========================================

    cli_text("")
    cli_h2("Results plotting")
    cli_text(glue(
        "Press ", col_green("ENTER"), " to start viewing the plots"
    ))

    invisible(readline())

    if (analysis_mode == 'SDGs') {
        prompt_export_plot_SDGs(analysis_mode,
                                name,
                                occurrence_SDG,
                                title = 'Occurrence of the SDGs',
                                subtitle = paste("Number of projects in",
                                                 " which each SDG appears"),
                                xlabel = 'SDG',
                                ylabel = 'Number of projects',
                                figname = 'occurrence')

        prompt_export_plot_SDGs(analysis_mode,
                                name,
                                matches_SDG,
                                title = "Matches of the SDGs",
                                subtitle = "Total sentences mapped to each SDG",
                                xlabel = "SDG",
                                ylabel = "Number of matches",
                                figname = 'matches')

        prompt_export_plot_SDGs(analysis_mode,
                                name,
                                main_SDGs,
                                title = 'Main SDG',
                                subtitle = paste("Most dominant SDGs across",
                                                 " the portfolio"),
                                xlabel = 'SDG',
                                ylabel = 'Number of projects',
                                figname = 'main_SDG')

        prompt_export_histogram(analysis_mode,
                                name,
                                results,
                                binwidth = 2,
                                title = "Distribution of the number of SDGs",
                                subtitle = "mapped by project",
                                figname = "SDGs_by_project")

        prompt_export_graph(analysis_mode,
                            name,
                            results,
                            figname = 'SDG_connections',
                            title = "Connections between the SDGs",
                            subtitle = "How often the SDGs coexist")
    } else if (analysis_mode == 'EUT') {
        prompt_export_plot_EUT(analysis_mode,
                               name,
                               occurrence_EUT,
                               title = paste0("Occurrence of the EU Taxonomy ",
                                              "Objectives"),
                               subtitle = paste("Number of projects in which",
                                                "each Objective appears"),
                               xlabel = 'EU Taxonomy Objective',
                               ylabel = 'Number of projects',
                               figname = 'occurrence')

        prompt_export_graph(analysis_mode,
                            name,
                            results,
                            figname = 'EUT_connections',
                            title = paste0("Connections between the EU ",
                                           "Taxonomy Objectives"),
                            subtitle = "How often the Objectives coexist")
    }

    cli_text("")
    cli_text("The analysis is done. Do yo wish to start a new one?\n")
    cli_text(glue(
        "Press ", style_option("Y"), " or ", style_option("y"), " to start a ",
        "new one. Press any other key to quit."
    ))

    start_new_analysis <- invisible(readline())

    if ((start_new_analysis == "Y") | (start_new_analysis == "y")) {
        app()
    } else {credits()}
}


credits <- function() {
    github_link <-

    cli_h1("Thank you!")
    cli_text("")
    cli_text(glue(
        "Thank you for using SAPIENT!\n",
        "For further information, please contact:"))
    cli_ul(c(
        glue("Ivonne Lobos Alva (", emphasize("ivonne.lobos@sei.org"), "),"),
        glue("Mario Cárdenas (",emphasize("mario.cardenas@sei.org"), "),"),
        glue("or Juan Camilo Betancur (",
             emphasize("juan.betancur@sei.org"),
             ").")
    ))
    cli_text("")
    cli_text("If you use this tool, please cite us as:")
    cli_text("")
    cli_text(glue(
        glue(
            "Lobos Alva, I.; Cárdenas Vélez, M.; ",
            "Betancur Jaramillo, J. C.; Hernández Orozco, E.; & ",
            "Maestre Másmela, D. (2022). ",
            style_italic("SAPIENT - "),
            style_italic("Sustainability mApper for Planning and "),
            style_italic("InvestmENT. ")
        ),

        github_link()
    ))
    cli_text("")
}


github_link <- function() {
    # El link debe ser reemplazado por un github de la línea ODS.
    return(
        custom_hyperlink(style_hyperlink(
            "https://github.com/SEI-LA-SDGs/SAPIENT.",
            "https://github.com/SEI-LA-SDGs/SAPIENT"
        ))
    )
}
