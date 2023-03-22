generate_testData <- function(tidy_texts) {
    SDGs <- as_tibble(read_csv(here('Settings/SDGs.csv'), col_types = 'cc'))
    test_results <- tidy_texts

    test_results <- test_results %>%
        mutate(
            Target = sample(
                SDGs$Target,
                size = nrow(test_results),
                replace = TRUE,
                prob = SDGs$Probability)
        )
    return(test_results)
}


identify_SDGs <- function(mapping_res,
                          drop_text = TRUE,
                          save = FALSE,
                          version_name = NULL) {
    mapping_res <- mapping_res %>%
        separate(Target, c('SDG', 'Target'), sep = '[.]') %>%
        mutate(Target = paste0(SDG, '.', Target))

    if (save == TRUE) {
        if (isSingleString(version_name)) {
            write.csv(mapping_res,
                      here(glue('Saves/clean-results_{version_name}.csv')),
                      row.names = FALSE)
        } else {
            warning('version_name should be a string')
        }
    }
    return(mapping_res)
}


color_by_SDG <- function(mapping_res) {
    SDG_colors <- as_tibble(read_csv(here('Settings/SDG_colors.csv'),
                                     col_types = 'cc'))

    mapping_res <- mapping_res %>%
        inner_join(SDG_colors, by = 'SDG')
}


count_matches <- function(mapping_res,
                          by,
                          sorted=NULL,
                          collapse_projects = FALSE) {
    if (isSingleString(by)) {
        if (any(by == 'total_matches')) {
            matches <- mapping_res %>%
                group_by(Project) %>%
                summarise(Frequency = n(), .groups = 'drop')
        } else {
            matches <- mapping_res %>%
                group_by(Project, mapping_res[, by]) %>%
                summarise(Frequency = n(), .groups = 'drop')

            if (by == 'Target') {
                matches <- identify_SDGs(matches)
            }
        }
    }

    matches <- as_tibble(matches)

    if (!is.null(sorted)) {
        sorting_ref <- as.list(matches[, sorted])[[1]]

        if (by != 'total_matches') {
            if (is.numeric(sorting_ref)) {
                matches <- matches %>%
                    arrange(Project, -sorting_ref)
            } else if (is.character(sorting_ref)) {
                sorting_ref <- str_sort(sorting_ref, numeric = TRUE)

                matches <- matches %>%
                    arrange(Project, sorting_ref)
            } else {
                warning(paste0("The argument 'sorted' must be either reference",
                               " a column of integers or strings"))
            }
        } else {
            matches <- matches %>%
                arrange(-sorting_ref)
        }
    }

    if (collapse_projects == TRUE &
        by != 'total_matches') {
        if (by == 'Target') {
            matches <- matches %>%
                group_by(SDG, Target) %>%
                summarise(Frequency = sum(Frequency), .groups = 'drop') %>%
                arrange(-Frequency) %>%
                color_by_SDG()
        } else {
            matches <- matches %>%
                group_by(SDG) %>%
                summarise(Frequency = sum(Frequency), .groups = 'drop') %>%
                arrange(-Frequency) %>%
                color_by_SDG()
        }

    } else if (collapse_projects == TRUE & by == 'total_matches') {
        cli_abort(paste0("It is not possible to collapse the results by ",
                         "Project when argument 'by' == 'total_matches', ",
                         "only when 'by' == 'SDG' or 'Target'"))
    }

    matches <- as_tibble(matches)

    return(matches)
}


count_occurrence <- function(mapping_res,
                             by = 'Target',
                             collapse_projects = FALSE) {
    if (by %in% c('SDG', 'Target')) {
        occurrence <- mapping_res %>%
            count_matches(by = 'Target',
                          sorted = 'Frequency') %>%
            mutate(Frequency = 1) %>%
            color_by_SDG()
    } else {cli_abort("Argument 'by' must be either 'SDG' or 'Target'")}

    if (by == 'SDG') {
        occurrence <- occurrence %>%
            group_by(Project, SDG) %>%
            summarise(Frequency = sum(Frequency), .groups = 'drop') %>%
            mutate(Frequency = 1) %>%
            arrange(Project, str_sort(SDG, numeric = TRUE)) %>%
            color_by_SDG()
    }

    if (collapse_projects == 'TRUE') {
        if (by == 'SDG') {
            occurrence <- occurrence %>%
                group_by(SDG) %>%
                summarise(Frequency = sum(Frequency), .groups = 'drop') %>%
                arrange(-Frequency) %>%
                color_by_SDG()
        }
        if (by == 'Target') {
            occurrence <- occurrence %>%
                group_by(SDG, Target) %>%
                summarise(Frequency = sum(Frequency), .groups = 'drop') %>%
                arrange(-Frequency) %>%
                color_by_SDG()
        }
    }
    return(as_tibble(occurrence))
}

get_main_SDG <- function(mapping_res,
                         from_binary = TRUE,
                         collapse_SDG = FALSE) {
    if (from_binary == TRUE) {
        mapping_res <- mapping_res %>%
            count_occurrence(by = 'SDG')

    } else if (from_binary == FALSE) {
        mapping_res <- mapping_res %>%
            count_matches(by = 'SDG', sorted = 'Frequency') %>%
            color_by_SDG()
    } else {cli_abort("Argument 'from_binary' must be either TRUE or FALSE")}

    main <- mapping_res %>%
        group_by(Project) %>%
        slice_max(Frequency, n = 1) %>%
        mutate(Frequency = 1) %>%
        arrange(str_sort(SDG, numeric = TRUE)) %>%
        group_by(Project) %>%
        slice(1)

    if (collapse_SDG == TRUE) {
        main <- main %>%
            group_by(SDG) %>%
            summarise(Frequency = sum(Frequency), .groups = 'drop') %>%
            color_by_SDG()
    }

    return(main)
}


get_SDGs_proj <- function(mapping_res) {
    SDGs_project <- count_occurrence(mapping_res,
                                     by = 'SDG',
                                     collapse_projects = FALSE)

    SDGs_project <- SDGs_project %>%
        mutate(Frequency = 1) %>%
        group_by(Project) %>%
        summarise(Frequency = sum(Frequency), .groups = 'drop')
    return(SDGs_project)
}


results_matrix <- function(mapping_res,
                           by = 'SDG',
                           relative_freqs = FALSE,
                           with_main_SDG = TRUE) {
    unit <- '# matches'

    matrix_results <- mapping_res %>%
        count_matches(by = by,
                      sorted = 'Frequency',
                      collapse_projects = FALSE) %>%
        dplyr::mutate(Color = NULL)

    matrix_results <- pivot_wider(matrix_results,
                                  names_from = by,
                                  values_from = 'Frequency')

    projects <- as.list(matrix_results %>% select(Project))[[1]]

    matrix_results <- matrix_results %>%
        dplyr::select(str_sort(colnames(matrix_results), numeric = TRUE)) %>%
        replace(is.na(.), 0) %>%
        column_to_rownames('Project')

    if (with_main_SDG == TRUE) {
        main_goals <- get_main_SDG(mapping_res,
                                   from_binary = FALSE,
                                   collapse_SDG = FALSE)$SDG
    }

    if (relative_freqs == TRUE) {
        unit <- '%'

        for (i in 1:nrow(matrix_results)) {
            matrix_results[i, ] <-
                round(100 * matrix_results[i, ] / sum(matrix_results[i, ]), 1)
        }
    }

    matrix_results <- matrix_results %>%
        rownames_to_column('Project') %>%
        add_column(Unit = rep(unit, nrow(matrix_results)), .after = 1)

    if (with_main_SDG == TRUE) {
        matrix_results <- matrix_results %>%
            add_column(Main_SDG = main_goals)
    }
    return(matrix_results)
}


generate_network <- function(mapping_res) {
    # Count SDGs

    res <- mapping_res %>%
        count_occurrence('SDG') %>%
        mutate(Color = NULL)

    # Get unique projects with more than one SDG mapped
    projects_for_net <- res %>% count(Project) %>% filter(n > 1)
    projects_for_net <- as.character(projects_for_net$Project)

    # Iterate through documents
    tibblist <- list()
    for (project in unique(projects_for_net)) {
        a <- res %>% filter(Project == project)
        a <- as.list(a$SDG)

        a <- combn(a, 2, simplify = FALSE)

        sources <- list()
        targets <- list()
        for (i in 1:length(a)) {
            sources <- c(sources, a[[i]][1])
            targets <- c(targets, a[[i]][2])
        }

        net <- tibble(source = sources,
                      target = targets)

        tibblist <- c(tibblist, list(net))
    }

    # Concatenates the list of tibble into a single tibble
    tibblist <- as_tibble(data.table::rbindlist(tibblist))

    net <- tibblist %>%
        mutate(source = as.numeric(str_remove_all(source, "[^0-9]")),
               target = as.numeric(str_remove_all(target, "[^0-9]")))

    min <- list()
    max <- list()
    for (i in 1:nrow(net)) {
        if (net[[i, 1]] > net[[i, 2]]) {
            max <- c(max, net[[i, 1]])
            min <- c(min, net[[i, 2]])
        } else if (net[[i, 1]] < net[[i, 2]]) {
            max <- c(max, net[[i, 2]])
            min <- c(min, net[[i, 1]])
        } else {cli_abort("Something is wrong")}
    }

    net <- tibble(source = min, target = max)

    net <- net %>%
        mutate(source = as.numeric(source),
               target = as.numeric(target)) %>%
        arrange(source, target) %>%
        group_by(source, target) %>%
        summarise(weight = n(), .groups = 'drop') %>%
        mutate(source = glue("SDG {source}"),
               target = glue("SDG {target}"))

    nodes <- c(as.list(net$source), as.list(net$target))
    nodes <- str_sort(unique(nodes), numeric = TRUE)

    net <- graph_from_data_frame(net, directed = FALSE, vertices = nodes)

    V(net)$degree <- strength(net, mode='total')
    V(net)$color <- generate_color_palette(all_SDGs = FALSE, SDG_vector = V(net))
    E(net)$color <- "gray"

    return(net)
}


# ===== EUT-specific functions ================================================
generate_network_EUT <- function(mapping_res) {
    res <- mapping_res %>%
        count_matches(by = 'SDG',
                      sorted = 'Frequency') %>%
        mutate(Frequency = 1)

    # Get unique projects with more than one SDG mapped
    projects_for_net <- res %>% count(Project) %>% filter(n > 1)
    projects_for_net <- as.character(projects_for_net$Project)

    # Iterate through documents
    tibblist <- list()
    for (project in unique(projects_for_net)) {
        a <- res %>% filter(Project == project)
        a <- as.list(a$SDG)

        a <- combn(a, 2, simplify = FALSE)

        sources <- list()
        targets <- list()
        for (i in 1:length(a)) {
            sources <- c(sources, a[[i]][1])
            targets <- c(targets, a[[i]][2])
        }

        net <- tibble(source = sources,
                      target = targets)

        tibblist <- c(tibblist, list(net))
    }

    # Concatenates the list of tibble into a single tibble
    tibblist <- as_tibble(data.table::rbindlist(tibblist))

    net <- tibblist

    min <- list()
    max <- list()
    for (i in 1:nrow(net)) {
        if (net[[i, 1]][[1]] > net[[i, 2]][[1]]) {
            max <- c(max, net[[i, 1]][[1]])
            min <- c(min, net[[i, 2]][[1]])
        } else if (net[[i, 1]][[1]] < net[[i, 2]][[1]]) {
            max <- c(max, net[[i, 2]][[1]])
            min <- c(min, net[[i, 1]][[1]])
        } else {cli_abort("Something is wrong")}
    }

    net <- tibble(source = min, target = max)

    net <- net %>%
        arrange(source, target) %>%
        group_by(source, target) %>%
        summarise(weight = n(), .groups = 'drop')

    nodes <- c(as.list(net$source), as.list(net$target))
    nodes <- str_sort(unique(nodes), numeric = TRUE)

    net <- graph_from_data_frame(net, directed = FALSE, vertices = nodes)

    V(net)$degree <- strength(net, mode='total')
    V(net)$color <- "gray"
    E(net)$color <- "gray"

    return(net)
}


count_occurrence_EUT <- function(mapping_res,
                                 collapse_projects = FALSE) {
    occurrence_EUT <- count_matches(mapping_res,
                                    by = 'SDG',
                                    sorted = 'Frequency')

    if (collapse_projects == TRUE) {
        occurrence_EUT <- occurrence_EUT %>%
            group_by(SDG) %>%
            summarise(Frequency = n()) %>%
            arrange(desc(Frequency))
    } else {
        occurrence_EUT <- occurrence_EUT %>%
            group_by(Project, SDG) %>%
            summarise(Frequency = n()) %>%
            arrange(desc(Frequency)) %>%
            pivot_wider(names_from = SDG,
                        values_from = Frequency,
                        values_fill = 0)
    }

    return(occurrence_EUT)
}


# Found in StackOverflow.
# Answer by https://stackoverflow.com/users/303052/jamesatha
# Thread: https://stackoverflow.com/questions/38385521/r-determine-if-a-variable
# -is-a-string
isSingleString <- function(input) {
    is.character(input) & length(input) == 1
}

