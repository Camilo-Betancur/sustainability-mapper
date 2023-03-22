# This function asks the user if he/she/they want to export the tabular data
prompt_export_summary <- function() {
    cli_text("")
    cli_h2(glue(
        "Do you wish to ", emphasize("export the results data"), "?"
    ))
    cli_alert_info(paste0(
        "Save data: press {style_option('Y')} or {style_option('y')} and hit ",
        "Enter\n",
        "Discard data: press {style_option('N')} or {style_option('n')} and ",
        "hit Enter\n\n"
    ))
    answer <- invisible(readline())

    if ((answer == 'y') | (answer == 'Y')) {
        cli_alert_info("Results data will be exported")
    } else if ((answer == 'n') | (answer == 'N')) {
        cli_alert_warning("Results data will not be exported")
    } else {
        cli_alert_warning(glue(
            "You should introduce either ", style_option("Y"),  " / ",
            style_option("Y"), " or ", style_option("n"), " / ",
            style_option("N"), " but introduced ",  "{answer}", ". Try again."))

        prompt_export_summary()
    }
    return(answer)
}


# This function asks the user if he/she/they want to export a plot
prompt_export_plot_SDGs <- function(analysis_mode,
                                    project_name,
                                    data,
                                    title = NULL,
                                    subtitle = NULL,
                                    xlabel,
                                    ylabel,
                                    font = "Roboto Condensed",
                                    fontsize_barlabs = 14,
                                    fontsize_title = 62.5,
                                    fontsize_subt = 50,
                                    fontsize_axis = 30,
                                    figname = NULL,
                                    dpi = 300,
                                    scale = 1,
                                    transparent_bg = FALSE) {
    folder <- here(output_folder_selector(analysis_mode, project_name, 'img'))

    plot <- plot_results_SDGs(data,
                              title,
                              subtitle,
                              xlabel,
                              ylabel = ylabel,
                              font = font,
                              fontsize_barlabs = 7,
                              fontsize_title = 20,
                              fontsize_subt = 16,
                              fontsize_axis = 15,
                              dpi = 96,
                              scale = 1)

    print(plot)

    cli_h3("Do you wish to save this plot?")
    cli_alert_info(paste0(
        "Save plot: press {style_option('Y')} or {style_option('y')} and hit ",
        "Enter\n",
        "Discard plot: press {style_option('N')} or {style_option('n')} and ",
        "hit Enter\n\n"
    ))

    answer <- invisible(readline())

    if ((answer == 'y') || (answer == 'Y')) {
        plot <- plot_results_SDGs(data,
                                  title,
                                  subtitle,
                                  xlabel,
                                  ylabel,
                                  font,
                                  fontsize_barlabs,
                                  fontsize_title,
                                  fontsize_subt,
                                  fontsize_axis,
                                  dpi = dpi,
                                  scale = 1)

        export_plot(analysis_mode,
                    project_name,
                    plot,
                    figname = figname,
                    dpi = dpi,
                    scale = scale)
    } else if ((answer == 'n') || (answer == 'N')) {
        cli_alert_warning("Plot '{figname}.png' will not be saved")
    } else {
        cli_alert_warning(glue(
            "You should introduce either ", style_option("Y"),  " / ",
            style_option("Y"), " or ", style_option("n"), " / ",
            style_option("N"), " but introduced ",  "{answer}", ". Try again."))
        prompt_export_plot(data = data,
                           project_name = project_name,
                           title = title,
                           subtitle = subtitle,
                           xlabel = xlabel,
                           ylabel = ylabel,
                           font = font,
                           fontsize_barlabs = fontsize_barlabs,
                           fontsize_title = fontsize_title,
                           fontsize_subt = fontsize_subt,
                           fontsize_axis = fontsize_axis,
                           figname = figname,
                           dpi = dpi,
                           scale = scale)
    }
}


# This function takes one mapping summary from an analysis mode and returns a
# corresponding graph.
prompt_export_graph <- function(analysis_mode,
                                project_name,
                                mapping_res,
                                concentric = FALSE,
                                figname = NULL,
                                title = "Interactions between the SDGs",
                                subtitle = "In the World Bank's portfolio",
                                font = "Roboto Condensed",
                                fontsize_base = 30,
                                fontsize_title = 40,
                                fontsize_subt = 32,
                                dpi = 300,
                                scale = 1) {
    # Loads the folder names in which the graph will be exported
    folder <- output_folder_selector(analysis_mode, project_name, 'img')

    # Creates graph
    g <- plot_network(analysis_mode,
                      mapping_res,
                      concentric,
                      FALSE,
                      figname,
                      title,
                      subtitle,
                      font,
                      fontsize_base = 15,
                      fontsize_title = 20,
                      fontsize_subt = 16,
                      dpi = 96,
                      scale = 1)

    print(g)

    cli_h3("Do you wish to save this graph?")
    cli_alert_info(paste0(
        "The position of the nodes is different for each iteration of the ",
        "graph. This means the exported result will have different node ",
        "positions\n\n",
        "Save graph: press {style_option('Y')} or {style_option('y')} and hit ",
        "Enter\n",
        "Discard graph: press {style_option('N')} or {style_option('n')} and ",
        "hit Enter\n\n"
    ))

    answer <- invisible(readline())

    if ((answer == 'y') || (answer == 'Y')) {
        g <- plot_network(analysis_mode,
                          mapping_res,
                          concentric,
                          TRUE,
                          figname,
                          title,
                          subtitle,
                          font,
                          fontsize_base = 25,
                          fontsize_title = 40,
                          fontsize_subt = 30,
                          dpi = 300,
                          scale = 1,
                          weight_barheight = 4,
                          degree_keyheight = 0.25)

        export_graph(analysis_mode,
                     project_name,
                     g,
                     figname,
                     dpi = dpi)

        # cli_alert_success(glue(
        #     "Plot successfully exported to the path ",
        #     style_underline(style_italic(col_br_red(glue(
        #         "\'{folder}/{figname}.png\'\n\n"))))
        # ))
    } else if ((answer == 'n') || (answer == 'N')) {
        cli_alert_warning("Plot '{figname}.png' will not be saved")
    } else {
        cli_alert_warning(glue(
            "You should introduce either ", style_option("Y"),  " / ",
            style_option("Y"), " or ", style_option("n"), " / ",
            style_option("N"), " but introduced ",  "{answer}", ". Try again."))

        prompt_export_graph(analysis_mode,
                            project_name,
                            mapping_res,
                            concentric,
                            figname,
                            title,
                            subtitle,
                            font,
                            fontsize_base,
                            fontsize_title,
                            fontsize_subt,
                            dpi,
                            scale)
    }
}


prompt_export_histogram <- function(analysis_mode,
                                    project_name,
                                    data,
                                    binwidth = 2,
                                    title = NULL,
                                    subtitle = NULL,
                                    xlabel = "Number of Goals",
                                    ylabel = "Number of projects",
                                    font = "Roboto Condensed",
                                    fontsize_title = 62.5,
                                    fontsize_subt = 50,
                                    fontsize_axis = 30,
                                    kde = FALSE,
                                    figname = "SDG_distribution_proj",
                                    dpi = 300,
                                    scale = 1,
                                    transparent_bg = FALSE) {
    folder <- output_folder_selector(analysis_mode, project_name, 'img')

    histogram <- plot_SDG_distribution(analysis_mode,
                                       project_name,
                                       data,
                                       binwidth,
                                       title,
                                       subtitle,
                                       xlabel,
                                       ylabel,
                                       font,
                                       fontsize_title = 20,
                                       fontsize_subt = 16,
                                       fontsize_axis = 15,
                                       kde = kde,
                                       savefig = FALSE,
                                       figname = figname,
                                       dpi = 96,
                                       scale = 1,
                                       transparent_bg = FALSE)

    print(histogram)

    cli_h3("Do you wish to save this plot?")
    cli_alert_info(paste0(
        "Save plot: press {style_option('Y')} or {style_option('y')} and hit ",
        "Enter\n",
        "Discard plot: press {style_option('N')} or {style_option('n')} and ",
        "hit Enter\n\n"
    ))

    answer <- invisible(readline())

    if ((answer == 'y') || (answer == 'Y')) {
        histogram <- plot_SDG_distribution(analysis_mode,
                                           project_name,
                                           data,
                                           binwidth,
                                           title,
                                           subtitle,
                                           xlabel,
                                           ylabel,
                                           font,
                                           fontsize_title,
                                           fontsize_subt,
                                           fontsize_axis,
                                           kde,
                                           TRUE,
                                           figname,
                                           dpi,
                                           scale,
                                           transparent_bg)
    } else if ((answer == 'n') || (answer == 'N')) {
        cli_alert_warning("Plot '{figname}.png' will not be saved")
    } else {
        cli_alert_warning(glue(
            "You should introduce either ", style_option("Y"),  " / ",
            style_option("Y"), " or ", style_option("n"), " / ",
            style_option("N"), " but introduced ",  "{answer}", ". Try again."))
    }
}


# ===== EUT-speciphic ==========================================================
prompt_export_plot_EUT <- function(analysis_mode,
                                   project_name,
                                   data,
                                   title = NULL,
                                   subtitle = NULL,
                                   xlabel,
                                   ylabel,
                                   font = "Roboto Condensed",
                                   fontsize_barlabs = 14,
                                   fontsize_title = 62.5,
                                   fontsize_subt = 50,
                                   fontsize_axis = 30,
                                   figname = NULL,
                                   dpi = 300,
                                   scale = 1,
                                   transparent_bg = FALSE) {
    folder <- here(output_folder_selector(analysis_mode, project_name, 'img'))

    plot <- plot_results_EUT(data,
                             title,
                             subtitle,
                             xlabel = xlabel,
                             ylabel = ylabel,
                             font = font,
                             fontsize_barlabs = 7,
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
        plot <- plot_results_EUT(data,
                                 title,
                                 subtitle,
                                 xlabel,
                                 ylabel,
                                 font,
                                 fontsize_barlabs,
                                 fontsize_title,
                                 fontsize_subt,
                                 fontsize_axis,
                                 dpi = dpi,
                                 scale = 1,
                                 y_axis_lineheight = 0.35)

        export_plot(analysis_mode,
                    project_name,
                    plot,
                    figname = figname,
                    dpi = dpi,
                    scale = scale)

    } else if ((answer == 'n') || (answer == 'N')) {
        cli_alert_warning("Plot '{figname}.png' will not be saved")
    } else {
        cli_alert_warning(glue(
            "You should introduce either ", style_option("Y"),  " / ",
            style_option("Y"), " or ", style_option("n"), " / ",
            style_option("N"), " but introduced ",  "{answer}", ". Try again."))
        prompt_export_plot(data = data,
                           project_name = project_name,
                           title = title,
                           subtitle = subtitle,
                           xlabel = xlabel,
                           ylabel = ylabel,
                           font = font,
                           fontsize_barlabs = fontsize_barlabs,
                           fontsize_title = fontsize_title,
                           fontsize_subt = fontsize_subt,
                           fontsize_axis = fontsize_axis,
                           figname = figname,
                           dpi = dpi,
                           scale = scale)
    }
}
