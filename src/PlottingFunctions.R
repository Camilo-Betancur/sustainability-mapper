# ===== Plotting functions =====================================================

plot_results_SDGs <- function(data,
                              title = NULL,
                              subtitle = NULL,
                              xlabel,
                              ylabel,
                              font = "Roboto Condensed",
                              fontsize_barlabs = 14,
                              fontsize_title = 20,
                              fontsize_subt = 16,
                              fontsize_axis = 15,
                              dpi = 96,
                              scale = 1) {
    data$SDG <- factor(data$SDG)

    data$SDG <- fct_reorder(data$SDG, data$Frequency, sort, .desc = TRUE)

    fig <- ggplot(data, aes(SDG, Frequency)) +
        geom_col(aes(fill = Color)) +
        geom_text(
            aes(SDG, Frequency, label = Frequency),
            angle = 90,
            vjust = 0.35,
            hjust = 1.3,
            colour = 'white',
            size = fontsize_barlabs
        ) +
        scale_fill_identity() +
        ggtitle(title, subtitle) +
        xlab(xlabel) +
        ylab(ylabel) +
        theme_minimal() +
        theme(
            aspect.ratio = 0.4,
            axis.title = element_text(family = font),
            axis.title.x = element_text(size = fontsize_axis,
                                        margin = ggplot2::margin(15, 0, 0, 0)),
            axis.title.y = element_text(size = fontsize_axis,
                                        margin = ggplot2::margin(0, 15, 0, 0)),
            axis.text.x = element_text(angle = 90,
                                       vjust = 0.5,
                                       hjust = 0),
            axis.text = element_text(size = fontsize_axis,
                                     family = font),
            legend.position = 'none',
            plot.title = element_text(size = fontsize_title,
                                      face = 'bold',
                                      hjust = 0,
                                      margin = ggplot2::margin(0, 0, 0, 0),
                                      family = font),
            plot.subtitle = element_text(size = fontsize_subt,
                                         hjust = 0,
                                         margin = ggplot2::margin(0, 0, 25, 0),
                                         family = font)
        )
    return(fig)
}


plot_network <- function(analysis_mode,
                         mapping_res,
                         concentric = FALSE,
                         savefig = FALSE,
                         figname = NULL,
                         title = "Interactions between the SDGs",
                         subtitle = "In the World Bank's portfolio",
                         font = "Roboto Condensed",
                         fontsize_base = 15,
                         fontsize_title = 20,
                         fontsize_subt = 16,
                         dpi = 96,
                         scale = 1,
                         weight_barheight = 6,
                         degree_keyheight = 0.75) {
    if (analysis_mode == 'SDGs') {
        net <- generate_network(mapping_res)
    } else if (analysis_mode == 'EUT') {
        net <- generate_network_EUT(mapping_res)
    } else {cli_alert_warning(
        "The parameter 'analysis mode' should be either 'SDGs' or 'EUT'.")
    }

    if (concentric == TRUE) {
        g <- ggraph(net, 'focus', focus = node_is_center()) +
            ggforce::geom_circle(aes(x0 = 0, y0 = 0, r = r),
                                 data.frame(r = 1:3),
                                 colour = 'grey')
    } else {
        # could also be ggraph(net, layout = 'igraph', algorithm = 'nicely')
        g <- ggraph(net, layout = 'igraph', algorithm = 'gem')
    }

    g <- g +
        # Edges' settings
        geom_edge_arc(aes(
            colour = weight,
            # alpha = weight,
            width = weight
        ),
        strength = 0.25) +
        # Nodes' settings
        geom_node_point(aes(size = degree,
                            fill = color),
                        shape = 21,
                        color = 'gray4'
        ) +
        geom_node_label(aes(label = names(as.list(V(net))),
                            size = degree),
                        show.legend = FALSE,
                        alpha = 0.85,
                        colour = 'black',
                        repel = TRUE,
                        family = font,
                        label.size = NA
        ) +
        # Scales
        scale_fill_identity() +
        scale_edge_width(range = c(0.1, 3), guide = 'none') +
        scale_edge_colour_viridis(
            breaks = as.integer(quantile(E(net)$weight,
                                         probs = seq(0.25, 1, by = 0.25))),
            alpha = 0.7,
            begin = 0.1,
            end = 0.8,
            direction = -1,
            discrete = FALSE,
            option = "C",
            name = 'Weight',
            position = 'left',
            guide = guide_edge_colourbar(
                barwidth = weight_barheight / (1.618 * 4),
                barheight = weight_barheight,
                label.position = "right",

            )) +
        scale_size_continuous(
            name = 'Degree',
            range = c(4, 8),
            breaks = as.integer(quantile(V(net)$degree,
                                         probs = seq(0.25, 1, by = 0.25))),
            limits = c(min(V(net)$degree),
                       max(V(net)$degree)),
            guide = guide_legend(
                keyheight = unit(degree_keyheight, "cm"),
                keywidth = unit(0.7, "cm"),
                label.position = "right",
                legend.position = "bottom",
                label.hjust = unit(-0.15, "cm"),
                title.hjust = 0.3,
                reverse = TRUE
            )
        ) +
        scale_label_size_continuous(range = c(3, 4),
                                    limits = c(min(V(net)$degree),
                                               max(V(net)$degree)),
                                    trans = 'log') +
        # General settings and aesthetics configurations
        theme_graph(
            background = 'white',
            title_margin = 0,
            subtitle_margin = 20,
            title_size = fontsize_title,
            subtitle_size = fontsize_subt,
            base_size = fontsize_base,
            base_family = font,
            plot_margin = ggplot2::margin(15, 15, 5, 15)
        ) +
        # Title
        ggtitle(title,
                subtitle
        ) +
        theme(
            legend.position = 'right',
            legend.direction = 'vertical',
            legend.box = 'vertical',
            legend.margin = ggplot2::margin(),
            legend.box.margin = ggplot2::margin(),
            legend.box.just = 'top',
            legend.key.width = unit(0.7, "cm"),
            legend.key.height = unit(0.3, "cm"),
            legend.spacing = unit(1, "cm"),
            legend.text = element_text(margin = ggplot2::margin(0, 0, 0, 0)),
            plot.subtitle = element_text(margin = ggplot2::margin(0, 0, 5, 0))
        ) +
        guides(colour = guide_legend(byrow = TRUE),
               width = guide_legend(byrow = TRUE),
        )
    return(g)
}


plot_SDG_distribution <- function(analysis_mode,
                                  project_name,
                                  mapping_res,
                                  binwidth = 2,
                                  title = NULL,
                                  subtitle = NULL,
                                  xlabel = "Number of Goals",
                                  ylabel = "Number of projects",
                                  font = "Roboto Condensed",
                                  fontsize_title = 20,
                                  fontsize_subt = 16,
                                  fontsize_axis = 15,
                                  kde = FALSE,
                                  savefig = FALSE,
                                  figname = NULL,
                                  dpi = 96,
                                  scale = 1,
                                  transparent_bg = FALSE) {
    folder <- output_folder_selector(analysis_mode, project_name, 'img')

    SDG_dist <- get_SDGs_proj(mapping_res)

    histo <- ggplot(SDG_dist, aes(Frequency)) +
        geom_histogram(binwidth = binwidth, boundary = 0)

    xticks <- as.list(round(ggplot_build(histo)$data[[1]][4], 4))[[1]]
    xticks <- c(xticks, tail(xticks, n = 1) + (xticks[2] - xticks[1]))

    max_val <- as.list(round(ggplot_build(histo)$data[[1]][1], 1))[[1]]
    max_val <- max(max_val)

    mean_goals <- mean(SDG_dist$Frequency)

    histo <- histo +
        scale_x_continuous(breaks = xticks,
                           labels = round(xticks, 1)
        ) +
        theme(legend.position = 'none') +
        ggtitle(title) +
        xlab(xlabel) +
        ylab(ylabel) +
        theme_minimal() +
        theme(
            aspect.ratio = 0.4,
            axis.title = element_text(size = fontsize_axis,
                                      family = font),
            axis.text.x = element_text(angle = 0,
                                       vjust = 0.5,
                                       hjust = 1,
                                       size = fontsize_axis),
            axis.text.y = element_text(size = fontsize_axis),
            axis.text = element_text(family = font),
            plot.title = element_text(size = fontsize_title,
                                      face = 'bold',
                                      hjust = 0,
                                      margin = ggplot2::margin(0, 0, 0, 0),
                                      family = font),
            plot.subtitle = element_text(size = fontsize_subt,
                                         hjust = 0,
                                         margin = ggplot2::margin(0, 0, 25, 0),
                                         family = font),
            legend.position = 'none'
        ) +
        geom_vline(xintercept = mean_goals,
                   linetype = 'dashed',
                   color = 'red',
                   lwd = 1) +
        geom_text(aes(x = mean_goals + 0.1,
                      y = max_val*1.03,
                      label = glue::glue("Mean = {round(mean_goals, 1)}"),
                      hjust = 0,
                      vjust = 0,
                      colour = 'red',
                      alpha = 1,
        ),
        size = fontsize_axis/.pt,
        family = font
        ) +
        ggtitle(title, subtitle)

    if (kde == TRUE) {
        histo <- histo +
            geom_density(aes(y = after_stat(count * 2),
                             colour = 'darkturquoise'),
                         kernel = "gaussian",
                         lwd = 1,
                         linetype = 1
            )
    }

    if (savefig == TRUE) {
        if (isSingleString(figname)) {
            if (transparent_bg == FALSE) {
                ggsave(
                    here(glue('{folder}/{figname}.png')),
                    plot = histo,
                    device = 'png',
                    scale = scale,
                    units = 'cm',
                    dpi = dpi,
                    bg = 'white'
                )

                cli_alert_success(glue(
                    "Plot successfully exported to the path ",
                    style_path(glue("{folder}/{figname}.png"))
                ))
            } else if (transparent_bg == TRUE) {
                ggsave(
                    here(glue('{folder}/{figname}.png')),
                    plot = histogram,
                    device = 'png',
                    scale = scale,
                    units = 'cm',
                    dpi = dpi
                )

                cli_alert_success(glue(
                    "Plot successfully exported to the path ",
                    style_underline(style_italic(col_br_red(glue(
                        "\'{folder}/{figname}.png\'"))))
                ))
            } else {
                cli_abort(paste0("The argument 'transparent_bg' must be ",
                                 "either TRUE or FALSE"))
            }
        } else {
            cli_abort("The argument 'figname' must be a single string")
        }
    }
    return(histo)
}


# ===== EUT-specific functions =================================================
plot_results_EUT <- function(data,
                             title = NULL,
                             subtitle = NULL,
                             xlabel,
                             ylabel,
                             font = "Roboto Condensed",
                             fontsize_barlabs = 7,
                             fontsize_title = 20,
                             fontsize_subt = 16,
                             fontsize_axis = 15,
                             dpi = 96,
                             scale = 1,
                             y_axis_lineheight = 1) {
    xlabels <- str_replace_all(unique(data$SDG), "[ ]", "\n")

    fig <- ggplot(data,
                  aes(fct_rev(fct_reorder(SDG, Frequency)), Frequency)) +
        geom_col() +
        geom_text(
            aes(SDG, Frequency, label = Frequency),
            angle = 90,
            vjust = 0.35,
            hjust = 1.3,
            colour = 'white',
            size = fontsize_barlabs
        ) +
        ggtitle(title, subtitle) +
        xlab(xlabel) +
        ylab(ylabel) +
        theme_minimal() +
        theme(
            aspect.ratio = 0.4,
            axis.title = element_text(family = font),
            axis.title.x = element_text(size = fontsize_axis,
                                        margin = ggplot2::margin(15, 0, 0, 0)),
            axis.title.y = element_text(size = fontsize_axis,
                                        margin = ggplot2::margin(0, 15, 0, 0)),
            axis.text.x = element_text(angle = 90,
                                       vjust = 0.5,
                                       hjust = 0,
                                       lineheight = y_axis_lineheight),
            axis.text = element_text(size = fontsize_axis,
                                     family = font),
            legend.position = 'none',
            plot.title = element_text(size = fontsize_title,
                                      face = 'bold',
                                      hjust = 0,
                                      margin = ggplot2::margin(0, 0, 0, 0),
                                      family = font),
            plot.subtitle = element_text(size = fontsize_subt,
                                         hjust = 0,
                                         margin = ggplot2::margin(0, 0, 25, 0),
                                         family = font)
        ) +
        scale_x_discrete(labels = xlabels)
    return(fig)
}


