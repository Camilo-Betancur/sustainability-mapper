custom_hyperlink <- function(string) {
    return(style_underline(style_italic(col_blue(string))))
}


style_path <- function(string) {
    return(custom_hyperlink(paste0("\"", string, "\"")))
}


emphasize <- function(string) {
    return(col_red(string))
}


emphasize_t <- function(string) {
    return(col_red(string))
}


if (isTRUE(rstudioapi::getThemeInfo()$dark)) {
    emphasize_light <- function(string) {
        return(col_magenta(string))
    }
} else {
    emphasize_light <- function(string) {
        return(col_br_red(string))
    }
}


style_option <- function(string) {
    return(col_green(string))
}


generate_color_palette <- function(all_SDGs = TRUE, SDG_vector = NULL) {
    palette <- as_tibble(read.csv(here('Settings/SDG_colors.csv')))
    if (all_SDGs == FALSE) {
        if (!is.null(SDG_vector)) {
            palette <- as_tibble(read.csv(here('Settings/SDG_colors.csv')))
            palette <- palette %>% column_to_rownames("SDG")
            palette <- palette[SDG_vector, ]
        } else {
            cli_abort(paste0("The argument 'SDG_vector' must be a vector of ",
                             "SDGs -- e.g., c('SDG 1', 'SDG 3', 'SDG n') -- ",
                             "if 'all_SDGs' is set to FALSE"))
        }
    }
    return(palette)
}
