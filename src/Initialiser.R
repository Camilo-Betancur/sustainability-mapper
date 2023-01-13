initialise_app <- function(font = 'Roboto Condensed', dpi = 96) {
    library(caTools)
    library(cli)
    library(DBI)
    library(ggplot2)
    library(ggraph)
    library(glue)
    library(here)
    library(igraph)
    library(jsonlite)
    library(pdftools)
    library(randomForest)
    library(readr)
    library(RSQLite)
    library(showtext)
    library(SnowballC)
    library(sysfonts)
    library(tidygraph)
    library(tidytext)
    library(tidyverse)
    library(tm)
    source(here('src', 'DataReader.R'))
    source(here('src', 'Mapper.R'))
    source(here('src', 'ViewResults.R'))
    source(here('src', 'AnalysisModule.R'))
    source(here('src', 'Model.R'))

    font_add(font,
             here('Settings',
                  'Fonts',
                  font,
                  glue('{str_remove_all(font, "[[:space:]]")}-Regular.ttf')),
             here('Settings',
                  'Fonts',
                  font,
                  glue('{str_remove_all(font, "[[:space:]]")}-Bold.ttf')),
             here('Settings',
                  'Fonts',
                  font,
                  glue('{str_remove_all(font, "[[:space:]]")}-Italic.ttf')),
    )

    showtext_opts(dpi)
    showtext_auto(enable = TRUE)

    dir_checker()
}


outputs_folder <- function(analysis_mode, kind) {
    analysis_mode <- as.character(analysis_mode)[[1]]
    folders <- jsonlite::fromJSON(here("Settings", "output_folders.json"))
    return(folders[[analysis_mode]][[kind]])
}


dir_checker <- function(silent = TRUE) {
    required <- scan(here("Settings", "dirs_required.txt"),
                     what = 'character',
                     sep = '\n')
    required <- here(required)

    for (dir in required) {
        if (!dir %in% list.dirs(here())) {
            dir.create(dir)
            cli_text("Created",
                     style_italic(col_br_red(" '{dir}' ")),
                     "successfully")
        } else {
            if (silent == FALSE) {
                print(glue("{dir} exists."))
            }
        }
    }
}
