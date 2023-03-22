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

    for (source_file in list.files(here("src"))) {
        source(here("src", source_file))
    }


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
