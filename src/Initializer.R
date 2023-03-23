initialize_app <- function(font = 'Roboto Condensed', dpi = 96) {
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
    source(here::here("src", "DataAnalysis.R"))
    source(here::here("src", "DataManagement.R"))
    source(here::here("src", "DataProcessing.R"))
    source(here::here("src", "DataReader.R"))
    source(here::here("src", "Initializer.R"))
    source(here::here("src", "InterfaceModule.R"))
    source(here::here("src", "InterfaceStyle.R"))
    source(here::here("src", "Mapper.R"))
    source(here::here("src", "Model.R"))
    source(here::here("src", "PlottingFunctions.R"))
    source(here::here("src", "Prompts.R"))


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
