app <- function() {
    source(here::here('src', 'Initialiser.R'))

    initialise_app()

    run_mapper()
}

app()
