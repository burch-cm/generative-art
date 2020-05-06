# in which I mess around with some stuff
library(tidyverse) # data manip
library(ambient) # noise generation
library(rlang) # functioning all the functions
library(TSP) # polygon creation using trav sp porblem
library(scico) # prettier plots

##### helper functions/tools #####

clean_plot <- function() {
    # strips any plot extranea
    # let's pretend that 'extranea' is a word
    return(
        list(
            coord_equal(),
            theme_void(),
            guides(fill = FALSE, color = FALSE)
        )
    )
}

tour <- function(dat, method = NULL) {
    tsp <- TSP::ETSP(dat[c("x", "y")])
    solution <- TSP::solve_TSP(tsp, method = method)
    arrange(dat, order(solution))
}

add_noise <- function(.dat, vars = c("x", "y"), method = gen_perlin, seed = 1) {
    noise_x <- method(.dat[[vars[1]]], .dat[[vars[2]]], seed = seed)
    noise_y <- method(.dat[[vars[2]]], .dat[[vars[1]]], seed = seed)
    mutate(.dat,
           noise_x,
           noise_y)
}

layergon <- function(dat) {
    dat %>% 
        mutate(x = x*g, y = y*g, g = -g) %>%
        tour()
}

##### code #####

##### build data object #####

make_dat <- function(
    points  = 100,
    groups  = 10,
    add_noise = FALSE,
    polygon = tour,
    gap     = 1,
    seed    = 1
) {
    # set the seed for reproducibility
    set.seed(seed)
    
    # create the initial data frame
    dat <- tibble(
        x = rnorm(points), 
        y = rnorm(points), 
        g = sample(groups, points, TRUE)
    )
    
    # create the offset for each group
    grp <- tibble(
        g = 1:groups,
        x_shift = rnorm(groups) * gap,
        y_shift = rnorm(groups) * gap
    )
    
    # join data and grp offset
    dat <- dplyr::full_join(dat, grp, by = "g")
    
    # add in ambient noise if selected
    if (add_noise) {
        dat <- dat %>%
            add_noise() %>%
            mutate(x = x + noise_x,
                   y = y + noise_y) %>%
            select(-noise_x,
                   -noise_y)
    }
    
    # add polygon shapes
    dat <- dat %>%
        group_split(g) %>%
        map_dfr(~polygon(.x))
    
    # return the data object
    return(dat)
}

make_art <- function(dat, ...) {
    require(scico)
    ggplot(
        data = dat, 
        mapping = aes(
            x = x + x_shift, 
            y = y + y_shift, 
            fill = g, 
            group = g
        )
    ) +
        geom_polygon(show.legend = FALSE) + 
        clean_plot() +
        scale_fill_scico(...)
}
