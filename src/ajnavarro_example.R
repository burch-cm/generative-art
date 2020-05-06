library(tidyverse)

##### example #####
# from @djnavarro

# start generating data
set.seed(1)
obj <- tibble(
    x = rnorm(100),
    y = rnorm(100),
    g = sample(1:10, 100, replace = TRUE)
)

# plot
ggplot(obj, aes(x = x, y = y, group = g)) +
    geom_point(aes(color = g), show.legend = FALSE) +
    coord_equal() +
    theme_void()

# same but different
ggplot(obj, aes(x = x, y = y)) +
    geom_hex(aes(color = g), show.legend = FALSE) +
    coord_equal() +
    theme_void()

# paths
ggplot(obj, aes(x = x, y = y)) +
    geom_path(aes(color = g), show.legend = FALSE) +
    coord_equal() +
    theme_void()

# polygons
library(scico)
ggplot(obj, aes(x = x, y = y, group = g)) +
    geom_polygon(aes(fill = g), show.legend = FALSE) +
    coord_equal() +
    theme_void() +
    scale_fill_scico(palette = "lajolla")

# add spread
grp <- tibble(
    g = 1:10,
    x_shift = rnorm(10),
    y_shift = rnorm(10)
)

obj <- full_join(obj, grp)

# offset polygons
ggplot(
    data = obj,
    mapping = aes(
        x = x + x_shift,
        y = y + y_shift,
        fill = g,
        group = g
    )
) +
    geom_polygon(show.legend = FALSE) +
    coord_equal() +
    theme_void() +
    scale_fill_scico(palette = "lajolla")

# TSP smoothing
library(TSP)
tour <- function(obj) {
    obj$tour <- unname(c(solve_TSP(ETSP(obj[,c("x","y")]))))
    arrange(obj, order(tour))
}


build_art <- function(
    points = 100,   # total number of points
    groups = 10,    # number of groups
    polygon = tour, # function used to organise points
    gap = 1,        # standard deviation of the "shift" separating groups
    seed = 1        # numeric seed to use
) {
    
    # set the seed
    set.seed(seed)
    
    # create the initial data frame
    obj <- tibble(
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
    
    # merge obj with grp
    obj <- full_join(obj, grp, by = "g") 
    
    # split obj by group and apply the "polygon" mapping
    # function separately to each group
    obj <- obj %>%
        group_split(g) %>%
        map_dfr(~polygon(.x))
    
    return(obj) # output
}

draw_art <- function(obj, ...) {
    ggplot(
        data = obj, 
        mapping = aes(
            x = x + x_shift, 
            y = y + y_shift, 
            fill = g, 
            group = g
        )
    ) +
        geom_polygon(show.legend = FALSE) + 
        coord_equal() + 
        theme_void() + 
        scale_fill_scico(...)
}


build_art(seed = 1) %>% draw_art(palette = "lajolla")
build_art(seed = 1) %>% draw_art(palette = "vik")
build_art(seed = 2) %>% draw_art(palette = "vik")
build_art(points = 1000, groups = 5, gap = 2) %>% 
    draw_art(palette = "vik", alpha = 0.8)
build_art(
    points = 10000, 
    groups = 1000,
    gap = 15, 
    seed = 10
) %>% draw_art(palette = "tokyo")

build_art(
    points = 5000,
    groups = 200,
    gap = 0,
    polygon = function(obj) {
        obj %>% 
            mutate(x = x*g, y = y*g, g = -g) %>%
            tour()
    }
) %>% draw_art(palette = "oslo")
