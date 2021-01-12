library(here)
library(tidyverse)
library(flametree)
library(ggforce)
library(jasmines)

seed <- 201

genuary_tree <- function(seed) {

  set.seed(seed)
  version <- "99"
  fname <- paste0("softloud_tree_", version, "_", seed, "_A4.jpg")

  cat("making tree...\n")

  tree <- flametree_grow(
    seed = seed,
    time = 13,
    scale = c(0.6, 0.9, 0.9)
  ) %>%
    mutate(
      coord_x = coord_x * 3,
      coord_y = coord_y * 2
    )

  leaf <- tree %>%
    filter(id_time >= 12) %>%
    sample_frac(1)

  cat("making background...\n")

  set.seed(seed)
  scf <- 2
  shft <- .3
  background <- scene_rows(n = 500, grain = 500, vertical = TRUE) %>%
    mutate(ind = 1:n()) %>%
    mutate(seed = use_seed(seed)) %>%
    mutate(x = (x + shft) / scf, y = (y + shft) / scf) %>% 
    unfold_warp(iterations = 160, scale = .005) %>%
    mutate(x = x * scf - shft, y = y * scf - shft) %>% 
    mutate(x = x * 18 - 10, y = y * 18) %>%
    prepare_data() %>%
    sample_frac(.05)

  cat("making leaves...\n")

  set.seed(seed)
  # grass <-  leaf %>%
  #   mutate(
  #     x = coord_x + rnorm(n()) * 2,
  #     y = coord_y + rnorm(n()) * 2
  #   ) %>%
  #   mutate(ind = 1:n()) %>%
  #   mutate(seed = use_seed(seed)) %>%
  #   unfold_warp(iterations = 80, scale = .01) %>%
  #   prepare_data(colour = "time") %>%
  #   sample_frac(.1)

  cat("making image...\n")

  tree_col <- "ghostwhite"
  
  pic <- tree %>%
    ggplot() +
    geom_segment(
      data = background,
      mapping = aes(
        x = x,
        y = y,
        xend = xend,
        yend = yend
      ),
      show.legend = FALSE,
      size = .1,
      alpha = 1,
      colour = "#262938"
    ) +
    # geom_segment(
    #   data = grass,
    #   mapping = aes(
    #     x = x,
    #     y = y,
    #     xend = xend,
    #     yend = yend,
    #     #colour = time
    #   ),
    #   colour = "black",
    #   show.legend = FALSE,
    #   size = .5,
    #   alpha = 1
    # ) +
    geom_bezier(
      mapping = aes(
        x = coord_x,
        y = coord_y,
        size = 0 + seg_wid * 15,
        group = id_path
      ),
      colour = tree_col,
      show.legend = FALSE,
      lineend = "round",
      alpha = 1
    ) +
    geom_point(
      mapping = aes(
        x = coord_x,
        y = coord_y,
        size = pt_sz
      ),
      data = leaf %>% mutate(pt_sz = 1),
      stroke = 0,
      colour = tree_col
    ) +
    jasmines:::theme_mono("#B2B4B3") +
    scale_size_identity() +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0)) +
    #scale_color_gradientn(colours = shades) +
    #scico::scale_color_scico(palette = "grayC") +
    coord_equal(
      xlim = c(-10.5, 7.5) * 4800/6788,
      ylim = c(0, 18)
    )

  x_px <- 4800
  y_px <- 6788
  
  
  ggsave(
    filename = here("image", fname),
    plot = pic,
    width = x_px/300,
    height = y_px/300,
    dpi = 300
  )
}


prepare_data <- function(data){

  ribbon <- data
  ribbon$x <- ribbon$x + rnorm(length(ribbon$x), sd = .1)
  ribbon$y <- ribbon$y + rnorm(length(ribbon$y), sd = .1)
  
  ribbon2 <- ribbon %>%
    rename(xend = x, yend = y) %>%
    mutate(time = time - 1) %>%
    filter(time > 0)
  ribbon <- ribbon %>%
    filter(time < max(time))
  ribbon$xend <- ribbon2$xend
  ribbon$yend <- ribbon2$yend

  return(ribbon)
}


blend <- function(x, y, p = .5) {
  x <- col2rgb(x)
  y <- col2rgb(y)
  z <- round(p*x + (1-p)*y)
  z <- rgb(red = z[1, ]/255,
           green = z[2, ]/255,
           blue = z[3, ]/255)
  return(z)
}


for(s in seed) {
  cat("\nusing seed ", s, "...\n", sep = "")
  genuary_tree(s)
}
