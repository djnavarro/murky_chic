library(Rcpp)
library(cairocore)
library(cairobasic)

sourceCpp(here::here("source", "softloud_border12.cpp"))

# parameters
seed <- 4
layers <- 10
scheme <- 0

# fixed
iter <- 10000000
prefix <- "softloud_border12_"
transparency <- "aa"
adjust <- function(x) {x}
brd <- 0
filter_y <- c(-.5, 1)
filter_x <- c(-.75, .75)

set.seed(seed)


if(scheme == 0) {
  bg <- "grey10"
  pl <- "scico::grayC"
  adjust <- function(x) {
    x <- adjustcolor(x[length(x):1], 1, 1.5, 1.5, 1.5)
    x[runif(length(x)) < .1] <- "#8b0000"
    return(x)
  }
}
if(scheme == 1) {
  bg <- "white"
  pl <- "scico::grayC"
}
if(scheme == 2) {
  bg <- "hotpink4"
  pl <- "ggthemes::Gold-Purple Diverging"
}
if(scheme == 3) {
  bg <- "ghostwhite"
  pl <- "scico::oslo"
}
if(scheme == 4) {
  bg <- "bisque"
  pl <- "scico::bilbao"
}
if(scheme == 5) {
  bg <- "slateblue4"
  pl <- "grDevices::TealRose"
}
if(scheme == 6) {
  bg <- "black"
  pl <- "viridis::viridis"
}
if(scheme == 7) {
  bg <- "azure"
  pl <- "viridis::magma"
}
if(scheme == 8) {
  bg <- "lavender"
  pl <- "scico::tokyo"
}
if(scheme == 9) {
  bg <- "white"
  pl <- "viridis::magma"
}
if(scheme == 10) {
  bg <- "lemonchiffon3"
  pl <- "scico::bamako"  
}
if(scheme == 11) {
  bg <- "grey60"
  pl <- "scico::berlin"  
}
if(scheme == 12) {
  bg <- "grey20"
  pl <- "scico::lajolla"  
}
if(scheme == 13) {
  bg <- "midnightblue"
  pl <- "ggthemes::Sunset-Sunrise Diverging"
}
if(scheme == 14) {
  bg <- "mediumpurple4"
  pl <- "grDevices::PuRd"
}
if(scheme == 15) {
  bg <- "mediumpurple4"
  pl <- "grDevices::Purples"
}
if(scheme == 16) {
  bg <- "#612B21"
  pl <- "scico::batlow"
}
if(scheme == 17) {
  bg <- "grey10"
  pl <- "grDevices::Purple-Blue"
}
if(scheme == 18) {
  bg <- "black"
  pl <- "viridis::magma"
}
if(scheme == 19) {
  bg <- "black"
  pl <- "grDevices::PuOr"
}
if(scheme == 20) {
  bg <- "lightsteelblue4"
  pl <- "scico::bilbao"
  adjust <- function(x) {adjustcolor(x, 1, 1.5, 1.5, 1.5)}
}
if(scheme == 21) {
  bg <- "ivory1"
  pl <- "scico::cork"
}
if(scheme == 22) {
  bg <- "black"
  pl <- "viridis::magma"
  brd <- 0
}
if(scheme == 23) {
  bg <- "grey10"
  pl <- "grDevices::rainbow"
  adjust <- function(x) {
    x <- adjustcolor(x[length(x):1], 1, .7, .7, .7)
    #x[runif(length(x)) < .1] <- "#8b0000"
    return(x)
  }
}
if(scheme == 24) {
  bg <- "grey40"
  pl <- "scico::vik"
}
if(scheme == 25) {
  bg <- "white"
  pl <- "scico::grayC"
  brd <- -2500
}
if(scheme == 26) {
  bg <- "black"
  pl <- "scico::grayC"
  brd <- -2500
}
if(scheme == 27) {
  bg <- paletteer::paletteer_c("grDevices::TealRose",4)[2]
  bg <- adjustcolor(bg,1,.6,.6,.6)
  pl <- "grDevices::TealRose"
  brd <- -2000
}
if(scheme == 28) {
  bg <- "grey30"
  pl <- "viridis::viridis"
}
if(scheme == 29) {
  bg <- "antiquewhite"
  pl <- "scico::bilbao"
}
if(scheme == 30) {
  bg <- adjustcolor("slateblue4", 1, .5, .5, .5)
  pl <- "grDevices::Oranges"
  filter_y <- c(-1.25, 1.75)
  filter_x <- c(-1.25, 1.75)
}
if(scheme == 36) {
  bg <- adjustcolor("slateblue4", 1, .5, .5, .5)
  pl <- "scico::hawaii"
  filter_y <- c(.2, 4.2)
  filter_x <- c(-2, 2)
}
if(scheme == 37) {
  bg <- adjustcolor("darkorchid4", 1, .5, .5, .5)
  pl <- "grDevices::Purples"
  filter_y <- c(2, 4)
  filter_x <- c(-.4, 1.6)
  transparency <- "ff"
}
if(scheme == 38) {
  bg <- "#201010"
  pl <- "gameofthrones::targaryen"
  adjust <- function(x) {
    adjustcolor(x,1,.9, .9, .9)
  }
  transparency <- "60"
}
if(scheme == 39) {
  bg <- adjustcolor("darkorchid4", 1, .5, .5, .5)
  pl <- "grDevices::Purples"
}




cat("generating...\n")


df <- flame(iter, layers)
df <- as.data.frame(df)
names(df) <- c("x","y","c")
df <- df[-(1:100),]

keep <- df$y > filter_y[1] & df$y < filter_y[2] & 
  df$x > filter_x[1] & df$x < filter_x[2]
df <- df[keep, ]
df$c[df$c < -1] <- -1
df$c[df$c > 1] <- 1

# Manually scale the co-ordinates to the image size
px <- 5000
xrng <- max(df[,1]) - min(df[,1])
yrng <- max(df[,2]) - min(df[,2])
rng <- max(c(xrng, yrng))

xdiff <- max(c(yrng - xrng, 0))/2
ydiff <- max(c(xrng - yrng, 0))/2

df[,1] <- brd + (df[,1] - min(df[,1]) + xdiff) / rng * (px - 2*brd)
df[,2] <- brd + (df[,2] - min(df[,2]) + ydiff) / rng * (px - 2*brd)


# Manually create a vector of colours
col_idx <- as.integer((df[,3] - min(df[,3])) / (max(df[,3]) - min(df[,3])) * 255) + 1L
pal <- paletteer::paletteer_c(palette = pl, n = 256)
pal <- adjust(pal)
pal <- gsub("FF$", transparency, pal)

# Replace with murky chic
blend <- function(x, y, p = .5) {
  x <- col2rgb(x)
  y <- col2rgb(y)
  z <- round(p*x + (1-p)*y)
  z <- rgb(red = z[1, ]/255, 
           green = z[2, ]/255, 
           blue = z[3, ]/255)
  return(z)
} 
pal <- c(blend("#666a67", "white"), "#23424c", "#2F3D00", "white")
pal <- colorRampPalette(pal)
pal <- pal(256)

col <- pal[col_idx]

fname <- paste0(prefix, seed, "_", layers, "_", scheme, ".png")
fpath <- here::here("image", fname)

cat("rendering...\n")

cb <- cairobasic::CairoBasic$new(width = px, height = px, bg = bg, antialias = TRUE)
cb$add_circles(x=df[,1], y = df[,2], r = 3, fill = col, colour = NA)
cb$write_png(fpath)
