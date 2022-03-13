####################################################################
library(gsubfn)
library(tidyverse)
setwd("~/flores/mondrian/code")


  s <- sample(15:26, 1)
  v1 <- sample(c("F", "+", "-", "|"), size = s, replace = TRUE, prob = c(8,12,12,2))
  v2 <- sample("[]", 3, replace = TRUE) %>% str_extract_all("\\d*\\+|\\d*\\-|F|L|R|\\[|\\]|\\|") %>% unlist
  v3 <- sample(1:(s+1), size = length(v2)) %>% sort
  for(i in 1:length(v3)){
    c(v1[1:(v3[i] + i - 1)], v2[i], v1[(v3[i] + i - 1):length(v1)]) -> v1
  }
  
  axiom <- "F-F-F-F"
  rules <- list("F"=paste(v1, collapse=""))
  angle <- sample(c(90, 90), 1)
  depth <- sample(3:4,1)
  ds <- jitter(1)
  
  for (i in 1:depth) axiom <- gsubfn(".", rules, axiom)
  
  actions <- str_extract_all(axiom, "\\d*\\+|\\d*\\-|F|L|G|R|\\[|\\]|\\|") %>% unlist
  
  # Estas variables guardan el estado actual del punto
  x_current <- 0
  y_current <- 0
  a_current <- 0
  d_current <- 0
  
  status <- tibble(x = x_current, 
                   y = y_current, 
                   alfa = a_current,
                   depth = d_current)
  
  lines <- data.frame(x = numeric(), 
                      y = numeric(), 
                      xend = numeric(), 
                      yend = numeric())
  
  for (action in actions) 
  {
    if (action=="F") {
      lines <- lines %>% add_row(x = x_current,
                                 y = y_current,
                                 xend = x_current + (ds^d_current) * cos(a_current * pi / 180),
                                 yend = y_current + (ds^d_current) * sin(a_current * pi / 180)) 
      x_current <- x_current + (ds^d_current) * cos(a_current * pi / 180)
      y_current <- y_current + (ds^d_current) * sin(a_current * pi / 180)
      d_current <- d_current + 1
    }
    if (action=="G") {
      x_current <- x_current + (ds^d_current) * cos(a_current * pi / 180)
      y_current <- y_current + (ds^d_current) * sin(a_current * pi / 180)
      d_current <- d_current + 1
    }
    if (action=="|") {
      lines <- lines %>% add_row(x = x_current,
                                 y = y_current,
                                 xend = x_current + (ds^d_current) * cos(a_current * pi / 180),
                                 yend = y_current + (ds^d_current) * sin(a_current * pi / 180)) 
      x_current <- x_current + (ds^d_current) * cos(a_current * pi / 180)
      y_current <- y_current + (ds^d_current) * sin(a_current * pi / 180)
      d_current <- d_current + 1
    }
    if (action=="+") {
      a_current <- a_current - angle
    }
    if (action=="-") {
      a_current <- a_current + angle
    }
    if (action=="[") { 
      status <- status %>% add_row(x = x_current, 
                                   y = y_current, 
                                   alfa = a_current,
                                   depth = d_current)
    }
    
    if (action=="]") {
      
      x_current <- tail(status, 1) %>% pull(x)
      y_current <- tail(status, 1) %>% pull(y)
      a_current <- tail(status, 1) %>% pull(alfa)
      d_current <- tail(status, 1) %>% pull(depth)
      
      status <- head(status, -1)
      
    }
  }
  
  
  
  
  
  lines %>%
    mutate(x = round(x, 1),
           y = round(y, 1),
           xend = round(xend, 1),
           yend = round(yend, 1)) %>%
    distinct(x, y, xend, yend) -> lines
  
  select(lines, x3 = x, y3 =y) %>%
    bind_rows(select(lines, x3 = xend, y3 =yend)) %>%
    distinct(x3, y3) -> points
  
  n <- 10
  split(points, rep(1:ceiling(nrow(points)/n), 
                    each = n, 
                    length.out = nrow(points))) -> points_divided
  
  lapply(points_divided, function(sub) {
    sub %>% 
      crossing(lines) %>%
      filter(x == x3 | y == y3 | xend == x3 | yend == y3) %>%
      filter(x != x3 | y != y3 , xend != x3 | yend != y3) %>%
      mutate(id = row_number())
  }) %>% bind_rows() -> squares1
  
  
  # nos quedamos con aquellos squares tal que alguno de los dos nuevos lados existe en el dataset
  
  bind_rows(
    squares1 %>%
      inner_join(lines, c("x" = "x", "y" = "y", "x3" = "xend", "y3" = "yend")),
    squares1 %>%
      inner_join(lines, c("xend" = "x", "yend" = "y", "x3" = "xend", "y3" = "yend")),
    squares1 %>%
      inner_join(lines, c("x3" = "x", "y3" = "y", "x" = "xend", "y" = "yend")),
    squares1 %>%
      inner_join(lines, c("x3" = "x", "y3" = "y", "xend" = "xend", "yend" = "yend"))) %>%
    distinct(x, y, xend, yend, x3, y3, id) -> squares2
  
  squares2 %>% anti_join(squares2 %>% filter(x == xend, xend == x3),
                         by = c("x", "y", "xend", "yend", "x3", "y3", "id")) -> squares2
  squares2 %>% anti_join(squares2 %>% filter(y == yend, yend == y3),
                         by = c("x", "y", "xend", "yend", "x3", "y3", "id")) -> squares2
  
  squares2 %>%
    mutate(xmax = pmax(x, xend, x3),
           xmin = pmin(x, xend, x3),
           ymax = pmax(y, yend, y3),
           ymin = pmin(y, yend, y3)) %>%
    mutate(A = (xmax - xmin) * (ymax - ymin) / 2) -> squares
  
  
  colors <- c("#FEFFFA","#000002","#F60201","#FDED01", "#1F7FC9")
  
  qnts <- quantile(squares$A, probs = seq(0, 1, 0.05), na.rm = FALSE,
                   names = TRUE, type = 7)
  
  
  
  ggplot() +
    geom_rect(aes(xmax = xmax,
                  xmin = xmin,
                  ymax = ymax,
                  ymin = ymin,
                  fill = id %% length(colors) %>% jitter(amount=.025)),
              data = squares %>% filter(A >= qnts[1]),
              lwd = 2,
              color = "white") +
    geom_segment(aes(x = x, y = y, xend = xend, yend = yend),
                 data = lines,
                 lwd = .65,
                 lineend = "square",
                 color = "#000002") +
    scale_fill_gradientn(colors = colors) +
    theme_void() +
    theme(legend.position = "none") +
    coord_equal() -> plot
  
  width <- max(points$x3) - min(points$x3)
  height <- max(points$y3) - min(points$y3)
  
  whmax <- 8
  if (width >= height) {
    w <- whmax
    h <- whmax * height / width 
  } else {
    h <- whmax
    w <- whmax * width / height
  }
  
  name <- paste(sample(letters,6), collapse = "")
  
  ggsave(paste0(name,".png"), plot, width = w, height = h)
  
  


