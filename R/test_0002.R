# Set parameters ---------------------------------------------------------------

# Defined
iteration_id <- "test_0002"
initial_seed <- 517002
bg_colour <- "#FFFFFF"
line_colour <- "#000000"

# Derived
set.seed(initial_seed)
seed_vec <- sample(seq(1,1000000), 2)

# For prominent line
control_points <- 5
lines <- 25

# For scribbles
scribble_cp <- 16
scribble_plots <- lines
scribble_lines <- 10
scribble_colour <- "#DCCCBB"

# Create data ------------------------------------------------------------------

# Prominent line
set.seed(seed_vec[1])
data <- tibble::tibble(
  group = rep(1:lines, each = control_points),
  x = runif(lines*control_points, min = 0, max = 0.1),
  y = runif(lines*control_points))
  
d <- ggplot2::ggplot() +
  ggforce::stat_bspline(
    data = data,
    ggplot2::aes(x = x, y = y, group = group),
    n = 20000)
  
t <- ggplot2::layer_data(d)

# Scribbles
set.seed(seed_vec[2])
data_scribbles <- tibble::tibble(
  group = rep(1:scribble_plots, each = scribble_cp*scribble_lines),
  x = runif(scribble_plots*scribble_lines*scribble_cp),
  y = runif(scribble_plots*scribble_lines*scribble_cp))

d_scribbles <- ggplot2::ggplot() +
  ggforce::stat_bspline(
    data = data_scribbles,
    ggplot2::aes(x = x, y = y, group = group),
    n = 20000)

t_scribbles <- ggplot2::layer_data(d)

# Build plot -------------------------------------------------------------------

plot_list <- list()
for (i in 1:lines) {
  
  p <- ggplot2::ggplot() +
    ggplot2::geom_point(
      data = t_scribbles,
      ggplot2::aes(x = x, y = y, group = group), # alpha = index
      shape = 16, colour = scribble_colour, size = 0.5) +
    ggplot2::geom_point(
      data = t |> dplyr::filter(group == i),
      ggplot2::aes(x = x, y = y, size = index), # alpha = index
      shape = 16, colour = line_colour) +
    ggplot2::theme_void() +
    ggplot2::theme(
      legend.position = "none",
      plot.background = ggplot2::element_rect(colour = "#FFFFFF", size = 3),
      plot.margin = ggplot2::margin(20, 20, 20, 20, unit = "pt"))
  
  plot_list[[i]] <- p
  
}

# Prepare grid of plots
plots_in_grid <- patchwork::wrap_plots(plot_list)

# Export to file
ggplot2::ggsave(
  here::here(glue::glue("img/tests/{`iteration_id`}.png")),
  plots_in_grid, width = 20, height = 20, units = "cm", dpi = 600, device = ragg::agg_png)