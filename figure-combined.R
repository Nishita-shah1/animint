library(animint2)

# gradient descent setup
loss_fn <- function(x) x^2 + 2 * sin(2 * x)
grad_fn <- function(x) 2*x + 4 * cos(2 * x)

set.seed(42)
x_start <- 2.5
lr      <- 0.02
n_iter  <- 30

x_vals <- numeric(n_iter + 1)
x_vals[1] <- x_start
for (i in seq_len(n_iter)) {
  x_vals[i+1] <- x_vals[i] - lr * grad_fn(x_vals[i])
}

curve_df <- data.frame(
  x    = seq(-3, 3, length.out = 300),
  loss = loss_fn(seq(-3, 3, length.out = 300))
)

point_df <- data.frame(
  iteration    = 0:n_iter,
  x            = x_vals,
  loss         = loss_fn(x_vals),
  gradient     = grad_fn(x_vals),
  abs_gradient = abs(grad_fn(x_vals))
)

label_df <- data.frame(
  iteration = 0:n_iter,
  text_grad = paste0("Iter: ", 0:n_iter,
                     "  |grad|= ", round(abs(grad_fn(x_vals)), 4)),
  text_land = paste0("Iter: ", 0:n_iter,
                     "  x=", round(x_vals, 4),
                     "  loss=", round(loss_fn(x_vals), 4)),
  text_loss = paste0("Iter: ", 0:n_iter,
                     "  loss=", round(loss_fn(x_vals), 4))
)

arrow_df <- data.frame(
  iteration = 1:n_iter,
  x_start   = x_vals[1:n_iter],
  x_end     = x_vals[2:(n_iter+1)],
  y_start   = loss_fn(x_vals[1:n_iter]),
  y_end     = loss_fn(x_vals[2:(n_iter+1)])
)

# plot 1 - gradient value vs iteration
gradient_plot <- ggplot() +
  geom_tallrect(
    data = point_df,
    aes(xmin = iteration - 0.5, xmax = iteration + 0.5),
    clickSelects = "iteration",
    alpha = 0.3
  ) +
  geom_line(
    data   = point_df,
    aes(x  = iteration, y = abs_gradient),
    colour = "#6A4C93",
    size   = 1
  ) +
  geom_point(
    data   = point_df,
    aes(x  = iteration, y = abs_gradient),
    colour = "#6A4C93",
    size   = 2
  ) +
  geom_point(
    data         = point_df,
    aes(x        = iteration, y = abs_gradient),
    showSelected = "iteration",
    colour       = "#E76F51",
    size         = 6
  ) +
  geom_text(
    data         = label_df,
    aes(x        = 8,
        y        = 6,
        label    = text_grad),
    showSelected = "iteration",
    hjust        = 0,
    vjust        = 0,
    size         = 15,
    colour       = "#E76F51"
  ) +
  scale_x_continuous(limits = c(0, 20)) +
  scale_y_continuous(limits = c(0, 7)) +
  labs(
    title = "|Gradient| vs Iteration",
    x     = "Iteration",
    y     = "|Gradient|"
  ) +
  theme_bw() +
  theme(plot.title = element_text(size = 13))

# plot 2 - loss landscape
landscape_plot <- ggplot() +
  geom_line(
    data   = curve_df,
    aes(x  = x, y = loss),
    colour = "#555555",
    size   = 1
  ) +
  geom_point(
    data   = point_df,
    aes(x  = x, y = loss),
    colour = "#2A9D8F",
    size   = 2,
    alpha  = 0.3
  ) +
  geom_segment(
    data         = arrow_df,
    aes(x        = x_start, xend = x_end,
        y        = y_start, yend = y_end),
    showSelected = "iteration",
    arrow        = arrow(length = unit(0.3, "cm")),
    colour       = "#E76F51",
    size         = 1.2
  ) +
  geom_point(
    data         = point_df,
    aes(x        = x, y = loss),
    showSelected = "iteration",
    colour       = "#2A9D8F",
    size         = 7
  ) +
  geom_text(
    data         = label_df,
    aes(x        = -1,
        y        = 8.5,
        label    = text_land),
    showSelected = "iteration",
    hjust        = 0,
    vjust        = 0,
    size         = 15,
    colour       = "#2A9D8F"
  ) +
  coord_cartesian(xlim = c(-1, 3.5), ylim = c(-1.5, 9)) +
  labs(
    title = "Loss Landscape",
    x     = "x (parameter value)",
    y     = "Loss f(x)"
  ) +
  theme_bw() +
  theme(plot.title = element_text(size = 13))

# plot 3 - loss vs iteration
loss_plot <- ggplot() +
  geom_tallrect(
    data = point_df,
    aes(xmin = iteration - 0.5, xmax = iteration + 0.5),
    clickSelects = "iteration",
    alpha = 0.3
  ) +
  geom_line(
    data   = point_df,
    aes(x  = iteration, y = loss),
    colour = "#264653",
    size   = 1
  ) +
  geom_point(
    data   = point_df,
    aes(x  = iteration, y = loss),
    colour = "#264653",
    size   = 2
  ) +
  geom_point(
    data         = point_df,
    aes(x        = iteration, y = loss),
    showSelected = "iteration",
    colour       = "#E9C46A",
    size         = 6
  ) +
  geom_text(
    data         = label_df,
    aes(x        = 8,
        y        = 4,
        label    = text_loss),
    showSelected = "iteration",
    hjust        = 0,
    vjust        = 0,
    size         = 15,
    colour       = "#264653"
  ) +
  scale_x_continuous(limits = c(0, 20)) +
  scale_y_log10() +
  labs(
    title = "Loss vs Iteration (log scale)",
    x     = "Iteration",
    y     = "Loss f(x) - log scale"
  ) +
  theme_bw() +
  theme(plot.title = element_text(size = 13))

viz <- animint(
  gradient  = gradient_plot,
  landscape = landscape_plot,
  loss      = loss_plot,
  time      = list(variable = "iteration", ms = 800),
  title     = "Gradient Descent Visualization",
  source    = "https://github.com/Nishita-shah1/animint/blob/main/figure-combined.R",
  width     = list(gradient = 550, landscape = 600, loss = 550),
  height    = list(gradient = 420, landscape = 440, loss = 420)
)

viz
animint2pages(viz, "animint-viz")
