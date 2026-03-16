library(animint2)

#student
student_df <- data.frame(
  student = paste("S", 1:10, sep=""),
  math    = c(78, 85, 60, 92, 55, 88, 73, 95, 67, 80),
  science = c(82, 79, 65, 88, 70, 91, 68, 97, 72, 75),
  grade   = c("B","A","C","A","C","A","B","A","C","B")
)


#gradient descent


loss_fn <- function(x) x^2 + 2 * sin(2 * x)
grad_fn <- function(x) 2*x + 4 * cos(2 * x)
set.seed(42)
x_start <- 2.5
lr      <- 0.1
n_iter  <- 20
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
  iteration = 0:n_iter,
  x         = x_vals,
  loss      = loss_fn(x_vals)
)


arrow_df <- data.frame(
  iteration = 1:n_iter,
  x_start   = x_vals[1:n_iter],
  x_end     = x_vals[2:(n_iter+1)],
  y_start   = loss_fn(x_vals[1:n_iter]),
  y_end     = loss_fn(x_vals[2:(n_iter+1)])
)


student_plot <- ggplot() +
  geom_point(
    data = student_df,
    aes(x      = math,
        y      = science,
        colour = grade),
    clickSelects = "grade",
    size  = 6,
    alpha = 0.8
  ) +
  labs(
    title = "Plot 1: Student Marks (click a dot to filter grade)",
    x     = "Math Score",
    y     = "Science Score"
  ) +
  theme_bw()


landscape_plot <- ggplot() +

  geom_line(
    data = curve_df,
    aes(x = x, y = loss),
    colour = "#555555",
    size   = 1
  ) +

  geom_segment(
    data = arrow_df,
    aes(x    = x_start,
        xend = x_end,
        y    = y_start,
        yend = y_end),
    showSelected = "iteration",
    arrow  = arrow(length = unit(0.3, "cm")),
    colour = "#E76F51",
    size   = 1.2
  ) +
  # Current point on curve
  geom_point(
    data = point_df,
    aes(x = x, y = loss),
    showSelected = "iteration",
    colour = "#2A9D8F",
    size   = 5
  ) +
  labs(
    title = "Plot 2: Gradient Descent Loss Landscape",
    x     = "x (parameter value)",
    y     = "Loss f(x)"
  ) +
  theme_bw()


loss_plot <- ggplot() +
  # Clickable background bars
  geom_tallrect(
    data = point_df,
    aes(xmin = iteration - 0.5,
        xmax = iteration + 0.5),
    clickSelects = "iteration",
    alpha = 0.3
  ) +

  geom_line(
    data = point_df,
    aes(x = iteration,
        y = loss),
    colour = "#264653",
    size   = 1
  ) +

  geom_point(
    data = point_df,
    aes(x = iteration,
        y = loss),
    showSelected = "iteration",
    colour = "#E9C46A",
    size   = 4
  ) +
  labs(
    title = "Plot 3: Loss vs Iteration (click a bar to select step)",
    x     = "Iteration",
    y     = "Loss f(x)"
  ) +
  theme_bw()



viz <- animint(
  student   = student_plot,
  landscape = landscape_plot,
  loss      = loss_plot,
  time      = list(variable = "iteration", ms = 800),
  title     = "Student Marks + Gradient Descent Visualization",
  source    = "https://github.com/Nishita-shah1/animint/blob/main/figure-combined.R"
)

viz


animint2pages(viz, "animint-viz")
