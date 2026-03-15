library(animint2)

df <- data.frame(
  student  = paste("S", 1:10, sep=""),
  math     = c(78, 85, 60, 92, 55, 88, 73, 95, 67, 80),
  science  = c(82, 79, 65, 88, 70, 91, 68, 97, 72, 75),
  grade    = c("B","A","C","A","C","A","B","A","C","B")
)

viz <- animint(
  scatter = ggplot() +
    geom_point(
      data = df,
      aes(x = math,
          y = science,
          colour = grade),
      clickSelects = "grade",   
      size = 6,
      alpha = 0.8
    ) +
    labs(
      title = "Student Marks: Math vs Science",
      x     = "Math Score",
      y     = "Science Score"
    ) +
    theme_bw(),
  
  title  = "Student Marks Interactive Plot",
  source = "https://github.com/Nishita-shah1/animint/blob/main/simple-plot.R"
)

viz