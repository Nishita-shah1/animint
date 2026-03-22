# animint — GSoC 2026 Application Tasks
 
This repository contains R source code for my GSoC 2026 application for the [animint2](https://github.com/tdhock/animint2) project — **Animated Interactive ggplots**.
 
---
 
## Files
 
### `figure-combined.R` — Easy Task
An interactive visualization combining gradient descent with animint2.
 
**Live demo:** https://Nishita-shah1.github.io/animint-viz/
 
Three linked plots:
- **|Gradient| vs Iteration** — shows gradient magnitude shrinking toward zero
- **Loss Landscape** — shows current position on `f(x) = x² + 2sin(2x)` with step arrows
- **Loss vs Iteration (log scale)** — shows loss decreasing across 30 iterations
 
Click any bar to jump to that iteration. Animation plays automatically at 800ms per step.
