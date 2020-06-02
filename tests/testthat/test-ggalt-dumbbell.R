context("Dumbbell")

test_that("geom_dumbbell is rendered", {
  d <- data.frame(trt = LETTERS[1:5], l = c(20, 40, 10, 30, 50), r = c(70, 50, 30, 60, 80))

  g <- ggplot(d, aes(y = trt, x = l, xend = r)) +
    ggalt::geom_dumbbell(
      size = 3, color = "#e3e2e1",
      colour_x = "#5b8124", colour_xend = "#bad744",
      dot_guide = TRUE, dot_guide_size = 0.25
    ) +
    labs(x = NULL, y = NULL, title = "ggplot2 geom_dumbbell with dot guide") +
    theme_minimal() +
    theme(panel.grid.major.x = element_line(size = 0.05)) +
    theme(panel.grid.major.y = element_blank())
  
  L <- plotly_build(g)

  # visual testing fails ):
  # while running vdiffr::manage_cases shows a successful doppelganger 
  #expect_doppelganger(L, "dumbbell")

  # Check for four layers
  expect_equivalent(length(L$x$data), 4)
  # dot_guide ends at left data points
  expect_equivalent(L$x$data[[1]]$x[len <- !is.na(L$x$data[[1]]$y)][seq(2, sum(len), 2)], d$l)
  # segment connects left and right data points
  expect_equivalent(L$x$data[[2]]$x[len <- !is.na(L$x$data[[2]]$y)][seq(1, sum(len), 2)], d$l)
  expect_equivalent(L$x$data[[2]]$x[len <- !is.na(L$x$data[[2]]$y)][seq(2, sum(len), 2)], d$r)
  # point layers correspond to left and right data points
  expect_equivalent(L$x$data[[3]]$x, d$l)
  expect_equivalent(L$x$data[[4]]$x, d$r)
})
