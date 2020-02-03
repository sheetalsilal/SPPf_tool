sliderInputLeft <- function (label,...) {
  fluidRow(
    column(4, div(class = 'sl', label)),
    column(8, sliderInput(label = NULL, ticks = FALSE,...))
  )
}