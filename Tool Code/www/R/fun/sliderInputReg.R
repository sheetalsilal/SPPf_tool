sliderInputReg <- function (label,...) {
  fluidRow(
    column(12, div(class = 'reg', label), sliderInput(label = NULL, ticks = FALSE,...))
  )
}