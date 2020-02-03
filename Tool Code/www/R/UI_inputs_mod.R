sliderInputLeft <- function (label,...) {
  fluidRow(
    column(4, div(class = 'sl', label)),
    column(8, sliderInput(label = NULL, ticks = FALSE,...))
  )
}

sliderInputReg <- function (label,...) {
  fluidRow(
    column(12, div(class = 'reg', label), sliderInput(label = NULL, ticks = FALSE,...))
  )
}

numericInputReg <- function (label,...) {
  fluidRow(
    column(12, div(class = 'reg', label), numericInput(label = NULL,...))
  )
}