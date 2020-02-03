numericInputReg <- function (label,...) {
  fluidRow(
    column(12, div(class = 'reg', label), numericInput(label = NULL,...))
  )
}