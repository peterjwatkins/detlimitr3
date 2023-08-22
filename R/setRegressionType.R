set_regression <- function(model) {
  rt <-
    ifelse(
      model == "l" |
        model == "linear" | model == "Linear" |  model == "L" ,
      "Linear",
      ifelse(
        model == "q" |
          model == "quad" |  model == "Quad" | model == "Quadratic",
        "Quadratic",
        ifelse(
          model == "p" |
            model == "power" |
            model == "power" | model == "Power",
          "Power",
          "u"
        ) ## model type is 'u'nkwown
      )
    )
  return(rt)
}