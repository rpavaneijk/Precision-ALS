alsfrs_progression_category <- function(x) {
  case_when(
    x < 0.8 ~ "SP",
    x >= 0.8 & x <= 1.35 ~ "NP",
    x > 1.35 ~ "FP"
  )
}