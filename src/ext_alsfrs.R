source("src/ext_common.R")

ext_alsfrs <- suppressWarnings(ext_load(
    "P-ALS_Ext_ALSFRS-R.xlsx",
    col_types = c(
        "text", # ID
        "text", # Site
        "date", # Date of Assessment
        "numeric", # Age of Assessment
        rep("numeric", times = 15) # ...
    )
)) |>
    ext_normalize_names() |>
    rename_with(\(x) str_replace(x, "x(\\d+[abx]?)_", "q\\1_")) |>
    rename_with(\(x) str_replace_all(x, "hygine", "hygiene")) |>
    rename(age_at_assessment = "age_of_assessment")
