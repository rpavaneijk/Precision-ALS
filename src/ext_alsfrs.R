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
    mutate(
        mitos = {
            walking_selfcare <- q8_walking <= 1 | q6_dressing_and_hygiene <= 1
            swallowing <- q3_swallowing <= 1
            communication <- q1_speech <= 1 | q4_handwriting <= 1
            breathing <- q10_dyspnea <= 1 | q12_respiratory_insufficiency <= 2
            walking_selfcare + swallowing + communication + breathing
        }
    )
