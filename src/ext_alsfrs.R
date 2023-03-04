source("src/ext_common.R")

ext_alsfrs <- suppressWarnings(ext_read(
    "data/P-ALS_Ext_ALSFRS-R.xlsx",
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
        peg_carrier = case_when(
            !is.na(q5a_cutting_food_without_gastrostomy) &
                is.na(q5b_cutting_food_with_gastrostomy) &
                is.na(q5x_cutting_food_with_gastrostomy_status_unknown) ~ FALSE,
            !is.na(q5b_cutting_food_with_gastrostomy) &
                is.na(q5a_cutting_food_without_gastrostomy) &
                is.na(q5x_cutting_food_with_gastrostomy_status_unknown) ~ TRUE
        ),
        kings_c = case_when(
            q10_dyspnea == 0 | q12_respiratory_insufficiency < 4 ~ "4B",
            peg_carrier == TRUE ~ "4A",
            peg_carrier == FALSE ~ {
                bulbar <- any(c(q1_speech, q2_salivation, q3_swallowing) < 4)
                upper <- any(c(
                    q4_handwriting,
                    q5a_cutting_food_without_gastrostomy,
                    q5x_cutting_food_with_gastrostomy_status_unknown
                ) < 4)
                lower <- q8_walking < 4
                as.character(bulbar + upper + lower)
            }
        ),
        mitos = {
            walking_selfcare <- q8_walking <= 1 | q6_dressing_and_hygiene <= 1
            swallowing <- q3_swallowing <= 1
            communication <- q1_speech <= 1 | q4_handwriting <= 1
            breathing <- q10_dyspnea <= 1 | q12_respiratory_insufficiency <= 2
            walking_selfcare + swallowing + communication + breathing
        }
    )
