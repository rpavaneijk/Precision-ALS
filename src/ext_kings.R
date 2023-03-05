source("src/ext_alsfrs.R")
source("src/ext_main.R")

ext_kings <- ext_alsfrs |>
    left_join(ext_main, by = "id") |>
    mutate(
        age_of_assessment = coalesce(
            age_of_assessment,
            (date_of_assessment - date_of_birth) / dyears(1),
        ),
        peg_carrier = case_when(
            !is.na(q5a_cutting_food_without_gastrostomy) &
                is.na(q5b_cutting_food_with_gastrostomy) &
                is.na(q5x_cutting_food_with_gastrostomy_status_unknown) ~ FALSE,
            !is.na(q5b_cutting_food_with_gastrostomy) &
                is.na(q5a_cutting_food_without_gastrostomy) &
                is.na(q5x_cutting_food_with_gastrostomy_status_unknown) ~ TRUE,
            gastrostomy == TRUE & date_of_assessment >= date_of_gastrostomy ~ TRUE,
            gastrostomy == TRUE & age_of_assessment >= age_at_gastrostomy ~ TRUE,
            gastrostomy == TRUE & date_of_assessment < date_of_gastrostomy ~ FALSE,
            gastrostomy == TRUE & age_of_assessment < age_at_gastrostomy ~ FALSE,
            gastrostomy == FALSE ~ FALSE,
        ),
        kings = case_when(
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
    ) |>
    select(id, date_of_assessment, age_of_assessment, kings)
