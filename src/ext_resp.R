source("src/ext_common.R")

ext_resp <- ext_load(
    "P-ALS_Ext_Respiratory_Assessments.xlsx",
    col_types = c(
        "text", # ID
        "text", # Site
        "date", # Date of Assessment
        "numeric", # Age of Assessment
        rep("numeric", 31) # ...
    )
) |>
    ext_normalize_names()
