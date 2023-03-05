library(dplyr)
library(ggplot2)
library(ggfortify)
library(lazyeval)
library(rlang)
library(stringr)
library(survival)
library(tibble)
library(tidyr)

source("src/ext_main.R")
source("src/ext_kings.R")
source("src/ext_alsfrs.R")

ext_fit_survival_curves <- function(data, origin, events, groups = NULL) {
    res <- list()
    for (o in origin) {
        for (e in names(events)) {
            key <- str_glue("{e}_from_{o}_overall")
            age_at_event_col <- str_glue("age_at_{e}")
            age_at_origin_col <- str_glue("age_at_{o}")
            event_data <- data |> mutate(
                time = .data[[age_at_event_col]] - .data[[age_at_origin_col]],
                event = f_eval(events[[e]], data)
            )
            res[[key]] <- survfit(Surv(time, event) ~ 1, data = event_data)

            for (g in groups) {
                key <- str_glue("{e}_from_{o}_by_{g}")
                group_data <- event_data |> mutate(group = .data[[g]])
                res[[key]] <- survfit(Surv(time, event) ~ group, data = group_data)
            }
        }
    }
    res
}

data <- ext_main |> mutate(
    gene = factor(case_when(
        c9orf72_status == "Positive" ~ "C9orf72",
        sod1_status == "Positive" ~ "SOD1",
        fus_status == "Positive" ~ "FUS"
    )),
    site_of_onset = factor(case_when(
        bulbar_onset ~ "Bulbar",
        spinal_onset ~ "Spinal",
        cervical_onset ~ "Cervical",
        cognitive_onset ~ "Cognitive",
        respiratory_onset ~ "Respiratory",
    )),
    proximal_or_distal_onset = factor(case_when(
        proximal_onset ~ "Proximal",
        distal_onset ~ "Distal",
    ), levels = c("Proximal", "Distal")),
)

time_to_event <- ext_fit_survival_curves(data,
    origin = c("onset", "diagnosis"),
    groups = c("sex", "gene", "site_of_onset", "proximal_or_distal_onset"),
    events = list(
        "death" = ~ vital_status == "Deceased",
        "niv" = ~ niv == TRUE,
        "gastrostomy" = ~ gastrostomy == TRUE,
        "tracheostomy" = ~ tracheostomy == TRUE
    )
)

for (key in names(time_to_event)) {
    m <- str_match(key, "^([a-z]+)_from_([a-z]+)")
    event <- str_replace_all(m[[2]], "_", " ")
    origin <- str_replace_all(m[[3]], "_", " ")
    if (str_ends(key, "_overall")) {
        p <- autoplot(time_to_event[[key]],
            main = str_glue("Time to {event} from {origin}"),
            xlab = "Time (years)", ylab = "Survival probability (%)"
        )
    } else {
        group <-
            str_extract(key, "_by_([a-z_]+)$", group = 1) |>
            str_replace_all("_", " ")
        p <- autoplot(time_to_event[[key]],
            main = str_glue("Time to {event} from {origin} by {group}"),
            xlab = "Time (years)", ylab = "Survival probability (%)"
        )
    }
    ggsave(str_glue("output/{key}.png"), plot = p)
}
