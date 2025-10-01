library(tidyverse)
library(janitor)
library(here)
library(scales)
library(ggthemes)
library(MCOE)
library(ggrepel)


con <- mcoe_sql_con()


cci <- tbl(con, "DASH_ALL") %>%
  filter(
    #(cds == "27660680000000"),
    indicator == "CCI"
    # id == "ccidownload2021.txt") %>%
    #        ReportingCategory == "TA",
    #        CharterSchool =="All",
    #        Dass == "All"
  ) %>%
  collect()


cci.mry <- cci %>%
  filter(
    str_detect(countyname, "Monterey"),
    rtype == "D",
    reportingyear == "2024",
    currdenom >= 10
  ) |>
  select(districtname, studentgroup, studentgroup.long, starts_with("curr")) |>
  select(
    -starts_with("curr_prep_non_reg"),
    -starts_with("curr_prep_statefed"),
    -starts_with("curr_prep_trans")
  ) |>
  select(
    -starts_with("curr_aprep_non_reg"),
    -starts_with("curr_aprep_statefed"),
    -starts_with("curr_aprep_trans")
  ) |>
  select(-currprate_enrolled:-currnsizegroup) |>
  select(-currdenom_without_prloss:-currdeclined)


cci_graph <- function(var, tit) {
  cci %>%
    mutate(rate = {{ var }}) %>%

    ggplot(aes(
      y = rate,
      x = fct_reorder(definition, rate),
      label = round2(rate, 2)
    )) +
    geom_segment(
      aes(
        x = fct_reorder(definition, rate),
        xend = fct_reorder(definition, rate),
        y = 0,
        yend = rate
      ),
      color = "orange",
      size = 2
    ) +
    geom_point(color = "orange", size = 5, alpha = 0.6) +
    expand_limits(y = c(0, 5)) +
    coord_flip() +
    geom_text(size = 3, color = "black") +
    theme_hc() +
    mcoe_theme +
    labs(
      x = "",
      y = "",
      color = "",
      title = tit,
      subtitle = "South Monterey County JUHSD - 2021",
      caption = "Source: https://www6.cde.ca.gov/californiamodel/ccireport2021?&year=2021&cdcode=2766068&scode=&reporttype=schools"
    )

  ggsave(here("output", paste0(tit, ".png")), width = 7, height = 5)
}
