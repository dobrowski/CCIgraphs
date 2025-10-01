# packages
library(tidyverse)
library(ggforce) # for per-slice radii (lets us "explode" a wedge)
library(patchwork)
library(scales)
library(googlesheets4)

# uses the funciton in fun-breakout

# Short definitions of readiness paths -------
# fmt: skip
prep.df <- tribble(
  ~areas,   ~definition,
  "curr_prep_summative_pct",  "Prepared - Standard Met on both CAASPP ELA and Math",
  "curr_prep_apexam_pct",  "Prepared - 3+ on two Advanced Placement Exams",
  "curr_prep_ibexam_pct",  "Prepared - 4+ on two International Baccalaureate Exams",
  "curr_prep_collegecredit_pct",  "Prepared - C- or better on 2 semesters or 3 quarters of College Credit",
  "curr_prep_agplus_pct",  "Prepared - met A-G plus additional criteria",
  "curr_prep_cteplus_pct",  "Prepared - complete CTE pathway plus additional criteria",
  "curr_prep_ssb_pct",  "Prepared - State Seal of Biliteracy and CAASPP ELA",
  "curr_prep_milsci_pct",  "Prepared - two years of Leadership/Military Science plus additional criteria",
  "curr_prep_reg_pre_pct",  "Prepared - Completed registered Pre-Apprentiseship",
  "curr_aprep_summative_pct",  "Approaching Prepared - Standard Nearly Met on both CAASPP ELA and Math",
  "curr_aprep_collegecredit_pct",  "Approaching Prepared - C- or better on 1 semesters or 2 quarters of College Credit",
  "curr_aprep_ag_pct",  "Approaching Prepared - met A-G",
  "curr_aprep_cte_pct",  "Approaching Prepared - complete CTE pathway",
  "curr_aprep_milsci_pct",  "Approaching Prepared - two years of Leadership/Military Science",
  "curr_nprep_pct",  "Not Prepared"
)


### Single working example -----

cci.mry |>
  filter(
    str_detect(districtname, "Monterey Peninsula"),
    studentgroup == "ALL"
  ) |>
  pivot_longer(
    cols = c(curr_prep, curr_aprep, curr_nprep),
    names_to = "Preparation_Level"
  ) |>
  mutate(
    Preparation_Level = case_match(
      Preparation_Level,
      "curr_prep" ~ "Prepared",
      "curr_aprep" ~ "Approaching\nPrepared",
      "curr_nprep" ~ "Not Prepared"
    )
  ) |>
  pivot_longer(
    cols = c(ends_with("pct")),
    names_to = "areas",
    values_to = "Percent"
  ) |>
  left_join(prep.df) |>
  filter(str_starts(definition, str_sub(Preparation_Level, 1, 5))) |>

  # mutate(Prepared = str_squish(str_to_lower(as.character(Prepared)))) |>
  pie_with_breakout_bar(
    #    df = example_df,
    dist = "Monterey Peninsula",
    studentgroup = "All",
    category = Preparation_Level,
    main_value = value,
    subcategory = definition,
    breakout_cat = "Approaching\nPrepared", # <- which wedge to explode and analyze
    breakout_value = Percent, # used for the bars
    palette = c("#ffffbf", "#fc8d59", "#99d594")
  ) +
  mcoe_theme


#### wrapping function -------

breakout.cci <- function(df, dist, sg = "ALL", prep_level = "Prepared") {
  df.filt <- df |>
    filter(str_detect(districtname, dist), studentgroup == sg)

  ssgg <- df.filt$studentgroup.long[1]

  df.filt |>
    pivot_longer(
      cols = c(curr_prep, curr_aprep, curr_nprep),
      names_to = "Preparation_Level"
    ) |>
    mutate(
      Preparation_Level = case_match(
        Preparation_Level,
        "curr_prep" ~ "Prepared",
        "curr_aprep" ~ "Approaching\nPrepared",
        "curr_nprep" ~ "Not Prepared"
      )
    ) |>
    pivot_longer(
      cols = c(ends_with("pct")),
      names_to = "areas",
      values_to = "Percent"
    ) |>
    left_join(prep.df) |>
    filter(str_starts(definition, str_sub(Preparation_Level, 1, 5))) |>

    # mutate(Prepared = str_squish(str_to_lower(as.character(Prepared)))) |>
    pie_with_breakout_bar(
      #    df = example_df,
      dist = dist,
      studentgroup = ssgg,
      category = Preparation_Level,
      main_value = value,
      subcategory = definition,
      breakout_cat = prep_level, # <- which wedge to explode and analyze
      breakout_value = Percent, # used for the bars
      palette = c("#ffffbf", "#fc8d59", "#99d594")
    ) +
    mcoe_theme
}


breakout.cci(cci.mry, "Salinas Union", "LTEL", "Approaching\nPrepared")


### Nested for loops to run for all student groups at districts participating in CCI session ------

for (i in c(
  "Salinas Union",
  "South Monterey County"
  #  "North Monterey County", "Soledad", "Pacific Grove"
)) {
  sg.list <- cci.mry |>
    filter(str_detect(districtname, i)) |>
    select(studentgroup) |>
    unique() |>
    unlist()

  for (j in sg.list) {
    for (k in c("Prepared", "Approaching\nPrepared")) {
      breakout.cci(cci.mry, i, j, k)

      #fmt:skip
      ggsave(
        here(  "output",paste0("CCI breakdown ", i, " ",j, " - ", str_sub(k, 1, 5),  " - ",  Sys.Date(), ".png" ) ),
        width = 12,
        height = 6
      )
    }
  }
}


#### Process for a School Analysis

cci.school <- cci %>%
  filter(
    str_detect(countyname, "Monterey"),
    str_detect(schoolname, "Toro"),
    rtype == "S",
    reportingyear == "2024",
    currdenom >= 10
  ) |>
  select(
    districtname,
    schoolname,
    studentgroup,
    studentgroup.long,
    starts_with("curr")
  ) |>
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
  select(-currdenom_without_prloss:-currdeclined) |>
  mutate(districtname = schoolname)


#

df.filt <- cci.school |>
  filter(str_detect(districtname, "Toro"), studentgroup == "ALL")

ssgg <- df.filt$studentgroup.long[1]

#

df.filt |>
  pivot_longer(
    cols = c(curr_prep, curr_aprep, curr_nprep),
    names_to = "Preparation_Level"
  ) |>
  mutate(
    Preparation_Level = case_match(
      Preparation_Level,
      "curr_prep" ~ "Prepared",
      "curr_aprep" ~ "Approaching\nPrepared",
      "curr_nprep" ~ "Not Prepared"
    )
  ) |>
  pivot_longer(
    cols = c(ends_with("pct")),
    names_to = "areas",
    values_to = "Percent"
  ) |>
  left_join(prep.df) |>
  filter(str_starts(definition, str_sub(Preparation_Level, 1, 5))) |>

  # mutate(Prepared = str_squish(str_to_lower(as.character(Prepared)))) |>
  pie_with_breakout_bar(
    #    df = example_df,
    dist = "Toro",
    studentgroup = ssgg,
    category = Preparation_Level,
    main_value = value,
    subcategory = definition,
    breakout_cat = "Approaching\nPrepared", # <- which wedge to explode and analyze
    breakout_value = Percent, # used for the bars
    palette = c("#ffffbf", "#fc8d59", "#99d594")
  ) +
  mcoe_theme


breakout.cci.school <- function(df, dist, sg = "ALL", prep_level = "Prepared") {
  cci.school <- df %>%
    filter(
      str_detect(countyname, "Monterey"),
      str_detect(schoolname, dist),
      rtype == "S",
      reportingyear == "2024",
      currdenom >= 10
    ) |>
    select(
      districtname,
      schoolname,
      studentgroup,
      studentgroup.long,
      starts_with("curr")
    ) |>
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
    select(-currdenom_without_prloss:-currdeclined) |>
    mutate(districtname = schoolname)

  #

  df.filt <- cci.school |>
    filter(str_detect(districtname, dist), studentgroup == sg)

  ssgg <- df.filt$studentgroup.long[1]

  #

  df.filt |>
    pivot_longer(
      cols = c(curr_prep, curr_aprep, curr_nprep),
      names_to = "Preparation_Level"
    ) |>
    mutate(
      Preparation_Level = case_match(
        Preparation_Level,
        "curr_prep" ~ "Prepared",
        "curr_aprep" ~ "Approaching\nPrepared",
        "curr_nprep" ~ "Not Prepared"
      )
    ) |>
    pivot_longer(
      cols = c(ends_with("pct")),
      names_to = "areas",
      values_to = "Percent"
    ) |>
    left_join(prep.df) |>
    filter(str_starts(definition, str_sub(Preparation_Level, 1, 5))) |>

    # mutate(Prepared = str_squish(str_to_lower(as.character(Prepared)))) |>
    pie_with_breakout_bar(
      #    df = example_df,
      dist = dist,
      studentgroup = ssgg,
      category = Preparation_Level,
      main_value = value,
      subcategory = definition,
      breakout_cat = prep_level, # <- which wedge to explode and analyze
      breakout_value = Percent, # used for the bars
      palette = c("#ffffbf", "#fc8d59", "#99d594")
    ) +
    mcoe_theme
}


breakout.cci.school(cci, "Toro", "SWD", "Approaching\nPrepared")

# Loop for Schools

for (i in c("Mount Toro")) {
  sg.list <- cci |>
    filter(
      str_detect(schoolname, i),
      reportingyear == max(reportingyear),
      !is.na(curr_prep)
    ) |>
    select(studentgroup) |>
    unique() |>
    unlist()

  for (j in sg.list) {
    for (k in c("Prepared", "Approaching\nPrepared")) {
      breakout.cci.school(cci, i, j, k)

      #fmt:skip
      ggsave(
        here(  "output",paste0("CCI breakdown ", i, " ",j, " - ", str_sub(k, 1, 5),  " - ",  Sys.Date(), ".png" ) ),
        width = 12,
        height = 6
      )
    }
  }
}

#### End ------
