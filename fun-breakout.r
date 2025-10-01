library(tidyverse)
library(patchwork)
library(scales)

pie_with_breakout_bar <- function(
  df,
  dist,
  studentgroup,
  category, # e.g., Section
  main_value, # e.g., Count or Amount_for_pie
  subcategory, # e.g., Sub
  breakout_cat, # e.g., "Math"
  breakout_value = NULL, # e.g., Minutes_for_breakout; defaults to main_value
  donut_inner = 0.55,
  explode_bump = 0.12, # now interpreted as TRANSLATION distance
  palette = NULL,
  pct_of = c("slice", "whole")
) {
  pct_of <- match.arg(pct_of)
  category <- rlang::ensym(category)
  mainval <- rlang::ensym(main_value)
  subcat <- rlang::ensym(subcategory)
  bvalue_q <- rlang::enquo(breakout_value)
  use_bval <- !rlang::quo_is_null(bvalue_q)

  label_r <- (donut_inner + 1) / 2

  # -------- helper to build a ring slice polygon (global center at 0,0) --------
  make_ring_slice <- function(start, end, r0, r1, dx = 0, dy = 0, n = 120) {
    ang_out <- seq(start, end, length.out = n)
    ang_in <- seq(end, start, length.out = n)
    tibble(
      x = c(r1 * cos(ang_out), r0 * cos(ang_in)) + dx,
      y = c(r1 * sin(ang_out), r0 * sin(ang_in)) + dy
    )
  }

  # ---- main donut data (build from df; keep a single center and translate coords) ----
  main <- df |>
    select(!!category, !!mainval) |>
    distinct() |> # avoid double-counting from any later pivots
    group_by(!!category) |>
    summarise(.value = sum(!!mainval, na.rm = TRUE), .groups = "drop") |>
    mutate(
      frac = .value / sum(.value),
      end = 2 * pi * cumsum(frac),
      start = lag(end, default = 0),
      mid = (start + end) / 2,
      is_breakout = as.character(!!category) == breakout_cat,
      dx = if_else(is_breakout, explode_bump * cos(mid), 0),
      dy = if_else(is_breakout, explode_bump * sin(mid), 0)
    )

  # polygons for each slice (global center 0,0; only coords are shifted for the breakout)
  polys <- main |>
    mutate(slice_id = row_number(), n_pts = pmax(12L, ceiling(200 * frac))) |>
    rowwise() |>
    mutate(
      poly = list(make_ring_slice(
        start,
        end,
        donut_inner,
        1,
        dx,
        dy,
        n = n_pts
      ))
    ) |>
    ungroup() |>
    select(slice_id, !!category, poly) |>
    unnest(poly)

  # label positions (apply same translation as the polygon)
  labels_df <- main |>
    transmute(
      !!category,
      lx = dx + label_r * cos(mid),
      ly = dy + label_r * sin(mid),
      lab = paste0(!!category, "\n", percent(frac, 0.1))
    )

  # colors
  pal <- if (is.null(palette)) {
    p <- scales::hue_pal()(n_distinct(pull(main, !!category)))
    names(p) <- main |> pull(!!category) |> as.character()
    p
  } else {
    palette
  }

  cats <- df |>
    select({{ category }}) |>
    distinct() |>
    pull({{ category }}) |>
    as.character() |>
    sort()

  breakout_color <- pal[match(breakout_cat, cats)]

  p_main <- ggplot(polys) +
    geom_polygon(
      aes(x, y, group = slice_id, fill = !!category),
      color = "white",
      linewidth = 0.7
    ) +
    geom_text(
      data = labels_df,
      aes(lx, ly, label = lab),
      lineheight = 0.95,
      size = 3.2
    ) +
    coord_fixed() +
    scale_fill_manual(values = pal) +
    labs(
      title = paste0("CCI distribution\nin ", dist, "\nfor ", studentgroup),
      subtitle = paste("Category:", breakout_cat),
      x = NULL,
      y = NULL
    ) +
    theme_void(base_size = 12) +
    theme(
      legend.position = "none",
      plot.title = element_text(hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5)
    )

  # ---- breakout bars (unchanged logic) ----
  sub_raw <- df |>
    filter(as.character(!!category) == breakout_cat)

  if (use_bval) {
    sub <- sub_raw |>
      group_by(!!subcat) |>
      summarise(.value = sum(!!bvalue_q, na.rm = TRUE), .groups = "drop")

    print(sub)

    xlab <- rlang::as_label(bvalue_q)
  } else {
    sub <- sub_raw |>
      group_by(!!subcat) |>
      summarise(.value = sum(!!mainval, na.rm = TRUE), .groups = "drop")
    xlab <- rlang::as_name(mainval)
  }

  denom <- if (pct_of == "slice") {
    sum(sub$.value, na.rm = TRUE)
  } else {
    sum(df[[rlang::as_name(mainval)]], na.rm = TRUE)
  }

  sub <- sub |>
    mutate(
      #frac = .value / denom,
      #      lbl = percent(frac, 0.1)
      lbl = percent(.value / 100, 0.1)
    )

  p_breakout <- ggplot(
    sub,
    aes(x = .value, y = forcats::fct_reorder(!!subcat, .value))
  ) +
    geom_col(fill = breakout_color) +
    geom_text(aes(label = lbl), hjust = -0.05, size = 4) +
    scale_x_continuous(
      expand = expansion(mult = c(0, .15)),
      labels = label_comma()
    ) +
    labs(
      title = paste0("Breakout of ‘", breakout_cat, "’"),
      x = xlab,
      y = NULL,
      subtitle = paste("% of", pct_of)
    ) +
    theme_minimal(base_size = 12) +
    theme(
      panel.grid.minor = element_blank(),
      plot.title = element_text(hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5),
    )

  p_main + p_breakout + patchwork::plot_layout(widths = c(1.1, 1))
}
