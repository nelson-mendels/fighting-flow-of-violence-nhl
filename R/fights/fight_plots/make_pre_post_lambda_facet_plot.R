# Combines the pre/post lambda scatter plots for any number of season ranges
# into a single side-by-side figure, one combined figure per floor x view.
#
# Panels are rebuilt (not merely reused) so that the stats box is sized for
# the width each panel actually gets inside the composition. When every
# panel shares the same axis limit, only the leftmost keeps the y axis
# title, tick labels and ticks, so the panels sit closer together and are
# directly comparable.
#
# single_fight / floors / etc. are passed through to
# make_pre_post_lambda_plots() so the panels match the standalone figures.
#
# data_to_facet: list of season-filtered master data frames, in the order
#   the panels should appear, e.g. list(pbp_10_24, pbp_24_26)
#
# Saved to plots/fights/ (not a season-specific subfolder), since a faceted
# figure spans multiple ranges.

make_pre_post_lambda_facet_plot <- function(
    working.dir,
    results_to_facet,
    data_to_facet,
    floors = c(10, 25),
    panel_width = 2,
    plot_height = 2.2,
    title_size = 7,
    file_ext = "jpg"
) {
  
  if (length(results_to_facet) != length(data_to_facet)) {
    stop(
      "results_to_facet and data_to_facet must be the same length: got ",
      length(results_to_facet), " and ", length(data_to_facet), "."
    )
  }
  
  if (length(results_to_facet) < 2) {
    stop("Need at least two ranges to facet.")
  }
  
  n_panels <- length(results_to_facet)
  
  output_dir <- file.path(working.dir, "plots", "fights")
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  
  season_pretty <- function(x) {
    x <- as.character(x)
    ifelse(
      str_detect(x, "^\\d{8}$"),
      paste0(substr(x, 1, 4), "-", substr(x, 7, 8)),
      x
    )
  }
  
  range_label <- function(pbp_used) {
    seasons <- pbp_used %>%
      distinct(season) %>%
      filter(!is.na(season)) %>%
      pull(season) %>%
      as.character() %>%
      sort()
    paste0(season_pretty(seasons[1]), " to ", season_pretty(seasons[length(seasons)]))
  }
  
  panel_labels <- vapply(data_to_facet, range_label, character(1))
  
  for (f in floors) {
    for (view in c("full", "zoom3")) {
      
      key <- paste0("floor_", f, "_", view)
      
      panels <- lapply(results_to_facet, function(res) res[[key]])
      
      if (any(vapply(panels, is.null, logical(1)))) {
        warning(
          "Missing panel for floor = ", f, ", view = ", view,
          "; skipping this combined figure."
        )
        next
      }
      
      # Drop the duplicated y axis only when every panel is on the same
      # scale; otherwise each panel needs its own labelled axis.
      axis_limits <- vapply(panels, function(pn) pn$axis_limit, numeric(1))
      shared_axis <- length(unique(axis_limits)) == 1
      
      panel_plots <- lapply(seq_len(n_panels), function(i) {
        
        p <- panels[[i]]$plot +
          labs(title = panel_labels[i]) +
          theme(plot.title = element_text(size = title_size, hjust = 0.5, color = "grey15"))
        
        if (shared_axis && i > 1) {
          p <- p + theme(
            axis.title.y = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks.y = element_blank()
          )
        }
        
        p
      })
      
      combined <- wrap_plots(panel_plots, ncol = n_panels)
      
      file_out <- file.path(
        output_dir,
        paste0("pre_post_lambda_facet_floor", f, "_", view, ".", file_ext)
      )
      
      ggsave(
        filename = file_out,
        plot = combined,
        width = panel_width * n_panels,
        height = plot_height,
        units = "in",
        dpi = 600,
        bg = "white"
      )
      
      message("Saved: ", file_out)
    }
  }
  
  message("")
  message("In LaTeX (main version, floor 10, full view):")
  message(
    "\\includegraphics[width=0.9\\textwidth]{plots/fights/pre_post_lambda_facet_floor",
    floors[1], "_full.", file_ext, "}"
  )
  
  invisible(NULL)
}