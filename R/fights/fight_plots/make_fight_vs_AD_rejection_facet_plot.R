# Combines the fight-vs-AD-rejection bar charts for any number of season
# ranges into a single side-by-side figure, one combined figure per
# event-count cutoff.
#
# Panels share a common y axis: the y limit is the maximum needed across all
# panels, and only the leftmost panel carries the y axis title, tick labels
# and ticks. This keeps the panels directly comparable and removes the
# duplicated axis furniture between them.
#
# results_to_facet: list of return values from
#   make_fight_vs_AD_rejection_plots(), in the order the panels should appear
# data_to_facet: list of the matching season-filtered master data frames,
#   same order and length, used only for the per-panel range labels
#
# Saved to plots/fights/ (not a season-specific subfolder), since a faceted
# figure spans multiple ranges.

make_fight_vs_AD_rejection_facet_plot <- function(
    working.dir,
    results_to_facet,
    data_to_facet,
    cutoffs = c(0, 10, 20, 30, 40, 50, 60, 70),
    panel_width = 4,
    plot_height = 3.2,
    title_size = 9,
    file_ext = "png"
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
  
  for (cutoff in cutoffs) {
    
    key <- as.character(cutoff)
    
    panels <- lapply(results_to_facet, function(res) res$plots[[key]])
    
    if (any(vapply(panels, is.null, logical(1)))) {
      warning("Missing panel for cutoff = ", cutoff, "; skipping this combined figure.")
      next
    }
    
    # Common y limit across panels: the largest rejection rate at this
    # cutoff in any range, plus the same headroom the single-panel version
    # leaves for its two-line bar labels.
    max_rate <- max(vapply(
      results_to_facet,
      function(res) {
        max(res$summary_by_cutoff$reject_rate[res$summary_by_cutoff$cutoff == cutoff])
      },
      numeric(1)
    ))
    
    y_upper <- max_rate * 1.12 + 0.10
    
    panel_plots <- lapply(seq_len(n_panels), function(i) {
      
      p <- panels[[i]] +
        labs(title = panel_labels[i]) +
        theme(plot.title = element_text(size = title_size, hjust = 0.5, color = "grey15"))
      
      # Shared y scale on every panel
      p <- suppressMessages(
        p + scale_y_continuous(
          labels = percent_format(accuracy = 1),
          breaks = seq(0, 1, by = 0.25),
          limits = c(0, y_upper),
          expand = expansion(mult = c(0, 0.02))
        )
      )
      
      # Only the leftmost panel keeps the y axis furniture
      if (i > 1) {
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
      paste0("fight_vs_AD_rejection_facet_cutoff_", cutoff, ".", file_ext)
    )
    
    ggsave(
      filename = file_out,
      plot = combined,
      width = panel_width * n_panels,
      height = plot_height,
      dpi = 300,
      bg = "white"
    )
    
    message("Saved: ", file_out)
  }
  
  message("")
  message("In LaTeX (cutoff 0):")
  message(
    "\\includegraphics[width=0.95\\textwidth]{plots/fights/fight_vs_AD_rejection_facet_cutoff_0.",
    file_ext, "}"
  )
  
  invisible(NULL)
}
