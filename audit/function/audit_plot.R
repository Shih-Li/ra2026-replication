# ==============================================================================
# audit/function/audit_plot.R
#
# Purpose:
#   Standard empirical MIS sensitivity visualization.
#
#   Shows |Delta_k| for exact MIS paths in both directions.
#   Points mark MIS non-nestedness events.
#
# Public function:
#   plot_mis_audit()
# ==============================================================================

plot_mis_audit <- function(
    audit,
    output_file = NULL,
    title = NULL,
    width = 7,
    height = 5,
    dpi = 300
) {
  
  if (
    !requireNamespace(
      "ggplot2",
      quietly = TRUE
    )
  ) {
    stop(
      "Install ggplot2 before plotting.",
      call. = FALSE
    )
  }
  
  if (
    !inherits(
      audit,
      "mis_audit"
    )
  ) {
    stop(
      "`audit` must come from run_mis_audit().",
      call. = FALSE
    )
  }
  
  path <- audit$path
  
  if (is.null(title)) {
    
    title <- paste0(
      audit$baseline$study_id,
      " — ",
      audit$baseline$target
    )
  }
  
  p <- ggplot2::ggplot(
    path,
    ggplot2::aes(
      x = k,
      y = abs_delta_mis,
      colour = direction
    )
  ) +
    
    ggplot2::geom_line(
      linewidth = 0.8,
      na.rm = TRUE
    ) +
    
    ggplot2::labs(
      title = title,
      x = "Removals (k)",
      y = expression(abs(Delta[k])),
      colour = "Direction"
    ) +
    
    ggplot2::theme_classic() +
    
    ggplot2::theme(
      legend.position = "top"
    )
  
  non_nested <- path[
    !is.na(path$nested_mis) &
      path$nested_mis == FALSE &
      is.finite(path$abs_delta_mis),
    ,
    drop = FALSE
  ]
  
  if (nrow(non_nested) > 0L) {
    
    p <- p +
      ggplot2::geom_point(
        data = non_nested,
        ggplot2::aes(
          x = k,
          y = abs_delta_mis,
          colour = direction
        ),
        inherit.aes = FALSE,
        size = 2
      )
  }
  
  if (!is.null(output_file)) {
    
    dir.create(
      dirname(output_file),
      recursive = TRUE,
      showWarnings = FALSE
    )
    
    ggplot2::ggsave(
      filename = output_file,
      plot = p,
      width = width,
      height = height,
      dpi = dpi
    )
  }
  
  invisible(p)
}