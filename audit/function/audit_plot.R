# ==============================================================================
# audit/function/audit_plot.R
#
# Purpose:
#   Standard Figure-3-style visualization:
#
#   Top:
#     |Delta_k| for MIS and greedy paths.
#     Points mark MIS non-nestedness events.
#
#   Bottom:
#     Jaccard overlap between MIS and greedy deletion sets.
#
# Public function:
#   plot_mis_audit()
# ==============================================================================


.audit_open_graphics_device <- function(
    file,
    width,
    height,
    dpi
) {
  
  ext <- tolower(
    tools::file_ext(
      file
    )
  )
  
  if (ext == "pdf") {
    
    grDevices::pdf(
      file,
      width = width,
      height = height
    )
    
  } else if (ext == "png") {
    
    grDevices::png(
      file,
      width = width * dpi,
      height = height * dpi,
      res = dpi
    )
    
  } else {
    
    stop(
      "Output figure must end in .pdf or .png.",
      call. = FALSE
    )
  }
}


plot_mis_audit <- function(
    audit,
    output_file = NULL,
    title = NULL,
    width = 7,
    height = 7,
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
  
  
  # --------------------------------------------------------------------------
  # Long-form influence path
  # --------------------------------------------------------------------------
  
  mis_lines <- data.frame(
    k =
      path$k,
    
    direction =
      path$direction,
    
    method =
      "MIS",
    
    influence =
      path$abs_delta_mis,
    
    stringsAsFactors = FALSE
  )
  
  greedy_ok <- (
    "abs_delta_greedy" %in%
      names(path) &&
      any(
        is.finite(
          path$abs_delta_greedy
        )
      )
  )
  
  if (greedy_ok) {
    
    greedy_lines <- data.frame(
      k =
        path$k,
      
      direction =
        path$direction,
      
      method =
        "Greedy",
      
      influence =
        path$abs_delta_greedy,
      
      stringsAsFactors = FALSE
    )
    
    influence_data <- rbind(
      mis_lines,
      greedy_lines
    )
    
  } else {
    
    influence_data <- mis_lines
  }
  
  
  # --------------------------------------------------------------------------
  # Default title
  # --------------------------------------------------------------------------
  
  if (is.null(title)) {
    
    title <- paste0(
      audit$baseline$study_id,
      " — ",
      audit$baseline$target
    )
  }
  
  
  # --------------------------------------------------------------------------
  # Upper panel
  # --------------------------------------------------------------------------
  
  p1 <- ggplot2::ggplot(
    influence_data,
    ggplot2::aes(
      x = k,
      y = influence,
      colour = direction,
      linetype = method
    )
  ) +
    
    ggplot2::geom_line(
      linewidth = 0.8,
      na.rm = TRUE
    ) +
    
    ggplot2::labs(
      title = title,
      x = NULL,
      y = expression(
        abs(Delta[k])
      ),
      colour = "Direction",
      linetype = "Method"
    ) +
    
    ggplot2::theme_classic() +
    
    ggplot2::theme(
      legend.position = "top"
    )
  
  
  # Mark non-nested MIS events.
  non_nested <- path[
    !is.na(path$nested_mis) &
      path$nested_mis == FALSE &
      is.finite(path$abs_delta_mis),
    ,
    drop = FALSE
  ]
  
  if (nrow(non_nested) > 0L) {
    
    p1 <- p1 +
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
  
  
  # --------------------------------------------------------------------------
  # Lower panel: MIS/greedy overlap
  # --------------------------------------------------------------------------
  
  overlap <- path[
    is.finite(
      path$jaccard_greedy
    ),
    ,
    drop = FALSE
  ]
  
  if (nrow(overlap) > 0L) {
    
    p2 <- ggplot2::ggplot(
      overlap,
      ggplot2::aes(
        x = k,
        y = jaccard_greedy,
        colour = direction
      )
    ) +
      
      ggplot2::geom_line(
        linewidth = 0.8
      ) +
      
      ggplot2::coord_cartesian(
        ylim = c(
          0,
          1
        )
      ) +
      
      ggplot2::labs(
        x = "Removals (k)",
        y = "Jaccard"
      ) +
      
      ggplot2::theme_classic() +
      
      ggplot2::theme(
        legend.position = "none"
      )
    
  } else {
    
    p2 <- ggplot2::ggplot() +
      
      ggplot2::annotate(
        "text",
        x = 0,
        y = 0,
        label = "Greedy comparison not run"
      ) +
      
      ggplot2::theme_void()
  }
  
  
  # --------------------------------------------------------------------------
  # Save combined figure
  # --------------------------------------------------------------------------
  
  if (!is.null(output_file)) {
    
    dir.create(
      dirname(output_file),
      recursive = TRUE,
      showWarnings = FALSE
    )
    
    .audit_open_graphics_device(
      file = output_file,
      width = width,
      height = height,
      dpi = dpi
    )
    
    on.exit(
      grDevices::dev.off(),
      add = TRUE
    )
    
    grid::grid.newpage()
    
    layout <- grid::grid.layout(
      nrow = 2,
      ncol = 1,
      heights = grid::unit(
        c(
          2,
          1
        ),
        "null"
      )
    )
    
    grid::pushViewport(
      grid::viewport(
        layout = layout
      )
    )
    
    print(
      p1,
      vp = grid::viewport(
        layout.pos.row = 1,
        layout.pos.col = 1
      )
    )
    
    print(
      p2,
      vp = grid::viewport(
        layout.pos.row = 2,
        layout.pos.col = 1
      )
    )
  }
  
  
  invisible(
    list(
      influence = p1,
      overlap = p2,
      output_file = output_file
    )
  )
}