# ==============================================================================
# audit/function/audit_plot.R
#
# Purpose:
#   Standard empirical MIS sensitivity visualization.
#
#   y-axis:
#       Delta beta_k = beta_after,k - beta_0
#
#   bottom x-axis:
#       removals k
#
#   top x-axis:
#       removal fraction (%)
#
#   Horizontal solid line:
#       Delta beta_k = 0
#
#   Horizontal dashed line:
#       Delta beta_k = -beta_0
#       (the change at which the refitted coefficient reaches zero)
#
#   Vertical dotted line:
#       first k at which the exact-refitted coefficient reaches/crosses zero
#
#   Special audit values take priority over nearby regular axis ticks.
#
# Public function:
#   plot_mis_audit()
# ==============================================================================


# ------------------------------------------------------------------------------
# Remove ordinary ticks that are too close to a special audit tick
# ------------------------------------------------------------------------------

.audit_drop_near_special <- function(
    regular,
    special,
    gap_fraction = 0.40
) {
  
  regular <- sort(
    unique(
      regular[
        is.finite(regular)
      ]
    )
  )
  
  if (
    length(regular) < 2L ||
    length(special) == 0L ||
    !is.finite(special)
  ) {
    return(regular)
  }
  
  
  spacing <- stats::median(
    diff(regular)
  )
  
  
  if (
    !is.finite(spacing) ||
    spacing <= 0
  ) {
    return(regular)
  }
  
  
  min_gap <- gap_fraction * spacing
  
  
  regular[
    abs(regular - special) > min_gap
  ]
}


# ------------------------------------------------------------------------------
# Clean axis labels
# ------------------------------------------------------------------------------

.audit_regular_label <- function(x) {
  
  if (
    abs(x - round(x)) <
    1e-10
  ) {
    return(
      as.character(
        as.integer(
          round(x)
        )
      )
    )
  }
  
  
  format(
    x,
    digits = 4,
    trim = TRUE,
    scientific = FALSE
  )
}


# ------------------------------------------------------------------------------
# Main plotting function
# ------------------------------------------------------------------------------

plot_mis_audit <- function(
    audit,
    output_file = NULL,
    title = NULL,       # retained for compatibility; intentionally not plotted
    width = 7,
    height = 5,
    dpi = 300,
    write_tex = TRUE,
    tex_file = NULL,
    tex_graphics_path = NULL
) {
  
  
  # --------------------------------------------------------------------------
  # Requirements
  # --------------------------------------------------------------------------
  
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
  baseline <- audit$baseline
  
  
  # --------------------------------------------------------------------------
  # Baseline
  # --------------------------------------------------------------------------
  
  beta0 <- as.numeric(
    baseline$beta_original[1]
  )
  
  
  n <- if (
    "N" %in% names(baseline)
  ) {
    as.integer(
      baseline$N[1]
    )
  } else {
    as.integer(
      baseline$n[1]
    )
  }
  
  
  if (!is.finite(beta0)) {
    stop(
      "`beta_original` is not finite.",
      call. = FALSE
    )
  }
  
  
  if (
    !is.finite(n) ||
    n <= 0L
  ) {
    stop(
      "Audit sample size is invalid.",
      call. = FALSE
    )
  }
  
  
  # --------------------------------------------------------------------------
  # Resolve path column names
  # --------------------------------------------------------------------------
  
  beta_after <- if (
    "beta_after" %in% names(path)
  ) {
    path$beta_after
  } else {
    path$beta_mis
  }
  
  
  delta_beta <- if (
    "delta_beta" %in% names(path)
  ) {
    path$delta_beta
  } else if (
    "delta_mis" %in% names(path)
  ) {
    path$delta_mis
  } else {
    
    as.numeric(beta_after) -
      beta0
  }
  
  
  valid_refit <- if (
    "valid_refit" %in% names(path)
  ) {
    path$valid_refit
  } else if (
    "valid_mis" %in% names(path)
  ) {
    path$valid_mis
  } else {
    rep(
      TRUE,
      nrow(path)
    )
  }
  
  
  nested <- if (
    "nested" %in% names(path)
  ) {
    path$nested
  } else if (
    "nested_mis" %in% names(path)
  ) {
    path$nested_mis
  } else {
    rep(
      NA,
      nrow(path)
    )
  }
  
  
  # --------------------------------------------------------------------------
  # Plot data
  # --------------------------------------------------------------------------
  
  plot_data <- data.frame(
    
    k = as.integer(
      path$k
    ),
    
    removal_fraction = as.numeric(
      path$removal_fraction
    ),
    
    direction = as.character(
      path$direction
    ),
    
    beta_after = as.numeric(
      beta_after
    ),
    
    delta_beta = as.numeric(
      delta_beta
    ),
    
    valid_refit = valid_refit,
    
    nested = nested,
    
    stringsAsFactors = FALSE
  )
  
  
  plot_data <- plot_data[
    plot_data$valid_refit &
      is.finite(plot_data$k) &
      is.finite(plot_data$beta_after) &
      is.finite(plot_data$delta_beta),
    ,
    drop = FALSE
  ]
  
  
  if (nrow(plot_data) == 0L) {
    stop(
      "No valid exact-refit results are available for plotting.",
      call. = FALSE
    )
  }
  
  
  plot_data$direction <- factor(
    plot_data$direction,
    levels = c(
      "Decrease",
      "Increase"
    )
  )
  
  
  max_k <- max(
    plot_data$k
  )
  
  
  max_pct <- 100 *
    max_k /
    n
  
  
  # --------------------------------------------------------------------------
  # First zero crossing of beta_after
  # --------------------------------------------------------------------------
  
  zero_candidate <- plot_data[
    beta0 *
      plot_data$beta_after <= 0,
    ,
    drop = FALSE
  ]
  
  
  if (nrow(zero_candidate) > 0L) {
    
    zero_candidate <- zero_candidate[
      order(
        zero_candidate$k
      ),
      ,
      drop = FALSE
    ]
    
    
    k_zero <- zero_candidate$k[1]
    
    pct_zero <- 100 *
      k_zero /
      n
    
  } else {
    
    k_zero <- NA_integer_
    
    pct_zero <- NA_real_
  }
  
  
  # --------------------------------------------------------------------------
  # Bottom axis: k
  #
  # Special k_zero takes priority over nearby regular ticks.
  # --------------------------------------------------------------------------
  
  k_regular <- pretty(
    c(
      0,
      max_k
    ),
    n = 6
  )
  
  
  k_regular <- k_regular[
    k_regular >= 0 &
      k_regular <= max_k
  ]
  
  
  if (is.finite(k_zero)) {
    
    k_regular <- .audit_drop_near_special(
      regular = k_regular,
      special = k_zero,
      gap_fraction = 0.40
    )
    
    
    k_breaks <- sort(
      unique(
        c(
          k_regular,
          k_zero
        )
      )
    )
    
  } else {
    
    k_breaks <- k_regular
  }
  
  
  k_labels <- vapply(
    k_breaks,
    function(x) {
      
      if (
        is.finite(k_zero) &&
        abs(x - k_zero) < 1e-10
      ) {
        
        as.character(
          k_zero
        )
        
      } else {
        
        .audit_regular_label(x)
      }
    },
    character(1)
  )
  
  
  # --------------------------------------------------------------------------
  # Top axis: removal fraction (%)
  # --------------------------------------------------------------------------
  
  pct_regular <- pretty(
    c(
      0,
      max_pct
    ),
    n = 5
  )
  
  
  pct_regular <- pct_regular[
    pct_regular >= 0 &
      pct_regular <= max_pct
  ]
  
  
  if (is.finite(pct_zero)) {
    
    pct_regular <- .audit_drop_near_special(
      regular = pct_regular,
      special = pct_zero,
      gap_fraction = 0.55
    )
    
    
    pct_breaks <- sort(
      unique(
        c(
          pct_regular,
          pct_zero
        )
      )
    )
    
  } else {
    
    pct_breaks <- pct_regular
  }
  
  
  pct_labels <- vapply(
    pct_breaks,
    function(x) {
      
      if (
        is.finite(pct_zero) &&
        abs(x - pct_zero) < 1e-10
      ) {
        
        format(
          x,
          digits = 3,
          nsmall = 2,
          trim = TRUE,
          scientific = FALSE
        )
        
      } else {
        
        .audit_regular_label(x)
      }
    },
    character(1)
  )
  
  
  # --------------------------------------------------------------------------
  # y axis
  #
  # Signed Delta beta.
  #
  # beta_after reaches zero when:
  #
  #     Delta beta = -beta_0
  # --------------------------------------------------------------------------
  
  zero_beta_threshold <- -beta0
  
  
  y_min <- min(
    plot_data$delta_beta,
    0,
    zero_beta_threshold,
    na.rm = TRUE
  )
  
  
  y_max <- max(
    plot_data$delta_beta,
    0,
    zero_beta_threshold,
    na.rm = TRUE
  )
  
  
  y_regular <- pretty(
    c(
      y_min,
      y_max
    ),
    n = 6
  )
  
  
  y_regular <- y_regular[
    y_regular >= y_min &
      y_regular <= y_max
  ]
  
  
  # Preserve both substantive reference values:
  #   0       = no coefficient change
  #   -beta_0 = beta_after reaches zero
  
  y_regular <- .audit_drop_near_special(
    regular = y_regular,
    special = zero_beta_threshold,
    gap_fraction = 0.30
  )
  
  
  y_breaks <- sort(
    unique(
      c(
        y_regular,
        0,
        zero_beta_threshold
      )
    )
  )
  
  
  y_labels <- vapply(
    y_breaks,
    function(x) {
      
      if (
        abs(
          x -
          zero_beta_threshold
        ) <
        1e-10
      ) {
        
        format(
          x,
          digits = 4,
          nsmall = 2,
          trim = TRUE,
          scientific = FALSE
        )
        
      } else {
        
        .audit_regular_label(x)
      }
    },
    character(1)
  )
  
  
  # --------------------------------------------------------------------------
  # Plot
  # --------------------------------------------------------------------------
  
  p <- ggplot2::ggplot(
    plot_data,
    ggplot2::aes(
      x = k,
      y = delta_beta,
      colour = direction,
      group = direction
    )
  ) +
    
    
    # Exact-refit MIS paths
    ggplot2::geom_line(
      linewidth = 0.9,
      na.rm = TRUE
    ) +
    
    
    # No change in coefficient
    ggplot2::geom_hline(
      yintercept = 0,
      colour = "grey40",
      linewidth = 0.55
    ) +
    
    
    # beta_after = 0
    ggplot2::geom_hline(
      yintercept = zero_beta_threshold,
      linetype = "dashed",
      colour = "grey45",
      linewidth = 0.6
    ) +
    
    
    # Requested colors
    ggplot2::scale_colour_manual(
      values = c(
        "Decrease" = "#ed7a72",
        "Increase" = "#76d6c6"
      ),
      drop = FALSE
    ) +
    
    
    # Bottom: removals k
    # Top: removal fraction
    ggplot2::scale_x_continuous(
      
      name = "Removals (k)",
      
      limits = c(
        0,
        max_k
      ),
      
      breaks = k_breaks,
      
      labels = k_labels,
      
      sec.axis = ggplot2::sec_axis(
        
        trans = ~ . / n * 100,
        
        name = "Removal fraction (%)",
        
        breaks = pct_breaks,
        
        labels = pct_labels
      ),
      
      expand = ggplot2::expansion(
        mult = c(
          0,
          0.01
        )
      )
    ) +
    
    
    ggplot2::scale_y_continuous(
      
      name = expression(
        Delta * beta[k]
      ),
      
      breaks = y_breaks,
      
      labels = y_labels,
      
      expand = ggplot2::expansion(
        mult = c(
          0.04,
          0.04
        )
      )
    ) +
    
    
    ggplot2::labs(
      colour = "Direction"
    ) +
    
    
    ggplot2::theme_classic(
      base_size = 12
    ) +
    
    
    ggplot2::theme(
      
      # Requested: legend below the figure
      legend.position = "bottom",
      
      legend.direction = "horizontal",
      
      plot.title = ggplot2::element_blank(),
      
      axis.title.x.top =
        ggplot2::element_text(
          margin = ggplot2::margin(
            b = 6
          )
        ),
      
      axis.text.x.top =
        ggplot2::element_text(
          margin = ggplot2::margin(
            b = 3
          )
        )
    )
  
  
  # --------------------------------------------------------------------------
  # Zero-crossing vertical guide
  # --------------------------------------------------------------------------
  
  if (is.finite(k_zero)) {
    
    p <- p +
      
      ggplot2::geom_vline(
        xintercept = k_zero,
        linetype = "dotted",
        colour = "grey30",
        linewidth = 0.7
      )
  }
  
  
  # --------------------------------------------------------------------------
  # Non-nested MIS events
  # --------------------------------------------------------------------------
  
  non_nested <- plot_data[
    !is.na(plot_data$nested) &
      plot_data$nested == FALSE,
    ,
    drop = FALSE
  ]
  
  
  if (nrow(non_nested) > 0L) {
    
    p <- p +
      
      ggplot2::geom_point(
        data = non_nested,
        ggplot2::aes(
          x = k,
          y = delta_beta,
          colour = direction
        ),
        shape = 4,
        size = 1.8,
        stroke = 0.7,
        inherit.aes = FALSE,
        show.legend = FALSE
      )
  }
  
  
  # --------------------------------------------------------------------------
  # Save PDF
  # --------------------------------------------------------------------------
  
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
  
  
  # --------------------------------------------------------------------------
  # Minimal flexible LaTeX wrapper
  # --------------------------------------------------------------------------
  
  if (
    isTRUE(write_tex) &&
    !is.null(output_file)
  ) {
    
    if (is.null(tex_file)) {
      
      tex_file <- paste0(
        tools::file_path_sans_ext(
          output_file
        ),
        ".tex"
      )
    }
    
    
    if (is.null(tex_graphics_path)) {
      
      tex_graphics_path <- basename(
        output_file
      )
    }
    
    
    tex_lines <- c(
      
      "\\begin{figure}",
      
      "  \\centering",
      
      paste0(
        "  \\includegraphics[width=\\linewidth]{",
        tex_graphics_path,
        "}"
      ),
      
      "  % \\caption{...}",
      
      "  % \\label{fig:...}",
      
      "\\end{figure}"
    )
    
    
    writeLines(
      tex_lines,
      tex_file,
      useBytes = TRUE
    )
  }
  
  
  invisible(
    p
  )
}