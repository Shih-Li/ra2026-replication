# ============================================================
# 05_figures.R
# Reproduce Figure 1
# ============================================================

if (!exists("ROOT", inherits = TRUE)) source("code/01_setup.R")


run_figures <- function(root) {
  fig_dir <- ensure_dir(file.path(root, "output", "figures", "main"))
  dfile <- file.path(root, "data", "processed", "main_dataset.rds")
  casen_file <- file.path(root, "data", "source", "cleaned", "Casen_employment_2009to2015.dta")
  require_files(c(dfile, casen_file))
  d <- readRDS(dfile)

  mean_emp <- function(var, gender, treatment, fu = NULL) {
    mask <- d$lb_genero == gender & d$treatment == treatment
    if (!is.null(fu)) mask <- mask & d[[fu]] == 1
    safe_mean(d[[var]][mask])
  }
  plotdat <- dplyr::bind_rows(
    data.frame(gender="Women", treatment="Control", time=1:3,
               rate=c(mean_emp("lb_employ",1,0), mean_emp("employed",1,0,"FirstFU"), mean_emp("SEG2_employed",1,0,"SecondFU"))),
    data.frame(gender="Women", treatment="Treatment", time=1:3,
               rate=c(mean_emp("lb_employ",1,1), mean_emp("employed",1,1,"FirstFU"), mean_emp("SEG2_employed",1,1,"SecondFU"))),
    data.frame(gender="Men", treatment="Control", time=1:3,
               rate=c(mean_emp("lb_employ",0,0), mean_emp("employed",0,0,"FirstFU"), mean_emp("SEG2_employed",0,0,"SecondFU"))),
    data.frame(gender="Men", treatment="Treatment", time=1:3,
               rate=c(mean_emp("lb_employ",0,1), mean_emp("employed",0,1,"FirstFU"), mean_emp("SEG2_employed",0,1,"SecondFU")))
  )
  utils::write.csv(plotdat, file.path(fig_dir,"Figure1A_data.csv"), row.names=FALSE)

  draw_exp_panel <- function(gender, ylim, title) {
    z <- plotdat[plotdat$gender==gender,]
    zt <- z[z$treatment=="Treatment",]; zc <- z[z$treatment=="Control",]
    plot(zt$time, zt$rate, type="l", lwd=2, ylim=ylim, xlim=c(.9,3.3), xaxt="n",
         xlab="", ylab="", main=title)
    axis(1, at=1:3, labels=c("Baseline","First Follow-up","Second Follow-up"))
    lines(zc$time, zc$rate, lwd=2, lty=2)
    legend("bottomright", legend=c("Treatment","Control"), lty=c(1,2), lwd=2, bty="n")
  }

  grDevices::pdf(file.path(fig_dir,"Figure1A_1.pdf"), width=7, height=4.5)
  draw_exp_panel("Men", c(.85,1), "Panel A: Employment Rates in Experiment, Men")
  grDevices::dev.off()
  grDevices::pdf(file.path(fig_dir,"Figure1A_2.pdf"), width=7, height=4.5)
  draw_exp_panel("Women", c(.60,.80), "Employment Rates in Experiment, Women")
  grDevices::dev.off()
  grDevices::pdf(file.path(fig_dir,"Figure1A.pdf"), width=7, height=8)
  old <- par(mfrow=c(2,1), mar=c(4,4,3,1))
  draw_exp_panel("Men", c(.85,1), "Panel A: Employment Rates in Experiment, Men")
  draw_exp_panel("Women", c(.60,.80), "Employment Rates in Experiment, Women")
  par(old)
  grDevices::dev.off()

  casen <- read_stata(casen_file)
  require_cols(casen, c("Year","Women","Men"), "CASEN employment data")
  casen$Women <- casen$Women/100
  casen$Men <- casen$Men/100
  utils::write.csv(casen[,c("Year","Women","Men")], file.path(fig_dir,"Figure1B_data.csv"), row.names=FALSE)

  grDevices::pdf(file.path(fig_dir,"Figure1B.pdf"), width=8, height=4)
  old <- par(mar=c(5,4,4,4)+.1)
  plot(casen$Year, casen$Women, type="l", lwd=2, ylim=c(.35,.45), xlab="Year", ylab="",
       main="Panel B: Employment Rates in CASEN Survey, by gender")
  axis(2, at=seq(.35,.45,.02), labels=sprintf("%.2f",seq(.35,.45,.02)))
  par(new=TRUE)
  plot(casen$Year, casen$Men, type="l", lwd=2, lty=2, axes=FALSE, xlab="", ylab="", ylim=c(.62,.72))
  axis(4, at=seq(.62,.72,.02), labels=sprintf("%.2f",seq(.62,.72,.02)))
  legend("bottomright", legend=c("Women (left-axis)","Men (right-axis)"), lty=c(1,2), lwd=2, bty="n")
  par(old)
  grDevices::dev.off()

  message("Figure 1 outputs written to output/figures/main.")
  invisible(plotdat)
}


run_figures(ROOT)
