# Convenience runner for the fixed/complete replication.
# All scripts remain flat under code/.

rm(list=ls())
source(file.path(if (dir.exists(file.path(getwd(), "code"))) getwd() else dirname(getwd()), "code", "analysis_hhattrit_fixed.R"), local = FALSE)
