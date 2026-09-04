# 08_session_info.R
# Record the software environment after the full replication completes.

session_file <- file.path(paths$results, paste0(paths$result_prefix, "session-info.txt"))

sink(session_file)
sessionInfo()
sink()

message("Session information written to: ", session_file)
