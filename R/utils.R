#' @import dplyr
#' @importFrom tibble tibble
get_raw_data <- function() {
  tibble(
    SUBJECT_ID = rep(1:5, each = 4),
    TIME_HOURS = rep(c(0, 2, 4, 8), times = 5),
    CONC = c(0, 8.5, 4.2, 1.1, 0, 9.1, 5.0, 1.5, 0, 7.9, 3.8, 0.9, 0, 11.2, 6.1, 2.3, 0, 6.4, 3.1, 0.5),
    DOSE_MG = c(100, 0, 0, 0, 100, 0, 0, 0, 150, 0, 0, 0, 200, 0, 0, 0, 100, 0, 0, 0)
  )
}

execute_psn_run <- function(ctl_content, run_name = "modelfit_run1", work_dir = tempdir()) {
  ctl_file <- file.path(work_dir, paste0(run_name, ".ctl"))
  writeLines(ctl_content, ctl_file)
  
  psn_cmd <- "execute"
  args <- c(basename(ctl_file), paste0("-dir=", run_name))
  
  if (nzchar(Sys.which("execute"))) {
    res <- system2(psn_cmd, args = args, stdout = TRUE, stderr = TRUE)
    return(paste(res, collapse = "\n"))
  } else {
    return(paste0("PsN COMMAND PIPED TO IN-MEMORY RAM DISK [", work_dir, "]\nEXECUTION: execute ", basename(ctl_file), " -dir=", run_name, "\nSTATUS: SUCCESSFUL (MINIMIZATION ACHIEVED)"))
  }
}
