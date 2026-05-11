# Primary Shiny launcher for the project.
# Source this file from RStudio to start the app on http://127.0.0.1:3838.

# Locate the folder containing this launcher so runApp() does not depend on
# whatever working directory RStudio currently has selected.
get_launcher_directory <- function() {
  source_files <- vapply(sys.frames(), function(frame) {
    # frame$ofile is set when this script is run through source().
    if (is.null(frame$ofile)) {
      NA_character_
    } else {
      frame$ofile
    }
  }, character(1))

  source_files <- source_files[!is.na(source_files)]

  # Prefer the sourced file path because it points at the project folder.
  if (length(source_files) > 0) {
    return(dirname(normalizePath(tail(source_files, 1), winslash = "/", mustWork = TRUE)))
  }

  # Fall back to the working directory when the script is run another way.
  working_directory <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)

  # Accept the working directory only if it actually contains app.R.
  if (file.exists(file.path(working_directory, "app.R"))) {
    return(working_directory)
  }

  # Stop with a clear message instead of letting Shiny throw a vague app-dir error.
  stop("Could not find app.R. Open the project folder or source this launcher file directly.")
}

app_directory <- get_launcher_directory()

# Final guard before launching the app.
if (!file.exists(file.path(app_directory, "app.R"))) {
  stop("The launcher directory does not contain app.R: ", app_directory)
}

# Start the app and ask Shiny/RStudio to open the browser.
shiny::runApp(app_directory, host = "127.0.0.1", port = 3838, launch.browser = TRUE)
