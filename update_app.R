library(here)

# Style code --------------------------------------------------------------

styler::style_file(here('app.R'))

# Write update time in app ------------------------------------------------

app_lines <- readLines(here('app.R'))

# Get the current date time
now_time <- lubridate::now(tzone = "America/Denver")

now_string <- format(now_time, "%Y-%m-%d %H:%M:%S %Z")

# Find the line containing "last_updated"
update_line <- grep("last_updated <-", app_lines)

# Modify the line to include the current date time
app_lines[update_line] <- sprintf('last_updated <- "%s"', now_string)

# Save the modified contents to the original file
writeLines(app_lines, 'app.R')

# Upload to Shiny Server --------------------------------------------------

rsconnect::deployApp()