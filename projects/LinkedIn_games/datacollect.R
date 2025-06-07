# 1) Load required libraries
library(telegram.bot)
library(openxlsx)

# 2) Read the bot token from environment
bot_token <- Sys.getenv("TELEGRAM_BOT_TOKEN")
if (!nzchar(bot_token)) {
  stop("ERROR: TELEGRAM_BOT_TOKEN is not set. ",
       "Please export TELEGRAM_BOT_TOKEN before running this script.")
}

# 3) Instantiate the bot (no continuous polling here)
bot <- Bot(token = bot_token)

# 4) Define user mapping (unchanged)
user_map <- list(
  "@pmtay"     = "PM",
  "@Hochinwei" = "CW"
)

# 5) Paths (relative to working directory, which must be LinkedIn_games)
excel_file <- "LinkedIn_games_data.xlsx"
state_file <- "last_update_id.txt"


# ────────────────────────────────────────────────────────────────────────────
# 6) Helper: process a single Telegram update (similar to handle_message)
handle_single_update <- function(bot, update) {
  text   <- update$message$text
  chatid <- update$message$chat_id
  
  # 6.1) If non-text, reply and skip
  if (is.null(text) || length(text) == 0) {
    bot$sendMessage(chat_id = chatid,
                    text    = "I can only process text messages.")
    return(invisible(NULL))
  }
  
  # 6.2) Identify sender → player
  sender <- update$message$from$username
  if (is.null(sender)) {
    bot$sendMessage(chat_id = chatid,
                    text    = "I don’t know who you are (no username).")
    return(invisible(NULL))
  }
  player <- user_map[[paste0("@", sender)]]
  if (is.null(player)) {
    bot$sendMessage(chat_id = chatid,
                    text    = "Unknown sender.")
    return(invisible(NULL))
  }
  
  # 6.3) Determine game type
  if (grepl("Tango", text, ignore.case = TRUE)) {
    game_col <- paste0(player, "_T")
  } else if (grepl("Zip", text, ignore.case = TRUE)) {
    game_col <- paste0(player, "_Z")
  } else if (grepl("Pinpoint", text, ignore.case = TRUE)) {
    game_col <- paste0(player, "_P")
  } else if (grepl("Crossclimb", text, ignore.case = TRUE)) {
    game_col <- paste0(player, "_C")
  } else {
    bot$sendMessage(chat_id = chatid,
                    text    = "Unknown game type.")
    return(invisible(NULL))
  }
  
  # 6.4) Extract numeric value (guesses or mm:ss → seconds)
  if (game_col %in% c("CW_P", "PM_P")) {
    guesses <- as.numeric(
      regmatches(text,
                 regexpr("[0-9]+(?= guesses)", text, perl = TRUE))
    )
    if (is.na(guesses)) {
      bot$sendMessage(chat_id = chatid,
                      text    = "No guesses found.")
      return(invisible(NULL))
    }
    value <- guesses
  } else {
    time_match <- regmatches(text, regexpr("[0-9]+:[0-9]+", text))
    if (length(time_match) == 0 || is.na(time_match)) {
      bot$sendMessage(chat_id = chatid,
                      text    = "No time found.")
      return(invisible(NULL))
    }
    parts  <- strsplit(time_match, ":")[[1]]
    minutes <- as.numeric(parts[1])
    seconds <- as.numeric(parts[2])
    value   <- minutes * 60 + seconds
  }
  
  # 6.5) Load or initialize the data frame, update & write back to Excel
  if (file.exists(excel_file)) {
    data <- read.xlsx(excel_file, sheet = 1, detectDates = TRUE)
    if (!inherits(data$date, "Date")) {
      data$date <- as.Date(data$date)
    }
  } else {
    data <- data.frame(
      date   = as.Date(character()),
      CW_T   = numeric(), CW_Z = numeric(),
      CW_P   = numeric(), CW_C = numeric(),
      PM_T   = numeric(), PM_Z = numeric(),
      PM_P   = numeric(), PM_C = numeric(),
      stringsAsFactors = FALSE
    )
  }
  
  # 6.6) If the last row’s cell in game_col is already non-NA, append
  # with date = (last row’s date + 1 day). Otherwise fill the last row.
  last_row <- nrow(data)
  
  if (last_row == 0) {
    # No rows yet → first row is today
    new_date <- Sys.Date()
    append_row <- TRUE
    
  } else if (!is.na(data[[game_col]][last_row])) {
    # Last row already has a value in this column → append next day
    new_date   <- data$date[last_row] + 1
    append_row <- TRUE
    
  } else {
    # Last row’s cell is still NA → fill it
    append_row <- FALSE
  }
  
  if (append_row) {
    new_row <- data.frame(
      date   = new_date,
      CW_T   = NA, CW_Z = NA, CW_P = NA, CW_C = NA,
      PM_T   = NA, PM_Z = NA, PM_P = NA, PM_C = NA,
      stringsAsFactors = FALSE
    )
    new_row[[game_col]] <- value
    data <- rbind(data, new_row)
    
  } else {
    # fill the empty cell in the last row
    data[[game_col]][last_row] <- value
  }

}
# ────────────────────────────────────────────────────────────────────────────
# 7) Ensure last_update_id.txt exists (initialize to zero if needed)
if (!file.exists(state_file)) {
  writeLines("0", con = state_file)
}

# 8) Read the last‐handled update ID
last_update_id <- as.integer(readLines(state_file, warn = FALSE))
if (is.na(last_update_id)) {
  last_update_id <- 0
}


# —–– DEBUGGING BLOCK —––––––––––––––––––––––––––––––––––––––––––––––––
# 1) Ensure no webhook is intercepting your updates
bot$removeWebhook()
Sys.sleep(1)

# 2) Show what Telegram thinks your webhook status is
wh <- bot$getWebhookInfo()
message("Webhook URL is: ", if (nzchar(wh$url)) wh$url else "(none)")

# 3) Inspect your saved offset
message("last_update_id read from file: ", last_update_id)
message("Will fetch with offset: ", last_update_id + 1)

# 4) Do a test fetch of everything in the backlog
test_updates <- bot$getUpdates(offset = 0, timeout = 0)
message("Backlog update_ids: ",
        paste(sapply(test_updates, function(u) u$update_id), collapse = ", "))
# —–– END DEBUGGING BLOCK —–––––––––––––––––––––––––––––––––––––––––––


# 9) Fetch new updates (one‐shot)
updates <- bot$getUpdates(
  offset  = last_update_id + 1,
  timeout = 0
)

# 10) Read or initialize the main data frame once
if (file.exists(excel_file)) {
  data <- read.xlsx(excel_file, sheet = 1, detectDates = TRUE)
  # ensure the date column is Date class
  if (!inherits(data$date, "Date")) {
    data$date <- as.Date(data$date)
  }
} else {
  data <- data.frame(
    date   = as.Date(character()),
    CW_T   = numeric(), CW_Z = numeric(),
    CW_P   = numeric(), CW_C = numeric(),
    PM_T   = numeric(), PM_Z = numeric(),
    PM_P   = numeric(), PM_C = numeric(),
    stringsAsFactors = FALSE
  )
}

# 11) Loop over new updates, updating 'data' in memory
new_max_id <- last_update_id
for (upd in updates) {
  tryCatch({
    handle_single_update(bot, upd)
    new_max_id <- max(new_max_id, upd$update_id)
  }, error = function(e) {
    message("Error processing update ", upd$update_id, ": ", e$message)
  })
}

# 12) Persist the updated data frame back to Excel
write.xlsx(data, file = excel_file)

# 13) Save the new maximum update ID back to state_file
writeLines(as.character(new_max_id), con = state_file)

message("Processed ", length(updates), " updates; last_update_id = ", new_max_id)
quit(status = 0)