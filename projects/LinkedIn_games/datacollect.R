# -----------------------------------------
## Remember to setwd("C:\\Users\\elmoe\\OneDrive - hull.ac.uk\\Github\\Quarto-Website\\PersonalWebsite\\projects\\LinkedIn_games")
## git login if required
## This is ignored by git.
# -----------------------------------------

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
  
  # 6.6) Append a new row if today’s game column is already filled,
  #      otherwise fill the existing last row
  if (nrow(data) == 0 || !is.na(data[[game_col]][nrow(data)])) {
    new_row <- data.frame(
      date   = Sys.Date(),
      CW_T   = NA, CW_Z = NA, CW_P = NA, CW_C = NA,
      PM_T   = NA, PM_Z = NA, PM_P = NA, PM_C = NA,
      stringsAsFactors = FALSE
    )
    new_row[[game_col]] <- value
    data <- rbind(data, new_row)
  } else {
    data[[game_col]][nrow(data)] <- value
  }
  
  write.xlsx(
    data,
    excel_file,
    overwrite  = TRUE,
    dateFormat = "dd-mm-yyyy"
  )
  
  # 6.7) Send confirmation back to user
  bot$sendMessage(chat_id = chatid,
                  text    = paste("Updated", game_col, "with value:", value))
}

# ────────────────────────────────────────────────────────────────────────────
# 7) Read the last‐handled update ID (or default to 0)
if (file.exists(state_file)) {
  last_update_id <- as.integer(readLines(state_file, warn = FALSE))
  if (is.na(last_update_id)) last_update_id <- 0
} else {
  last_update_id <- 0
}

# 8) Fetch new updates exactly once (timeout = 0 → return immediately)
updates <- getUpdates(
  bot     = bot,
  offset  = last_update_id + 1,
  timeout = 0
)

# 9) If no new updates, exit right away
if (length(updates) == 0) {
  message("No new updates. Exiting.")
  quit(status = 0)
}

# 10) Process each new update
new_max_id <- last_update_id
for (upd in updates) {
  tryCatch(
    {
      handle_single_update(bot, upd)
      new_max_id <- max(new_max_id, upd$update_id)
    },
    error = function(e) {
      message("Error processing update ", upd$update_id, ": ", e$message)
    }
  )
}

# 11) Save the updated max update ID back to last_update_id.txt
writeLines(as.character(new_max_id), con = state_file)

message("Processed ", length(updates), " updates; last_update_id = ", new_max_id)
quit(status = 0)