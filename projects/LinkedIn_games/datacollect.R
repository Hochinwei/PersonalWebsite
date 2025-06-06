# -----------------------------------------
# Remember to setwd("C:\\Users\\elmoe\\OneDrive - hull.ac.uk\\Github\\Quarto-Website\\PersonalWebsite\\projects\\LinkedIn_games")
# git login if required
# -----------------------------------------
  
# Load required libraries
library(telegram.bot)
library(openxlsx)

# Telegram bot token
bot <- Bot(token = "7600021349:AAEqgOPRDbJEwcw9A6Xva_Wz_3bajjYs8B0")

# Updater to listen for messages
updater <- Updater(bot = bot)

# Define user mapping
user_map <- list(
  "@pmtay" = "PM",
  "@Hochinwei" = "CW"
)

# Excel file path
excel_file <- "C:\\Users\\elmoe\\OneDrive - hull.ac.uk\\Github\\Quarto-Website\\PersonalWebsite\\projects\\LinkedIn_games\\LinkedIn_games_data.xlsx"  # Adjust if needed

# Message handler
handle_message <- function(bot, update) {
  # 1) Extract message text and guard against non-text
  text   <- update$message$text
  chatid <- update$message$chat_id
  
  if (is.null(text) || length(text) == 0) {
    bot$sendMessage(
      chat_id = chatid,
      text    = "I can only process text messages."
    )
    return()
  }
  
  # 2) Identify sender and map to player code
  sender <- update$message$from$username
  if (is.null(sender)) {
    bot$sendMessage(
      chat_id = chatid,
      text    = "I don’t know who you are (no username)."
    )
    return()
  }
  
  player <- user_map[[paste0("@", sender)]]
  if (is.null(player)) {
    bot$sendMessage(
      chat_id = chatid,
      text    = "Unknown sender."
    )
    return()
  }
  
  # 3) Determine game type (now safe to use grepl)
  if (grepl("Tango", text, ignore.case = TRUE)) {
    game_col <- paste0(player, "_T")
  } else if (grepl("Zip", text, ignore.case = TRUE)) {
    game_col <- paste0(player, "_Z")
  } else if (grepl("Pinpoint", text, ignore.case = TRUE)) {
    game_col <- paste0(player, "_P")
  } else if (grepl("Crossclimb", text, ignore.case = TRUE)) {
    game_col <- paste0(player, "_C")
  } else {
    bot$sendMessage(
      chat_id = chatid,
      text    = "Unknown game type."
    )
    return()
  }
  
  # 4) Extract numeric value (either guesses or mm:ss time)
  if (game_col %in% c("CW_P", "PM_P")) {
    guesses <- as.numeric(
      regmatches(text, regexpr("[0-9]+(?= guesses)", text, perl = TRUE))
    )
    if (is.na(guesses)) {
      bot$sendMessage(
        chat_id = chatid,
        text    = "No guesses found."
      )
      return()
    }
    value <- guesses
  } else {
    time_match <- regmatches(text, regexpr("[0-9]+:[0-9]+", text))
    if (length(time_match) == 0 || is.na(time_match)) {
      bot$sendMessage(
        chat_id = chatid,
        text    = "No time found."
      )
      return()
    }
    time_parts <- strsplit(time_match, ":")[[1]]
    minutes    <- as.numeric(time_parts[1])
    seconds    <- as.numeric(time_parts[2])
    value      <- minutes * 60 + seconds
  }
  
  # 5) Load or init data frame, update & write back to Excel
  if (file.exists(excel_file)) {
    data <- read.xlsx(
      excel_file,
      sheet       = 1,
      detectDates = TRUE
    )
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
  
  # 6) Append new row if needed, or fill existing row
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
    dateFormat = "dd-mm-yyyy"   # ← use four 'y' characters
  )
  
  # 7) Confirmation + optional GitHub push
  bot$sendMessage(
    chat_id = chatid,
    text    = paste("Updated", game_col, "with value:", value)
  )
  
  system("git add LinkedIn_games_data.xlsx")
  system("git commit -m \"Update game data\"")
  system("git push")
}

# Add handler
updater <- updater + MessageHandler(handle_message)

# Start polling
updater$start_polling()
