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
  # Extract message text
  text <- update$message$text
  sender <- update$message$from$username
  
  # Identify the player (PM or CW)
  player <- user_map[[paste0("@", sender)]]
  if (is.null(player)) {
    bot$sendMessage(chat_id = update$message$chat_id, text = "Unknown sender.")
    return()
  }
  
  # Determine the game type
  if (grepl("Tango", text, ignore.case = TRUE)) {
    game_col <- paste0(player, "_T")
  } else if (grepl("Zip", text, ignore.case = TRUE)) {
    game_col <- paste0(player, "_Z")
  } else if (grepl("Pinpoint", text, ignore.case = TRUE)) {
    game_col <- paste0(player, "_P")
  } else if (grepl("Crossclimb", text, ignore.case = TRUE)) {
    game_col <- paste0(player, "_C")
  } else {
    bot$sendMessage(chat_id = update$message$chat_id, text = "Unknown game type.")
    return()
  }
  
  # Extract numeric value
  if (game_col %in% c("CW_P", "PM_P")) {
    # Pinpoint → number of guesses
    guesses <- as.numeric(regmatches(text, regexpr("[0-9]+(?= guesses)", text, perl = TRUE)))
    if (is.na(guesses)) {
      bot$sendMessage(chat_id = update$message$chat_id, text = "No guesses found.")
      return()
    }
    value <- guesses
  } else {
    # Other games → extract time and convert to seconds
    time_match <- regmatches(text, regexpr("[0-9]+:[0-9]+", text))
    if (length(time_match) == 0) {
      bot$sendMessage(chat_id = update$message$chat_id, text = "No time found.")
      return()
    }
    time_parts <- strsplit(time_match, ":")[[1]]
    minutes <- as.numeric(time_parts[1])
    seconds <- as.numeric(time_parts[2])
    value <- minutes * 60 + seconds
  }
  
  # Load excel file to make sure dates are read back as Date class
  if (file.exists(excel_file)) {
    data <- read.xlsx(
      excel_file,
      sheet       = 1,
      detectDates = TRUE      # ensure any Excel‐formatted dates come in as R's Date
    )
    
    # Just in case: coerce 'date' column to Date if it wasn't recognized
    if (!inherits(data$date, "Date")) {
      data$date <- as.Date(data$date)
    }
  } else {
    data <- data.frame(
      date = as.Date(character()),
      CW_T = numeric(), CW_Z = numeric(), CW_P = numeric(), CW_C = numeric(),
      PM_T = numeric(), PM_Z = numeric(), PM_P = numeric(), PM_C = numeric(),
      stringsAsFactors = FALSE
    )
  }
  
  # ─────────────────────────────────────────────────────────────────
  # 2) Update the latest row if empty, else create a new row
  # ─────────────────────────────────────────────────────────────────
  if (nrow(data) == 0 || !is.na(data[[game_col]][nrow(data)])) {
    # Add a new row with today's date
    new_row <- data.frame(
      date = Sys.Date(),
      CW_T = NA, CW_Z = NA, CW_P = NA, CW_C = NA,
      PM_T = NA, PM_Z = NA, PM_P = NA, PM_C = NA,
      stringsAsFactors = FALSE
    )
    new_row[[game_col]] <- value
    data <- rbind(data, new_row)
  } else {
    # Fill in the latest row for the given game column
    data[[game_col]][nrow(data)] <- value
  }
  
  # Save back to Excel
  write.xlsx(
    data,
    excel_file,
    overwrite  = TRUE,
    dateFormat = "dd-mm-yyy"   # ← ensures Excel sees this column as Date
  )
  
  # Send confirmation
  bot$sendMessage(chat_id = update$message$chat_id,
                  text = paste("Updated", game_col, "with value:", value))
  
  # Optionally push to GitHub (if Git is set up and this script runs in your repo)
  system("git add LinkedIn_games_data.xlsx")
  system("git commit -m \"Update game data\"")
  system("git push")
}

# Add handler
updater <- updater + MessageHandler(handle_message)

# Start polling
updater$start_polling()
