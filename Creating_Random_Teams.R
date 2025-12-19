library(tidyverse)
library(readxl)
library(car)
library(dplyr)
library(readr)
library(stargazer)
setwd("~/Excel/SAL 213")

fantasy_data <- read_excel("Case_5.xlsx")

#Run Model For ADP and Position

fantasy_clean <- na.omit(fantasy_data)

fantasy_model <- lm(HalfPPR ~ ADP + QB1 + RB1 + RB2 +
                    WR1 + WR2 + WR3 + TE,
                    data = fantasy_clean)

summary(fantasy_model)

vif(fantasy_model)


#Add New Column for ADR

adp_to_adr <- function(adp_value) {
  if (adp_value >= 1 && adp_value <= 14) {
    return(1)
  } else if (adp_value >= 15 && adp_value <= 28) {
    return(2)
  }
  else if (adp_value > 28  && adp_value <= 42) {
    return(3)
  }
  else if (adp_value > 42 && adp_value <= 56) {
    return(4)
  }
  else if (adp_value > 56 && adp_value <= 70) {
    return(5)
  }
  else if (adp_value > 70 && adp_value <= 84) {
    return(6)
  }
  else if (adp_value > 84 && adp_value <= 98) {
    return(7)
  }
  else if (adp_value > 98 && adp_value <= 112) {
    return(8)
  }
  else if (adp_value > 112 && adp_value <= 126) {
    return(9)
  }
  else if (adp_value > 126 && adp_value <= 140) {
    return(10)
  }
  else if (adp_value > 140 && adp_value <= 154) {
    return(11)
  }
  else if (adp_value > 154 && adp_value <= 168) {
    return(12)
  }
  else if (adp_value > 168 && adp_value <= 182) {
    return(13)
  }
  else if (adp_value > 182 && adp_value <= 196) {
    return(14)
  }
  else if (adp_value > 196 && adp_value <= 210) {
    return(15)
  }
  else if (adp_value > 210 && adp_value <= 224) {
    return(16)
  }
  else if (adp_value > 224 && adp_value <= 238) {
    return(17)
  }
  else if (adp_value > 238 && adp_value <= 252) {
    return(18)
  }
  else if (adp_value > 252 && adp_value <= 266) {
    return(19)
  }
  else if (adp_value > 266 && adp_value <= 280) {
    return(20)
  }
  else if (adp_value > 280 && adp_value <= 294) {
    return(21)
  }
  else if (adp_value > 294 && adp_value <= 308) {
    return(22)
  }
  else if (adp_value > 308 && adp_value <= 322) {
    return(23)
  }
  else if (adp_value > 322 && adp_value <= 336) {
    return(24)
  }
  else if (adp_value > 336 && adp_value <= 350) {
    return(25)
  }
  else if (adp_value > 350 && adp_value <= 364) {
    return(26)
  }
  else if (adp_value > 364 && adp_value <= 378) {
    return(27)
  }
  else {
    return(28)
  }
}

fantasy_clean$ADR <- sapply(fantasy_clean$ADP, adp_to_adr)

view(fantasy_clean)

#Make Random Teams

year_2019 <- 2019
year_2018 <- 2018
year_2017 <- 2017
subset_data1 <- fantasy_clean[fantasy_clean$Year == year_2019, ]
subset_data2 <- fantasy_clean[fantasy_clean$Year == year_2018, ]
subset_data3 <- fantasy_clean[fantasy_clean$Year == year_2017, ]

# Initialize an empty data frame to store the selected players and their data
selected_players <- data.frame()

# Initialize an empty vector to store the names of the selected players
selected_player_names <- character()

# Initialize a vector to track used ADR values
used_adr_values <- integer()

# Specify the required positions and the number of players for each position
#### If Needed, change subset_data number
required_positions <- c("QB", "RB", "WR", "TE")
players_per_position <- 2

while (length(selected_player_names) < 28) {
  # Randomly select a player from the subset
  candidate_player <- subset_data2[sample(nrow(subset_data2), 1), ]
  candidate_name <- candidate_player$Player[1]
  
  # Check if the candidate player's name is not already selected
  if (!(candidate_name %in% selected_player_names)) {
    # Check if the candidate's ADR is not already used
    candidate_adr <- candidate_player$ADR[1]
    if (!(candidate_adr %in% used_adr_values)) {
      selected_player_data <- subset_data1[subset_data1$Player == candidate_name, ]
      selected_players <- rbind(selected_players, selected_player_data)
      selected_player_names <- c(selected_player_names, candidate_name)
      used_adr_values <- c(used_adr_values, candidate_adr)
    }
  }
}


view(selected_players)


# Get a list of selected QBs, RBs, WRs, and TEs
selected_qb <- selected_players[selected_players$Pos == "QB" & selected_players$QB1 == 1, ]
selected_rb <- selected_players[selected_players$Pos %in% c("RB") & (selected_players$RB1 == 1 | selected_players$RB2 == 1), ]
selected_wr <- selected_players[selected_players$Pos %in% c("WR") & (selected_players$WR1 == 1 | selected_players$WR2 == 1 | selected_players$WR3 == 1), ]
selected_te <- selected_players[selected_players$Pos %in% c("TE") & selected_players$TE == 1, ]

# Function to check if a team is stacked
is_team_stacked <- function(team) {
  # Check if the team has at least one QB with a 1 in QB1
  has_qb <- any(selected_qb$Tm == team)
  
  # Check if the team has at least one RB1, RB2, WR1, WR2, WR3, or TE with a 1
  has_stacked_positions <- any(
    selected_players$Tm == team &
      (
        (selected_players$Pos %in% c("RB") & (selected_players$RB1 == 1 | selected_players$RB2 == 1)) |
          (selected_players$Pos %in% c("WR", "TE") & (selected_players$WR1 == 1 | selected_players$WR2 == 1 | selected_players$WR3 == 1 | selected_players$TE == 1))
      )
  )
  
  if (has_qb && has_stacked_positions) {
    return(paste("Team", team, "is stacked."))
  }
  
  return(paste("Team", team, "is not stacked."))
}

# Check if the teams of selected QBs, RBs, WRs, and TEs are stacked and print the results
stacked_results <- sapply(unique(c(selected_qb$Tm, selected_rb$Tm, selected_wr$Tm, selected_te$Tm)), is_team_stacked)
cat(stacked_results, sep = "\n")


#Runs 100 Times
# Create a list to store the selected players' data frames and stacked results
# Create an empty list to store the results
all_results <- list()

# Perform the loop 100 times
for (i in 1:100) {
  year_2019 <- 2019
  subset_data1 <- fantasy_clean[fantasy_clean$Year == year_2019, ]
  
  # Initialize an empty data frame to store the selected players and their data
  selected_players <- data.frame()
  
  # Initialize an empty vector to store the names of the selected players
  selected_player_names <- character()
  
  # Initialize a vector to track used ADR values
  used_adr_values <- integer()
  
  # Specify the required positions and the number of players for each position
  required_positions <- c("QB", "RB", "WR", "TE")
  players_per_position <- 2
  
  while (length(selected_player_names) < 28) {
    # Randomly select a player from the subset
    candidate_player <- subset_data1[sample(nrow(subset_data1), 1), ]
    candidate_name <- candidate_player$Player[1]
    
    # Check if the candidate player's name is not already selected
    if (!(candidate_name %in% selected_player_names)) {
      # Check if the candidate's ADR is not already used
      candidate_adr <- candidate_player$ADR[1]
      if (!(candidate_adr %in% used_adr_values)) {
        selected_player_data <- subset_data1[subset_data1$Player == candidate_name, ]
        selected_players <- rbind(selected_players, selected_player_data)
        selected_player_names <- c(selected_player_names, candidate_name)
        used_adr_values <- c(used_adr_values, candidate_adr)
      }
    }
  }
  
  # Append the selected players data to the list
  all_results[[i]] <- selected_players
}

view(all_results[[1]])

# Export all the results, for example, in CSV files
for (i in 1:100) {
  write.csv(all_results[[i]], file = paste0("2019_selected_players_", i, ".csv"))
}


# Define the data for the new columns
RB1_Stack <- rep("RB1_Stack_Value", 100)
RB2_Stack <- rep("RB2_Stack_Value", 100)
WR1_Stack <- rep("WR1_Stack_Value", 100)
WR2_Stack <- rep("WR2_Stack_Value", 100)
WR3_Stack <- rep("WR3_Stack_Value", 100)
TE_Stack <- rep("TE_Stack_Value", 100)

# Loop through the list of data frames and add the new columns
for (i in 1:100) {
  all_results[[i]]$RB1_Stack <- RB1_Stack[i]
  all_results[[i]]$RB2_Stack <- RB2_Stack[i]
  all_results[[i]]$WR1_Stack <- WR1_Stack[i]
  all_results[[i]]$WR2_Stack <- WR2_Stack[i]
  all_results[[i]]$WR3_Stack <- WR3_Stack[i]
  all_results[[i]]$TE_Stack <- TE_Stack[i]
}

for (i in 1:length(all_results)) {
  all_results[[i]]$RB1_Stack <- 0
  all_results[[i]]$RB2_Stack <- 0
  all_results[[i]]$WR1_Stack <- 0
  all_results[[i]]$WR2_Stack <- 0
  all_results[[i]]$WR3_Stack <- 0
  all_results[[i]]$TE_Stack <- 0
  
  for (j in 1:nrow(all_results[[i]])) {
    player_team <- all_results[[i]]$Tm[j]
    has_qb <- any(all_results[[i]]$QB1 == 1 & all_results[[i]]$Tm == player_team)
    
    if (all_results[[i]]$RB1[j] == 1) {
      all_results[[i]]$RB1_Stack[j] <- as.integer(has_qb)
    }
    
    if (all_results[[i]]$RB2[j] == 1) {
      all_results[[i]]$RB2_Stack[j] <- as.integer(has_qb)
    }
    
    if (all_results[[i]]$WR1[j] == 1) {
      all_results[[i]]$WR1_Stack[j] <- as.integer(has_qb)
    }
    
    if (all_results[[i]]$WR2[j] == 1) {
      all_results[[i]]$WR2_Stack[j] <- as.integer(has_qb)
    }
    
    if (all_results[[i]]$WR3[j] == 1) {
      all_results[[i]]$WR3_Stack[j] <- as.integer(has_qb)
    }
    
    if (all_results[[i]]$TE[j] == 1) {
      all_results[[i]]$TE_Stack[j] <- as.integer(has_qb)
    }
  }
}

# Create a list to store the results
best_ball <- list()

# Loop through each data frame in all_results
for (i in 1:100) {
  current_data <- all_results[[i]] # Access the i-th data frame in the list
  
  # Get unique weeks in the current data frame
  unique_weeks <- unique(current_data$Week)
  
  # Create a data frame to store the data for the week with the max scores
  max_scores_df <- data.frame()
  
  # Loop through each unique week
  for (week in unique_weeks) {
    week_data <- subset(current_data, Week == week)
    
    # Find the maximum and second maximum HalfPPR scores for QB, RBs, and WRs
    best_qb <- max(week_data$HalfPPR[week_data$Pos == "QB"])
    best_rb <- max(week_data$HalfPPR[week_data$Pos %in% c("RB", "FB")])
    best_wr <- max(week_data$HalfPPR[week_data$Pos == "WR"])
    # Check if there is valid data for tight ends
    if (any(week_data$Pos == "TE")) {
      best_te <- max(week_data$HalfPPR[week_data$Pos == "TE"])
    } else {
      best_te <- NA
    }
    
    # Find the second-best RB, WR, and the best flex player
    second_best_rb <- max(week_data$HalfPPR[week_data$Pos %in% c("RB", "FB") & week_data$HalfPPR < best_rb])
    second_best_wr <- max(week_data$HalfPPR[week_data$Pos == "WR" & week_data$HalfPPR < best_wr])
    
    # Filter the original rows for the players with the max scores
    max_qb_data <- week_data[week_data$Pos == "QB" & week_data$HalfPPR == best_qb, ]
    max_rb_data <- week_data[week_data$Pos %in% c("RB", "FB") & week_data$HalfPPR == best_rb, ]
    second_best_rb_data <- week_data[week_data$Pos %in% c("RB", "FB") & week_data$HalfPPR == second_best_rb, ]
    max_wr_data <- week_data[week_data$Pos == "WR" & week_data$HalfPPR == best_wr, ]
    second_best_wr_data <- week_data[week_data$Pos == "WR" & week_data$HalfPPR == second_best_wr, ]
    max_te_data <- week_data[week_data$Pos == "TE" & week_data$HalfPPR == best_te, ]
    
    # Find the best flex player who hasn't already appeared in another position
    available_flex_players <- setdiff(week_data$Player, c(max_qb_data$Player, max_rb_data$Player, second_best_rb_data$Player, max_wr_data$Player, second_best_wr_data$Player, max_te_data$Player))
    best_flex <- max(week_data$HalfPPR[week_data$Player %in% available_flex_players & !week_data$Pos %in% c("QB")])
    
    # Filter the original rows for the best flex player
    best_flex_data <- week_data[week_data$Player %in% available_flex_players & week_data$HalfPPR == best_flex, ]
    
    # Append the data for the week with the max scores to the main data frame
    max_scores_df <- rbind(max_scores_df, max_qb_data, max_rb_data, second_best_rb_data,
                           max_wr_data, second_best_wr_data, max_te_data, best_flex_data)
  }
  
  # Store the data frame with the max scores for the current data frame in the list
  best_ball[[i]] <- max_scores_df
}


# Loop through each data frame in the best_ball list and save it as a CSV
for (i in 1:length(best_ball)) {
  filename <- paste0("2019_best_ball_", i, ".csv")  # Define the filename
  write.csv(best_ball[[i]], file = filename, row.names = FALSE)  # Export the data frame to a CSV
}



# Load CSVs
best_ball_2017 <- list()

for (i in 1:100) {
  filename <- paste0("2017_best_ball_", i, ".csv")
  best_ball_data <- read.csv(filename)
  best_ball_2017[[i]] <- best_ball_data
}

best_ball_2018 <- list()

for (i in 1:100) {
  filename <- paste0("2018_best_ball_", i, ".csv")
  best_ball_data <- read.csv(filename)
  best_ball_2018[[i]] <- best_ball_data
}

best_ball_2019 <- list()

for (i in 1:100) {
  filename <- paste0("2019_best_ball_", i, ".csv")
  best_ball_data <- read.csv(filename)
  best_ball_2019[[i]] <- best_ball_data
}


#Make One Table For All Players
combined_best_ball_2017 <- do.call(rbind, best_ball_2017)
combined_best_ball_2018 <- do.call(rbind, best_ball_2018)
combined_best_ball_2019 <- do.call(rbind, best_ball_2019)

total_best_ball <- rbind(combined_best_ball_2017, combined_best_ball_2018, combined_best_ball_2019)

total_best_ball <- na.omit(total_best_ball)


# Specify the file path where you want to save the CSV file
csv_file_path <- "total_best_ball.csv"

# Export the data frame to a CSV file
write.csv(total_best_ball, file = csv_file_path, row.names = FALSE)


fantasy_model <- lm(HalfPPR ~ RB1_Stack + RB2_Stack +
                      WR1_Stack + WR2_Stack + TE_Stack,
                    data = total_best_ball)

summary(fantasy_model)

vif(fantasy_model)

fantasy_model2 <- lm(FullPPR ~ RB1_Stack + RB2_Stack +
                       WR1_Stack + WR2_Stack + TE_Stack,
                     data = total_best_ball)

summary(fantasy_model2)

vif(fantasy_model2)

stargazer(fantasy_model2, type = "html", out = "fullppr_gazer.html")



library(dplyr)

# Calculate the number of full groups and the remaining rows
total_rows <- nrow(total_best_ball)
group_size <- 112
full_groups <- floor(total_rows / group_size)
remaining_rows <- total_rows %% group_size

# Add a Group column to your data frame to identify each row's group
total_best_ball <- total_best_ball %>%
  mutate(Group = rep(1:(full_groups + 1), times = c(rep(group_size, full_groups), remaining_rows)))

# Calculate the sum of HalfPPR for each group
summarized_data <- total_best_ball %>%
  group_by(Group) %>%
  summarize(Sum_HalfPPR = sum(HalfPPR))

# summarized_data now contains the sum of HalfPPR for each group

# Assuming "result" is your summarized data frame
write.csv(summarized_data, "summarized_data.csv", row.names = FALSE)

