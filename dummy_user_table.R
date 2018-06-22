##------Code to generate dummy user table------##
##------After generating dummy_weekly_joins------##

## Clear Workspace and load required libraries and files
rm(list=ls())

library(data.table)
library(Hmisc)

user_df <- fread('../output/cohort_retention_channel.csv')

#calculate number of weekly churned users
user_df$V1 <- NULL
user_df$remaining_users <- Lag(user_df$cohort_active_users, shift = -1)
user_df$churned_users <- user_df$cohort_active_users - user_df$remaining_users
user_df$churned_users[user_df$churned_users < 0] <- 0
user_df$churned_users[is.na(user_df$churned_users)] <- 0

# Create joining table
cohort_size <- user_df[join_cohort==transaction_week,.(join_cohort,cohort_active_users,channel_name)]
save(cohort_size, file = '../cleaned/cohort_size_channel.bin')
# first_transaction <- list()
# last_transaction <- list()
# channel <- list()

# Creating table for active users with join date and channel. generate transaction date as NA
df_list <- list()
counter <- 1

for(i in 1:length(cohort_size$join_cohort)){
  counter <- counter + 1
  first_transaction <- rep(cohort_size$join_cohort[i], cohort_size$cohort_active_users[i])
  last_transaction <- rep(NA, cohort_size$cohort_active_users[i])
  channel <- rep(cohort_size$channel_name[i], cohort_size$cohort_active_users[i])
  temp_df <- cbind.data.frame(first_transaction, last_transaction, channel)
  df_list[[counter]] <- temp_df
}

activeusertable <- do.call(rbind, df_list)

# rep first transaction, last transaction and channel like previous step but for churned users only
churneduserdf <- user_df[user_df$churned_users != 0]

df_list <- list()
counter <- 1

for(i in 1:length(churneduserdf$join_cohort)){
  counter <- counter + 1
  first_transaction <- rep(churneduserdf$join_cohort[i], churneduserdf$churned_users[i])
  last_transaction <- rep(churneduserdf$transaction_week[i], churneduserdf$churned_users[i])
  channel <- rep(churneduserdf$channel_name[i], churneduserdf$churned_users[i])
  temp_df <- cbind.data.frame(first_transaction, last_transaction, channel)
  df_list[[counter]] <- temp_df
}

churnedusertable <- do.call(rbind, df_list)

# Calculate num active and num churned user for each weekly cohort and channel
x <- as.data.frame(table(churnedusertable$first_transaction, churnedusertable$channel))
y <- as.data.frame(table(activeusertable$first_transaction, activeusertable$channel))

colnames(x) <- c("join_cohort","channel","churned_users")
colnames(y) <- c("join_cohort", "channel", "active_users")

# merge both to get the number of NAs that need be generated for last transaction date
x <- merge(x, y, by = c("join_cohort", "channel"), all=T)
x$nastogen <- x$active_users - x$churned_users
x <- x[x$nastogen > 0, ]

# Generate NAs and rbind to churnedusertable to get a user table with churned as well as active users
df_list <- list()
counter <- 1

for(i in 1:length(x$join_cohort)){
  counter <- counter + 1
  first_transaction <- rep(x$join_cohort[i], x$nastogen[i])
  last_transaction <- rep(NA, x$nastogen[i])
  channel <- rep(x$channel[i], x$nastogen[i])
  temp_df <- cbind.data.frame(first_transaction, last_transaction, channel)
  df_list[[counter]] <- temp_df
}

userswithnatd <- do.call(rbind, df_list)

usertable <- rbind(churnedusertable, userswithnatd)

# Convert NAs to last transaction week to show users as active
usertable$last_transaction[is.na(usertable$last_transaction)] <- "2017-12-31"

# Sort and add rownames and userid
usertable <- usertable[order(usertable$first_transaction, usertable$channel), ]
rownames(usertable) <- seq(1:nrow(usertable))
usertable$user_id <- seq(100301, 100300+nrow(usertable))

# Little clean-up and save
usertable <- data.table(usertable[, c(4,3,1,2)])
usertable$first_transaction <- as.Date(usertable$first_transaction)
usertable$last_transaction <- as.Date(usertable$last_transaction)
usertable$channel <- as.character(usertable$channel)
save(usertable, file = '../cleaned/user_table.bin')

#test_df <- user_df[1, ]
# df_list <- list()
# counter <- 1

#     for(l in 1:length(user_df$channel_name)){
#       counter <- counter + 1
#       first_transaction <- rep(user_df$join_cohort[j], user_df$churned_users[k])
#       last_transaction <- rep(user_df$transaction_week[k], user_df$churned_users[k])
#       channel_name <- rep(user_df$channel_name[l], user_df$churned_users[k])
#       temp_df <- cbind.data.frame(first_transaction, last_transaction, channel_name)
#       df_list[[counter]] <- temp_df
#     }
# }

# 
# for(j in 1:length(test_df$join_cohort)){
#   for(k in 1:length(test_df$transaction_week))
#     for(l in 1:length(test_df$channel_name)){
#       first_transaction <- rep(test_df$join_cohort[j], test_df$churned_users[j])
#       last_transaction <- rep(test_df$transaction_week[k], test_df$churned_users[k])
#       channel_name <- rep(test_df$channel_name[l], test_df$churned_users[l])
#       temp_df <- cbind.data.frame(first_transaction, last_transaction, channel_name)
#     }
# }
