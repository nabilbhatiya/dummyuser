##------Code to make the dummy_user_table look real------##
##------After running dummy_weekly_joins and dummy_user_table------##

## Clear Workspace and load required libraries and files
rm(list=ls())

library(data.table)

load('../cleaned/user_table.bin')
load('../cleaned/user_table_CA.bin')
# This file is created manually from reference user table
rpttogen <- fread('../rawdata/rptmeansdpop.csv')

usertable$transacting_days <- usertable$last_transaction - usertable$first_transaction

# Create rpt using rnorm giving mean and sd from rpttogen file
df_list <- list()
counter <- 1

for(i in 1:length(rpttogen$channel)){
  rpt <- rnorm(rpttogen$population[i], mean = rpttogen$meanrpt[i], sd = rpttogen$sdrpt[i])
  temp_df <- cbind.data.frame(rpt)
  temp_df$channel <- rpttogen$channel[i]
  df_list[[counter]] <- temp_df
  counter <- counter + 1
}

rpt <- do.call(rbind, df_list)
rpt <- rpt[order(rpt$channel), ]

# rpt should never be 0 or negative
# Setting minimum threshold on rpt as pre-decided
rpt$rpt[rpt$channel == "Referral" & rpt$rpt < 5] <- 5.0
rpt$rpt[rpt$channel == "Organic" & rpt$rpt < 4] <- 4.0
rpt$rpt[rpt$channel == "Paid Search" & rpt$rpt < 3] <- 3.0
rpt$rpt[rpt$channel == "Pinterest" & rpt$rpt < 2] <- 2.0
rpt$rpt[rpt$channel == "Facebook" & rpt$rpt < 1] <- 1.0

usertable <- usertable[order(usertable$channel)]

usertable <- cbind(usertable, rpt[, "rpt"])
setnames(usertable, "V2", "rpt")

# for(i in 1:length(usertable$last_transaction)){
#   if(usertable$last_transaction[i] > max(usertable$last_transaction) - 10*7){
#     usertable$churn_flag[i] <- FALSE
#   }else{
#     usertable$churn_flag[i] <- TRUE
#   }
# }

# users older than 10 weeks are churned

# Setting 10 weeks as churn threshold
usertable$churn_flag[usertable$last_transaction >= (max(usertable$last_transaction) - 10*7)] <- FALSE
usertable$churn_flag[usertable$last_transaction < (max(usertable$last_transaction) - 10*7)] <- TRUE


# join_time_period = no. of weeks since first transaction recorded in database
# revenue can be calculated as rpt * number of transacting weeks
# last_transacting_time_period = no. # of weeks since start of time. if your transaction 
# time_period_to_churn = last_transacting_time_period - join_time_period + churn threshold (only for churned users)
usertable$transacting_days <- NULL
usertable$join_time_period <- (as.numeric(usertable$first_transaction - min(usertable$first_transaction)) / 7) + 1
usertable$last_transacting_time_period <- (as.numeric(usertable$last_transaction - min(usertable$first_transaction)) / 7) + 1
usertable$transacting_time_period <- (as.numeric(usertable$last_transaction - usertable$first_transaction) / 7) + 1
usertable$time_period_to_churn[usertable$churn_flag == TRUE] <- ((usertable$last_transacting_time_period[usertable$churn_flag == TRUE] - usertable$join_time_period[usertable$churn_flag == TRUE]) + 10)
usertable$revenue <- usertable$rpt * usertable$transacting_time_period

usertable$revenue <- round(usertable$revenue, 1)
usertable <- usertable[order(usertable$user_id)]
usertable <- usertable[, .(user_id, revenue, channel, first_transaction, last_transaction, transacting_time_period, rpt,join_time_period, last_transacting_time_period, churn_flag, time_period_to_churn)]
save(usertable, file = '../output/user_table3.bin')
