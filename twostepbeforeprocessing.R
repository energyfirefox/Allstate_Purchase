

# obtained 2 steps before purchasing

train <- read.csv("data/train.csv")

# add + 1 step
next_step_df <- train[-1 , c("A", "B", "C", "D", "E", "F", "G", "record_type")]
colnames(next_step_df) <- c("next_A", "next_B", "next_C", "next_D", "next_E", "next_F", "next_G", "next_record")
rich_train_df <- cbind(train[-nrow(train), ], next_step_df)

table(rich_train_df$next_record)

# add +2 step 
extra_next_step <- rich_train_df[-1, c("next_A", "next_B", "next_C", "next_D", "next_E", "next_F", "next_G", "next_record")]
colnames(extra_next_step) <-  c("next_A1", "next_B1", "next_C1", "next_D1", "next_E1", "next_F1", "next_G1", "extra_next_record")
rich_train_df_full <- cbind(rich_train_df[-nrow(rich_train_df), ], extra_next_step)


rich_train_df_full <- rich_train_df_full[rich_train_df_full$record_type < 1, ]
rich_train_df_full <- rich_train_df_full[rich_train_df_full$next_record < 1, ]

write.csv(rich_train_df_full, "rich_train_2step.csv")

