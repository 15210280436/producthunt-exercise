library(dplyr)
set.seed(1212)

# create a dummy data frame
dummy_df <- matrix(rnorm(20, mean = 100, sd = 10),
                   ncol = 2, byrow = FALSE) %>%
  round() %>%
  as_data_frame() 
colnames(dummy_df) <- c("VAR_X", "VAR_Y")
head(dummy_df)