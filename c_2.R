library(dplyr)
library(infer)

set.seed(1212)

# create a dummy data frame
dummy_df <- matrix(rnorm(20, mean = 100, sd = 10), # 随机产生均值是100、标准差为10且为2列的20个数字
                   ncol = 2, byrow = FALSE) %>%
  round() %>%  # 去掉小数
  as_data_frame() #  转换为dataframe
colnames(dummy_df) <- c("VAR_X", "VAR_Y")
head(dummy_df)

dummy_df %>% 
  specify(response = "VAR_X") %>% # 选择VAR_X列数据
  generate(reps = 3000, type = "bootstrap") %>% # 随机抽取3000次，每次有放回的抽取20个数字
  calculate(stat = 'mean') %>% # 计算每组的均值
  summarise(
    lower = quantile(stat, 0.025),
    upper = quantile(stat, 0.975)
  )


calc_mean_ci <- function(df1,var) {
  df1 %>%
    mutate(var=df1[[var]]) %>% 
    specify(response = var) %>% 
    generate(reps = 3000, type = "bootstrap") %>% 
    calculate(stat = 'mean') %>% 
    summarise(
      lower = quantile(stat, 0.025),
      upper = quantile(stat, 0.975)
    ) 
}
lapply(colnames(dummy_df), calc_mean_ci, df1 = dummy_df)

