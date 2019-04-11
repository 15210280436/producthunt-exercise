library(magrittr)
library(purrr)
set.seed(1212)

# A set of cards consists of N cards
N = 586

# is it a complete set?
complete_set <- function(n, m) {
  deck = sample(n, size = m, replace = TRUE) # sample 随机采样函数 replace = TRUE 有放回采样
  ifelse(length(unique(deck)) == n, TRUE, FALSE) #如果卡片去重等于586返回TRUE 
}

# what are the odds of getting full set if you purchase many cards?
sims <- replicate(10000, complete_set(N, 3000)) #做10000次随机抽样来看集齐卡片的概率
table(sims) %>% prop.table() #prop.table 转化为百分比

# each card costs x cents
# each unique card costs y cents
random_cost = .05
unique_cost = .25

card_purchase <- function(n, full_set, rand, uniq) {
  # purchase n cards from full set
  deck = sample(full_set, size = n, replace = TRUE)
  # topup if not full set
  n * rand  + (full_set - length(unique(deck))) * uniq
}
card_purchase(3000, N, random_cost, unique_cost)

purchases = 100:2000
sim_costs <- map_dbl(purchases, ~ card_purchase(.x, N, random_cost, unique_cost))
plot(purchases, sim_costs, xlab = "# Cards Purchase", ylab = "Costs")

# to reduce uncertainty
sim_costs <- map_dbl(purchases, ~ {
  replicate(300, expr = card_purchase(.x, N, random_cost, unique_cost)) %>% #每次抽样300次取均值 根据均值做循环
    # average of trials
    mean()
})
names(sim_costs) <- purchases
plot(purchases, sim_costs, xlab = "# Cards Purchase", ylab = "Costs")

# 5个红球 5个蓝球 取3次，第一个和第三个相同的概率
red_ball=5
blue_boll=5
ball_set <- function(red,blue,n){
  temp=sample(red+blue, size = n, replace = FALSE)
  ifelse((temp[1]<=red & temp[3]<=red) | (temp[1]>red & temp[3]>red), TRUE, FALSE)
}
ball_prop <- replicate(1000, ball_set(red_ball,blue_boll,3)) #做10000次随机抽样来看集齐卡片的概率
table(ball_prop) %>% prop.table() #prop.table 转化为百分比

c_1=0.6
c_2=0.5
