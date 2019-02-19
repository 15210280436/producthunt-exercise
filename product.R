library(tidyverse)
library(ggthemes)
library(ggplot2)
library(lubridate)

ALLTopics <- readr::read_csv("data/ALLTopics.csv")
PostsForAnalysis <- readr::read_csv("data/PostsForAnalysis.csv")
PostsTopicsForAnalysis <- readr::read_csv("data/PostsTopicsForAnalysis.csv")
UsersForAnalysis <- readr::read_csv("data/UsersForAnalysis.csv")

ALLTopics %>% 
  select(name,num_posts,num_followers) %>% 
  group_by(name) %>% 
  summarise(n=num_posts,num_followers) %>% 
  arrange(desc(n)) %>% 
  head(20) %>% 
  ggplot(mapping=aes(x=reorder(name, -n),y=n,size=num_followers))+
  geom_bar(stat="identity",width = .1, fill = "lightblue")+
  geom_point(col = "navyblue")+
  labs(x = "Topic", y = "# Posts", size = "# Followers") + 
  theme(axis.text.x = element_text(angle = 30))

# Does More Posts Mean More Followers? ------------------------------------

ALLTopics %>% 
  # filter topics with most posts
  top_n(20, wt = num_posts) %>% # 根据选择num_posts前20数据
  ggplot(aes(reorder(name,-num_posts), num_posts)) + #reorder 根据-num_posts倒序排序的name
  geom_bar(stat = 'identity', width = .1, fill = "lightblue") + # fill 按照分类
  geom_point(aes(size = num_followers), col = "navyblue") +  # 点的颜色是navy blue
  labs(x = "Topic", y = "# Posts", size = "# Followers") + # 标记横纵坐标以及分类的名称
  theme(axis.text.x = element_text(angle = 30)) #斜着保证横坐标显示全


PostsForAnalysis %>% 
  mutate(year=lubridate::year(PostsForAnalysis$date)) %>% 
  select(year,votes_count,time_of_day) %>% 
  group_by(year,time_of_day) %>% 
  summarise(vote=sum(votes_count)) %>% 
  ggplot(mapping=aes(x=factor(year),y=vote/1e3))+
  geom_bar(stat="identity", aes(fill=time_of_day),position = "dodge", width = .3)+
  scale_fill_brewer(palette = "Set2")+ 
  labs(x = "Year", y = "# Votes ('000)", fill = "Time Of The Day")

PostsForAnalysis %>% 
  mutate(year = year(created_at), 
         # make it easier to plot 
         time_of_day = factor(time_of_day, 
                              levels = c("Morning", "Afternoon", "Evening", "Night"), 
                              ordered = TRUE)) %>% 
  group_by(year, time_of_day) %>% 
  summarise(votes = sum(votes_count)) %>% 
  ggplot(aes(factor(year), votes / 1e3, fill = time_of_day)) +
  geom_bar(stat = "identity", position = "dodge", width = .3) + 
  scale_fill_brewer(palette = "Set2") + 
  labs(x = "Year", y = "# Votes ('000)", fill = "Time Of The Day")


UsersForAnalysis %>% 
  filter(posts_count>30) %>% 
  select(user_id,name) %>% 
  inner_join(PostsForAnalysis %>% select(id,user_id,votes_count),by="user_id") %>% 
  group_by(user_id,name) %>% 
  summarise(n=mean(votes_count)) %>% 
  ungroup() %>% 
  top_n(n=20,wt=n) %>% 
  ggplot(mapping=aes(reorder(name,n),y=n))+
  geom_point(size=3,col="salmon")+
  geom_bar(stat="identity",width=.1,fill="salmon",alpha=.3) + 
  coord_flip()+ # 横轴柱状图
  labs(x = "User", y = "Average Votes Count Per Post")

# find total received votes by user
spotters <- UsersForAnalysis %>% 
  filter(posts_count > 30) %>% 
  select(user_id, name) %>% 
  left_join(PostsForAnalysis %>% select(id, user_id, votes_count), by = "user_id") %>% 
  group_by(user_id, name) %>% 
  summarise(recv_votes_pp = mean(votes_count)) %>% 
  ungroup()

# using barbell plot to highlight top 20 spotters
spotters %>% 
  top_n(n = 20, wt = recv_votes_pp) %>% 
  ggplot(aes(reorder(name, recv_votes_pp), recv_votes_pp)) +
  geom_bar(stat = "identity", width = .1, fill = "salmon", alpha = .3) + 
  geom_point(size = 3, col = "salmon") + 
  coord_flip() +
  labs(x = "User", y = "Average Votes Count Per Post")

# posts that perform better than 50% 

popular_posts <- PostsForAnalysis %>% 
  filter(votes_count>=median(votes_count)) %>% 
  pull(id)

popular_cats <- PostsTopicsForAnalysis %>% 
  filter(id %in% popular_posts) %>% 
  gather(topic,tagged,-id) %>% 
  filter(!is.na(tagged)) %>% 
  group_by(topic) %>% 
  summarise(n=n()) %>% 
  filter(topic!="tech")

popular_cats %>% 
  top_n(n=10,wt=n) %>% 
  ggplot(mapping = aes(reorder(topic,n),y=n,fill = reorder(topic, -n)))+
  geom_bar(stat = "identity")+ 
  scale_y_continuous(breaks = seq(0, max(popular_cats$n), 500), 
                     # give it little break so it looks better
                     limits = c(0, max(popular_cats$n) + 500)) + # 限制最大值，最小值
  scale_fill_brewer(palette = "Paired") + # 确定颜色
  coord_polar(theta = "y", direction = 1) + # 饼图
  # hide axis y text as legend shows ranking already
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank()) + # 不显示y轴标题
  labs(x = "Topic", y = "Count", fill = "Topic")

popular_posts <- PostsForAnalysis %>% 
  filter(votes_count >= median(PostsForAnalysis$votes_count)) %>% 
  pull(id)

# appearance of category
popular_cats <- PostsTopicsForAnalysis %>% 
  filter(id %in% popular_posts) %>% 
  gather(topic, tagged, -id) %>% 
  filter(!is.na(tagged)) %>% 
  group_by(topic) %>% 
  summarise(n = n()) %>% 
  arrange(desc(n)) %>% 
  # remove 'tech' so it does not overwhelm others
  filter(topic != "tech")

# visualization inspired by Apple watch face

popular_cats %>% 
  top_n(10, wt = n) %>% 
  ggplot(aes(reorder(topic, n), n, fill = reorder(topic, -n))) + 
  geom_bar(stat = "identity") + 
  scale_y_continuous(breaks = seq(0, max(popular_cats$n), 500), 
                     # give it little break so it looks better
                     limits = c(0, max(popular_cats$n) + 500)) +
  scale_fill_brewer(palette = "Paired") +
  coord_polar(theta = "y", direction = 1) + 
  # hide axis y text as legend shows ranking already
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank()) + 
  labs(x = "", y = "Count", fill = "Topic")


