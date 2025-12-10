##importing csvs
library(readr)
library(dplyr)
library(ggplot2)
library(tidyr)
#time series
prov.emp <- read_csv("row-time_col-prov-emprate.csv")
prov.unemp <- read_csv("row-time_col-prov-unemprate.csv")
age.emp <- read_csv("row-time_col-age-emprate.csv")
age.unemp <- read_csv("row-time_col-age-unemprate.csv")
gen.emp <- read_csv("row-time_col-gen-emprate.csv")
gen.unemp <- read_csv("row-time_col-gen-unemprate.csv")
time.type <- read_csv("row-time_col-emptype.csv")
#others
gen.type <- read_csv("row-gen_col-emptype.csv")
age.type <- read_csv("row-age_col-emptype.csv")
age.gen.emp <- read_csv("row-age_col-gen-unemp.csv")

gen.unemp
total.genderm <- mean(gen.unemp$Men)
total.genderm
total.genderw <- mean(gen.unemp$Women)
total.genderw 
total.gender <- mean(c(gen.unemp$Men, gen.unemp$Women))
total.gender

#employment & unemployment rate by province
avg.prov <- bind_rows(
  prov.emp %>% summarise(across(where(is.numeric),mean)),
  prov.unemp %>% summarise(across(where(is.numeric),mean))
) %>%
  t() %>%
  as.data.frame() %>%
  rename(employment_rate=V1,unemployment_rate=V2) %>%
  arrange(unemployment_rate) %>%
  tibble::rownames_to_column("Provinces")
avg.prov

avg.long <- avg.prov %>%
  pivot_longer(
    cols=c("employment_rate","unemployment_rate"),
    names_to = "Rate_Type",
    values_to = "Rate"
  )
avg.long
  
ggplot(avg.long,aes(
  x= reorder(Provinces, Rate * (Rate_Type == "unemployment_rate")),
  y=Rate,
  fill=Rate_Type)) + 
  geom_col(position='dodge') + 
  geom_text(
    aes(label = paste0(round(Rate, 1),"%")),
    position = position_dodge(width = 0.8),
    hjust = -0.1,        # move text slightly outside the bar since it's flipped
    size = 3
  )+
  coord_flip() +
  labs(x="Province", y="Rate (%)", fill="Type",title='Employment/Unemployment Rates by Province (2025)') +
  scale_fill_manual(
    name="Rate Type",
    values=c("employment_rate" = "#2cc418",
             "unemployment_rate" = "#a32727"
             ),
    labels = c(
      "employment_rate"   = "Employment Rate",
      "unemployment_rate" = "Unemployment Rate")
    ) +
  theme_minimal()

ggsave("employment & unemployment rate by province.png", 
       plot = last_plot(), width = 8, height = 5, dpi = 300)

#employment & unemployment rate by age group
avg.age<- bind_rows(
    age.emp %>% summarise(across(where(is.numeric),mean)),
    age.unemp %>% summarise(across(where(is.numeric),mean))
  ) %>%
    t() %>%
    as.data.frame() %>%
    rename(employment_rate=V1,unemployment_rate=V2) %>%
    arrange(unemployment_rate) %>%
    tibble::rownames_to_column("Age Group")
  avg.age

avg.long.age <- avg.age %>%
  pivot_longer(
    cols=c("employment_rate","unemployment_rate"),
    names_to = "Rate_Type",
    values_to = "Rate"
  )
avg.long.age

ggplot(avg.long.age,aes(
  x=`Age Group`,
  y=Rate,
  fill=Rate_Type)) + 
  geom_col(position='dodge') + 
  geom_text(
    aes(label = paste0(round(Rate, 1),"%")),
    position = position_dodge(width = 0.8),
    vjust = -0.3,        # move text slightly outside the bar since it's flipped
    size = 3
  )+
  labs(x="Age Group", y="Rate (%)", fill="Type",title='Employment/Unemployment Rates by Age Group (2025)') +
  scale_fill_manual(
    name="Rate Type",
    values=c("employment_rate" = "#2cc418",
             "unemployment_rate" = "#a32727"
    ),
    labels = c(
      "employment_rate"   = "Employment Rate",
      "unemployment_rate" = "Unemployment Rate")
  ) +
  theme_minimal()

ggsave("employment & unemployment rate by age group.png", 
       plot = last_plot(), width = 8, height = 5, dpi = 300)

#Unemployment rate by age group/gender
age.gen.emp
long.age.gen <- age.gen.emp %>%
  pivot_longer(
    cols = c("Unemployment_Men","Unemployment_Women"),
    names_to = "Rate_Type",
    values_to = "Rate"
  )

long.age.gen

ggplot(long.age.gen,aes(
  x=`Age group`,
  y=Rate,
  fill=Rate_Type)) + 
  geom_col(position='dodge') + 
  geom_text(
    aes(label = paste0(round(Rate, 1),"%")),
    position = position_dodge(width = 0.8),
    vjust = -0.3,        # move text slightly outside the bar since it's flipped
    size = 3
  )+
  labs(x="Age Group", y="Rate (%)", fill="Type",title='Unemployment Rates by Age Group & Gender (2025)') +
  scale_fill_manual(
    name="Gender",
    values=c("Unemployment_Men" = "#3c8cd6",
             "Unemployment_Women" = "#f081c5"
    ),
    labels = c(
      "Unemployment_Men"   = "Men",
      "Unemployment_Women" = "Women")
  ) +
  theme_minimal()

ggsave("Une mployment rate by age groupgendger.png", 
       plot = last_plot(), width = 8, height = 5, dpi = 300)

#unemployment over time by gender
row_name <- c("Jan","Feb","Mar","Apr","May","Jun",
                "Jul","Aug","Sep","Oct")
gen.unemp$Month <- row_name
gen.unemp
gen.unemp$Month <- factor(
  gen.unemp$Month,
  levels = c("Jan","Feb","Mar","Apr","May","Jun",
             "Jul","Aug","Sep","Oct","Nov","Dec"),
  ordered=TRUE
)

yminmax <- range(c(gen.unemp$Women,gen.unemp$Men))

ggplot(gen.unemp,aes(x=Month,y=Women,group=1)) + 
  labs(title="Unemployment over Time by Gender",
       x="2025",
       y="Unemployment rate (%)") +
  geom_line(aes(color="Women")) +
  geom_point(aes(color="Women")) +
  geom_line(data=gen.unemp,aes(x=Month,y=Men,group=1,
                               color="Men"),
            ) +
  geom_point(data=gen.unemp,aes(x=Month,y=Men,group=1,
                                color="Men"),
             ) +
  geom_smooth(
    aes(y = Women, color = "Women"),
    method = "lm", se = FALSE, linetype = "dotted"
  ) +
  geom_smooth(
    aes(y = Men, color = "Men"),
    method = "lm", se = FALSE, linetype = "dotted"
  ) +
  scale_color_manual(
    name = "Gender",
    values = c(
      "Women" = "#f081c5",
      "Men"   = "#3c8cd6"
    )
  )+
  coord_cartesian(ylim=yminmax) +
  theme_minimal()


ggsave("unemployment over time by gender.png", 
       plot = last_plot(), width = 8, height = 5, dpi = 300)

gen.unemp <- gen.unemp %>%
  mutate(Total = rowMeans(across(c(Women,Men))))

print(mean(gen.unemp$Total))
print(gen.unemp)
mean_total <- mean(gen.unemp$Total)
mean_total

ggplot(gen.unemp, aes(x = Month, y = Total, group = 1)) +
  geom_line(color = "red", linewidth = 0.5) +
  labs(
    title = "Unemployment Over Time in Canada (2025)",
    x = "2025",
    y = "Unemployment rate (%)"
  ) +
  geom_hline(yintercept = mean_total, 
             color = "red", 
             linewidth = 0.5,
             linetype="dashed") +
  
  theme_minimal()

ggsave("unemployment over time in canada.png", 
       plot = last_plot(), width = 8, height = 5, dpi = 300)


#unemployment over time by age
age.unemp$Month <- factor(
  gen.unemp$Month,
  levels = c("Jan","Feb","Mar","Apr","May","Jun",
             "Jul","Aug","Sep","Oct","Nov","Dec"),
  ordered=TRUE
)

yminmax <- range(c(age.unemp$`15-24`,age.unemp$`25-44`,age.unemp$`45-64`,age.unemp$`65+`))
  
ggplot(age.unemp,aes(x=Month,y=`15-24`,group=1)) + 
  labs(title="Unemployment over Time by Age Group",
       x="2025",
       y="Unemployment rate (%)") +
  geom_line(aes(color="15-24")) +
  geom_point(aes(color="15-24")) +
  geom_line(data=age.unemp,aes(x=Month,y=`25-44`,group=1,color="25-44")) +
  geom_point(data=age.unemp,aes(x=Month,y=`25-44`,group=1,color="25-44")) +
  geom_line(data=age.unemp,aes(x=Month,y=`45-64`,group=1,color="45-64")) +
  geom_point(data=age.unemp,aes(x=Month,y=`45-64`,group=1,color="45-64")) +
  geom_line(data=age.unemp,aes(x=Month,y=`65+`,group=1,color="65+")) +
  geom_point(data=age.unemp,aes(x=Month,y=`65+`,group=1,color="65+")) +
  scale_color_discrete(name="Age Group")+
  coord_cartesian(ylim=yminmax) +
  theme(
    panel.grid.major = element_line(color = "grey80"),
    panel.grid.minor = element_line(color = "grey90")
    )+
  theme_minimal()

ggsave("unemployment over time by age.png", 
       plot = last_plot(), width = 8, height = 5, dpi = 300)

#job types by age
gen.type <- read_csv("row-gen_col-emptype.csv")
age.type <- read_csv("row-age_col-emptype.csv")
age.type
names(age.type)

age.type.bar <- age.type[,1:2]
age.type.bar

age.type.bar <- age.type.bar %>%
  mutate(Highlight = ifelse(`Age Group` == "25-44", "Highlight", "Normal"))

ggplot(age.type.bar, aes(
  x=`Age Group`,
  y=`Full-Time`,
  fill=Highlight))+
  geom_col() +
  scale_fill_manual(values = c("Normal" = "steelblue", "Highlight" = "red")) +
  guides(fill = "none") +
  theme_minimal()

ggsave("job types bxcxccxcxe.png", 
       plot = last_plot(), width = 8, height = 5, dpi = 300)

age.type.table <- age.type %>%
  rowwise() %>%
  mutate(
    across(
      c(`Full-Time`, `Part-Time`,Unemployed),
      ~ round((.x / sum(c_across(c(`Full-Time`, `Part-Time`,"Unemployed","Participation Rate"))))*100,2)
    )
  ) %>%
  ungroup() %>%
  rename_with(
    ~ paste0(.x, " (%)"),
    all_of(c("Full-Time", "Part-Time", "Unemployed","Participation Rate")
    )
  )
age.type.table
df.type <- data.frame(age.type.table) 
write.csv(df.type , "age_type_percentages.csv", row.names = FALSE)


age.type.table <- age.type %>%
  rowwise() %>%
  mutate(
    across(
      c(`Full-Time`, `Part-Time`, `Unemployed`),
      ~ (.x / sum(c_across(c(`Full-Time`, `Part-Time`, Unemployed)))) * 100
    )
  ) %>%
  ungroup() %>%
  rename_with(
    ~ paste0(.x, " (%)"),
    c("Full-Time", "Part-Time", "Unemployed")
  )


age.type <- age.type[,-ncol(age.type)]
age.type
age.type.df <- age.type %>%
  t() %>%
  as.data.frame()

colnames(age.type.df) <- age.type.df[1,]
age.type.df <- age.type.df[-1,]

age.type.df <- age.type.df %>%
  tibble::rownames_to_column("Employment Type")
tibble(age.type.df)

age.type.long <- age.type.df %>%
  pivot_longer(
    cols=c("15-24","25-44","45-64","65+"),
    names_to = "age_group",
    values_to = "count"
  ) %>%
  mutate(count = as.numeric(count)) %>%
  
  group_by(`Employment Type`) %>%
  arrange(`Employment Type`, age_group) %>%
  mutate(
    pct = count / sum(count),
    cumulative = cumsum(pct),
    mid = cumulative - pct/2,         # middle of slice
    label_y = mid * 1.25
)%>%
  ungroup()
age.type.long

age.type.long

ggplot(age.type.long, aes(
  x = "",
  y = pct,
  fill = age_group
)) +
  geom_col(width = 1, color = "white") +
  coord_polar("y") +
  facet_wrap(~ `Employment Type`) +
  labs(
    title = "Employment Type Distribution by Age Group (2025)",
    fill = "Age Group"
  ) +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank()
  )

ggsave("job types by age_pie.png", 
       plot = last_plot(), width = 8, height = 5, dpi = 300)

ggplot(age.type.long, aes(
  x=`Employment Type`,
  y=count,
  fill=age_group)) +
  geom_col(position='dodge') + 
  labs(x="Employment Type", y="Count", fill="age_group",
       title='Employment Type by Age (2025)') +
  scale_fill_discrete(name="Age Group") +
  theme_minimal()

ggsave("job types by age.png", 
       plot = last_plot(), width = 8, height = 5, dpi = 300)

#job type by gender
gen.type
gen.type <- gen.type[,-ncol(gen.type)]
gen.type
gen.type.df <- gen.type %>%
  t() %>%
  as.data.frame()

colnames(gen.type.df) <- gen.type.df[1,]
gen.type.df <- gen.type.df[-1,]

gen.type.df <- gen.type.df %>%
  tibble::rownames_to_column("Employment Type")
tibble(gen.type.df)

gen.type.long <- gen.type.df %>%
  pivot_longer(
    cols=c("Men","Women"),
    names_to = "Gender",
    values_to = "count"
  )
gen.type.long <- gen.type.long %>%
  mutate(count = as.numeric(count))
gen.type.long

print(gen.type.long$count[1]/gen.type.long$count[2])
print(gen.type.long$count[4]/gen.type.long$count[3])

ggplot(gen.type.long, aes(
  x=`Employment Type`,
  y=count,
  fill=Gender)) +
  geom_col(position='dodge') + 
  labs(x="Employment Type", y="Count", fill="Gender",
       title='Employment Type by Gender (2025)') +
  scale_fill_manual(
    name="Gender",
    values=c("Men" = "#3c8cd6",
             "Women" = "#f081c5"
    ),
    labels = c(
      "employment_rate"   = "Employment Rate",
      "unemployment_rate" = "Unemployment Rate")
  ) +
  theme_minimal()

ggsave("job types by gender.png", 
       plot = last_plot(), width = 8, height = 5, dpi = 300)

#participation rate by province
part.prov <- read_csv("partbyprov.csv")

ggplot(part.prov, aes(
  x= reorder(Province, `Participation Rate`),
  y=`Participation Rate`)) +
  labs(title="Participation Rate by Province (%)",
       x="Province") +
  geom_text(
    aes(label = paste0(round(`Participation Rate`, 1),"%")),
    position = position_dodge(width = 0.8),
    vjust = -0.3,        # move text slightly outside the bar since it's flipped
    size = 3
  )+
  geom_bar(stat="identity",fill='maroon') +
  theme_minimal()

ggsave("participation rate by province.png", 
       plot = last_plot(), width = 8, height = 5, dpi = 300)
