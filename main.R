library("httr")
library("readxl")
library("ggplot2")
library("dplyr")

GET("https://query.data.world/s/lgk6hu5luph72ou4rwkviumorsxxay", write_disk(tf <- tempfile(fileext = ".xlsx")))
df <- read_excel(tf)
df2 <- df[c("year", "total_victims")]
df3 <- df2 %>% group_by(year) %>% summarize(victims = sum(total_victims))

df1980 <- filter(df3, year < 1990)
df1980_89 <- df1980 %>% summarize(victims_total = sum(victims))

df1990 <- filter(df3, year >= 1990 & year < 2000)
df1990_99 <- df1990 %>% summarize(victims_total = sum(victims))

df2000 <- filter(df3, year >= 2000 & year < 2010)
df2000_09 <- df2000 %>% summarize(victims_total = sum(victims))

df2010 <- filter(df3, year >= 2010)
df2010_19 <- df2010 %>% summarize(victims_total = sum(victims))

victims_decades <- bind_rows(df1980_89, df1990_99, df2000_09, df2010_19, .id = NULL)
victims_decades$decades <- c("1980s", "1990s", "2000s", "2010s")

axes <- ggplot(data = victims_decades, mapping = aes(x = decades, y = victims_total, fill = victims_total))

axes + geom_bar(stat = "identity") + scale_fill_gradient(low = "pink", high = "red") + theme_bw() + geom_label(aes(label = victims_total), vjust = 1.15, size = 3) + labs(title = "Number of victims in U.S. mass shootings")



