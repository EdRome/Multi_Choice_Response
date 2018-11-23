library(ggplot2)
library(dplyr)
library(plotly)

#-------------Class definition-----------#
num <- setRefClass(
  "num",
  fields = list(df = "data.frame", var_num = "data.frame"),
  methods = list(
    group_values = function(var) {
      var_num <<- df %>% count(df[[var]])
      colnames(var_num) <<- c(var, "n")
      var_num <<- var_num %>% filter(n > 2)
    }
  )
)

csv = read.csv("C:/Users/Lalo/Documents/R/Resources/multipleChoiceResponses.csv")
df <- as.data.frame(csv)
df <- df %>% select(
  gender = Q1,
  age = Q2,
  country = Q3,
  education = Q4,
  major = Q5,
  title = Q6
)
num_father <- num$new(df = df)
for (col_nam in colnames(df)) {
  assign(paste("num.", col_nam, sep = ""), num_father$group_values(col_nam))
}

p_age <-
  plot_ly(
    num.age,
    x = ~ age,
    y = ~ n,
    type = 'bar',
    marker = list(
      color = 'rgb(158,202,225)',
      line = list(color = 'rgb(8,48,107)',
                  width = 1.5)
    )
  ) %>%
  layout(
    title = 'Ages',
    xaxis = list(title = ""),
    yaxis = list(title = "")
  )
p_major <- plot_ly(num.major,
                   labels = ~ major,
                   values = ~ n,
                   type = 'pie') %>%
  layout(
    title = "Major Titles",
    xaxis = list(
      showgrid = FALSE,
      zeroline = FALSE,
      showticklabels = FALSE
    ),
    yaxis = list(
      showgrid = FALSE,
      zeroline = FALSE,
      showticklabels = FALSE
    )
  )
p_gender <- plot_ly(num.gender,
                   labels = ~ gender,
                   values = ~ n,
                   type = 'pie') %>%
  layout(
    title = "Gender",
    xaxis = list(
      showgrid = FALSE,
      zeroline = FALSE,
      showticklabels = FALSE
    ),
    yaxis = list(
      showgrid = FALSE,
      zeroline = FALSE,
      showticklabels = FALSE
    )
  )