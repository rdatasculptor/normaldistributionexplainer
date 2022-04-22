library(echarts4r)
library(dplyr)
library(RColorBrewer)

steps <- c("Start", "Z-score kleiner dan -2",
           "Z-score tussen -1.5 en -2",
           "Z-score tussen -1 en -1.5",
           "Z-score tussen -0.5 en -1",
           "Z-score tussen 0 en -0.5",
            "Z-score vlak boven 0 en 0.5",
           "Z-score tussen 0.5 en 1",
           "Z-score tussen 1 en 1.5",
           "Z-score tussen 1.5 en 2",
           "Z-score groter dan 2", 
           "And here you are :-)")
explanation <- c("", 
                 "First 2% of the population",
                 "Next 5%",
                 "Next 9%",
                 "Next 15%",
                 "Next 19%",
                 "Next 19%",
                 "Next 15%",
                 "Next 9%",
                 "Next 5%",
                 "Final 2% of the population", 
                 "And here you are :-)")

for (i in 2:length(explanation)) explanation[i] <- gsub("SCORE",(i-2)*10,explanation[i])

titles <- list()
for (i in 1:length(steps)){
  titles[[i]] <- list(subtext = paste0(explanation[i]), left = 5, subtextStyle = list(color = "grey", fontSize = "40"))
}

distribution <- list(c(1,2),
                     c(3,4,5,6,7),
                     c(8:16),
                     c(17:31),
                     c(32:50),
                     c(51:69),
                     (70:84),
                     c(85:93),
                     c(94:98),
                     c(99,100))
set.seed(1)
df0 <- data.frame(group = steps[1], 
                  id = 1:100, 
                  spot = sample(20:80, 100, replace = TRUE), 
                  score = sample(70:90, 100, replace = TRUE)) |> mutate(size = 20)

dflist <- list()
dflist[[1]] <- df0
set.seed(1)
for (i in 1:10){
  df01 <- df0 |> filter(id %in% distribution[[i]]) |> mutate(
    spot = sample(((i*10)-9):((i*10)-1), length(distribution[[i]]), replace = TRUE), 
    score = sample(1:(length(distribution[[i]])*2), length(distribution[[i]]), replace = TRUE)
  )
  df02 <- df0 |> filter(!id %in% distribution[[i]]) 
  df0 <- bind_rows(df01,df02) |> 
    arrange(id) |> mutate(group = steps[i+1])
  dflist[[i+1]] <- df0
}

df <- bind_rows(dflist) |> arrange(group, id, score)

dfyou <- df |> filter(group == "Z-score groter dan 2") |> mutate(group = "And here you are :-)")
df <- df |> bind_rows(dfyou)
dfsquare <- data.frame(group = steps[1:11],id = 102, spot = 5, square = 5, size = 80, label = 10)

dflistsquare <- list()

for (i in 1:9){
  dfsquare2 <- dfsquare |> mutate(id = 102 + i,spot = spot + (i*10), label = (i + 1)*10)
  dflistsquare[[i]] <- dfsquare2
}

dfsquare <- bind_rows(dflistsquare) |> bind_rows(dfsquare)
dfsquareyou <- dfsquare |> filter(group == "Start") |> mutate(group = "And here you are :-)")
dfsquare <- dfsquare |> bind_rows(dfsquareyou)

dfyou <- data.frame(group = steps[1:11], id = 101, spot = 50, thisisyou = 100, size = 20, label2 = "This is you")
dfyou2 <-  dfyou[1,] |> mutate(group ="And here you are :-)", spot = 65, thisisyou = 25)
dfyou <- dfyou |> bind_rows(dfyou2)

df <- df |> bind_rows(dfsquare) |> bind_rows(dfyou)
df$group <- factor(df$group, levels = steps)
df <- df |> arrange(group, id)
df$id <- factor(df$id, levels = 1:111)
df <- df |> arrange(group, id)
df$spot <- (df$spot-50)/20

p1 <- df |>
  group_by(group) |>
  e_charts(spot, timeline = TRUE) |>
  e_timeline_serie(title = titles) |>
  e_timeline_opts(inverse = TRUE, show = FALSE, top = 5, bottom = 200, left = 5,
                  autoPlay = TRUE,
                  lineStyle = list(show = FALSE),
                  symbol ="roundRect",
                  progress= list(lineStyle=list(opacity=0), itemStyle=list(color="grey"), 
                                 label=list(show=TRUE, fontSize=12,                       
                                            color ="grey")),
                  orient = "vertical",
                  padding=0,
                  playInterval=4000,
                  tooltip = list(formatter = '{b}'),
                  label = list(show=TRUE, fontSize=12, #rotate = 20, 
                               color ="grey", position = 10),
                  itemStyle = list(color="grey"),
                  lineStyle = list(color="transparent"),
                  controlStyle = list(color="grey", borderColor="grey", itemSize = 30, itemGap=20, showPlayBtn=TRUE),
                  checkpointStyle = list(color="grey",borderWidth =0, symbol="roundRect")) |>
  e_scatter(score, bind=id, symbol="path://M224 256c70.7 0 128-57.31 128-128s-57.3-128-128-128C153.3 0 96 57.31 96 128S153.3 256 224 256zM274.7 304H173.3C77.61 304 0 381.6 0 477.3c0 19.14 15.52 34.67 34.66 34.67h378.7C432.5 512 448 496.5 448 477.3C448 381.6 370.4 304 274.7 304z",  
            zlevel = 2, opacity = 0, animationDuration = 2000, 
            label=list(show=F, formatter = "{b}", fontSize=20,position = "top",
            backgroundColor = "transparent", color = "transparent")) |>
  e_scatter(thisisyou, bind=label2, symbol="path://M224 256c70.7 0 128-57.31 128-128s-57.3-128-128-128C153.3 0 96 57.31 96 128S153.3 256 224 256zM274.7 304H173.3C77.61 304 0 381.6 0 477.3c0 19.14 15.52 34.67 34.66 34.67h378.7C432.5 512 448 496.5 448 477.3C448 381.6 370.4 304 274.7 304z",
            zlevel = 3, opacity = 0, animationDuration = 2000,
            label=list(show=T, formatter = "{b}", fontSize=14,position = "top",                                                                                                                                                                                                                                                                                                                                                             backgroundColor = "transparent", color = "grey", opacity = 1)) |>
  e_hide_grid_lines() |>
  e_x_axis(show = TRUE, min = -3, max = 3, maxInterval = 0.5, name = "Z-Score", axisLabel = list(fontSize = 20)) |>
  e_y_axis(show = FALSE,maxInterval = 10, min = 0, max = 110) |>
  e_add_unnested("symbolSize", size) |>
  e_color(brewer.pal(n = 8, name = "Set1")[c(c(2,7))]) |>
  e_legend(show = FALSE)# |>
  
p1
htmlwidgets::saveWidget(p1, file="normaldistributionexplainer.html")