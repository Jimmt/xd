library(jsonlite)
library(plotly)
library(dplyr)
library(stringr)

messages <- fromJSON("message.json", flatten=TRUE)
msgs <- messages$messages

# 1
by.person <- msgs %>% group_by(sender_name) %>% summarize(n = n())
by.person <- by.person[with(by.person, order(n)),]
xform <- list(categoryorder = "array",
              categoryarray = rev(by.person$sender_name))

people.plot <- plot_ly(
  x = by.person$sender_name,
  y = by.person$n,
  name = "People",
  type = "bar"
) %>% layout(
  title = "Messages sent by person",
  xaxis = xform, 
  yaxis = list(title="# of Messages"))

# 2
ats <- lapply(messages$participants$name, function(x) {paste0("@", x)})
tab <- lapply(msgs$content, function(x) { x == ats})
truths <- as.data.frame(do.call(rbind, tab))
colnames(truths) <- ats
true.counts <- lapply(colnames(truths), function(x) {table(truths[x])["TRUE"]})
at.df <- as.data.frame(do.call(rbind, Map(data.frame, A=ats, B=true.counts)))
at.df <- at.df[with(at.df, order(B)),]

xform2 <- list(categoryorder = "array",
              categoryarray = rev(at.df$A))

# 2
at.plot <- plot_ly(
  x = at.df$A,
  y = at.df$B,
  name = "Ats",
  type = "bar"
) %>% layout(
  title = "@ by person",
  xaxis = xform2, 
  yaxis = list(title="# of @s"))

# 3
xd.count = sum(str_count(msgs$content, "xd"), na.rm = TRUE)
colon3.count = sum(str_count(msgs$content, ":3"), na.rm = TRUE)
xd.compare.plot <- plot_ly(
  x = c(":3", "xd"),
  y = c(colon3.count, xd.count),
  name = "xd",
  type = "bar"
) %>% layout(
  title = "xd vs :3",
  yaxis = list(title="Count"))

# 4
msgs <- msgs %>% mutate(xds = str_count(msgs$content, "xd"))
msgs <- msgs %>% mutate(
  date = as.POSIXct(
    as.numeric(as.character(format(msgs$timestamp_ms / 1000, scientific = FALSE))),
    origin="1970-01-01",tz="GMT"))
msgs <- msgs %>% mutate(yrmonthday = format(date, "%y-%m-%d"))

# xds.over.time.plot <- plot_ly(msgs %>% group_by(monthyr) %>% summarize(sum = sum(xds)), 
#              x = ~monthyr, y = ~sum, type = 'scatter', mode = 'lines')

xds.over.time.plot <- plot_ly(msgs %>% group_by(yrmonthday) %>% summarize(sum = sum(xds)), 
                              x = ~yrmonthday, y = ~sum, type = 'scatter', mode = 'lines') %>%
                      layout(title = "xds per day", yaxis = list(title="xd count"))


