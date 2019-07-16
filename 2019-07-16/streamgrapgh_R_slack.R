library(tidyverse)
library(streamgraph)

# read the data ----

r4ds_members <-
        readr::read_csv(
                "https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-07-16/r4ds_members.csv"
        )

inds <- grep("messages_in", names(r4ds_members))
r4ds_members <- r4ds_members[, c(inds, 1)]

names(r4ds_members) <-
        c(
                'Messages in public channels',
                'Messages in private channels',
                'Messages in shared channels',
                'messages in Direct Messages',
                'date'
        )

# wide to long ----

r4ds_long <- gather(
        r4ds_members,
        message_type,
        total,
        'Messages in public channels':'messages in Direct Messages',
        factor_key = TRUE
)


# Create streamgraph ----
r4ds_long %>%
        group_by(date, message_type) %>%
        tally(wt = total) %>%
        streamgraph("message_type", "n", "date") %>%
        sg_axis_x(1, "month", "%b/%Y")







        
        
        