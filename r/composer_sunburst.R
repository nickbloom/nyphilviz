# Title: Composer Sunburst

# Creator: Nick Bloom

# Date: May 12-ish, 2015

# Purpose: To generate JSON code for NY Philharmonic sunburst D3 diagram

# Dependences (AFAIK): Code from philviz.R

# --------------------------------------------------------------------------

# Yeah, I know I used another JSON package in the philviz code, deal with it.


library('jsonlite')

# Load the massaged CSV
t <- nbl('~/Desktop/reformatted_compins.csv')


# Get df of instruments

ci_count_df <- t %>% group_by(cleaname, soloistinstrument)  %>% 
  summarize(n = n())

names(ci_count_df) <- c('name', 'inname', 'size')

# GET COMPCOUNT 2 from philviz.R

o2 <- ci_count_df[ci_count_df$name %in% compcount2$cleaname,]

# Get top 10 (if they're there). `slice` was behaving badly, that why the commands are split

o22 <- o2 %>% 
  group_by(name) %>% 
  arrange(-size)

o22 <- o22 %>% 
  slice(1:10)
o22 <- data.frame(o22)

# Start putting it together
jso <- lapply(unique(o22$name), function(x) toJSON(o22[o22$name == x, 2:3]))

# More JSON prepping
jsdf <- data.frame(name=unique(o22$name), children = unlist(jso))

# More JSON building
oo <- toJSON(jsdf, by.row=TRUE)

# Putting it all together
oo2 <- paste('{"name":"compins",\n"children":\n', oo, '}')

# Cleaning up on R's behalf
oo2 <- str_replace_all(oo2, '\\\\"', '"')
oo2 <- str_replace_all(oo2, ':"\\[', ':\\[')
oo2 <- str_replace_all(oo2, '\\]"\\}', '\\]\\}')
oo2 <- str_replace_all(oo2, 'inname', 'name')

# Write it
write(oo2, file='.../compins.json')

