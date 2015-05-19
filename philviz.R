# Title: Philviz

# Creator: Nick Bloom

# Date: Sometime in April 2015

# Purpose: To generate JSON code for NY Philharmonic DC visualizations. MAKE SURE YOU CLEAN THE XML FIRST WITH THE PYTHON SCRIPT

# External Dependences (AFAIK): purrr, dplyr, XML, igraph, RJSONIO, jsonlite, stringr, magrittr
# --------------------------------------------------------------------------

library('httr')
library('XML')
library('igraph')
library('RJSONIO')

source('philviz_functions.R')


# Parses cleaned XML file (see reformat_nyphil.py) and makes a list out of the elements
anybar('yellow')
nyp <- xmlTreeParse('../nyphil-clean.xml')
nyp <- xmlToList(nyp)
anybar('green')

# Running the df-ifying functions; try call is to continue past errors
anybar('yellow')
soloist_dfs <- lapply(nyp, function(x) try(nyp_lister(x)))
anybar('green')

anybar('yellow')
nyphil <- lapply(nyp, function(x) try(nyp_lister_nosolo(x)))
anybar('green')


# This part I still can't work out for the life of me. Something wonky happens in the way I or R creates the soloist list, such that the items in each data.frame column are still lists. I'm working on it...

valids <- soloist_dfs %>% keep(., ~ is.data.frame(.))

sdf <- bind_rows(valids) %>% data.frame(.) %>% 
  na_omit(.)

# Soloist-less data.frame is fine, though...

valids2 <- nyphil %>% keep(., ~ is.data.frame(.))

nyphil <- bind_rows(valids2) %>% data.frame(.) %>% 
  na_omit(.)

# Here is me trying (in vain) to un-CF the soloist data.frame


sdf <- apply(sdf, 2, function(x) set_names(x, seq(1:length(x))))

sdf2 <- apply(sdf, 2, function(x) unlist(x))

goodnames <- intersect(names(sdf2[[1]]),names(sdf2[[2]])) %>% 
  intersect(., names(sdf2[[3]]))

sdf3 <- lapply(sdf2, function(x) x[goodnames])



# This works...okay 

sdff <- as.data.frame(sdf2)
sdff$date <- str_replace_all(sdff$date,'T05:00:00Z','')


# Extract date info into separate columns

sdff$year <- str_extract(sdff$date,'[0-9]{4}')
sdff$month <- str_extract(sdff$date,perl('(?<=-)[0-9]{2}(?=-)'))
sdff$day <- str_extract(sdff$date,perl('[0-9]{2}$'))

sdff$time <- as.factor(sdff$time)


# --------------------------------------------------------------------------
# FULL NO-SOLOIST DATA FRAME

# Cleaning up dates

nyphil$date <- str_replace_all(nyphil$date,'T05:00:00Z','')

nyphil$year <- str_extract(nyphil$date,'[0-9]{4}')
nyphil$month <- str_extract(nyphil$date,perl('(?<=-)[0-9]{2}(?=-)'))
nyphil$day <- str_extract(nyphil$date,perl('[0-9]{2}$'))

nyphil$time <- as.factor(nyphil$time)


# Cleaning up composer names
nyphil <- nyphil[!is.na(nyphil$composername),]

nyphil$cleaname <- nyphil$composername %>% 
  map(., ~ namefix(., ',')) %>% 
  unlist(.)

# Ahhh manual fixes - the cornerstone of any automated scraping task!

nyphil$cleaname <- str_replace_all(nyphil$cleaname,'J. S. -Gounod Bach', 'Bach-Gounod')
nyphil$cleaname <- str_replace_all(nyphil$cleaname,'é', 'e')
nyphil$cleaname <- str_replace_all(nyphil$cleaname,'Ã©', 'e')
nyphil$cleaname <- str_replace_all(nyphil$cleaname,'í', 'i')
nyphil$cleaname <- str_replace_all(nyphil$cleaname,'Ã­', 'i')
nyphil$cleaname <- str_replace_all(nyphil$cleaname,'á', 'a')
nyphil$cleaname <- str_replace_all(nyphil$cleaname,'š', 's')
nyphil$cleaname <- str_replace_all(nyphil$cleaname,'ö', 'o')
nyphil$cleaname <- str_replace_all(nyphil$cleaname,'ò', 'o')
nyphil$cleaname <- str_replace_all(nyphil$cleaname,'Jr. John Corigliano', 'John Corigliano Jr.')
nyphil$cleaname <- str_replace_all(nyphil$cleaname,'Jr. Johann Strauss', 'Johann Strauss Jr.')
nyphil$cleaname <- str_replace_all(nyphil$cleaname,'Sr. Johann Strauss', 'Johann Strauss Sr.')
nyphil$cleaname <- str_replace_all(nyphil$cleaname,'Jr. & Josef Johann Strauss', 'Josef & Johann Strauss Jr.')

# Get rid of non-composers
nyphil <- nyphil[! nyphil$cleaname == 'Anthem',]
nyphil <- nyphil[! nyphil$cleaname == 'Traditional',]

# Count how many times a composer has been played

compcount <- nyphil %>% 
  group_by(cleaname) %>% 
  summarize(count=n()) %>% 
  data.frame(.) %>% 
  arrange(-count)

# Keep the top 250, and 25
compcount250 <- compcount[1:250,]
compcount2 <- compcount[1:25,]

# Subset big data frame to just top 250
nypnet <- nyphil[nyphil$cleaname %in% compcount250$cleaname,]

# Start building the data frame
composers <- data.frame(cleaname = unique(nypnet$cleaname))

# Generate composer ID
composers$compid <- seq(1,nrow(composers))

# Merge with info from full data frame

nypnet_mer <- merge(nypnet, composers, by = 'cleaname')


# This generates the raw tie data.frame for each composers, and binds the list

anybar('yellow')
net_df  <-  unique(nypnet_mer$progid) %>% 
  map(., ~ networkize(.))
anybar('green')

net_df  <-  bind_rows(net_df) %>% 
  as.matrix(.)


# Yay igraph!

cgr <- graph.edgelist(net_df)

cgr_adj <- get.adjacency(cgr) %>% 
  graph.adjacency(., mode='upper', weighted=TRUE)

evs <- evcent(cgr)$vector

# Create raw edgelist

edge <- data.frame(cbind(get.edgelist(cgr_adj), 
  round(E(cgr_adj)$weight, 3))) %>% 
  set_names(c('source', 'target', 'value'))

edge$value <- as.numeric(edge$value)

# Create NAMED edgelist
edgen <- data.frame(cbind(get.edgelist(cgr_adj, names=FALSE) -1, 
  round(E(cgr_adj)$weight, 3 ))) %>% 
  set_names(c('source', 'target', 'value'))

# Bind them together, Lord

ed1 <- cbind(edge[,1], edgen[,1])
ed2 <- cbind(edge[,2], edgen[,2])

edc <- rbind(ed1, ed2) %>% 
  unique(.) %>% 
  data.frame(.)  %>% 
  set_names(c('name', 'compid')) 

edc$compid <- as.numeric(as.character(edc$compid))

# Setting up the node size magic
edgeloops <- edge[edge$source == edge$target,]
realedge <- edgen[edgen$source != edgen$target,]

# Getting top 10 links for each composer
edgeslice <- realedge %>% 
  group_by(source) %>% 
  unique(.) %>% 
  arrange(-value)  %>% 
  slice(., 1:10)

edgeslice2 <- realedge %>% 
  group_by(target) %>% 
  unique(.) %>% 
  arrange(-value) %>% 
  slice(., 1:10) %>% 
  set_names(c('target', 'source', 'value'))

# Making sure to get everyone, since some people aren't sources, but are targets
edgeslice2 <- edgeslice2[! edgeslice2$source %in% edgeslice$source, ]

edgeslice <- rbind(edgeslice, edgeslice2)

# This gets the number of times a composer was actually played twice, not just the number of overall plays he/she got

comp_per <- nypnet_mer %>% 
  group_by(cleaname) %>% 
  summarize(count = n(), nprogs = length(unique(progid)))
comp_per <-  transmute(comp_per, playedtwice = count - nprogs, cleaname = cleaname)

comp_per2 <- merge(comp_per, composers, by='cleaname')



# This is probably my favorite innovation - using dplyr to compile the connections list for JSON

clcs_1 <- edgeslice %>% 
  group_by(source)  %>% 
  summarize(t=paste(target,collapse=', ') %>% 
  paste('[', ., ']', sep='') )  %>% 
  data.frame(.)  %>% 
  set_names(c('compid','links'))

clcs_2 <- edgeslice2 %>% 
  group_by(target)  %>% 
  summarize(t=paste(source,collapse=', ') %>% 
  paste('[', ., ']', sep='') )  %>% 
  data.frame(.)  %>% 
  set_names(c('compid','links'))

clcs_2 <- clcs_2[! clcs_2$compid %in% clcs_1$compid, ]


# Tying everything up with a bow

comp_attr <- merge(edc, clcs_1, by = 'compid')
comp_attr2 <- merge(edc, clcs_2, by = 'compid')
comp_attr <- rbind(comp_attr, comp_attr2)

ca_final <- merge(comp_attr, comp_per2, by.x = 'name', by.y = 'cleaname') %>% 
  set_names(c('name', 'compid', 'links', 'loops', 'target'))
ca_final$target <- NULL

ca_final <- arrange(ca_final, compid)

edges <- toJSON(edgeslice, dataframe='rows')

#write(edges, file='~/Desktop/composeredges.json')

nodes  <- toJSON(ca_final,dataframe='rows')
#write(nodes,file="~/Desktop/composernodes.json")

# Write it out, write it out, write it out

outj <- paste('{"nodes":', nodes, ',\n\n"links":', edges, '}', sep='')
outj <- str_replace_all(outj, '\\"\\[', '\\[')
outj <- str_replace_all(outj, '\\]\\"', '\\]')
write(outj, file='~/Desktop/composergraph.json')

# edgeslice$target[!edgeslice$target %in% comp_attr$compid]

# write(toJSON(ca_final$name),file='~/Desktop/composers.json')




# --------------------------------------------------------------------------
# INSTRUMENTS AND COMPOSERS
## This is the code to make a clean CSV for the sunburst diagram (composer_sunburst.R)


# Get unique list of composers
comps <- nyphil[,c('composername', 'cleaname')] %>% unique(.)

sdf_i <- sdf2[, c('soloistinstrument', 'composername')]

sol_name <- merge(sdf_i, comps, by='composername', all.x=TRUE)

topins <- sort(table(sol_name$soloistinstrument), decreasing=TRUE)[1:50] %>% names(.)

sols <- sol_name[sol_name$soloistinstrument %in% topins,]

sols <- sols[, 2:3]

sols <- na.omit(sols)

sols <- sols[!sols$cleaname=='',]

sols <- sols[!is.null(sols)]

sols$soloistinstrument <- unlist(sols$soloistinstrument)
