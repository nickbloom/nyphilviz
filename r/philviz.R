library('httr')
library('XML')
library('igraph')
library('RJSONIO')

source('philviz_functions.R')


# Parses cleaned XML file (see philclean.py)
anybar('yellow')
nyp <- xmlTreeParse('.../nyphil-clean.xml')
nyp <- xmlToList(nyp)
anybar('green')

anybar('yellow')
solist_dfs <- lapply(nyp, function(x) try(nyp_lister(x)))
anybar('green')

anybar('yellow')
nyphil <- lapply(nyp, function(x) try(nyp_lister_nosolo(x)))
anybar('green')

valids <- solist_dfs %>% keep(., ~ is.data.frame(.))

sdf <- bind_rows(valids) %>% data.frame(.) %>% na_omit(.)


valids2 <- nyphil %>% keep(., ~ is.data.frame(.))

nyphil <- bind_rows(valids2) %>% data.frame(.) %>% na_omit(.)

sdf <- apply(sdf, 2, function(x) set_names(x, seq(1:length(x))))

sdf2 <- apply(sdf, 2, function(x) unlist(x))

goodnames <- intersect(names(sdf2[[1]]),names(sdf2[[2]])) %>% intersect(., names(sdf2[[3]]))

sdf3 <- lapply(sdf2, function(x) x[goodnames])

sdff <- as.data.frame(sdf2)
sdff$date <- str_replace_all(sdff$date,'T05:00:00Z','')

sdff$year <- str_extract(sdff$date,'[0-9]{4}')
sdff$month <- str_extract(sdff$date,perl('(?<=-)[0-9]{2}(?=-)'))
sdff$day <- str_extract(sdff$date,perl('[0-9]{2}$'))

sdff$time <- as.factor(sdff$time)

fh <- sdff %>% filter(soloistinstrument=='French Horn',soloistroles=='S')

Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

fhbyyear <- sdff %>% group_by(year) %>% filter(soloistinstrument=='French Horn') %>% summarize(st=n())

showsbyyear <- sdff %>% group_by(year)  %>% summarize(nshows=n())



fhby <- merge(fhbyyear, showsbyyear)
fhby$fhprop <- fhby$st/fhby$nshows

fhp <- ggplot(fhby) + geom_bar(aes(x=year, y=fhprop), stat='identity') + theme_bw() + theme(axis.text.x = element_text(angle = 90,  size=8, vjust=1))

tt <- unlist(sdf$soloistinstrument)

t1 <- unlist(sdf[[1]])

sdf <- sdf[-28,]

t2 <- t$worksinfo[3]

t1 <- t %>% map(., ~ .[grep('soloists', .)])


### INSTRUMENTS AND COMPOSERS


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


#### FULL DATA (NO SOLOISTS)

nyphil$date <- str_replace_all(nyphil$date,'T05:00:00Z','')

nyphil$year <- str_extract(nyphil$date,'[0-9]{4}')
nyphil$month <- str_extract(nyphil$date,perl('(?<=-)[0-9]{2}(?=-)'))
nyphil$day <- str_extract(nyphil$date,perl('[0-9]{2}$'))

nyphil$time <- as.factor(nyphil$time)

namefix <- function(x, patt){
  splits <- str_split(x, patt) %>% unlist(.) %>% str_trim(.)
  if(length(splits)>1){
    newn <- paste(rev(splits), collapse=' ') %>% str_replace_all(., perl('[ ]{2,100}'), ' ')
  } else{
    newn <- str_replace_all(x, patt, '') %>% str_trim(.)
    newn <- str_replace_all(newn, perl('\\[.+?\\]'), '')
  }
  newn <- str_replace_all(newn, perl('\\[.+?\\]'), '') %>% str_trim(.)
  newn
}

nyphil <- nyphil[!is.na(nyphil$composername),]
nyphil$cleaname <- nyphil$composername %>% map(., ~ namefix(., ',')) %>% unlist(.)
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


compcount <- nyphil %>% group_by(cleaname) %>% summarize(count=n()) %>% data.frame(.) %>% arrange(-count)
compcount <- compcount[1:250,]
compcount2 <- compcount[1:26,]
compcount2 <- compcount2[!compcount2$cleaname=='Anthem',]
compcount2 <- compcount2[!compcount2$cleaname=='Traditional',]

nypnet <- nyphil[nyphil$cleaname %in% compcount$cleaname,]




composers <- data.frame(cleaname = unique(nypnet$cleaname))


composers$compid <- seq(1,nrow(composers))

nypnet_mer <- merge(nypnet, composers, by='cleaname')

networkize <- function(x){
  cdf <- expand.grid(nypnet_mer$cleaname[nypnet_mer$progid==x], nypnet_mer$cleaname[nypnet_mer$progid==x])
}

anybar('blue')
net_df  <-  unique(nypnet_mer$progid) %>% map(., ~ networkize(.))
anybar('green')

net_df  <-  bind_rows(net_df) %>% as.matrix(.)

cgr <- graph.edgelist(net_df)

cgr_adj <- get.adjacency(cgr) %>% graph.adjacency(., mode='upper', weighted=TRUE)


evs <- evcent(cgr)$vector

edge <- data.frame(cbind(get.edgelist(cgr_adj), round( E(cgr_adj)$weight, 3 ))) %>% set_names(c('source', 'target', 'value'))
edge$value <- as.numeric(edge$value)

edgen <- data.frame(cbind(get.edgelist(cgr_adj, names=FALSE) -1, round( E(cgr_adj)$weight, 3 ))) %>% set_names(c('source', 'target', 'value'))

ed1 <- cbind(edge[,1], edgen[,1])
ed2 <- cbind(edge[,2], edgen[,2])

edc <- rbind(ed1, ed2) %>% unique(.) %>% data.frame(.)  %>% set_names(c('name', 'compid')) 
edc$compid <- as.numeric(as.character(edc$compid))

edgeloops <- edge[edge$source==edge$target,]
realedge <- edgen[edgen$source!=edgen$target,]

edgeslice <- realedge %>% group_by(source)%>% unique(.) %>% arrange(-value)  %>% slice(., 1:10)

edgeslice2 <- realedge %>% group_by(target)%>% unique(.) %>% arrange(-value) %>% slice(., 1:10) %>% set_names(c('target', 'source', 'value'))

edgeslice2 <- edgeslice2[!edgeslice2$source %in% edgeslice$source,]

edgeslice <- rbind(edgeslice, edgeslice2)

comp_per <- nypnet_mer %>% group_by(cleaname) %>% summarize(count=n(), nprogs = length(unique(progid)))
comp_per <-  transmute(comp_per, playedtwice = count - nprogs, cleaname=cleaname)

comp_per2 <- merge(comp_per, composers, by='cleaname')




clcs_1 <- edgeslice %>% group_by(source)  %>% summarize(t=paste(target,collapse=', ') %>% paste('[', ., ']', sep='') )  %>% data.frame(.)  %>% set_names(c('compid','links'))
clcs_2 <- edgeslice2 %>% group_by(target)  %>% summarize(t=paste(source,collapse=', ') %>% paste('[', ., ']', sep='') )  %>% data.frame(.)  %>% set_names(c('compid','links'))

clcs_2 <- clcs_2[!clcs_2$compid %in% clcs_1$compid,]



comp_attr <- merge(edc, clcs_1, by='compid')
comp_attr2 <- merge(edc, clcs_2, by='compid')
comp_attr <- rbind(comp_attr, comp_attr2)

ca_final <- merge(comp_attr, comp_per2, by.x='name', by.y='cleaname') %>% set_names(c('name', 'compid', 'links', 'loops', 'target'))
ca_final$target <- NULL

ca_final <- arrange(ca_final, compid)


edges <- toJSON(edgeslice, dataframe='rows')

#write(edges, file='~/Desktop/composeredges.json')

nodes  <- toJSON(ca_final,dataframe='rows')
#write(nodes,file="~/Desktop/composernodes.json")

outj <- paste('{"nodes":', nodes, ',\n\n"links":', edges, '}', sep='')
outj <- str_replace_all(outj, '\\"\\[', '\\[')
outj <- str_replace_all(outj, '\\]\\"', '\\]')
write(outj, file='~/Desktop/composergraph.json')

edgeslice$target[!edgeslice$target %in% comp_attr$compid]

write(toJSON(ca_final$name),file='~/Desktop/composers.json')
