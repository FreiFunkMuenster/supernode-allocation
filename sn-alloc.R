library("jsonlite")

download.file("https://graphite.freifunk-muensterland.de/render/?target=aliasByNode(aliasSub(sumSeriesWithWildcards(gateways.*.l2tp.if_count-br*,%201),%20%27if_count-br%27,%20%27domaene-%27),%202)&format=json&from=NOW-7d&to=NOW", "ffms-l2tp.json")
download.file("https://graphite.freifunk-muensterland.de/render/?width=586&height=308&target=aliasByNode(aliasSub(sumSeriesWithWildcards(perSecond(scale(gateways.*.interface-bat*.if_octets.*,%208)),%201,%204),%20%27interface-bat%27,%20%27domaene-%27),%201)&format=json&from=NOW-7d&to=NOW", "ffms-tx-rx.json")

# read the JSON files and merge the data based on supernode name 
l2tp <- fromJSON("ffms-l2tp.json", flatten=TRUE)
tx.rx <- fromJSON("ffms-tx-rx.json", flatten=TRUE)
jct <- match(l2tp$target, tx.rx$target)

# create data frame with supernode data (name, physical server, relative performance)
sn <- data.frame(
	name=c("remue-08","parad0x","c1024","fanlin","des1","des2","remue-09","nightbounce","corny","handle"),
	srv=c("remue","parad0x","c1024","fanlin","des1","des2","remue","nightbounce","corny","handle"),
	perf=c(0.67,1.0,1.0,1.0,1.0,1.0,0.67,1.0,1.0,0.5)
)

# normalize the performance 
sn$perf = sn$perf/sum(sn$perf)

# add columns to the supernode data to store their available capacity 
sn <- cbind(sn, l2tp=sn$perf, tx.rx=sn$perf)

# create a data frame for the domain list including ther average L2TP connections and total traffic
total <- data.frame(dom=as.character(), l2tp=as.numeric(), tx.rx=as.numeric())

# fill the data frame with domain names, mean L2TP connections and mean traffic
for (i in 1:length(jct)) {
	total <- rbind(total, data.frame(
		dom=l2tp$target[i], 
		l2tp=mean(l2tp$datapoints[[i]][,1], na.rm=TRUE), 
		tx.rx=mean(tx.rx$datapoints[jct[i]][[1]][,1], na.rm=TRUE))
	)
}

# drop entries where the number of L2TP connections is NA
total <- total[!is.na(total$l2tp),]

# scale the L2TP and traffic data to the available supernode capacity
total$l2tp <- total$l2tp/sum(total$l2tp)*sum(sn$perf)/2
total$tx.rx <- total$tx.rx/sum(total$tx.rx)*sum(sn$perf)/2

# add a rank column to the domain list to sort them with respect to L2TP connections and traffic
total <- cbind(total, rank=total$l2tp*total$tx.rx)

total <- total[order(total$rank, decreasing=TRUE),]

# add two columns to the domain list in order to store the assigned supernodes
total <- cbind(total, gw1="", gw2="")
levels(total$gw1) <- levels(sn$name)
levels(total$gw2) <- levels(sn$name)

# assign the supernodes to the domains
# Beginning with the 'largest' domain, the two supernodes with most available capacity are assigned to that domain. 
# If the domain has a higher value for its traffic, the first and third ranked supernode with highest traffic capacity are assigend
# In the case that those two supernodes are located on the same physical server, the first and second supernodes are used.
# If the domain has a higher value for its L2TP connection count, the first and second supernodes with highest L2TP capacity are assigend. 
# If the selected supernodes are located on the same physical server, the first and third ones are used.
# This two disparate methods are necessary to keep the number of similary supernode combinations low.
# LIMITATION: The method needs to be modified if more than two supernodes are located on the same physical server.
for (i in 1:length(total$dom)) {
	if (max(sn$l2tp) < max(sn$tx.rx)) {
		sn <- sn[order(sn$l2tp, decreasing=TRUE),]
		total$gw1[i] <- sn$name[1]
		sn$l2tp[1] <- sn$l2tp[1] - total$l2tp[i]
		sn$tx.rx[1] <- sn$tx.rx[1] - total$tx.rx[i]
		if (sn$srv[1] != sn$srv[3]) {
		total$gw2[i] <- sn$name[3] 
		sn$l2tp[3] <- sn$l2tp[3] - total$l2tp[i]
		sn$tx.rx[3] <- sn$tx.rx[3] - total$tx.rx[i]
		}
		else { total$gw2[i] <- sn$name[2]
		sn$l2tp[2] <- sn$l2tp[2] - total$l2tp[i]
		sn$tx.rx[2] <- sn$tx.rx[2] - total$tx.rx[i]
		}
	}
	else {
		sn <- sn[order(sn$tx.rx, decreasing=TRUE),]
		total$gw1[i] <- sn$name[1]
		sn$l2tp[1] <- sn$l2tp[1] - total$l2tp[i]
		sn$tx.rx[1] <- sn$tx.rx[1] - total$tx.rx[i]
		if (sn$srv[1] != sn$srv[2]) {
		total$gw2[i] <- sn$name[2] 
		sn$l2tp[2] <- sn$l2tp[2] - total$l2tp[i]
		sn$tx.rx[2] <- sn$tx.rx[2] - total$tx.rx[i]
		}
		else { total$gw2[i] <- sn$name[3]
		sn$l2tp[3] <- sn$l2tp[3] - total$l2tp[i]
		sn$tx.rx[3] <- sn$tx.rx[3] - total$tx.rx[i]
		}
	}

}

# write the results into csv files
write.csv2(sn, file="sn-perf.csv")
write.csv2(total, file="sn-alloc.csv")
