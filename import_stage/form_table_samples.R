stmt <- paste0(
	"SELECT distinct(sample_name) FROM tags_recaptures;"
)
sampling <- dbGetQuery(link$conn, stmt)
sampling[['order']] <- as.numeric(sampling$sample_name)

sampling[['start_date']] <- suppressWarnings(parse_date_time(NA, orders='mdyhms'))
sampling[['end_date']] <- suppressWarnings(parse_date_time(NA, orders='mdyhms'))

sampling[['seasonal']] <- FALSE
sampling[['seasonal']][sampling[['sample_name']] %in%  
	c(  "1",  "7",  "8",  "9", 
		 "11", "15", "17", "18",
		 "20", "23", "24", "25", 
		 "27", "30", "31", "32", 
		 "34", "35", "36", "37", 
		 "38", "40", "41", "41.8", 
		 "43", "45", "46", "47", 
		 "48", "49", "50", "51", 
		 "52", "53", "54", "55", 
		 "56", "57", "58", "59", 
		 "60", "61", "62", "63", 
		 "64", "65", "66", "67", 
		 "68", "69", "70", "71", 
		 "72", "73", "74", "75") ] <- TRUE

for (i in 1:nrow(sampling)) {
	stmt <- paste0(
		"SELECT distinct(date) FROM tags_recaptures ",
		"WHERE sample_name = '", sampling[i,'sample_name'], "';"
	)
	### FUCKING DATE PARSING!
	date <- strsplit(x=dbGetQuery(link$conn,stmt)[['date']],"/")
	detection_date <- parse_date_time(x=date, orders='mdyhms') 
	detection_date[detection_date > now()] <- 
		detection_date[detection_date > now()] - years(100)
	sampling[i,'start_date'] <- min(detection_date, na.rm=TRUE)
	sampling[i,'end_date'] <- max(detection_date, na.rm=TRUE)
	if (getOption('verbose',FALSE)) print(sampling[i,])
}

sampling[['start_julian_day']] <- yday(sampling[['start_date']])
sampling[['end_julian_day']] <- yday(sampling[['end_date']])
sampling[['year']] <- factor(year(sampling[['start_date']]))

library(reshape2)
library(ggplot2)
sample_melt <- melt(
	data=sampling[,c('sample_name','order','seasonal','start_julian_day','end_julian_day','year')], 
	id.vars=c('sample_name','order','seasonal','year')
)
ggplot(
	data=sample_melt[sample_melt[['seasonal']],], 
	aes(x=value, y=order, colour=year)
) + geom_point()

sample_number_map <- list(  
	 "1" = 10,
	 "7" = 11,
	 "8" = 12,

	 "9" = 13,
	"11" = 14,
	"15" = 15,
	"17" = 16,

	"18" = 17,
	"20" = 18,
	"23" = 19,
	"24" = 20,

	"25" = 21,
	"27" = 22,
	"30" = 23,
	"31" = 24,

	"32" = 25,
	"34" = 26,
	"35" = 26,
	"36" = 27,
	"37" = 28,
	
	"38" = 29,
	"40" = 30,
	"41" = 31,
	"41.8" = 32,
	
	"43" = 33,
	"45" = 34,
	"46" = 35,
	"47" = 36,
	
	"48" = 37,
	"49" = 38,
	"50" = 39,
	"51" = 40,
	
	"52" = 41,
	"53" = 42,
	"54" = 43,
	"55" = 44,
	
	"56" = 45,
	"57" = 46,
	"58" = 47,
	"59" = 48,
	
	"60" = 49,
	"61" = 50,
	"62" = 51,
	"63" = 52,
	
	"64" = 53,
	"65" = 54,
	"66" = 55,
	"67" = 56,
	
	"68" = 57, 
	"69" = 58,
	"70" = 59)
#	"71",
#	
#	"72",
#	"73",
#	"74",
#	"75")

season_map <- ((unlist(sample_number_map)-1) %% 4)+1

sampling[['season']] <- NA
sampling[['sample_number']] <- NA
for ( i in 1:nrow(sampling)) {
	idx <- as.character(sampling[i,'sample_name'])
	if (idx %in% names(sample_number_map)  ) {
		sampling[i,'sample_number'] <- sample_number_map[[idx]]
		sampling[i,'season'] <- season_map[[idx]]
	}
}

dbWriteTable(conn=link$conn, name='data_sampling',value=sampling,
						 overwrite=TRUE, row.names=FALSE)

## Embarassed to write code like this:  <3 !
sample_name_to_sample_number <- function(sample_name) {
	return(sample_number_map[sample_name])
}
assign(x='sample_number_map', value=unlist(sample_number_map),
			 envir=environment(sample_name_to_sample_number))
## End terrible... <3

## Sampling starts when it starts due to smolt sampling, so it's not
## a year divided into clear quarters!
sn <- sampling[sampling[['seasonal']],]
sn <- sn[order(sn$start_date),]
sn <- aggregate(x=sn$start_julian_day, by=sn['season'], quantile, probs=0.5)
days_of_year_set <- 1:366
breaks <- sn$x
intervals <- list(
	spring = days_of_year_set[
		days_of_year_set >= breaks[1] & days_of_year_set < breaks[2]],
	summer = days_of_year_set[
		days_of_year_set >= breaks[2] & days_of_year_set < breaks[3]],
	autumn = days_of_year_set[
		days_of_year_set >= breaks[3] & days_of_year_set < breaks[4]],
	winter = days_of_year_set[
		days_of_year_set < breaks[1] | days_of_year_set >= breaks[4]]
)
for (i in names(intervals)) {
	intervals[[i]] <- data.frame(intervals[[i]],i)
	names(intervals[[i]]) <- c('day','season_name')
}
intervals <- do.call(what=rbind, args=intervals)
season_map <- intervals[order(intervals[['day']]),]
season_map[['season_number']] <- rep(NA,nrow(season_map))
season_map[season_map[['season_name']] == 'spring','season_number'] <-1
season_map[season_map[['season_name']] == 'summer','season_number'] <-2
season_map[season_map[['season_name']] == 'autumn','season_number'] <-3
season_map[season_map[['season_name']] == 'winter','season_number'] <-4

day_of_year_to_season <- function(day, output='season_name') {
	if (!(output %in% c('season_name','season_number'))) {
		stop("Output type must be 'season_name' or 'season_number'.")}
	if (all(day >= 1 & day <= 366)) {
		return(map[day,output])
	} else {
		stop("Argument 'day' must be (1,366).")
	}
}
assign(x='map', value=season_map)

day_count <- aggregate(season_map[,1], season_map['season_name'], length)
season_duration <- day_count$x
names(season_duration) <- day_count[,1]


	



