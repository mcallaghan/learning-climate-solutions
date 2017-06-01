rm(list=ls())

library(ggplot2)
library(dplyr)
library(tidyr)
library(stringr)
library(RColorBrewer)

library(RPostgreSQL)

source("/home/galm/pg_keys.R")
source("functions.R")

drv <- dbDriver("PostgreSQL")

# creates a connection to the postgres database
# note that "con" will be used later in each connection to the database
con <- dbConnect(drv, dbname = "tmv_app",
                 host = "localhost", port = 5432,
                 user = pg_user, password = pg_pw)

papers <- data.frame(
  PY = seq(1985,2016)
)

papers$n <- as.numeric(lapply(papers$PY,ppy,con))
papers$query <- "all"

#cut papers by assessment period
papers$AP <- cut(papers$PY,
                 c(0,1985,1990.1,1995.1,2001.1,2007.1,2013.1,Inf),
                 c(NA,"AR1","AR2","AR3","AR4","AR5","AR6")
)

q <- 'SELECT COUNT(DISTINCT "UT") FROM "scoping_doc"
  INNER JOIN "scoping_doc_query" ON ("scoping_doc"."UT" = "scoping_doc_query"."doc_id") 
  WHERE "scoping_doc_query"."query_id" = 365'

total <- as.numeric(dbGetQuery(con, q))


##########################################
## Summarise paper totals by AP
apCounts <- papers %>%
  group_by(AP) %>%
  summarise(
    total = formatC(sum(n),format="d", big.mark=',',preserve.width="none"),
    midY = median(PY),
    maxV = max(n),
    n = sum(n)
  ) %>%
  ungroup() %>%
  filter(AP %in% c("AR1","AR2","AR3","AR4","AR5","AR6")) %>%
  mutate(
    total = paste0("[",total,"]")
  )

IPCC_totals <- data.frame(
  AR = rep(seq(1,5),2),
  AP = rep(c("AR1","AR2","AR3","AR4","AR5"),2),
  syear = rep(c(1985,1990,1995,2001,2007),2),
  contemporary = c(rep("Current Cycle",5),rep("Previous Cycles",5)),
  source = "IPCC"
)

IPCC_totals$n <- as.numeric(apply(IPCC_totals,1,arr,con))

APsums <- apCounts %>%
  select(AP,n) %>%
  mutate(source="WoS",contemporary="Current Cycle")

all_data <- rbind(APsums,select(IPCC_totals,AP,contemporary,n,source))

all_data$source <- factor(all_data$source,levels=c("WoS","IPCC"))

## calculate and plot shares
IPCC_shares <- filter(IPCC_totals,contemporary=="Current Cycle") %>%
  left_join(rename(select(APsums,AP,n),APtotal=n)) %>%
  mutate(share=n/APtotal)

###############################################
## GRAPHS

## Colorscale 
APscale <- brewer.pal(6,"Spectral")

####################
### Figure 1: Growth
ggplot() +
  geom_bar( # Bars for each year, colour coded by AP
    data = filter(papers,PY > 1985 & PY < 2017),
    aes(PY,n,fill=AP),
    stat="identity",
    colour="grey22"
  ) +
  geom_text( # Labels
    data=apCounts,
    aes(label=total,x=midY,y=maxV+1000),
    hjust = 0.5
  ) +
  labs(x="Year",y="Number of Publications") +
  theme_classic() +
  theme(
    text=element_text(size=12),
    legend.position=c(0.1,0.9),
    legend.justification=c(0,1),
    legend.direction="horizontal",
    panel.grid.major.y=element_line(size=0.2,colour="grey22"),
    panel.border=element_rect(size=0.2,colour="grey22",fill=NA)
  ) +
  scale_fill_manual(values=APscale[1:6],name="Assessment Period") +
  scale_x_continuous(breaks = seq(1985,2015,by=5))

ggsave("plots/pubs_time_spectral.png",width=8,height=5)

###################
### Figure 3: WoS vs IPCC

all_data$contemporary <- factor(all_data$contemporary,levels=c("Previous Cycles","Current Cycle"))

mytheme <- theme(
  text=element_text(size=12),
  panel.grid.major.y=element_line(size=0.2,colour="grey22"),
  axis.line.y=element_line(size=0.2,colour="grey22"),
  panel.border=element_rect(size=0.2,colour="grey22",fill=NA),
  strip.background=element_rect(size=0.7,colour="grey22",fill=NA),
  axis.text.x=element_text(angle=90,hjust=1,vjust=0.5),
  axis.title.x=element_blank(),
  plot.margin=unit(c(0.2,0.2,0.2,0.2),"cm")
) 

## Graph bars of IPCC and WoS Refs
bars <-ggplot(filter(all_data,AP!="AR6")) +
  geom_bar(
    aes(source,n,alpha=contemporary,fill=AP),
    stat="identity",
    colour="grey22",
    size=0.1
  ) +
  scale_fill_manual(values=APscale[1:5],name="Assessment Period",guide=FALSE) +
  scale_alpha_manual(values=c(0.4,1),name="")+
  theme_classic() +
  mytheme +
  facet_wrap(~AP,nrow=1) +
  labs(x="",y="Number of Publications")

print(bars)

## Graph IPCC shares of WoS refs

shares <- ggplot(mutate(IPCC_shares,source="WoS")) +
  scale_fill_manual(values=APscale[1:5],name="Assessment Period",guide=FALSE) +
  theme_classic() +
  mytheme +
  theme(
    axis.text.x=element_text(colour=NA,angle=90,hjust=1,vjust=0.5),
    axis.ticks.x=element_line(size=0),
    panel.grid.major.y=element_line(size=0.2,colour="grey22")
  ) +
  facet_wrap(~AP,nrow=1) +
  labs(
    #x="Assessment Period",
    x="",
    y="Publication Share [%]"
  ) +
  geom_bar(
    aes(source,share*100,fill=AP),
    stat="identity",
    colour="grey22",
    size=0.1,
    width=0.8
  )
  
  print(shares)
  print(bars)
  source("functions.R")
  grid_arrange_shared_legend(bars,shares,position="bottom")

shares_lab <- shares + labs(x="Assessment Period") + 
  theme(axis.title.x = element_text(), text=element_text(size=12))
print(shares_lab) 

png("plots/merged_IPCC_spectral.png",width=8,height=5,units="in",res=200)
grid_arrange_shared_legend(bars,shares)
dev.off()

#######################################################
## Proportion of current

current <- IPCC_totals %>%
  spread(contemporary,n) %>%
  mutate(
    total = `Current Cycle` + `Previous Cycles`,
    cpcnt = `Current Cycle` / total
    )

ggplot(current)+
  scale_fill_manual(values=APscale[1:5],name="Assessment Period",guide=FALSE) +
  theme_classic() +
  mytheme +
  theme(
    axis.text.x=element_text(colour=NA,angle=90,hjust=1,vjust=0.5),
    axis.ticks.x=element_line(size=0),
    panel.grid.major.y=element_line(size=0.2,colour="grey22")
  ) +
  facet_wrap(~AP,nrow=1) +
  labs(
    x="Assessment Period",
    x="",
    y="Proportion of References from Curent Period"
  ) +
  geom_bar( 
    aes(1,cpcnt,fill=AP),
    stat="identity",
    colour="grey22",
    size=0.1,
    width=0.8
    )
ggsave("plots/current_trend.png",width=6,height=4,units="in")

#############################################################
## Now Project results

########################################################

papers <- filter(papers,PY<2017) %>% ungroup()

growth <- gRate(mutate(papers,n=records),"query",1990,2016,total=T)

growth <- scimetrix::gRate(mutate(papers,n=records),"query",1990,2016,total=T)

ygrowth <- filter(papers, PY < 2017) %>%
  arrange(PY) %>%
  mutate(pgrowth = (n - lag(n))/lag(n))

future <- select(filter(papers,PY>1985 & query=="all"),PY,n) %>%
  arrange(PY) %>%
  rbind(data.frame(PY=c(2017,2018,2019,2020,2021),n=NA)) %>%
  mutate(lower=NA,upper_1990=NA,upper_2010=NA)

for (i in 1:length(future$PY)) {
  if(!is.na(future$n[i])) {
    future$upper_1990[i]<-future$n[i]
    future$upper_2010[i]<-future$n[i]
    future$lower[i]<-future$n[i]
  } else {
    future$lower[i] <- future$lower[i-1]
    future$upper_1990[i] <- round(future$upper_1990[i-1]*1.16)
    future$upper_2010[i] <- round(future$upper_2010[i-1]*1.14)
  }
}

future_long <- select(future,-upper_1990) %>%
  gather(type,value,-PY)

future_long$AP <- cut(future_long$PY,
                      c(0,1985,1990.1,1995.1,2001.1,2007.1,2013.1,Inf),
                      c(NA,"AR1","AR2","AR3","AR4","AR5","AR6")
)

apCounts <- filter(future_long,type %in% c("lower")) %>%
  group_by(AP) %>%
  summarise(
    total = formatC(sum(value),format="d", big.mark=',',preserve.width="none"),
    midY = median(PY),
    maxV = max(value)
  ) %>%
  ungroup() %>%
  filter(AP %in% c("AR1","AR2","AR3","AR4","AR5","AR6"))

apCounts_upper <- filter(future_long,type %in% c("upper_2010")) %>%
  group_by(AP) %>%
  summarise(
    total = formatC(sum(value),format="d", big.mark=',',preserve.width="none"),
    midY = median(PY),
    maxV = max(value)
  ) %>%
  ungroup() %>%
  filter(AP %in% c("AR1","AR2","AR3","AR4","AR5","AR6"))

apCounts_upper[apCounts_upper$AP=="AR6","total"] = paste0(
  apCounts[apCounts$AP=="AR6","total"],
  " - \n",
  apCounts_upper[apCounts_upper$AP=="AR6","total"]
)

apCounts_upper = apCounts_upper %>%
  mutate(
    total = paste0("[",total,"]")
  )

apCounts_upper[apCounts_upper$AP=="AR6","maxV"] <- apCounts_upper[apCounts_upper$AP=="AR6","maxV"] - 10000
apCounts_upper[apCounts_upper$AP=="AR6","midY"] <- apCounts_upper[apCounts_upper$AP=="AR6","midY"] - 1

#######################################
## plot projections
ggplot(
) +
  geom_bar(
    data = filter(future_long,PY > 1985,type=="lower" ),
    aes(PY,value,fill=AP),
    stat="identity",
    colour="grey22"
  ) +
  geom_bar(
    data = filter(future_long,PY > 1985,type=="upper_2010" ),
    aes(PY,value,fill=AP),
    stat="identity",
    alpha=0.5,
    colour="grey22"
  ) +
  geom_text(
    data=apCounts_upper,
    aes(label=total,x=midY,y=maxV+4200),
    hjust = 0.5
  ) +
  scale_fill_brewer(palette="Spectral",name="Assessment Period") +
  labs(x="Year",y="Publications Count") +
  theme_classic() +
  theme(
    text=element_text(size=12),
    legend.position=c(0.1,0.9),
    legend.justification=c(0,1),
    legend.direction="horizontal",
    panel.grid.major.y=element_line(size=0.2,colour="grey22"),
    panel.border=element_rect(size=0.2,colour="grey22",fill=NA)
  ) + scale_x_continuous(breaks = seq(1985,2020,by=5))

ggsave("plots/pubs_time_projections_2016.png",width=8,height=5)

##############################################################
## Get all the papers and show by OECD cat
q <- paste0('SELECT "scoping_doc"."UT", "scoping_doc"."PY", "scoping_wc"."oecd", "scoping_wc"."oecd_fos_text", "scoping_wc"."text" as "WC" FROM "scoping_doc"
  INNER JOIN "scoping_doc_query" ON ("scoping_doc"."UT" = "scoping_doc_query"."doc_id") 
  INNER JOIN "scoping_wc_doc" ON ("scoping_doc"."UT" = "scoping_wc_doc"."doc_id") 
  INNER JOIN "scoping_wc" ON ("scoping_wc_doc"."wc_id" = "scoping_wc"."id")
  LEFT OUTER JOIN "scoping_wosarticle" ON ("scoping_doc"."UT" = "scoping_wosarticle"."doc_id") 
  WHERE "scoping_doc_query"."query_id" = ',365)

alldocs <- data.frame(dbGetQuery(con, q)) 

alldocs <- filter(alldocs,PY>1985)

pn <- scimetrix::paperNumbers(alldocs,"oecd",graph=T) + scale_x_continuous(breaks = seq(1985,2020,by=5))

shares <- scimetrix::paperShares(alldocs,"oecd","all","line",graph=T)  + 
  scale_x_continuous(breaks = seq(1985,2020,by=5)) +
  labs(y="Publication Share [%]") +
  scale_color_brewer(palette="Set3",name="Subject Area",guide=guide_legend(reverse=F))

shares

png("plots/merged_by_subject.png",width=10,height=6,units="in",res=200)
grid_arrange_shared_legend(pn,shares)
dev.off()

shares <- scimetrix::paperShares(alldocs,"oecd","all","line",graph=F)

stotals <- alldocs %>%
  group_by(oecd) %>%
  summarise(n=n()) %>%
  ungroup() %>%
  mutate(
    total =sum(papers$n),
    pcnt=n/total*100
    )

q <- paste0('SELECT COUNT(DISTINCT("scoping_doc"."UT")) FROM "scoping_doc"
  INNER JOIN "scoping_doc_query" ON ("scoping_doc"."UT" = "scoping_doc_query"."doc_id") 
            INNER JOIN "scoping_wc_doc" ON ("scoping_doc"."UT" = "scoping_wc_doc"."doc_id") 
            INNER JOIN "scoping_wc" ON ("scoping_wc_doc"."wc_id" = "scoping_wc"."id")
            LEFT OUTER JOIN "scoping_wosarticle" ON ("scoping_doc"."UT" = "scoping_wosarticle"."doc_id") 
            WHERE "scoping_wc"."oecd"=\'Social Sciences\' AND "scoping_doc_query"."query_id" = ',365)

stotal <-  as.numeric(data.frame(dbGetQuery(con, q)))

sscience <- alldocs %>%
  filter(oecd=="Social Sciences") %>%
  group_by(oecd_fos_text) %>%
  summarise(n=n()) %>%
  ungroup() %>%
  mutate(
    total = stotal,
    pcnt=n/total*100
  )

