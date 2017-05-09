rm(list=ls())

# DB stuff
library(RPostgreSQL)
library(dplyr)
library(tidyr)
library(ggplot2)
source("/home/galm/pg_keys.R")
source("functions.R")

drv <- dbDriver("PostgreSQL")

# creates a connection to the postgres database
# note that "con" will be used later in each connection to the database
con <- dbConnect(drv, dbname = "tmv_app",
                 host = "localhost", port = 5432,
                 user = pg_user, password = pg_pw)

fields = c("de","ti","ab")

df <- data.frame(
  s = c(
    "\'.*\'",
    "\'.*(review).*\'",
    "\'.*(systematic review)|(meta.{0,1}anal)|(narrative review)|(evidence synthesis)|(meta.{0,1}ethnography)|(model\\w* inter.{0,1}comparison).*\'",
    "\'.*(systematic review).*\'",
    "\'.*(meta.{0,1}anal).*\'",
    "\'.*(model\\w* inter.{0,1}comparison).*\'",
    "\'.*(narrative review).*\'",   
    "\'.*(evidence synthesis).*\'",
    "\'.*(meta.{0,1}ethnography).*\'"
  ), # these results below were copied from WoS on 24 Apr 2017
  hd_ts = c(
    210055,18929,5800,2280,4329,0,147,56,6
  ),
  hd_ti = c(
    210055,4437,3474,1710,2415,0,31,4,3
  ),
  ed_ts = c(62084,3636,1099,537,554,0,34,22,2),
  ed_ti = c(62084,883,593,380,229,0,6,0,1)
)

fields = c("ti")
df$cc_ti <-  as.numeric(lapply(df$s,wordsearch,fields,con,365,T))
fields = c("de","ti","ab","kwp")
df$cc_ts <-  as.numeric(lapply(df$s,wordsearch,fields,con,365,T))



ldf <- df %>%
  gather(var,n,-s) %>%
  separate(var,c("subj","fields"))

klabels <- c("ALL","review","all\nsubreviews","systematic\nreview","meta-analysis","model\nintercomparison","narrative\nreview","evidence\nsynthesis","meta-ethnography")
ldf$keyword <- factor(ldf$s,levels=df$s,labels=klabels)

ldf$subj <- factor(ldf$subj,levels=c("cc","ed","hd"),
                   labels=c("Climate Change","Testing in Education","Heart Disease"))

ldf$n[ldf$fields=="ts"] <- ldf$n[ldf$fields=="ts"] - ldf$n[ldf$fields=="ti"]

ldf$fields <- factor(ldf$fields,levels=c("ti","ts"),
                   labels=c("Title only","Title Abstract and Keywords"))

ldf <- ldf %>%
  group_by(subj) %>%
  mutate(
    total = max(n),
    p = n/total
  )

# base of graph
p <- ggplot(filter(ldf,keyword!="ALL")) +
  facet_grid(.~subj) +
  scale_alpha_manual(values=c(0.7,1)) +
  theme_bw() +
  theme(
    axis.text.x=element_blank(),
    legend.position="bottom",
    axis.ticks.x=element_blank()
  ) +
  scale_fill_brewer(type="qual",palette=4) +
  guides(fill = guide_legend(title="Keyword",nrow = 1)) +
  guides(alpha =guide_legend(title="Field Match",nrow = 2))

# absolute
p +
  geom_bar(
    aes(keyword,n,alpha=fields,fill=keyword),
    stat="identity",
    colour="grey22"
  ) +
  labs(x="",y="Number of references") +
  ggtitle("Reviews and Systematic Reviews in IPCC References")

# proportional
p +
  geom_bar(
    aes(keyword,p,alpha=fields,fill=keyword),
    stat="identity",
    colour="grey22"
  ) +
  labs(x="",y="Proportion of References") +
  ggtitle("Reviews and Systematic Reviews in Climate Change and other Areas")

ggsave("plots/reviews_domain.png",width=12,height=8)




### Do IPCC stuff

q <- 'SELECT wg,ar_id FROM "scoping_wg"'

IPCC <- dbGetQuery(con, q)

for (s in df$s) {
  IPCC[s] <- apply(IPCC,1,wgrefs,s)
}

IPCCl <- IPCC %>%
  gather(keyword, n,-wg,-ar_id)

IPCCl$keyword <- factor(IPCCl$keyword,levels=df$s,labels=klabels)

IPCCl <- IPCCl %>%
  group_by(wg,ar_id) %>%
  mutate(
    total = max(n)
  ) %>%
  ungroup() %>%
  mutate(
    p = n/total,
    ar_id = paste0("AR",ar_id),
    wg=paste0("WG",wg)
    )

p <- ggplot(filter(IPCCl,keyword!="ALL")) +
  facet_grid(ar_id~wg) +
  theme_bw() +
  theme(
    axis.text.x=element_blank(),
    legend.position="bottom",
    axis.ticks.x=element_blank()
  ) +
  labs(x="",y="Proportion of References") +
  scale_fill_brewer(type="qual",palette=4)+
  ggtitle("Reviews and Systematic Reviews in IPCC References") + 
  guides(fill = guide_legend(title="Keyword",nrow = 1))

p + geom_bar(
  aes(keyword,p,fill=keyword),
  stat="identity",
  colour="grey22"
) 

ggsave("plots/reviews_IPCC.png",width=12,height=8)

p + geom_bar(
  aes(keyword,n,fill=keyword),
  stat="identity",
  colour="grey22"
) + labs(y="Number of References")

q <- paste0('SELECT * FROM "scoping_ipccref"
  INNER JOIN "scoping_ipccref_wg"
             ON ("scoping_ipccref"."id" ="scoping_ipccref_wg"."ipccref_id")
             INNER JOIN "scoping_wg"
             ON ("scoping_ipccref_wg"."wg_id" = "scoping_wg"."id")
             WHERE "scoping_wg"."ar_id" = 5
             AND "scoping_ipccref"."text"::text ~* ',df$s[3])

ar5revs <- data.frame(dbGetQuery(con, q)) %>%
  select(authors,year,text,ar_id,wg,chapter)

write.table(ar5revs,"tables/ar5revs.tsv",sep="\t",row.names = F)



#### All docs
q <- paste0('SELECT "scoping_doc"."UT", "scoping_wc"."oecd", "scoping_wc"."text" as "WC" FROM "scoping_doc"
  INNER JOIN "scoping_doc_query" ON ("scoping_doc"."UT" = "scoping_doc_query"."doc_id") 
  INNER JOIN "scoping_wc_doc" ON ("scoping_doc"."UT" = "scoping_wc_doc"."doc_id") 
  INNER JOIN "scoping_wc" ON ("scoping_wc_doc"."wc_id" = "scoping_wc"."id")
  LEFT OUTER JOIN "scoping_wosarticle" ON ("scoping_doc"."UT" = "scoping_wosarticle"."doc_id") 
  WHERE "scoping_doc_query"."query_id" = ',365)

alldocs <- data.frame(dbGetQuery(con, q)) 

byoecd <- alldocs %>%
  group_by(oecd) %>%
  summarise(n=n())

p1 <- ggplot(alldocs) + geom_bar(aes(oecd)) + theme(axis.text.x=element_text(angle=60,hjust=1))


#### All docs
q <- paste0('SELECT "scoping_doc"."UT", "scoping_doc"."PY", "scoping_wc"."oecd", "scoping_wc"."text" as "WC" FROM "scoping_doc"
  INNER JOIN "scoping_doc_query" ON ("scoping_doc"."UT" = "scoping_doc_query"."doc_id") 
  INNER JOIN "scoping_wc_doc" ON ("scoping_doc"."UT" = "scoping_wc_doc"."doc_id") 
  INNER JOIN "scoping_wc" ON ("scoping_wc_doc"."wc_id" = "scoping_wc"."id")
  LEFT OUTER JOIN "scoping_wosarticle" ON ("scoping_doc"."UT" = "scoping_wosarticle"."doc_id") 
  WHERE "scoping_doc_query"."query_id" = ',365,'
  AND "scoping_wosarticle"."ti"::text ~* ',df$s[3],'
  OR "scoping_wosarticle"."ab"::text ~* ',df$s[3],'
  OR "scoping_wosarticle"."de"::text ~* ',df$s[3],'
  OR "scoping_wosarticle"."kwp"::text ~* ',df$s[3])

allrevs <- data.frame(dbGetQuery(con, q)) 

p2 <- ggplot(allrevs) + geom_bar(aes(oecd)) + theme(axis.text.x=element_text(angle=60,hjust=1))


revPY <- allrevs %>%
  group_by()



png("plots/disciplines.png",width=8,height=5,units="in",res=200)
grid.arrange(p1+ggtitle("All CC WoS"),p2 + ggtitle("Systematic Reviews"),nrow=1)
dev.off()

