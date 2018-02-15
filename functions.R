wordsearch <- function(regex,fields,con,query,count=F) {
  sel <- '"scoping_doc"."id", "scoping_doc"."title", "scoping_doc"."content", "scoping_doc"."PY", "scoping_doc"."source", "scoping_doc"."uploader_id", "scoping_doc"."date"' 
  if (count) {
    sel <- 'COUNT(DISTINCT "scoping_doc"."id")'
  }
  q <- paste0('SELECT ',sel,' FROM "scoping_doc"
  INNER JOIN "scoping_doc_query" ON ("scoping_doc"."id" = "scoping_doc_query"."doc_id") 
  LEFT OidER JOIN "scoping_wosarticle" ON ("scoping_doc"."id" = "scoping_wosarticle"."doc_id") 
  WHERE (("scoping_doc_query"."query_id" = ',query)
  i <- 0
  for (f in fields) {
    i <- i+1
    if (i==1) {
      op <- ' AND '
    } else {
      op <- ' OR '
    }
    q <- paste0(q,op,'"scoping_wosarticle"."',f,'"::text ~* ',regex)
  }
  q <- paste(q,'AND "scoping_doc_query"."query_id" = ',query,'))')
  docs <- dbGetQuery(con, q)
  if (count) {
    docs <- as.numeric(docs)
  }
  return(docs)
}

refsearch <- function(regex,con,count=F) {
  sel <- '*' 
  if (count) {
    sel <- 'COUNT(DISTINCT "scoping_ipccref"."id")'
  }
  q <- paste0('SELECT ',sel,' FROM "scoping_ipccref"
              WHERE ')
  q <- paste0(q,'"scoping_ipccref"."text"::text ~* ',regex)
  docs <- dbGetQuery(con, q)
  if (count) {
    docs <- as.numeric(docs)
  }
  return(docs)
}

wgrefs <- function(wg,regex){
  a <- wg[["ar_id"]]
  w <- wg[["wg"]]
  q = paste0('SELECT COUNT(*) FROM "scoping_ipccref"
  INNER JOIN "scoping_ipccref_wg"
             ON ("scoping_ipccref"."id" ="scoping_ipccref_wg"."ipccref_id")
             INNER JOIN "scoping_wg"
             ON ("scoping_ipccref_wg"."wg_id" = "scoping_wg"."id")
             WHERE "scoping_wg"."wg" = ',w,'AND "scoping_wg"."ar_id" = ',a,'
             AND "scoping_ipccref"."text"::text ~* ',regex)

  return(as.numeric(dbGetQuery(con, q)))
}

arr <- function(a,con){
  print(a)
  q = paste0('SELECT COUNT(*) FROM "scoping_ipccref"
  INNER JOIN "scoping_ipccref_wg"
             ON ("scoping_ipccref"."id" ="scoping_ipccref_wg"."ipccref_id")
             INNER JOIN "scoping_wg"
             ON ("scoping_ipccref_wg"."wg_id" = "scoping_wg"."id")
             WHERE "scoping_wg"."ar_id" = ',a[["AR"]])
  if (a[["contemporary"]]=="Current Cycle") {
    q = paste0(q,'AND "scoping_ipccref"."year" > ', a[["syear"]])
  } else {
    q = paste0(q,'AND "scoping_ipccref"."year" <= ',a[["syear"]])
  }
  
  return(as.numeric(dbGetQuery(con, q)))
}

ppy <- function(y,con){
  q = paste0('SELECT COUNT(DISTINCT "scoping_doc"."id") FROM "scoping_doc"
  INNER JOIN "scoping_doc_query" ON ("scoping_doc"."id" = "scoping_doc_query"."doc_id") 
  WHERE "scoping_doc"."PY" = ',y,'
  AND "scoping_doc_query"."query_id" =',2355)
  
  return(as.numeric(dbGetQuery(con, q)))
}

grid_arrange_shared_legend <- function(..., nrow = 1, ncol = length(list(...)), position = c("bottom", "right")) {
  require(grid)
  require(gridExtra)
  
  plots <- list(...)
  position <- match.arg(position)
  g <- ggplotGrob(plots[[1]] + theme(legend.position = position))$grobs
  legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]
  lheight <- sum(legend$height)
  lwidth <- sum(legend$width)
  gl <- lapply(plots, function(x) x + theme(
    legend.position = "none",  
    panel.grid.major.y=element_line(size=0.2,colour="grey22")#,
    #plot.margin=unit(c(0.2,0.2,2,0.2),"cm")
    ))
  gl <- c(gl, nrow = nrow, ncol = ncol)
  
  combined <- switch(position,
                     "bottom" = arrangeGrob(do.call(arrangeGrob, gl),
                                            legend,
                                            ncol = 1,
                                            heights = unit.c(unit(1, "npc") - lheight, lheight)),
                     "right" = arrangeGrob(do.call(arrangeGrob, gl),
                                           legend,
                                           ncol = 2,
                                           widths = unit.c(unit(1, "npc") - lwidth, lwidth)))
  grid.newpage()
  grid.draw(combined)
  
}

gRate <- function(df,field,y1,y2,total=F) {
  require(slam)
  df[,"z"] <- df[,field]
  df <- df %>%
    ungroup() %>%
    filter(PY >=y1 & PY <=y2 & PY%%5==0 & !is.na(z)) %>%
    group_by(z)
  
  totals <- df %>%
    mutate(
      v1 = n[which.min(PY)],
      v2 = n[which.max(PY)],
      Growth = round(((v2/v1)^(1/(y2-y1))-1)*100) ,
      period=paste0("Total: ",y1,"-",y2)
    ) %>%
    select(period,z,Growth) 
  
  df_sum <- df %>%
    mutate(
      change = n- lag(n),
      Growth = round(((n/lag(n))^(1/(PY-lag(PY)))-1)*100),
      period = paste0(lag(PY),"-",PY)
    ) %>%
    filter(!is.na(change)) %>%
    select(period,z,Growth)
  
  print(df_sum)
  
  totals <- unique(totals) 
  
  
  
  df_merge <- rbind(df_sum,totals) %>%
    spread(z,Growth)
  
  return(df_merge)
}

