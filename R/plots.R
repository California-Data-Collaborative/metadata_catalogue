

format_query <- function(query_str, params=NULL){
  q <- gsub(pattern='\n',replacement=" ",x=query_str)
  if(!is.null(params)){
    q <- do.call(sprintf, c(list(q),  params))
  }
  q
}

#' Get shapefiles for utilities in SCUBA.
#'
#' @param conn A database connection object.
#'
#' @return tbl with data on utilities for which we have data in SCUBA, 
#' including geometries
#'
#'
#' @export
get_scuba_utility_polygons<- function(conn){
  SCUBA_UTILITY_POLYGONS <- "
  WITH utils AS (
  	SELECT DISTINCT ON (utility_id, utility_name)
      utility_id, utility_name
    FROM cust_loc
  )
  
  SELECT s.*, utils.utility_name 
  FROM supplier_status_table s, utils
  WHERE s.utility_id = utils.utility_id
    AND utils.utility_id is not null
  "
  df <- pgGetGeom(conn, query=SCUBA_UTILITY_POLYGONS)
  # q <- format_query(SCUBA_UTILITY_POLYGONS)
  # query <- dbplyr::build_sql(dplyr::sql(q))
  # 
  # df <- dplyr::tbl(conn, query)
  # df <- dplyr::collect(df)
  df
}



plot_scuba_utilities <- function(lon,lat,zoom){
  # factpal <- colorFactor(topo.colors(nrow(utility_geoms)), utility_geoms$supplier_n)
  
  leaflet(utility_geoms) %>%
    addProviderTiles(providers$CartoDB.Positron) %>%
    addPolygons(fillColor = "#6F90A2",
                #fillColor = ~factpal(supplier_n),  
                color = "black",
                weight = 1, label =  utility_geoms$supplier_n,
                smoothFactor = 0.5,
                opacity = 1.0, fillOpacity = 0.6,
                highlightOptions = highlightOptions(color = "white", weight = 2,
                                                    bringToFront = TRUE)) %>%
    setView(lon,lat,zoom)
}


##' Get summary stats for data quality report
##'
##' @param conn the database connection object
##'
##' @import magrittr
##'
##' @export
get_qc_data <- function(conn){
  qc_query <- "
  WITH dateslist AS (
  SELECT * FROM
  generate_series(2007, date_part('year', current_date)::int) AS usage_year,
  generate_series(1, 12) AS usage_month
  ORDER BY usage_year, usage_month
  ),
  monthly_qc_stats AS (
  SELECT usage_year, usage_month, utility_name,
  sum(usage_ccf) as usage,
  count(distinct cust_loc_id) as cust_count,
  (1.0*sum(CASE WHEN (cust_loc_id IS NULL
  OR cust_loc_id = -99) THEN 1 ELSE 0 END))/count(*) AS nonmatch_ratio
  FROM usage
  GROUP BY usage_year, usage_month, utility_name
  ORDER BY usage_year, usage_month, utility_name
  )
  SELECT d.*, qc.utility_name, qc.usage, qc.cust_count, qc.nonmatch_ratio
  FROM dateslist d LEFT JOIN monthly_qc_stats qc
  ON d.usage_year = qc.usage_year
  AND d.usage_month = qc.usage_month
  "
  
  query <- dbplyr::build_sql(dbplyr::sql(qc_query))
  df <- dplyr::tbl(conn, query)
  df <- dplyr::collect(df) %>%
    dplyr::mutate(month = paste(usage_year, stringr::str_pad(usage_month, 2, pad = "0"), sep="") )
}
