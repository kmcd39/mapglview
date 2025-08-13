#' create.mapglview.popups
#' 
#' Create popups for a mapgl map with all columns except the geometry column.
#' 
#' @param sfx An `sf` object for which to create popups
#' 
#' @export create.mapglview.popups
create.mapglview.popups <- function(sfx) {
  
  # Get all attribute names
  popup <- names(sfx)[!names(sfx) %in% attr(sfx, "sf_column")]
  
  # Create formatted popup content
  popup_template <- paste(
    sprintf("<strong>%s: </strong>{%s}", popup, popup),
    collapse = "<br>"
  )
  
  # Create popup for each feature
  popup_info <-
    glue::glue(popup_template, 
               .envir = as.environment(sfx)
    )
  
  return(popup_info)
}



#' get.mapglview.breaks
#'
#' A flexible function to internally get breaks according to a given style.
#' 
#' @param z A continuous vector from which to generate breaks
#' @inheritParams mapglview
#' 
#' @importFrom mapsf mf_get_breaks
#' 
#' @export get.mapglview.breaks
get.mapglview.breaks <- function(
    z
    ,breaks.method = c("natural", "quantile", "equal")
    ,n.breaks = 4
    ) {
  
  breaks.method <- breaks.method[1]
  
  if(breaks.method %in%
     c("quantile", "equal", "natural")
  ) {
    
    # calculate breaks based on breaks method
    breaks <-
      dplyr::case_when(
        
        breaks.method == "quantile" ~
          unname(quantile(
            z
            ,probs = seq(0, 1,
                         by = 1 / n.breaks)
            ,na.rm = T))
        
        ,breaks.method == "equal" ~
          mapsf::mf_get_breaks(
            z,
            n.breaks, "equal"
          )
        ,breaks.method == "natural" ~
          mapsf::mf_get_breaks(
            z, n.breaks, "kmeans"
            #,warnLargeN = F 
          )
      )
    
  } else {
    warning("`breaks.method` not specified (",
            paste("quantile", "equal", "natural", collapse = ", "),
            ") are implemented; defaulting to natural (kmeans).")
    breaks <-
      mapsf::mf_get_breaks(
        z,
        n.breaks, "kmeans"
      )
  }
  
  return(breaks)
}



#' mapglview
#'
#'
#' @param x An object of class either `maplibregl` or `sf`. If `maplibregl`, it
#'   will add the data specified in `y` to the map; if `x` is `sf`, it will
#'   render a new `maplibre` map with `x` added to it.
#' @param y spatial data to add to the map, if `x` is already a `maplibregl`
#'   object (it's ignored otherwise).
#' @param zcol A column name from the data to interpolate colors across.
#' @param base.map.style style JSON to pass onto `mapgl::maplibre`. Not used if
#'   the function is being used to add data to an existing map.
#'   `mapgl::carto_style("dark-matter")`, `mapgl::carto_style("positron")`, or
#'   other.
#' @param include.legend Boolean to include legend or not; only relevant if
#'   `zcol` is also specified.
#' @param layer.name Name for layer. Shows up in legend title. If `NULL`, the
#'   name of the object being mapped will be used.
#' @param palette A character vector describing color hex codes from which to
#'   construct the color palette. Viridis by default. The number of colors
#'   provided doesn't have to match the number of breaks produced.
#' @param breaks.method How to create breaks in legend & interpolation if `zcol`
#'   is specified and is a continuous variable. One of `natural`, `quantile`, or
#'   `equal`. `natural` uses kmeans clustering.
#' @param n.breaks The number of breaks to include in the legend, for continuous
#'   variables.
#' @param alpha Layer opacity
#' @param size For lines or circles, linewidth or circle radius, respectively.
#'   Ignored for polygons.
#' @param before_id Name of layer to position the new layer ontop of; passed
#'   onto the add layer call.
#'
#' @export mapglview
#' 
mapglview <- function(
    x
    , y = NULL
    , zcol = NULL
    , base.map.style = mapgl::carto_style("dark-matter")
    , include.legend = T
    , layer.name = NULL
    , palette = viridis::viridis(5)
    , breaks.method = c("natural", "quantile", "equal")
    , n.breaks = 4
    #,visual.params = mapglview.defaults #     ,lwd = .1
    ,alpha = .9
    ,size = 5
    ,before_id = "place_country_2"
) {
  
  #browser()
  
  # determine if we're adding to a map or making a new one and begin
  # accordingly; also get name of sf object to name the maplibre layer
  
  if( "maplibregl" %in% class(x) ) { # here, we're starting w a map and adding data
    layer.name <- 
      ifelse( is.null(layer.name)
              ,deparse(substitute(y))[1]
              ,as.character(layer.name)
              )
    m <- x # |> mapgl::set_style(base.map.style)
    x <- y
  } else if("sf" %in% class(x) ) { # here, we're starting w data and creating the map
    layer.name <- 
      ifelse( is.null(layer.name)
               ,deparse(substitute(y))[1]
               ,as.character(layer.name)
      )
    m <- 
      mapgl::maplibre(style = base.map.style) |> 
      mapgl::fit_bounds(x)
  }
  # if x was already a map, and no y was supplied, end here.
  if(is.null(x))
    return(m)
  
  # add column for popup info.
  x$popup <- create.mapglview.popups(x)
  
  ## create interpolator if zcol is specified ##################
  if( is.null(zcol) )
    # (if zcol is not specified, just use the first color of the palette)
    interpolator <- palette[1]
  else {
    z <- dplyr::pull(x, zcol) 
    
    #browser()  
    # categorical z
    if( class(z) %in% c("factor", "character", "logical")) {
      # it seems like mapgl can have trouble w factors/logicals? so switch to character.
      breaks <- sort(unique(z)) |> as.character()
      x <- x |> mutate(!!rlang::sym(zcol) := as.character(!!rlang::sym(zcol)))
      pal <- grDevices::colorRampPalette(palette)(length(breaks))
      interpolator <- 
        mapgl::match_expr(
          column = zcol
          ,values = breaks
          ,stops = pal
          ,default = "#cccccc") 
      
    # continuous z
    } else if( class(z) %in% c("numeric", "integer")) {
      breaks <- get.breaks(z, breaks.method, n.breaks)
      pal <- grDevices::colorRampPalette(palette)(length(breaks))
      interpolator <- 
        mapgl::interpolate(
          column = zcol
          ,values = breaks
          ,stops = pal
          ,na_color = "#cccccc"
        )
      
      # round breaks for legend
      breaks <- breaks |> 
        round(digits = 
                dplyr::case_when(
                  min(breaks, na.rm = T) > 5e3 ~ -3 # to the thousands when the min. value is greater than 5000
                  ,max(breaks, na.rm = T) < 1 ~ 3   # to 2 or 3 decimal places if the maximum value is less than 1...
                  ,.default = 1     # or just 1 decimal otherwise.
                )
        )
    }
  }
  
  
  ## determine geometry and add appropriate layer type ##################
  
  geom_type <- sf::st_geometry_type(x, by_geometry = FALSE)
  #geom_type <- gsub("MULTI", "", geom_type)
  
  ### POLYGONS --> FILL LAYER ######
  if(grepl("POLYGON|COLLECTION|SURFACE|TIN|TRIANGLE", geom_type) ) {
    m <- m |> 
      mapgl::add_fill_layer(
        id = layer.name,
        source = x,
        fill_color = interpolator,
        ,popup = "popup"
        ,tooltip = zcol
        ,before_id = before_id
        ,fill_opacity = alpha
        ,hover_options =
          list( fill_color = "#EE8888" )
        
      )
  ### LINESTRINGS/CURVES --> LINE LAYER ####
  } else if(grepl("STRING|CURVE", geom_type) ) {
    m <- m |> 
      mapgl::add_line_layer(
        id = layer.name,
        source = x,
        line_color = interpolator,
        ,line_width = size # make interpolation based on zoom?
        ,popup = "popup"
        ,tooltip = zcol
        ,before_id = before_id
        ,line_opacity = alpha
        ,hover_options =
          list( line_color = "#EE8888" )
      )
  ### POINTS --> CIRCLE LAYER ####
  } else if(grepl("POINT", geom_type) ) {
    m <- m |> 
      mapgl::add_circle_layer(
        id = layer.name,
        source = x,
        circle_color = interpolator,
        ,circle_radius = size # make interpolation based on zoom?
        ,popup = "popup"
        ,tooltip = zcol
        ,before_id = before_id
        ,circle_opacity = alpha
        ,hover_options =
          list( circle_color = "#EE8888" )
      )
  }
  
  # add a legend if we asked for one AND a zcol was specified
  if(include.legend & !is.null(zcol)) {
    m <- m |> 
      mapgl::add_legend(
        legend_title = layer.name
        ,values = breaks
        ,colors = pal
        ,add = F #
        ,type =
          "categorical" # i think categorical style is more legible even for continuous data.
        #,unique_id = paste0(layer.name, "-legend")
        ,position = "bottom-left"
      )
  }
  
  return(m)
}
