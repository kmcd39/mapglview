library(tidyverse)
library(sf)

plcs <- tigris::places(state = 44
                       ) |> 
  rename_with(tolower)


plcs


plcs

ri.pts <- osmextract::oe_get(
  place = "us/rhode-island"
  ,layer = "points"
)

ri.pts |> nrow()

# install.packages("mapgl")

m1 <- mapgl::maplibre(
  ) |> 
  mapgl::fit_bounds(plcs) |> 
  mapgl::add_layer(
     type = "line"
    ,id = "plcs"
    ,source = st_boundary(plcs)
  ) |> 
  mapgl::add_circle_layer(
    source = sample_n(ri.pts, 5e2)
    ,id = "ri.pts"
    ,circle_color = "#EE2299"
  )

m1 |> class()



# mapgl::add_layer(
#     source = ri.pts
#   ,id = "ri.pts"
#   ,type = "circle"
#   ,paint = 
#     list("circle_color" = "#EE2299")
#   )


  
ri.pts$geometry |> 
    st_geometry_type() |> levels()

ri.pts |> st_crs()
plcs <- st_transform(plcs, st_crs(ri.pts))

# tmpg <- c(ri.pts$geometry, 
          # plcs$geometry)





mapglview(
  filter(ri.pts , !is.na(man_made) ) |> 
    select(-other_tags)
  ,zcol = "man_made"
)

m <- plcs |> 
  #st_boundary() |> 
  mapglview(
    zcol = "aland"
  ) 
  
m

tmp.pts <- ri.pts |> 
  filter( !is.na(man_made) ) |> 
  select(-other_tags)

mapglview(
  tmp.pts  
    ,zcol = "man_made"
  )
    
m |> 
  mapglview(
  tmp.pts
  ,palette = viridis::plasma(5)
  ,zcol = "man_made"
  )

# scratch -----------------------------------------------------------------

#' mapviews handling
#' 
 
# g = sf::st_geometry(x)
# if (inherits(g, "POINT") |
#     inherits(g, "MULTIPOINT") |
#     inherits(g, "sfc_POINT") |
#     inherits(g, "sfc_MULTIPOINT")) type = "pt"
# if (inherits(g, "LINESTRING") |
#     inherits(g, "MULTILINESTRING") |
#     inherits(g, "sfc_LINESTRING") |
#     inherits(g, "sfc_MULTILINESTRING")) type = "ln"
# if (inherits(g, "POLYGON") |
#     inherits(g, "MULTIPOLYGON") |
#     inherits(g, "sfc_POLYGON") |
#     inherits(g, "sfc_MULTIPOLYGON")) type = "pl"
# if (inherits(g, "sfc_GEOMETRY") |
#     inherits(g, "sfc_GEOMETRYCOLLECTION")) type = "gc" #getGe
# 
