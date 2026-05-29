# 01_data_convert.R — 데이터 형식 변환

if (!requireNamespace("sf", quietly = TRUE)) stop("sf 패키지가 필요합니다.")
if (!requireNamespace("dplyr", quietly = TRUE)) stop("dplyr 패키지가 필요합니다.")

# *** 데이터 로드 ***
segdata <- .rsg_load_segdata("data/segdata.rda")

# *** pattern1, pattern2 인구값 분리 ***
pattern1_mat <- as.data.frame(segdata[, 1:2])
pattern2_mat <- as.data.frame(segdata[, 5:6])

names(pattern1_mat) <- c("minority", "majority")
names(pattern2_mat) <- c("minority", "majority")

# *** 10 x 10 격자 polygon 생성 ***
bbox <- sf::st_bbox(
  c(xmin = 0, ymin = 0, xmax = 10, ymax = 10),
  crs = sf::st_crs(5179)
)

study_area <- sf::st_as_sfc(bbox)

grid <- sf::st_make_grid(
  study_area,
  n = c(10, 10),
  what = "polygons"
)

grid_sf <- sf::st_sf(
  grid_id = seq_along(grid),
  geometry = grid
)

# *** pattern1 sf 생성 ***
attr1 <- data.frame(
  grid_id = seq_len(nrow(pattern1_mat)),
  pattern1_mat
)

pattern1_sf <- dplyr::left_join(grid_sf, attr1, by = "grid_id")
pattern1_sf$total <- pattern1_sf$minority + pattern1_sf$majority

# *** pattern2 sf 생성 ***
attr2 <- data.frame(
  grid_id = seq_len(nrow(pattern2_mat)),
  pattern2_mat
)

pattern2_sf <- dplyr::left_join(grid_sf, attr2, by = "grid_id")
pattern2_sf$total <- pattern2_sf$minority + pattern2_sf$majority

# *** shp 저장 ***
if (!dir.exists("data")) dir.create("data")

sf::st_write(
  pattern1_sf,
  "data/pattern1.shp",
  delete_layer = TRUE,
  quiet = TRUE
)

sf::st_write(
  pattern2_sf,
  "data/pattern2.shp",
  delete_layer = TRUE,
  quiet = TRUE
)

message("SHP 변환 완료: data/pattern1.shp, data/pattern2.shp")
