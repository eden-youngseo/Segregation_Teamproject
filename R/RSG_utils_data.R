# RSG_utils_data.R

# segdata.rda 파일 로드
.rsg_load_segdata <- function(path) {
  env <- new.env()
  load(path, envir = env)
  
  # 불러온 환경에서 segdata 객체 추출
  if (!"segdata" %in% ls(env)) {
    stop("파일 내에 'segdata' 객체가 존재하지 않습니다.")
  }
  return(env$segdata)
}

# n x n 빈 그리드 sf 객체 생성
.rsg_make_grid_sf <- function(n) {
  require(sf)
  
  bbox <- st_bbox(c(xmin = 0, ymin = 0, xmax = n, ymax = n))
  study_area <- st_as_sfc(bbox)
  grid <- st_make_grid(study_area, n = c(n, n), what = "polygons")
  grid_sf <- st_sf(grid_id = 1:length(grid), geometry = grid)
  
  return(grid_sf)
}

# segdata 행렬을 sf 그리드 객체로 변환
.rsg_segdata_to_sf <- function(segdata, col_indices = NULL, col_names = NULL) {
  # 데이터 행 수 기반으로 n 계산 (정사각형 그리드 가정)
  n <- sqrt(nrow(segdata)) 
  grid_sf <- .rsg_make_grid_sf(n)
  
  if (is.null(col_indices)) {
    attr_data <- as.data.frame(segdata)
  } else {
    attr_data <- as.data.frame(segdata[, col_indices, drop = FALSE])
  }
  
  if (!is.null(col_names)) {
    if (length(col_names) != ncol(attr_data)) {
      stop("col_names 길이가 선택된 열 수와 다릅니다.")
    }
    colnames(attr_data) <- col_names
  }
  
  attr_data$grid_id <- 1:nrow(attr_data)
  result_sf <- merge(grid_sf, attr_data, by = "grid_id")
  
  return(result_sf)
}

# n x n 격자의 유클리드 거리 행렬 직접 생성
.rsg_grid_dist_matrix <- function(n) {
  xy <- expand.grid(x = 1:n, y = 1:n)
  return(as.matrix(dist(xy)))
}