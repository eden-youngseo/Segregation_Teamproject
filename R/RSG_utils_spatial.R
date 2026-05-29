# sf 객체나 행렬에서 중심점 좌표 추출
.rsg_get_coords <- function(x) {
  if (inherits(x, "sf")) {
    require(sf)
    geom_type <- unique(as.character(sf::st_geometry_type(x)))
    if (all(geom_type %in% c("POINT", "MULTIPOINT"))) {
      coords <- sf::st_coordinates(x)
    } else {
      coords <- sf::st_coordinates(suppressWarnings(sf::st_centroid(x)))
    }
    return(data.frame(x = coords[, 1], y = coords[, 2]))
  }
  
  xy <- as.data.frame(x)
  return(data.frame(x = as.numeric(xy[[1]]), y = as.numeric(xy[[2]])))
}

# 거리 행렬 생성
.rsg_dist_matrix <- function(data, dist_matrix, n) {
  if (!is.null(dist_matrix)) return(dist_matrix)
  if (is.null(data)) stop("거리 행렬을 계산할 공간 데이터(data)가 필요합니다.")
  
  coords <- .rsg_get_coords(data)
  return(as.matrix(dist(coords)))
}

# 거리 행렬을 Gaussian 가중치 행렬로 변환 및 행 표준화
.rsg_gaussian_weights <- function(dist_mat, bw) {
  if (is.null(bw) || bw <= 0) stop("대역폭(bandwidth)은 양수여야 합니다.")
  W <- exp(-(dist_mat^2) / (2 * bw^2))
  W <- W / rowSums(W)
  return(W)
}

# 로컬 인구 강도 계산 (행렬 연산 최적화)
.rsg_local_intensity <- function(W, pop) {
  return(as.vector(W %*% pop))
}

# OD 행렬 대각 제거 후 행 정규화 (유출량 0인 구역은 균등 가중치 대체)
.rsg_normalize_od <- function(phi) {
  diag(phi) <- 0
  row_sums  <- rowSums(phi)
  zero_rows <- row_sums == 0
  if (any(zero_rows))
    warning(sprintf("구역 %s의 총 유출량이 0입니다. 균등 가중치로 대체합니다.",
                    paste(which(zero_rows), collapse = ", ")))
  row_sums[zero_rows] <- ncol(phi)
  phi[zero_rows, ]    <- 1
  phi / row_sums
}