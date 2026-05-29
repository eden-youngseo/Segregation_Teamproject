# 02_example_run.R — 데이터 입력, 측도 계산 및 실행

if (!requireNamespace("sf", quietly = TRUE)) stop("sf 패키지가 필요합니다.")


# 0. 사용자가 수정할 부분

# 입력자료 형식 선택
#input_type <- "matrix"
input_type <- "shp"

#**********************************

# type 1. matrix
matrix_file <- "data/segdata.rda"
matrix_object_name <- "segdata"

pattern1_cols <- c(1, 2)
pattern2_cols <- c(5, 6)

# matrix/data.frame 입력일 때 공간 좌표
xy <- expand.grid(x = 1:10, y = 1:10)

#**********************************

# type 2. shp
shp_pattern1 <- "data/pattern1.shp"
shp_pattern2 <- "data/pattern2.shp"

#**********************************

group_a_col <- "minority"
group_b_col <- "majority"
total_col   <- "total"


# 공통 파라미터
bandwidth <- 2
bw_nsi    <- c(0.5, 1, 1.5, 2, 2.5, 3, 3.5, 4)
bw_s      <- c(0.5, 1, 2, 3, 5, 7)

# PCstar 거리감쇄 파라미터 예제값
d_mid <- c(0.5, 1.5, 2.5, 3.5, 4.5, 5.5, 6.5)
C_obs <- c(2.032, 0.736, 0.568, 0.597, 0.386, 0.558, 0.471)
m_fix <- 0.5
lm_fit <- lm(log10(C_obs) ~ I(d_mid^m_fix))
a_est <- as.numeric(coef(lm_fit)[1])
b_est <- -as.numeric(coef(lm_fit)[2])



make_dist_matrix_from_xy <- function(xy) {
  xy <- as.data.frame(xy)
  as.matrix(stats::dist(xy[, c("x", "y")]))
}

make_input_from_matrix <- function(mat, cols, xy) {
  pop <- as.data.frame(mat[, cols, drop = FALSE])
  names(pop) <- c(group_a_col, group_b_col)
  pop[[total_col]] <- pop[[group_a_col]] + pop[[group_b_col]]

  list(
    data = pop,
    xy = xy,
    dist_matrix = make_dist_matrix_from_xy(xy)
  )
}

make_input_from_shp <- function(path) {
  sf_data <- sf::st_read(path, quiet = TRUE)

  sf_data[[total_col]] <- sf_data[[group_a_col]] + sf_data[[group_b_col]]

  center_xy <- sf::st_coordinates(sf::st_centroid(sf::st_geometry(sf_data)))
  center_xy <- as.data.frame(center_xy)
  names(center_xy)[1:2] <- c("x", "y")

  list(
    data = sf_data,
    xy = center_xy[, c("x", "y")],
    dist_matrix = make_dist_matrix_from_xy(center_xy[, c("x", "y")])
  )
}



run_rsg_indices <- function(input_obj) {
  dat <- input_obj$data
  dist_mat <- input_obj$dist_matrix

  od_mat <- exp(-dist_mat * 0.5)
  diag(od_mat) <- 0

  list(
    RSG_I_Global = RSG_I_Global(
      data = dat,
      group_a = group_a_col,
      total = total_col,
      bandwidth = bandwidth,
      dist_matrix = dist_mat,
      verbose = FALSE
    )$index,

    RSG_I_Local = RSG_I_Local(
      data = dat,
      group_a = group_a_col,
      total = total_col,
      bandwidth = bandwidth,
      dist_matrix = dist_mat,
      verbose = FALSE
    )$index,

    RSG_D_Global = RSG_D_Global(
      data = dat,
      group_a = group_a_col,
      group_b = group_b_col,
      total = total_col,
      bandwidth = bandwidth,
      dist_matrix = dist_mat,
      verbose = FALSE
    )$index,

    RSG_D_Local = RSG_D_Local(
      data = dat,
      group_a = group_a_col,
      group_b = group_b_col,
      total = total_col,
      bandwidth = bandwidth,
      dist_matrix = dist_mat,
      verbose = FALSE
    )$index,

    RSG_E_Global = RSG_E_Global(
      data = dat,
      group_a = group_a_col,
      group_b = group_b_col,
      total = total_col,
      bandwidth = bandwidth,
      dist_matrix = dist_mat,
      verbose = FALSE
    )$index,

    RSG_E_Local = RSG_E_Local(
      data = dat,
      group_a = group_a_col,
      group_b = group_b_col,
      total = total_col,
      bandwidth = bandwidth,
      dist_matrix = dist_mat,
      verbose = FALSE
    )$index,

    RSG_NSI = RSG_NSI(
      data = dat,
      group_a = group_a_col,
      group_b = group_b_col,
      total = total_col,
      bandwidth = bw_nsi,
      dist_matrix = dist_mat,
      verbose = FALSE
    )$index,

    RSG_SP = RSG_SP(
      data = dat,
      group_a = group_a_col,
      group_b = group_b_col,
      total = total_col,
      proximity = "exp",
      dist_matrix = dist_mat,
      verbose = FALSE
    )$index,

    RSG_S = RSG_S(
      data = dat,
      group_a = group_a_col,
      group_b = group_b_col,
      bandwidth = bw_s,
      dist_matrix = dist_mat,
      verbose = FALSE
    )$index,

    RSG_PCstar = RSG_PCstar(
      data = dat,
      group_a = group_a_col,
      group_b = group_b_col,
      total = total_col,
      dist_matrix = dist_mat,
      a_param = a_est,
      b_param = b_est,
      m = m_fix,
      verbose = FALSE
    )$index,

    RSG_E_Activity = RSG_E_Activity(
      data = dat,
      group_a = group_a_col,
      group_b = group_b_col,
      total = total_col,
      od_matrix = od_mat,
      verbose = FALSE
    )$index,

    RSG_FSxPy = RSG_FSxPy(
      data = dat,
      group_a = group_a_col,
      group_b = group_b_col,
      total = total_col,
      od_matrix = od_mat,
      alpha = 0.3,
      beta = 1.0,
      max_level = 6L,
      verbose = FALSE
    )$index,

    RSG_Clustering = RSG_Clustering(
      data = dat,
      group_a = group_a_col,
      total = total_col,
      verbose = FALSE
    )$index,

    RSG_Gini = RSG_Gini(
      data = dat,
      group_a = group_a_col,
      group_b = group_b_col,
      dist_matrix = dist_mat,
      pattern_type = "nearest",
      verbose = FALSE
    )$index
  )
}


# 1. 입력자료 유형 검증

if (input_type == "matrix") {
  env <- new.env()
  load(matrix_file, envir = env)

  if (!exists(matrix_object_name, envir = env)) {
    stop("matrix_object_name에 해당하는 객체가 rda 파일 안에 없습니다.")
  }

  mat <- get(matrix_object_name, envir = env)

  pattern1_input <- make_input_from_matrix(mat, pattern1_cols, xy)
  pattern2_input <- make_input_from_matrix(mat, pattern2_cols, xy)
}

if (input_type == "shp") {
  pattern1_input <- make_input_from_shp(shp_pattern1)
  pattern2_input <- make_input_from_shp(shp_pattern2)
}

if (!input_type %in% c("matrix", "shp")) {
  stop("input_type은 'matrix' 또는 'shp'만 가능합니다.")
}



# 2. 출력


result_index <- list(
  pattern1 = run_rsg_indices(pattern1_input),
  pattern2 = run_rsg_indices(pattern2_input)
)


cat("RSG 측도 계산 결과\n")
cat("입력자료 형식:", input_type, "\n")

print(result_index)
