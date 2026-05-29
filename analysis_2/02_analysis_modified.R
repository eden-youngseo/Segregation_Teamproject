library(sf)
library(dplyr)

# RSG 패키지 로드
setwd("C:/Users/NA/Desktop/RSG_final/analysis_2")

devtools::load_all("..", quiet = TRUE)

# ── 데이터 로드 ────────────────────────────────────────────────────────────────
data_sf <- readRDS("data_sf.rds") %>%
  st_set_crs(5179) %>%
  filter(!is.na(저소득)) %>%
  arrange(구, 동) %>%
  mutate(unit_id = paste(구, 동, sep = "_"))

cat("분석 대상 행정동 수:", nrow(data_sf), "\n\n")

# 변수 확인
required_cols <- c("저소득", "일반인구_비교집단", "독거_비교집단", "독거_전체", "총인구")
missing_cols <- setdiff(required_cols, names(data_sf))
if (length(missing_cols) > 0) {
  stop("data_sf.rds에 필요한 변수가 없습니다: ", paste(missing_cols, collapse = ", "))
}

# ── 분석 파라미터 ──────────────────────────────────────────────────────────────
BW <- 1500
bw_list <- c(500, 1000, 1500, 2000, 3000)

# ══════════════════════════════════════════════════════════════════════════════
# 1단계: 사회적 고립 측정
# 저소득 독거노인이 전체 인구 안에서 얼마나 자기 집단과 접촉하는지 확인
# ══════════════════════════════════════════════════════════════════════════════
cat("=== 1단계: 사회적 고립 측정 ===\n")
cat("저소득 독거노인 / 전체 인구 기준 (bandwidth =", BW, "m)\n\n")

result_I_global <- RSG_I_Global(
  data      = data_sf,
  group_a   = "저소득",
  total     = "총인구",
  bandwidth = BW,
  verbose   = TRUE
)

result_I_local <- RSG_I_Local(
  data      = data_sf,
  group_a   = "저소득",
  total     = "총인구",
  bandwidth = BW,
  verbose   = FALSE
)

cat("\n[전역 고립지수]\n")
cat("RSG_I_Global =", round(result_I_global$index, 4), "\n")
cat("(0에 가까울수록 전체 인구와 잘 섞임, 1에 가까울수록 고립)\n\n")

local_I <- data.frame(
  구동            = paste0(data_sf$구, " ", data_sf$동),
  Local_Isolation = result_I_local$index
)
top_I <- local_I[order(-local_I$Local_Isolation), ][1:10, ]
cat("[고립지수 상위 10개 행정동]\n")
print(top_I)

# ══════════════════════════════════════════════════════════════════════════════
# 2단계-1: 저소득 독거노인 vs 일반인구
# 일반인구 = 전체 인구 - 저소득 독거노인
# ══════════════════════════════════════════════════════════════════════════════
cat("\n=== 2단계-1: 저소득 독거노인 vs 일반인구 분리 측정 ===\n")
cat("저소득 독거노인 vs 일반인구(전체 인구 - 저소득 독거노인) (bandwidth =", BW, "m)\n\n")

result_D_general_global <- RSG_D_Global(
  data      = data_sf,
  group_a   = "저소득",
  group_b   = "일반인구_비교집단",
  total     = "총인구",
  bandwidth = BW,
  verbose   = TRUE
)

result_D_general_local <- RSG_D_Local(
  data      = data_sf,
  group_a   = "저소득",
  group_b   = "일반인구_비교집단",
  total     = "총인구",
  bandwidth = BW,
  verbose   = FALSE
)

cat("\n[전역 상이지수: 일반인구 비교]\n")
cat("RSG_D_Global =", round(result_D_general_global$index, 4), "\n")

local_D_general <- data.frame(
  구동 = paste0(data_sf$구, " ", data_sf$동),
  Local_Dissimilarity_General = result_D_general_local$index
)
top_D_general <- local_D_general[order(-local_D_general$Local_Dissimilarity_General), ][1:10, ]
cat("[일반인구 비교 상이지수 상위 10개 행정동]\n")
print(top_D_general)

# ══════════════════════════════════════════════════════════════════════════════
# 2단계-2: 저소득 독거노인 vs 비교 독거노인
# 비교 독거노인 = 차상위 독거노인 + 일반 독거노인
# ══════════════════════════════════════════════════════════════════════════════
cat("\n=== 2단계-2: 저소득 독거노인 vs 비교 독거노인 분리 측정 ===\n")
cat("저소득 독거노인 vs 비교 독거노인(차상위+일반 독거노인) (bandwidth =", BW, "m)\n\n")

result_D_elderly_global <- RSG_D_Global(
  data      = data_sf,
  group_a   = "저소득",
  group_b   = "독거_비교집단",
  total     = "독거_전체",
  bandwidth = BW,
  verbose   = TRUE
)

result_D_elderly_local <- RSG_D_Local(
  data      = data_sf,
  group_a   = "저소득",
  group_b   = "독거_비교집단",
  total     = "독거_전체",
  bandwidth = BW,
  verbose   = FALSE
)

cat("\n[전역 상이지수: 비교 독거노인 비교]\n")
cat("RSG_D_Global =", round(result_D_elderly_global$index, 4), "\n")

local_D_elderly <- data.frame(
  구동 = paste0(data_sf$구, " ", data_sf$동),
  Local_Dissimilarity_Elderly = result_D_elderly_local$index
)
top_D_elderly <- local_D_elderly[order(-local_D_elderly$Local_Dissimilarity_Elderly), ][1:10, ]
cat("[비교 독거노인 비교 상이지수 상위 10개 행정동]\n")
print(top_D_elderly)

# ══════════════════════════════════════════════════════════════════════════════
# 보완: NSI
# ══════════════════════════════════════════════════════════════════════════════
cat("\n=== 보완: NSI (분리의 공간적 스케일) ===\n")

result_NSI_general <- RSG_NSI(
  data      = data_sf,
  group_a   = "저소득",
  group_b   = "일반인구_비교집단",
  total     = "총인구",
  bandwidth = bw_list,
  verbose   = FALSE
)

result_NSI_elderly <- RSG_NSI(
  data      = data_sf,
  group_a   = "저소득",
  group_b   = "독거_비교집단",
  total     = "독거_전체",
  bandwidth = bw_list,
  verbose   = FALSE
)

cat("[일반인구 비교 NSI]\n")
print(data.frame(bandwidth_m = bw_list, NSI = round(result_NSI_general$index, 4)))

cat("[비교 독거노인 비교 NSI]\n")
print(data.frame(bandwidth_m = bw_list, NSI = round(result_NSI_elderly$index, 4)))

# ══════════════════════════════════════════════════════════════════════════════
# 결과 저장
# 기존 03/04 코드와 호환되도록 D_global, D_local, NSI는 '비교 독거노인' 기준으로도 저장
# ══════════════════════════════════════════════════════════════════════════════
results <- list(
  I_global = result_I_global,
  I_local  = result_I_local,

  D_general_global = result_D_general_global,
  D_general_local  = result_D_general_local,
  NSI_general      = result_NSI_general,

  D_elderly_global = result_D_elderly_global,
  D_elderly_local  = result_D_elderly_local,
  NSI_elderly      = result_NSI_elderly,

  # 기존 코드 호환용: 독거노인 내부 비교 기준
  D_global = result_D_elderly_global,
  D_local  = result_D_elderly_local,
  NSI      = result_NSI_elderly,

  data_sf = data_sf
)

saveRDS(results, "results.rds")
cat("\nresults.rds 저장 완료\n")
