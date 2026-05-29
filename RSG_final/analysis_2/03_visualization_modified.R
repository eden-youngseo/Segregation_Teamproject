library(sf)
library(dplyr)
library(ggplot2)
library(scales)
library(RColorBrewer)
library(classInt)

FONT <- "Apple SD Gothic Neo"
setwd("C:/Users/NA/Desktop/RSG_final/analysis_2")

results <- readRDS("results.rds")
devtools::load_all("..", quiet = TRUE)
data_sf <- results$data_sf %>% st_set_crs(5179) %>% filter(!is.na(저소득))
data_sf$Local_Isolation     <- results$I_local$index
data_sf$Local_Dissimilarity <- results$D_elderly_local$index
data_sf$Local_Dissimilarity_General <- results$D_general_local$index
data_sf$Local_Dissimilarity_Elderly <- results$D_elderly_local$index

# ── 공통 유틸 ──────────────────────────────────────────────────────────────────
jenks_cut <- function(x, n = 5) {
  cut(x, breaks = classIntervals(x, n = n, style = "jenks")$brks, include.lowest = TRUE)
}

theme_choropleth <- function(title, subtitle) {
  list(
    theme_void(base_family = FONT),
    theme(
      plot.title      = element_text(size = 12, face = "bold", hjust = 0.5, margin = margin(b = 3)),
      plot.subtitle   = element_text(size = 8.5, hjust = 0.5, color = "grey40", margin = margin(b = 5)),
      legend.position = "right",
      legend.title    = element_text(size = 8.5, family = FONT),
      legend.text     = element_text(size = 7.5, family = FONT),
      plot.background = element_rect(fill = "white", color = NA),
      plot.margin     = margin(10, 10, 10, 10)
    ),
    labs(title = title, subtitle = subtitle)
  )
}

# ── Map 1: 국지적 고립지수 ─────────────────────────────────────────────────────
data_sf$iso_class <- jenks_cut(data_sf$Local_Isolation)

p_iso <- ggplot(data_sf) +
  geom_sf(aes(fill = iso_class), color = "white", linewidth = 0.08) +
  scale_fill_brewer(palette = "YlOrRd", name = "국지적\n고립지수",
                    labels = c("낮음", "", "중간", "", "높음"), na.value = "grey80") +
  theme_choropleth(
    "1단계: 사회적 고립 (RSG_I_Local)",
    paste0("저소득 독거노인 / 전체 인구 기준 | 전역 고립지수 = ", round(results$I_global$index, 4), " | bandwidth = 1,500 m")
  )

ggsave("map_isolation.png", p_iso, width = 9, height = 8, dpi = 200)
cat("map_isolation.png 저장\n")

# ── Map 2: 국지적 상이지수 (1500m) ────────────────────────────────────────────
data_sf$diss_class <- jenks_cut(data_sf$Local_Dissimilarity)

p_diss <- ggplot(data_sf) +
  geom_sf(aes(fill = diss_class), color = "white", linewidth = 0.08) +
  scale_fill_brewer(palette = "PuRd", name = "국지적\n상이지수",
                    labels = c("낮음", "", "중간", "", "높음"), na.value = "grey80") +
  theme_choropleth(
    "2단계: 소득 기반 분리 (RSG_D_Local)",
    paste0("저소득 독거노인 vs 비교 독거노인(차상위+일반) | 전역 상이지수 = ", round(results$D_elderly_global$index, 4), " | bandwidth = 1,500 m")
  )

ggsave("map_dissimilarity.png", p_diss, width = 9, height = 8, dpi = 200)
cat("map_dissimilarity.png 저장\n")

# ── Map 2-1: 국지적 상이지수 — 일반인구 비교 ───────────────────────────────────
data_sf$diss_general_class <- jenks_cut(data_sf$Local_Dissimilarity_General)

p_diss_general <- ggplot(data_sf) +
  geom_sf(aes(fill = diss_general_class), color = "white", linewidth = 0.08) +
  scale_fill_brewer(palette = "PuRd", name = "국지적\n상이지수",
                    labels = c("낮음", "", "중간", "", "높음"), na.value = "grey80") +
  theme_choropleth(
    "2-1단계: 저소득 독거노인 vs 일반인구 (RSG_D_Local)",
    paste0("일반인구 = 전체 인구 - 저소득 독거노인 | 전역 상이지수 = ",
           round(results$D_general_global$index, 4), " | bandwidth = 1,500 m")
  )

ggsave("map_dissimilarity_general.png", p_diss_general, width = 9, height = 8, dpi = 200)
cat("map_dissimilarity_general.png 저장\n")

# ── Combined (1단계 + 2단계 나란히) ────────────────────────────────────────────
place_grob <- function(g, row, col, layout) {
  grid::pushViewport(grid::viewport(layout.pos.row = row, layout.pos.col = col))
  grid::grid.draw(g)
  grid::popViewport()
}

png("map_combined.png", width = 1800, height = 820, res = 150)
grid::grid.newpage()
grid::pushViewport(grid::viewport(layout = grid::grid.layout(1, 2)))
place_grob(ggplotGrob(p_iso),  1, 1)
place_grob(ggplotGrob(p_diss), 1, 2)
dev.off()
cat("map_combined.png 저장\n")

# ── NSI 차트 ──────────────────────────────────────────────────────────────────
nsi_df   <- data.frame(bw = c(500, 1000, 1500, 2000, 3000), NSI = results$NSI$index)
aspatial <- results$NSI$aspatial

p_nsi <- ggplot(nsi_df, aes(x = bw, y = NSI)) +
  geom_hline(yintercept = aspatial, linetype = "dashed", color = "grey60", linewidth = 0.8) +
  annotate("text", x = 500, y = aspatial * 1.07,
           label = paste0("비공간 NSI (기준값) = ", round(aspatial, 4)),
           hjust = 0, size = 3.5, color = "grey50", family = FONT) +
  geom_line(color = "#9E0142", linewidth = 1.3) +
  geom_point(color = "#9E0142", size = 3.5) +
  geom_text(aes(label = round(NSI, 4)), vjust = -1, size = 3.2, color = "#9E0142", family = FONT) +
  scale_x_continuous(breaks = nsi_df$bw, labels = paste0(nsi_df$bw, " m")) +
  scale_y_continuous(limits = c(0, aspatial * 1.35)) +
  labs(title    = "보완: NSI (근린 정렬 지수)",
       subtitle = "bandwidth가 넓어질수록 NSI 감소 → 소득 기반 분리는 광역보다 근린 수준에 집중",
       x = "Bandwidth", y = "NSI") +
  theme_minimal(base_size = 12, base_family = FONT) +
  theme(plot.title       = element_text(face = "bold"),
        plot.subtitle    = element_text(color = "grey40", size = 9),
        panel.grid.minor = element_blank(),
        plot.background  = element_rect(fill = "white", color = NA),
        plot.margin      = margin(12, 16, 12, 12))

ggsave("chart_nsi.png", p_nsi, width = 8, height = 5, dpi = 200)
cat("chart_nsi.png 저장\n")

# ══════════════════════════════════════════════════════════════════════════════
# Bandwidth별 RSG_D_Local 비교 지도 (3 × 2 그리드)
# ══════════════════════════════════════════════════════════════════════════════
cat("\nbandwidth별 RSG_D_Local 계산 중...\n")

coords <- sf::st_coordinates(suppressWarnings(sf::st_centroid(data_sf)))
d_mat  <- as.matrix(dist(coords))

bw_list     <- c(500, 1000, 1500, 2000, 3000)
local_d_all <- vector("list", 5)
global_d    <- numeric(5)

for (i in seq_along(bw_list)) {
  cat(sprintf("  %d m...\n", bw_list[i]))
  r_loc        <- RSG_D_Local(data = data_sf, group_a = "저소득", group_b = "독거_비교집단",
                               total = "독거_전체", bandwidth = bw_list[i], dist_matrix = d_mat)
  r_glob       <- RSG_D_Global(data = data_sf, group_a = "저소득", group_b = "독거_비교집단",
                                total = "독거_전체", bandwidth = bw_list[i], dist_matrix = d_mat)
  local_d_all[[i]] <- r_loc$index
  global_d[i]      <- r_glob$index
}

# 모든 bandwidth에 걸친 동일 색상 척도 → bandwidth 간 절대값 비교 가능
d_max      <- max(sapply(local_d_all, max, na.rm = TRUE))
pur_colors <- brewer.pal(9, "PuRd")

theme_bw <- function() {
  theme_void(base_family = FONT) +
  theme(
    plot.title      = element_text(size = 10, face = "bold", hjust = 0.5, margin = margin(b = 2)),
    plot.subtitle   = element_text(size = 8, hjust = 0.5, color = "grey30", margin = margin(b = 0)),
    legend.position = "none",
    plot.background = element_rect(fill = "white", color = NA),
    plot.margin     = margin(6, 4, 6, 4)
  )
}

# bandwidth 지도 5개
bw_maps <- lapply(seq_along(bw_list), function(i) {
  tmp         <- data_sf
  tmp$local_d <- local_d_all[[i]]
  ggplot(tmp) +
    geom_sf(aes(fill = local_d), color = "white", linewidth = 0.05) +
    scale_fill_gradientn(colors = pur_colors, limits = c(0, d_max), na.value = "grey80") +
    labs(title    = paste0("bandwidth = ", bw_list[i], " m"),
         subtitle = paste0("전역 D = ", round(global_d[i], 4))) +
    theme_bw()
})

# 6번째 패널: bandwidth별 전역 D 꺾은선
d_df <- data.frame(bw = bw_list, D = global_d)

p_d_trend <- ggplot(d_df, aes(x = bw, y = D)) +
  geom_line(color = "#67001F", linewidth = 1.2) +
  geom_point(color = "#67001F", size = 3) +
  geom_text(aes(label = round(D, 4)), vjust = -1, size = 2.8, color = "#67001F", family = FONT) +
  scale_x_continuous(breaks = bw_list, labels = paste0(bw_list, " m")) +
  scale_y_continuous(limits = c(0, max(global_d) * 1.3)) +
  labs(title    = "전역 상이지수 D by Bandwidth",
       subtitle = "bandwidth 증가 → D 감소 (분리는 근린 규모에 집중)",
       x = "Bandwidth", y = "D") +
  theme_minimal(base_size = 9, base_family = FONT) +
  theme(plot.title       = element_text(face = "bold", size = 10),
        plot.subtitle    = element_text(color = "grey40", size = 7.5),
        panel.grid.minor = element_blank(),
        plot.background  = element_rect(fill = "white", color = NA),
        plot.margin      = margin(8, 10, 8, 8))

# 공유 컬러바 추출 — sf 데이터 기반 더미 플롯 사용
p_colorbar <- ggplot(data_sf) +
  geom_sf(aes(fill = Local_Dissimilarity), color = NA) +
  scale_fill_gradientn(
    colors = pur_colors, limits = c(0, d_max), na.value = "grey80",
    name   = "국지적\n상이지수",
    breaks = round(seq(0, d_max, length.out = 4), 3)
  ) +
  theme_void(base_family = FONT) +
  theme(legend.position   = "right",
        legend.title      = element_text(size = 8, family = FONT),
        legend.text       = element_text(size = 7, family = FONT),
        legend.key.height = unit(1.8, "cm"),
        legend.key.width  = unit(0.45, "cm"))

extract_legend <- function(p) {
  g      <- ggplotGrob(p)
  idx    <- which(sapply(g$grobs, function(x) x$name) == "guide-box")
  if (length(idx)) g$grobs[[idx[1]]] else grid::nullGrob()
}
colorbar_grob <- extract_legend(p_colorbar)

# ── 3×2 그리드 조립 ────────────────────────────────────────────────────────────
# 레이아웃: 3열(지도) + 1열(컬러바, 좁음)
#           1행(제목, 얇음) + 2행(지도 위) + 3행(지도 아래)
cat("map_bw_comparison.png 생성 중...\n")
png("map_bw_comparison.png", width = 2700, height = 1800, res = 150)

grid::grid.newpage()
grid::pushViewport(grid::viewport(layout = grid::grid.layout(
  nrow    = 3, ncol = 4,
  widths  = grid::unit(c(1, 1, 1, 0.18), "null"),
  heights = grid::unit(c(0.07, 1, 1),    "null")
)))

# 제목 행 (열 1~4 통합)
grid::pushViewport(grid::viewport(layout.pos.row = 1, layout.pos.col = c(1, 4)))
grid::grid.text(
  "소득 기반 거주지 분리 — bandwidth별 국지적 상이지수 비교 (동일 색상 척도 적용)",
  gp = grid::gpar(fontsize = 13, fontface = "bold", fontfamily = FONT)
)
grid::popViewport()

# 지도 5개 (행2~3, 열1~3)
positions <- list(c(2,1), c(2,2), c(2,3), c(3,1), c(3,2))
for (i in seq_along(bw_maps)) {
  grid::pushViewport(grid::viewport(
    layout.pos.row = positions[[i]][1],
    layout.pos.col = positions[[i]][2]
  ))
  grid::grid.draw(ggplotGrob(bw_maps[[i]]))
  grid::popViewport()
}

# 6번째 셀: 전역 D 꺾은선 (행3, 열3)
grid::pushViewport(grid::viewport(layout.pos.row = 3, layout.pos.col = 3))
grid::grid.draw(ggplotGrob(p_d_trend))
grid::popViewport()

# 컬러바 (행2~3, 열4)
grid::pushViewport(grid::viewport(layout.pos.row = c(2, 3), layout.pos.col = 4))
grid::grid.draw(colorbar_grob)
grid::popViewport()

dev.off()
cat("map_bw_comparison.png 저장 완료\n")
cat("\n=== 출력 파일 ===\n")
cat("  map_isolation.png       — 1단계 국지적 고립지수 지도\n")
cat("  map_dissimilarity.png   — 2단계 국지적 상이지수 지도\n")
cat("  map_combined.png        — 두 지도 나란히\n")
cat("  chart_nsi.png           — NSI bandwidth 차트\n")
cat("  map_bw_comparison.png   — bandwidth별 상이지수 지도 비교\n")
