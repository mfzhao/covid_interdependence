library(ggplot2)
main_theme <- theme_bw()
theme_set(main_theme)

term_labels <- c(
    '`ash_alter_rnstu.r(fit)`' = 'Multi-tile peers (FB)',
    '`fb_alter_btvrc.r(fit)`' = 'Tiles per peer device (FB)',
    '`ash_alter_nhd.r(fit)`' = 'Peer non-home-device fraction (Safegraph)',
    '`log_alter_mcbgv.r(fit)`' = 'Census block group visit per peer (Safegraph)',
    'stay_home.r' = 'Shelter-in-place',
    'ban_gmr.r' = 'Close gyms, movie theaters, restaurants',
    'stay_home.r' = 'Shelter-in-place',
    'alter_sh.r' = "Shelter-in-place (social alters)",
    'geo_alter_sh.r' = "Shelter-in-place (geo alters)",
    'comb_alter_sh.r' = "Shelter-in-place (all alters)",
    'alter_bgmr.r' = "Close gyms, movie theaters, restaurants (social alters)",
    'geo_alter_bgmr.r' = "Close gyms, movie theaters, restaurants (geo alters)",
    'comb_alter_bgmr.r' = "Close gyms, movie theaters, restaurants (all alters)"
)

table_term_labels <- c(
    '`ash_alter_rnstu.r(fit)`' = "Social alters' asinh(NSBTUs)",
    '`fb_alter_btvrc.r(fit)`' = "Social alters' BTVRC",
    '`ash_alter_nhd.r(fit)`' = "Social alters' asinh(NHDF)",
    '`log_alter_mcbgv.r(fit)`' = "Social alters' log(dCBGVs)",
    'stay_home.r' = 'Shelter-in-place',
    'ban_gmr.r' = 'Close gyms, movie theaters, restaurants',
    'stay_home.r' = 'Shelter-in-place',
    'alter_sh.r' = "Shelter-in-place (social alters)",
    'geo_alter_sh.r' = "Shelter-in-place (geo alters)",
    'comb_alter_sh.r' = "Shelter-in-place (all alters)",
    'alter_bgmr.r' = "Close gyms, movie theaters, restaurants (social alters)",
    'geo_alter_bgmr.r' = "Close gyms, movie theaters, restaurants (geo alters)",
    'comb_alter_bgmr.r' = "Close gyms, movie theaters, restaurants (all alters)"
)

term_types <- c(
    'Multi-tile peers (FB)' = 'Peers staying in home location',
    'Tiles per peer device (FB)' = "Reduction in peers' locations visited",
    'Peer non-home-device fraction (Safegraph)' = 'Peers staying in home location',
    'Census block group visit per peer (Safegraph)'  = "Reduction in peers' locations visited"
    )

outcome_labels <- c(
    'asinh_fb_rnstu.r' = 'Multi-tile users (FB)',
    'ash_rnstu.r' = 'Multi-tile users (FB)',
    'asinh_sg_nhd.r' = 'Non-home-device fraction (Safegraph)',
    'ash_nhd.r' = 'Non-home-device fraction (Safegraph)',
    'log_sg_cbgv.r' = 'Census block group visits per device (Safegraph)',
    'log_mcbgv.r' = 'Census block group visits per device (Safegraph)',
    'fb_btvrc.r' = 'Tiles visited per device (FB)'
)

outcome_table_labels <- c(
    'fb_btvrc.r' = 'BTVRC',
    'ash_rnstu.r' = 'asinh(NSBTUs)',
    'ash_nhd.r' = 'asinh(NHDF)',
    'log_mcbgv.r' = 'log(dCBGVs)'
    )

outcome_types <- c(
    'Tiles visited per device (FB)' = 'Locations visited',
    'Census block group visits per device (Safegraph)' = 'Locations visited',
    'Multi-tile users (FB)' = 'Leaving home location',
    'Non-home-device fraction (Safegraph)' = 'Leaving home location'
    )

get_outcome_source <- function(x) {
    ifelse(
    grepl('Safegraph', x),
    "Safegraph",
    "Facebook"
    )
}
outcome_sources <- get_outcome_source(outcome_types)

zero_color <- "red"

outcome_source_colors <- c(
    "Safegraph" = "#dd1c77",
    "Facebook" = "#3182bd"
    )

outcome_source_shapes <- c(
    "Safegraph" = 22,
    "Facebook" = 24
    )

alter_type_colors <- c(
    "alter" = "#3182bd",
    "geo_alter" = "#005a32",
    "same_state" = "#636363",
    "diff_state" = "#810f7c"
    )

alter_type_fills <- c(
    "alter" = "#9ecae1",
    "geo_alter" = "#a1d99b",
    "same_state" = "#bdbdbd",
    "diff_state" = "#c994c7"
    )
