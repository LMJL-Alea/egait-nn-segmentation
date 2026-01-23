#takes a quaternion times series, and contact points from right / left feet from gaitrite
#transforms the qts columns: from time, w, x, y, z to time, Foot, coord(w,x,y,z), values
add_segmentation_points <- function(
  qts,
  cp_right_stance,
  cp_right_swing,
  cp_left_stance,
  cp_left_swing
) {
  qts |>
    dplyr::mutate(
      ChangePoint = dplyr::case_when(
        time %in% cp_right_stance ~ "Right Heel Strike",
        time %in% cp_right_swing ~ "Right Toe off",
        time %in% cp_left_stance ~ "Left Heel Strike",
        time %in% cp_left_swing ~ "Left Toe off",
        TRUE ~ NA_character_
      )
    ) |>
    tidyr::pivot_longer(
      cols = -c(time, ChangePoint),
      names_to = "coord",
      values_to = "value"
    )
}

#takes a time series, and contact points from right / left feet
#plots four graphs for the four coords, with the contact points in different colors for the different feet
plot_ts <- function(
  ts,
  cp_right_stance,
  cp_right_swing,
  cp_left_stance,
  cp_left_swing
) {
  ts <- add_segmentation_points(
    ts,
    cp_right_stance,
    cp_right_swing,
    cp_left_stance,
    cp_left_swing
  )
  ts |>
    ggplot(aes(x = time, y = value)) +
    geom_line() +
    geom_point(
      data = dplyr::filter(ts, time %in% cp_right_stance),
      mapping = aes(color = ChangePoint),
      size = 2
    ) +
    geom_point(
      data = dplyr::filter(ts, time %in% cp_right_swing),
      mapping = aes(color = ChangePoint),
      size = 2
    ) +
    geom_point(
      data = dplyr::filter(ts, time %in% cp_left_stance),
      mapping = aes(color = ChangePoint),
      size = 2
    ) +
    geom_point(
      data = dplyr::filter(ts, time %in% cp_left_swing),
      mapping = aes(color = ChangePoint),
      size = 2
    ) +
    facet_wrap(vars(coord), scales = "free_y", ncol = 1) +
    theme_bw() +
    labs(x = "Time (s)", y = "", color = "Change Point") +
    theme(legend.position = "top")
}
