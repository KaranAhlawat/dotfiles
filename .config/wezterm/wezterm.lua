local wezterm = require 'wezterm';

return {
  -- font = wezterm.font("SpaceMono Nerd Font"),
  font = wezterm.font("MesloLGS Nerd Font"),
  font_size = 15,
  color_scheme = "Afterglow",
  exit_behavior = "Close",
  window_frame = {
    font = wezterm.font({family="MesloLGS Nerd Font", weight="Regular"}),
    font_size = 10.0,
  },
  enable_tab_bar = true,
  hide_tab_bar_if_only_one_tab = true
}
