local wezterm = require 'wezterm';
local colors = require('lua/rose-pine').colors()
local window_frame = require('lua/rose-pine').window_frame()

return {
  font = wezterm.font("MonoLisa"),
  font_size = 13,
  color_scheme = "rose-pine",
  colors = colors,
  window_frame = window_frame,
}
