local wezterm = require 'wezterm';
local mux = wezterm.mux

wezterm.on("gui-startup", function ()
  local _, _, window = mux.spawn_window{}
  window:gui_window():maximize()
end)

return {
  font = wezterm.font("Cascadia Code", {weight="Regular"}),
  font_size = 15,
  use_fancy_tab_bar = true,
  window_decorations = "RESIZE",
  color_scheme = "Material (base16)"
}
