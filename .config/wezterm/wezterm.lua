local wezterm = require("wezterm")

local function get_appearance()
	if wezterm.gui then
		return wezterm.gui.get_appearance()
	end
	return "Dark"
end

local function scheme_for_appearance(appearance)
	wezterm.log_info(appearance)
	if appearance:find("Dark") then
		return "vscode-dark"
	else
		return "vscode-light"
	end
end

local config = {
	color_scheme = scheme_for_appearance(get_appearance()),
	font = wezterm.font("monospace"),
	hide_tab_bar_if_only_one_tab = true,
}

return config
