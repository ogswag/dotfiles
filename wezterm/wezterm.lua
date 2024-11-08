-- Pull in the wezterm API
local wezterm = require 'wezterm'

-- This will hold the configuration.
local config = wezterm.config_builder()

-- Check if the operating system is macOS
if wezterm.target_triple:find("apple") then
    -- macOS-specific configurations
    config.front_end = 'WebGpu'
else
    -- Configurations for other operating systems
    config.front_end = 'WebGpu'
end

config.font = wezterm.font("FantasqueSansM Nerd Font Mono")
config.font_size = 15

config.enable_tab_bar = false

config.window_decorations = "RESIZE"

config.color_scheme = 'Catppuccin Mocha'

config.cursor_blink_ease_in = 'Constant'
config.cursor_blink_ease_out = 'Constant'
config.default_cursor_style = 'BlinkingBar'
config.cursor_blink_rate = 700

config.enable_kitty_graphics = true     -- Enable Kitty graphics protocol
config.enable_csi_u_key_encoding = true -- Enable CSI u key encoding
config.adjust_window_size_when_changing_font_size = false

-- and finally, return the configuration to wezterm
return config
