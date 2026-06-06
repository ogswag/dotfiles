return {
	{
		"keaising/im-select.nvim",
		config = function()
			require("im_select").setup({
				-- macOS: "com.apple.keylayout.ABC"
				-- Linux (ibus): "keyboard-us"
				-- Linux (fcitx5): "keyboard-us"
				default_im_select = "com.apple.keylayout.US",
				default_command = "im-select", -- or "ibus", "fcitx5-remote"
				-- Restore the default input method state when the following events are triggered
				-- "VimEnter" and "FocusGained" were removed for causing problems, add it by your needs
				set_default_events = { "InsertLeave", "CmdlineEnter" },

				-- Restore the previous used input method state when the following events
				-- are triggered, if you don't want to restore previous used im in Insert mode,
				-- e.g. deprecated `disable_auto_restore = 1`, just let it empty
				-- as `set_previous_events = {}` приветasdf
				set_previous_events = { "InsertEnter", "CmdlineLeave" },

				-- Show notification about how to install executable binary when binary missed
				keep_quiet_on_no_binary = false,
				-- Async run `default_command` to switch IM or not
				async_switch_im = true,
			})
		end,
	},
}
