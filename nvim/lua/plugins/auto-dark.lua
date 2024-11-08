return {
    "f-person/auto-dark-mode.nvim",
    opts = {
        update_interval = 1000,
        set_dark_mode = function()
            vim.api.nvim_set_option_value("background", "dark", {})
            vim.cmd("colorscheme catppuccin-mocha")
            vim.cmd.highlight('IndentLine guifg=#45475a')
            -- Current indent line highlight
            vim.cmd.highlight('IndentLineCurrent guifg=#b4befe')
        end,
        set_light_mode = function()
            vim.api.nvim_set_option_value("background", "light", {})
            vim.cmd("colorscheme dayfox")
            vim.cmd.highlight('IndentLine guifg=#D6CDC2')
            -- Current indent line highlight
            vim.cmd.highlight('IndentLineCurrent guifg=#E88624')
        end,
    },
}
