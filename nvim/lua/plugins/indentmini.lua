return {
    "nvimdev/indentmini.nvim",
    config = function()
        require("indentmini").setup() -- use default config
        -- Colors are applied automatically based on user-defined highlight groups.
        -- There is no default value.
    end,
}
