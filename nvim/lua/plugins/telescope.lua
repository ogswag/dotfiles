return {
    "nvim-telescope/telescope.nvim",
    dependencies = {
        "nvim-tree/nvim-web-devicons",
        "BurntSushi/ripgrep",
        "nvim-lua/plenary.nvim",
        "nvim-telescope/telescope-frecency.nvim",
        "nvim-telescope/telescope-file-browser.nvim",
        "natecraddock/telescope-zf-native.nvim",
        "ghassan0/telescope-glyph.nvim",
        "xiyaowong/telescope-emoji.nvim",
    },
    config = function()
        require("telescope").load_extension("frecency")
        require("telescope").load_extension("file_browser")
        require("telescope").load_extension("zf-native")
        require("telescope").load_extension("glyph")
        require("telescope").load_extension("emoji")
        require('telescope').setup({
            defaults = {
                layout_strategy = 'vertical',
                layout_config = {
                    vertical = {
                        width = 0.5,
                        height = 0.7,
                        preview_cutoff = 120,
                        prompt_position = 'top',
                    },
                },
                sorting_strategy = 'ascending',
            },
            -- other configuration values here
        })
        local builtin = require("telescope.builtin")
        vim.keymap.set("n", "<leader>f", builtin.find_files, { desc = "Telescope find files" })
        vim.keymap.set("n", "<leader>o", ":Telescope file_browser<CR>")
        vim.keymap.set("n", "<leader>r", builtin.oldfiles, { desc = "Recent files" })
        vim.keymap.set("n", "<leader>b", builtin.buffers, { desc = "Telescope buffers" })
        vim.keymap.set("n", "<leader>h", builtin.help_tags, { desc = "Telescope help tags" })
        vim.keymap.set("i", "<C-/>", "<esc>:Telescope current_buffer_fuzzy_find<CR>", {})
        vim.keymap.set("n", "<C-/>", ":Telescope current_buffer_fuzzy_find<CR>", {})
    end,
}
