return {
    "williamboman/mason.nvim",
    dependencies = {
        "williamboman/mason-lspconfig.nvim",
        "WhoIsSethDaniel/mason-tool-installer.nvim",
    },
    config = function()
        require("mason").setup()

        require("mason-lspconfig").setup({
            automatic_installation = true,
            ensure_installed = {
                "cssls",
                "eslint",
                "html",
                "jsonls",
                "ts_ls",
                "pyright",
                "clangd",
                "tailwindcss",
                "lua_ls",
            },
        })

        require("mason-tool-installer").setup({
            ensure_installed = {
                "prettier",
                "stylua", -- lua formatter
                "black",  -- python formatter
                "pylint",
                "eslint_d",
                "clang-format"
            },
        })
    end,
}
