return {
	{
		"mfussenegger/nvim-lint",
		event = { "BufReadPre", "BufNewFile" },
		dependencies = { "mason-org/mason.nvim" },
		-- :MasonInstall markdownlint-cli2
		config = function()
			local lint = require("lint")
			local config = vim.fn.stdpath("config") .. "/.markdownlint-cli2.jsonc"

			lint.linters_by_ft = {
				markdown = { "markdownlint-cli2" },
			}

			local md = lint.linters["markdownlint-cli2"]
			md.args = { "--config", config, "-" }

			vim.api.nvim_create_autocmd({ "BufReadPost", "BufWritePost", "InsertLeave" }, {
				group = vim.api.nvim_create_augroup("NvimLint", { clear = true }),
				callback = function()
					if vim.bo.filetype == "markdown" and vim.fn.executable("markdownlint-cli2") == 1 then
						lint.try_lint()
					end
				end,
			})
		end,
	},
}
