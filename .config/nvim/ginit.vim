vim.api.nvim_create_autocmd("UIEnter", {
  callback = function()
    vim.o.guifont = "Commit Mono:h14"
  end,
})
