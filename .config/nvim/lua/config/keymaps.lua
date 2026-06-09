local map = vim.keymap.set

-- Window navigation (Ctrl+hjkl, VS Code style)
map("n", "<C-h>", "<C-w>h", { desc = "Window left" })
map("n", "<C-j>", "<C-w>j", { desc = "Window down" })
map("n", "<C-k>", "<C-w>k", { desc = "Window up" })
map("n", "<C-l>", "<C-w>l", { desc = "Window right" })

-- Window resize with arrow keys
map("n", "<C-Up>", ":resize -2<CR>", { silent = true, desc = "Shrink height" })
map("n", "<C-Down>", ":resize +2<CR>", { silent = true, desc = "Grow height" })
map("n", "<C-Left>", ":vertical resize -2<CR>", { silent = true, desc = "Shrink width" })
map("n", "<C-Right>", ":vertical resize +2<CR>", { silent = true, desc = "Grow width" })

-- Clear search highlight on Escape
map("n", "<Esc>", "<cmd>nohlsearch<CR>")

-- Stay in indent mode after indenting
map("v", "<", "<gv", { desc = "Outdent" })
map("v", ">", ">gv", { desc = "Indent" })

-- Navigate wrapped lines naturally (useful in LaTeX)
map("n", "j", "v:count == 0 ? 'gj' : 'j'", { expr = true, silent = true })
map("n", "k", "v:count == 0 ? 'gk' : 'k'", { expr = true, silent = true })

-- Buffer navigation
map("n", "<S-h>", "<cmd>bprevious<CR>", { desc = "Prev buffer" })
map("n", "<S-l>", "<cmd>bnext<CR>", { desc = "Next buffer" })
map("n", "<leader>bd", "<cmd>bdelete<CR>", { desc = "Delete buffer" })

-- Save with Ctrl+S (VS Code muscle memory)
map({ "n", "i", "v" }, "<C-s>", "<Esc><cmd>w<CR>", { desc = "Save" })
map({ "n", "i", "v" }, "<C-s>", "<Esc><cmd>w<CR>", { desc = "Save" })

-- Plugin managers
map("n", "<leader>L", "<cmd>Lazy<CR>", { desc = "Lazy (plugins)" })
map("n", "<leader>M", "<cmd>Mason<CR>", { desc = "Mason (LSPs)" })

-- Diagnostics (LSP errors/warnings)
map("n", "<leader>cd", vim.diagnostic.open_float, { desc = "Diagnostic float" })
map("n", "[d", vim.diagnostic.goto_prev, { desc = "Prev diagnostic" })
map("n", "]d", vim.diagnostic.goto_next, { desc = "Next diagnostic" })

-- Center search results
map("n", "n", "nzz")
map("n", "N", "Nzz")

-- Insert-mode line home/end
map("i", "<C-a>", "<C-o>I", { silent = true })
map("i", "<C-e>", "<C-o>A", { silent = true })

-- Insert-mode movement
map("i", "<C-k>", "<Up>", { silent = true })
map("i", "<C-j>", "<Down>", { silent = true })
map("i", "<C-h>", "<Left>", { silent = true })
map("i", "<C-l>", "<Right>", { silent = true })
