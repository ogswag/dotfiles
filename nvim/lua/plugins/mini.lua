return {
    "echasnovski/mini.nvim",
    config = function()
        require("mini.ai").setup()    -- extends Around and Inside actions (a/i+sth)
        require("mini.align").setup() -- align text with ga
        require("mini.operators").setup()
        -- Operators:
        -- Evaluate text and replace with output. (g=)
        -- Exchange text regions. (gx)
        -- Multiply (duplicate) text. (gm)
        -- Replace text with register. (gr)
        -- Sort text. (gs)
        require("mini.surround").setup()  -- surround with sa, sd, sf, sF, sh, sr, sn
        require("mini.bracketed").setup() -- move with [ and ] around different places
        require("mini.move").setup()      -- move any selection with Alt+h/j/k/l
        require("mini.splitjoin").setup() -- toggle split and join with gS
        require("mini.jump2d").setup()    -- jump to any position on screen with pressing Enter
        require("mini.bufremove").setup() -- buffer removing (unshow, delete, wipeout), which saves window layout
    end,
}
