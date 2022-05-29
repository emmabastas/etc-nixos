{ vim_configurable
}:

vim_configurable.customize {
  name = "vim";
  wrapGui = true; # This gives us gvim which also adds clipboard support to vim
  vimrcConfig.customRC = ''
    " Relative line numbering
    set number
    set relativenumber

    set showmatch                   " Highlight matching brace
    set hlsearch                    " Highlight all search results
    set ruler                       " Show row and column ruler information
    set undolevels=1000             " Number of undo levels
    set backspace=indent,eol,start  " Backspace behaviour
    syntax on
    colorscheme murphy

    " Tabs and indentation
    set tabstop=4     " tabs are at proper location
    set expandtab     " don't use actual tab character (ctrl-v)
    set shiftwidth=4  " indenting is 2 spaces
    set autoindent    " turns it on
    set smartindent   " does the right thing (mostly) in programs

    " Display tabs
    set list
    set listchars=tab:>-

    " Highlight trailing spaces
    highlight ExtraWhitespace ctermbg=red guibg=red
    match ExtraWhitespace /\s\+$/
  '';
}
