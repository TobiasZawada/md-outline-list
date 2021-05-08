# Fold headings and list items in Markdown mode using the mouse
## Usage/Installation
Put `md-outline-list.el` into your `load-path` and insert the following lines into your user init file:
```emacs-lisp
(autoload 'md-outline-list-mode "md-outline-list")
(add-hook 'markdown-mode-hook #'md-outline-list-mode)
```

If you open a markdown file after that the list bullets and the heading texts are clickable.
Pressing <kbd>&langle;mouse-1&rangle;</kbd> will toggle between folded and unfolded state.
