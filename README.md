zlc.el
======

Zsh like completion system for Emacs.

![Screenshot of zlc selecting items in the *Completion* buffer](/images/screenshot.png)

Installation
============

From package.el with MELPA repository,

    M-x package-install zlc

Or you can just put zlc.el in your load path somewhere.

Then, put the following lines into you emacs config.

    (require 'zlc)
    (zlc-mode t)

Customization
=============

menu select
-----------

To simulate zsh's `menu select', zlc arranges movement commands for 4 directions.
If you want to use these commands, bind them to certain keys in your emacs config.

    (let ((map minibuffer-local-map))
      ;;; like menu select
      (define-key map (kbd "<down>")  'zlc-select-next-vertical)
      (define-key map (kbd "<up>")    'zlc-select-previous-vertical)
      (define-key map (kbd "<right>") 'zlc-select-next)
      (define-key map (kbd "<left>")  'zlc-select-previous)
    
      ;;; reset selection
      (define-key map (kbd "C-c") 'zlc-reset)
      )

complete immediately
--------------------

If you want zlc to select completion immediately when *Completions* buffer is created,
set non-Nil value to the `zlc-select-completion-immediately`.

    (setq zlc-select-completion-immediately t)

selected item's style
---------------------

You can customize the style of the selected item in the *Completions* buffer.

To change style, `M-x customize-face` and input `zlc-selected-completion-face`.
