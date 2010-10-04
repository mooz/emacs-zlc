zlc
===

Provides zsh like completion for minibuffer in Emacs.

![screenshot of zlc](http://github.com/mooz/emacs-zlc/raw/master/images/screenshot.png"Selecting items in the *Completion* buffer")

Install
=======

Place zlc.el into your site-lisp directory.

In you emacs config:

    (require 'zlc)

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
set non-Nil value to the @zlc-select-completion-immediately@.

    (setq zlc-select-completion-immediately t)

selected item's style
---------------------

You can customize the style of the selected item in the *Completions* buffer.

To change style, @M-x customize-face@ and input @zlc-selected-completion-face@.
