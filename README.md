# vc-got

This is an **experimental** Emacs VC back-end for the [Game of
Tree](http://gameoftrees.org/) version control system.

Use it at your own risk.

------

To start using it, you need to add `Got` to `vc-handled-backends` and
make sure that `vc-got` is within your `load-path`.  You shouldn't
require the library.

```emacs-lisp
(cl-pushnew 'Got vc-handled-backends)
```

With `use-package` something like this should be enough:

```emacs-lisp
(use-package vc-got
  :load-path "/path/to/vc-got/"
  :init (cl-pushnew 'Got vc-handled-backends)
  :defer t)
```
