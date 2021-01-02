# vc-got

This is an **experimental** Emacs VC back-end for the [Game of
Tree](http://gameoftrees.org/) version control system.

Use it at your own risk.

------

To start using it, you need to add `Got` to `vc-handled-backends` and
make sure that `vc-got` is within your `load-path`.  You shouldn't
require the library.

```emacs-lisp
(add-to-list 'vc-handled-backends 'Got)
```

It's highly recommended to add `".got"` to the list of
`vc-directory-exclusion-list`.

```emacs-lisp
(add-to-list 'vc-directory-exclusion-list ".got")
```

With `use-package` something like this should be enough:

```emacs-lisp
(use-package vc-got
  :load-path "/path/to/vc-got/"
  :defer t
  :init
  (add-to-list 'vc-handled-backends 'Got)
  (add-to-list 'vc-directory-exclusion-list ".got"))
```
