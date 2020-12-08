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

### vc-got-stage-mode

`vc-got-stage-mode` is a minor mode to stage individual changes
(currently you can't commit the staged changes).

The Emacs VC system usually operates at a *fileset* level: i.e. it can
commit/rollback/etc sets of file.  Yet, sometimes you may want to
commit only individual changes (eventually from multiple files), and
VC doesn't support this.  This is the motivation behind
`vc-got-stage-mode`.

The following keys are enabled by `vc-got-stage-mode`:

| <kbd>C-c g A</kbd> | Applies (i.e. stage in got) the marked changes |
| <kbd>C-c g b</kbd> | Go to beginning of change |
| <kbd>C-c g e</kbd> | Go to end of change |
| <kbd>C-c g n</kbd> | Go to next change |
| <kbd>C-c g p</kbd> | Go to previous change |
| <kbd>C-c g t</kbd> | Toggle mark |

A change is a set of sequential line added/removed by the diff, it is
a smaller unit than a *hunk*.

The staged changes are indicated by an arrow in the left fringe.
