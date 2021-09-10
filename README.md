# vc-got

This is an Emacs VC back-end for the [Game of
Tree](http://gameoftrees.org/) version control system

It's [available on ELPA][elpa]: to install it just `M-x package-install
RET vc-got RET`; vc-got will register itself, there's no need to
`require` it.

[elpa]: http://elpa.gnu.org/packages/vc-got.html


## Drawbacks

While I've been using this backend on a daily basis for the last
months, there are some rough edges.  Fetching updates in particular is
one action that *at the moment* is better to do by hand.  Pushing,
committing, blaming etc on the other hand are fully functional.

Note that pushing requires at least got 0.56, other functionalities
should work with older versions.


## Contributing

vc-got is on ELPA and thus a FSF copyright assignment is required.
It's really easy to get one (just send an email), and having one will
allow you to make further contribution to Emacs itself or to various
other packages.
