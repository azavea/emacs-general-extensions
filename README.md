emacs-general-extensions
========================
[![Build Status](https://travis-ci.org/azavea/emacs-general-extensions.svg?branch=master)](https://travis-ci.org/azavea/emacs-general-extensions)

An umbrella repository for various emacs lisp extensions to live in until breaking out on their own.

This repository is a work-in-progress, when modules become general enough, they will become their own packages, with documentation. At this time, it's not clear how all the parts will interact, so they'll live in one repo.

requirements
============

emacs 24+. Packages use lexical bindings.

usage
=====

Not much is available at the moment, but webjump scrapers can be tested. For example, To add a dom-scraped completion search of the underscore.js documentation, add the following to your emacs init file:

```
(add-to-list 'load-path "<path-to-repo>/emacs-general-extensions/")
(require 'webjump-scrapers)
(add-to-list 'webjump-sites (wjs/builtins "underscore"))
```

and run: `M-x webjump`, type `underscore` and press enter, press `TAB` to view comptions.
