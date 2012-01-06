#!/usr/bin/sbcl --script
;; Create stand-alone executable of 'cluster'
(load "cluster-dist.lisp")
(sb-ext:save-lisp-and-die "cluster" :executable t :toplevel 'main)