;;; config.el --- Scala Layer configuration File for Spacemacs
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(spacemacs|define-jump-handlers scala-mode)

(defvar scala-enable-eldoc nil
  "If non nil then eldoc-mode is enabled in the scala layer.")

(defvar scala-enable-gtags nil
  "If non nil then gtags is enabled in the scala layer.")

(defvar scala-sbt-window-position nil
  "Where to position the SBT window.
If `nil', just let `sbt-mode' figure it out. If `bottom', make a relatively
small window at the bottom of the frame.")

(defvar scala-auto-insert-asterisk-in-comments nil
  "If non-nil automatically insert leading asterisk in multi-line comments.")

(defconst scala-backends '(scala-ensime scala-metals)
  "Backend server implementation to enable advanced IDE language features")

(defvar scala-backend 'scala-ensime
  "Backend used to trigger IDE language features.
`scala-ensime' or `scala-metals' are currently supported")

(defvar scala-auto-start-backend nil
  "If non nil then ensime/metals will be started when a scala file is opened.")

(defvar scala-auto-treeview t
  "If non-nil automatically show treeview when views are recieved by metals.")
