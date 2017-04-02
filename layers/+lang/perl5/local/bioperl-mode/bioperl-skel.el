;; $Id$

;;
;; Template insertion skeletons for Bioperl minor mode
;;
;; Author: Mark A. Jensen
;; Email : maj -at- fortinbras -dot- us
;;
;; Part of The Documentation Project
;; http://www.bioperl.org/wiki/The_Documentation_Project
;;
;; This file is loaded upon bioperl-mode initialization
;;
;; templates based on bioperl.lisp by Heikki Lehvaslaiho
;;

;; Copyright (C) 2009 Mark A. Jensen

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3 of
;; the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied
;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;; PURPOSE.  See the GNU General Public License for more details.

;; You should have received a copy of the GNU General Public
;; License along with this program; if not, write to the Free
;; Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301 USA

;; For these to work properly, `bioperl-skel-elements' must be called
;; from within the buffer. This is done automatically when
;; bioperl-mode is enabled.

;; A gotcha:
;;
;; There is a funky requirement for `prompt-once', that seems to
;; have to do with the `skeleton-insert' internals.
;;
;; You appear to need to do a print of str before leaving
;; a skeleton or subskeleton, or you get an infinite loop
;;
;; e.g., in `bioperl-array-accessor-skel', we do
;;
;;    ( (prompt-once "Class name of base object: ")
;;      " Returns : An array of " str " objects"
;;      '(setq v1 str) )
;;
;; which completes and makes v1 available in the top-level
;; skeleton, rather than, say,
;;
;;    ( (prompt-once "Class name of base object: ")
;;      '(setq v1 str) )
;;    " Returns : An array of " v1 " objects"
;;
;; which hangs.

(defconst bioperl-skel-revision "$Id$"
  "The revision string of bioperl-skel.el")

(make-variable-buffer-local 'prompt-once-alist)
(defvar prompt-once-alist '()
  "An internal var for the skeleton prompt-once facility.")


(define-skeleton bioperl-insert-class
  "Insert a template for a new BioPerl class/method."
  nil
  bioperl-class-pod-skel)

(define-skeleton bioperl-insert-method-pod
  "Insert a standard pod template for BioPerl methods."
  nil
  bioperl-method-pod-skel)

(define-skeleton bioperl-insert-accessor
  "Insert a standard accessor (getter/setter)."
  nil
  bioperl-accessor-skel)

(define-skeleton bioperl-insert-array-accessor
  "Insert a set of standard object array methods."
  nil
  bioperl-array-accessor-skel)

(define-skeleton bioperl-insert-abstract-method
  "Insert an abstract (\"throw_not_implemented\") method."
  nil
  bioperl-abstract-method-skel)

(defvar bioperl-method-pod-skel
      '((prompt-once "Method name:")
	\N
	\N "=head2 " str "()"
	\N
	\N " Title   : " str
	\N " Usage   : "
	\N " Function: " -
	\N " Returns : "
	\N " Args    : "
	\N
	\N "=cut" \N)
"Skeleton for a basic method pod header."
 )

(defvar bioperl-class-pod-skel
  '(
    (prompt-once "Class name: ")
    "# $Id" "$" \N
    "#" \N
    "# BioPerl module for " str \N
    "#" \N
    "# Please direct questions and support issues to <bioperl-l@bioperl.org>" \N
    "#" \N
    ( (prompt-once "Caretaker: ")
      "# Cared for by " str '(setq v1 str) )
    ( (prompt-once "Email: ")
      " <" str ">" \N '(setq v2 str))
    "#" \N
    "# Copyright " v1 \N
    "#" \N
    "# You may distribute this module under the same terms as perl itself" \N \N
    "# POD documentation - main docs before the code" \N
    \N
    "=head1 NAME" \N
    \N
    str " - " - "DESCRIPTION of Object" \N
    \N
    "=head1 SYNOPSIS" \N
    \N
    "Give standard usage here" \N
    \N
    "=head1 DESCRIPTION" \N
    \N
    "Describe the object here" \N
    \N
    "=head1 FEEDBACK" \N
    \N
    "=head2 Mailing Lists" \N
    \N
    "User feedback is an integral part of the evolution of this and other" \N
    "Bioperl modules. Send your comments and suggestions preferably to" \N
    "the Bioperl mailing list.  Your participation is much appreciated." \N
    \N
    "  bioperl-l@bioperl.org                  - General discussion" \N
    "http://bioperl.org/wiki/Mailing_lists  - About the mailing lists" \N
    \N
    "=head2 Support" \N
    \N
    "Please direct usage questions or support issues to the mailing list:" \N
    \N
    "L<bioperl-l@bioperl.org>" \N
    \N
    "rather than to the module maintainer directly. Many experienced and" \N
    "reponsive experts will be able look at the problem and quickly" \N
    "address it. Please include a thorough description of the problem" \N
    "with code and data examples if at all possible." \N
    \N
    "=head2 Reporting Bugs" \N
    \N
    "Report bugs to the Bioperl bug tracking system to help us keep track" \N
    "of the bugs and their resolution. Bug reports can be submitted via" \N
    "the web:" \N
    \N
    "  http://bugzilla.open-bio.org/" \N
    \N
    "=head1 AUTHOR - " v1 \N
    \N
    "Email " v2 \N
    \N
    "Describe contact details here" \N
    \N
    "=head1 CONTRIBUTORS" \N
    \N
    "Additional contributors names and emails here" \N
    \N
    "=head1 APPENDIX" \N
    \N
    "The rest of the documentation details each of the object methods." \N
    "Internal methods are usually preceded with a _" \N
    \N
    "=cut" \N
    \N
    "# Let the code begin..." \N
    \N \N
    "package " str ";" \n
    "use strict;" \n
    \n
    "# Object preamble - inherits from Bio::Root::Root" \n
    \n
    "use Bio::Root::Root;" \n \n
    \n
    "use base qw(Bio::Root::Root );" \n
    \n
    "=head2 new" \N
    \N
    " Title   : new" \N
    " Usage   : my $obj = new " str "();" \N
    " Function: Builds a new " str " object" \N
    " Returns : an instance of " str \N
    " Args    :" \N
    \N
    "=cut" \N
    \N
    "sub new {" \n
    "my ($class,@args) = @_;" \n
    \n
    "my $self = $class->SUPER::new(@args);" \n
    "return $self;" \n
    "}" > \n
    \N
    "1;")
  "Skeleton for a BioPerl module template." )

(defvar bioperl-accessor-skel
  '(
    (prompt-once "Field name: ")
    "=head2 " str \N
    \N
    " Title   : " str \N
    " Usage   : $obj->" str "($newval)" \N
    " Function: " _ \N
    " Example : " \N
    " Returns : value of " str " (a scalar)" \N
    " Args    : on set, new value (a scalar or undef, optional)" \N
    \N
    "=cut" \N
    \N
    "sub " str " {" \n
    "my $self = shift;" > \n
    \n
    "return $self->{'" str "'} = shift if @_;"
    \n
    "return $self->{'" str "'};" \n
    "}" > \n
    \N)
  "Skeleton for a BioPerl accessor (getter/setter).")

(defvar bioperl-array-accessor-skel
  '(
    (prompt-once "Array base object: ")
    "=head2 get_" str "s" \N
    \N
    " Title   : get_" str "s" \N
    " Usage   : @arr = get_" str "s()" \N
    " Function: Get the list of " str "(s) for this object." \N
    " Example : " \N
    ( (prompt-once "Class name of base object: ")
      " Returns : An array of " str " objects" \N
      '(setq v1 str) )
    " Args    : " \N
    \N
    "=cut" \N
    \N
    "sub get_" str "s{" > \n
    "my $self = shift;" \n
    \n
    "return @{$self->{'_" str "s'}} if exists($self->{'_" str "s'});" \n
    "return ();" \n
    "}" \n
    \n
    "=head2 add_" str \N
    \N
    " Title   : add_" str \N
    " Usage   : " \N
    " Function: Add one or more " str "(s) to this object." \N
    " Example : " \N
    " Returns : " \N
    " Args    : One or more " v1 " objects." \N
    \N
    "=cut" \N
    \N
    "sub add_" str "{" > \n
    "my $self = shift;" \n
    \n
    "$self->{'_" str "s'} = [] unless exists($self->{'_" str "s'});" \n
    "push(@{$self->{'_" str "s'}}, @_);" \n
    "}" \n
    \n
    "=head2 remove_" str "s" \N
    \N
    " Title   : remove_" str "s" \N
    " Usage   : "
    \N
    " Function: Remove all " str "s for this class." \N
    " Example : " \N
    " Returns : The list of previous " str "s as an array of" \N
    "          " v1 " objects." \N
    " Args    : " \N
    \N
    "=cut" \N
    \N
    "sub remove_" str "s{" > \n
    "my $self = shift;" \n
    \n
    "my @arr = $self->get_" str "s();" \n
    "$self->{'_" str "s'} = [];" \n
    "return @arr;" \n
    "}" > \n
    \N)
  "Skeleton for object array get/add/remove methods.")

(defvar bioperl-abstract-method-skel
  '(
    (prompt-once "Method name: ")
    "=head2 " str \N
    \N
    " Title   : " str \N
    " Usage   : " \N
    " Function: " _ \N
    " Example : " \N
    " Returns : " \N
    " Args    : " \N
    \N
    "=cut" \N
    \N
    "sub " str "{" \n
    "my ($self) = @_;" \n
    \n
    "$self->throw_not_implemented();" \n
    "}" > \n
    \N
    )
  "Skeleton for an abstract BioPerl method (for interface classes).")

;;; skeleton helpers

(defun bioperl-skel-elements ()
  "Set some `skeleton-further-elements' for bioperl-skel in the buffer."
  (interactive)
  (setq skeleton-further-elements '( (\N "\n") )))

(defun prompt-once (prom)
  "Use in place of plain interactor string to prompt only once in a skeleton.
Entering a blank value quits the skeleton completely.

The skeleton system default behavior is to recursively insert a
skeleton as long as the user continues to provide input to a
prompt. The recursion ends when an empty string is entered as a
prompt. This is rather irritating when the user expects to make a
single entry and then move on. Using prompt-once as the
INTERACTOR for the skeleton or subskeleton will inhibit the
recursion.

Example:
 (define-skeleton myskel
  \"Insert a pretend skeleton, with prompts\"
  nil ;; important
  '(
    (prompt-once \"Enter froob:\") ;; NO quote
    \"Froob is \" str \\n
    \"Now do a sub-skeleton with prompts...\" \\n
    ( (prompt-once \"Sklarb:\")
      \"Sklarb is \" str \\n )
    \"You've been a wonderful audience. Good night.\" \\n) )"

  (condition-case nil
      (let ( (flag (assoc prom prompt-once-alist) )
	     (inp) )
	(cond
	 ( (not flag)
	  (setq inp (read-string prom))
	  (if (> (length inp) 0)
	      (add-to-list 'prompt-once-alist (cons prom "1"))
	    (if (> (length prompt-once-alist) 0)
		(signal 'quit t)))
	  inp)
	 ( flag
	   (setq prompt-once-alist (delq flag prompt-once-alist))
	   nil)))
    ('quit
     (if (= (length prompt-once-alist) 0)
	 (signal 'quit t)
       (setq prompt-once-alist nil)
       (signal 'quit 'recursive))
)))

(provide 'bioperl-skel)

;;; end bioperl-skel.el
