;; loop-constructs.el --- Sugary looping forms for emacs-lisp
;;
;; Author: Guillaume Marceau
;;
;; Copyright (C) 2002, Guillaume Marceau
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
;;

;;; Commentary:

;; Defines a few sugary loop constructs which claim to be both
;; versatile and seamlessly usable accross both functionnal and
;; imperative styles.
;; Namely :
;;   (spinner ((SYMBOL INIT-VALUE) ...) FORMS)  -  a functionnal loop in a can.
;;   (while-break ((SYMBOL INIT-VALUE) ...) FORMS) - an imperative loop masquerading
;;                                                     as a functionnal expression.
;;   (while-break-n NAME ((SYMBOL INIT-VALUE) ...) FORMS) - a nestable while-break
;;
;; And, taking advantage of the occasion, the sugary `until' is defined :
;;
;;   (until ((SYMBOL INIT-VALUE) ...) BODY)  - never returns nil, by definition
;;
;;

(require 'cl)

(defmacro spinner (loop-name bindings &rest body)
  "Evaluate all the FORM with each SYMBOL initialy bound to the corresponding INIT-VALUE.
Within the FORMs, calling (NAME NEW-VAL...) recursively loop with the SYMBOLs now
bound to the correcponding NEW-VALs. For example :
         (spinner spin ((i 1) (j 1))
           (cond ((> i 5) \"\")
                 ((> j 5) (concat \"\\n\" (spin (1+ i) 1)))
                 (t (concat (format \"(%d %d) \" i j) (spin i (1+ j)))))))

  evaluates to :
         \"(1 1) (1 2) (1 3) 
          (2 1) (2 2) (2 3) 
          (3 1) (3 2) (3 3)\"

NAME is optional, and if omitted, it defaults to `spin'.

  (spinner NAME? ((SYMBOL INIT-VALUE) ...) FORM...)"

  (if (listp loop-name) 
      (progn (setq body (cons bindings body))
             (setq bindings loop-name)
             (setq loop-name 'spin)))
  (let ((values (mapcar #'(lambda (b) (if (consp b) (cadr b) nil)) 
                        bindings))
        (names  (mapcar #'(lambda (b) (if (consp b) (car b)    b)) 
                        bindings)))
    `(labels ((,loop-name ,names ,@body))
       (,loop-name ,@values))))

(defvar while-break-symbol (gensym "while-break-symbol-"))

(defmacro break (value)
  "See `while-break'"
  `(throw ',while-break-symbol (cons ',while-break-symbol ,value)))

(defmacro break-n (loop-name value)
  "See `while-break-n'"
  `(throw ',loop-name (cons ',while-break-symbol ,value)))

(defun while-break-process (loop-name bindings forms)
  (let ((rtn-symbol (gensym "while-break-")))
    `(let (,@bindings ,rtn-symbol)
       (while (progn (setq ,rtn-symbol (catch ',loop-name ,@forms))
                     (if (eq (car-safe ,rtn-symbol) ',while-break-symbol) nil t)))
       (cdr ,rtn-symbol))))

(defmacro while-break (bindings &rest forms)
  "Binds the SYMBOLs to the INIT-VALUEs, then evaluates FORMS
repeatively until a call to (break VALUE). At that point, while-break
returns VALUE. For example:

   (while-break ((i 1)) (if (> i 30) (break i) (setq i (* 2 i))))
   returns 32

   (while-break ((SYMBOL INIT-VALUE) ...) FORMS ...)"
  (while-break-process while-break-symbol bindings forms))

(defmacro while-break-n (loop-name bindings &rest forms)
  "A named version of while-break. Binds the SYMBOLs to the
INIT-VALUEs, then evaluates FORMS repeatively until a call to (break-n
NAME VALUE). At that point, while-break returns VALUE. For example:

   (while-break-n this-loop ((i 1)) (if (> i 30) (break-n this-loop i) (setq i (* 2 i))))
   returns 32

   (while-break-n NAME ((SYMBOL INIT-VALUE) ...) FORMS ...)"
  (while-break-process loop-name bindings forms))

(defmacro until (bindings &rest forms)
  (let ((rtn-symbol (gensym "until-")))
    `(let (,@bindings ,rtn-symbol)
       (while (progn (setq ,rtn-symbol (progn ,@forms))
                     (not ,rtn-symbol)))
       ,rtn-symbol)))


(font-lock-add-keywords 'emacs-lisp-mode '(("\\<\\(spinner\\)\\>" . font-lock-keyword-face)
                                           ("\\<\\(while-break\\)\\>" . font-lock-keyword-face)
                                           ("\\<\\(while-break-n\\)\\>" . font-lock-keyword-face)
                                           ("\\<\\(until\\)\\>" . font-lock-keyword-face)))
(put 'spinner 'lisp-indent-function 1)
(put 'while-break 'lisp-indent-function 1)
(put 'while-break-n 'lisp-indent-function 2)
(put 'break-n 'lisp-indent-function 1)
(put 'until 'lisp-indent-function 1)

(provide 'loop-constructs)


