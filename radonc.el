;;; radonc.el --- Radiotherapy EQD2/BED calculator -*- lexical-binding: t; -*-

;; Author: Paul Martin Putora  <pmp@radonc.at>
;; Maintainer: Paul Martin Putora <pmp@radonc.at>
;; URL: https://github.com/chradonc/radonc.el.git
;; Version: 0.1
;; Package-Requires: ((emacs "26.1"))
;; Keywords: tools, science, radiation oncology, radiobiology, equivalent dose, calculator

;;; Commentary:
;; Provides a EQD2/BED calculator for radiotherapy fractionation
;; schemes. Results are displayed in a buffer with different α/β values.
;;
;; DISCLAIMER:
;; This software is provided “as is”, without warranty of any kind.
;; It is NOT a medical device/product and must not be used for clinical
;; decision making. Verify all results independently.
;;; Code:

(require 'cl-lib)   ;; cl-loop, cl-incf
(require 'subr-x)   ;; string-trim, string-empty-p

(defgroup radonc nil
  "radiation oncology utilities (EQD2/BED)."
  :group 'tools)

(defcustom radonc-eqd2-ab-list '((1.5 . "pink") (2 . "cyan") (3 . "green") (10 . "yellow"))
  "Alist of (α/β . COLOR) used by `radonc-eqd2-calc-colored`."
  :type '(repeat (cons number string))
  :group 'radonc)

(defcustom radonc-eqd2-results-buffer "*EQD2 Results*"
  "Name of the results buffer used by `radonc-eqd2-calc-colored`."
  :type 'string
  :group 'radonc)

;;;###autoload
(defun radonc-eqd2-calc ()
  "BED/EQD2 calculator for any number of fractionation schemes.

Keeps prompting for schemes until you press RET on an empty
\"Number of fractions\" prompt.

Results go to `radonc-eqd2-results-buffer` with a creation timestamp.
Buffer is left editable and shown via `pop-to-buffer`."
  (interactive)
  (let ((ab-list radonc-eqd2-ab-list)
        (bufname radonc-eqd2-results-buffer)
        (schemes nil)
        (i 1))
    ;; Collect schemes until blank input
    (cl-loop
     (let ((s (read-string (format "Scheme %d - Number of fractions (RET to finish): " i))))
       (if (string-empty-p (string-trim s))
           (cl-return)
         (let* ((n (string-to-number (string-trim s)))
                (d (read-number (format "Scheme %d - Dose per fraction (Gy): " i))))
           (push (list i n d) schemes)
           (cl-incf i)))))

    (setq schemes (nreverse schemes))
    (when (null schemes)
      (user-error "No schemes entered"))

    (with-current-buffer (get-buffer-create bufname)
      (setq-local buffer-read-only nil) ;; editable
      (erase-buffer)

      (insert (format "EQD2 / BED Calculator\nCreated: %s\n\n"
                      (format-time-string "%Y-%m-%d %H:%M:%S %Z")))

      (dolist (sch schemes)
        (pcase-let ((`(,idx ,n ,d) sch))
          (let ((D (* n d)))
            (insert (format "Scheme %d\nNumber of fractions: %g\nDose per fraction: %.2f Gy\nTotal Dose: %.2f Gy\n"
                            idx n d D))
            (insert "-------------------------------------\n")
            (dolist (ab ab-list)
              (let* ((alpha-beta (car ab))
                     (color (cdr ab))
                     ;; BED = nd * (1 + d/(α/β))
                     (bed (* D (+ 1.0 (/ (float d) alpha-beta))))
                     ;; EQD2 = BED / (1 + 2/(α/β))
                     (eqd2 (/ bed (+ 1.0 (/ 2.0 alpha-beta))))
                     (start (point)))
                (insert (format "α/β = %g Gy:  BED = %6.2f Gy  |  EQD2 = %6.2f Gy\n"
                                alpha-beta bed eqd2))
                (add-text-properties start (point)
                                     `(face (:foreground ,color)))))
            (insert "\n"))))

      (goto-char (point-min))
      (pop-to-buffer (current-buffer)))))

(provide 'radonc)
;;; radonc.el ends here
