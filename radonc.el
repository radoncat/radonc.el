;;; radonc.el --- Radiotherapy EQD2/BED calculator -*- lexical-binding: t; -*-
;; Author: Paul Martin Putora  <pmp@radonc.at>
;; Maintainer: Paul Martin Putora <pmp@radonc.at>
;; URL: https://github.com/chradonc/radonc.el.git
;; Version: 0.32111
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
  "Alist of (α/β . COLOR) used by `radonc-eqd2-calc`."
  :type '(repeat (cons number string))
  :group 'radonc)

(defcustom radonc-eqd2-results-buffer "*EQD2 Results*"
  "Name of the results buffer used by `radonc-eqd2-calc`."
  :type 'string
  :group 'radonc)

;; Buffer-local state for interactive adjustments
(defvar-local radonc--schemes nil)     ; list of (idx n d)
(defvar-local radonc--ab-list nil)
(defvar-local radonc--created-at nil)

(defvar radonc-eqd2-results-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "n") #'radonc-eqd2-scheme1-inc-fractions)
    (define-key map (kbd "p") #'radonc-eqd2-scheme1-dec-fractions)
    (define-key map (kbd "f") #'radonc-eqd2-scheme1-inc-dose-per-fraction)
    (define-key map (kbd "b") #'radonc-eqd2-scheme1-dec-dose-per-fraction)
    map)
  "Keymap for `radonc-eqd2-results-mode'.")

(define-derived-mode radonc-eqd2-results-mode special-mode "EQD2-Results"
  "Major mode for EQD2/BED results.

Keys:
  <n>  add 1 fraction to Scheme 1
  <p>  remove 1 fraction from Scheme 1 (min 1)
  <f>  increase dose/fraction by 0.1 Gy (Scheme 1)
  <b>  decrease dose/fraction by 0.1 Gy (Scheme 1)
  <q>  quit window"
  (setq buffer-read-only t))

(defun radonc--render-results-buffer ()
  "Render the EQD2/BED results into the current buffer from buffer-local state."
  (let ((inhibit-read-only t))
    (erase-buffer)
    (insert (format "EQD2 / BED Calculator\nCreated: %s\n\n" radonc--created-at))
    (insert
     (concat
      "Keys:\n"
      "  <n>  add 1 fraction to Scheme 1\n"
      "  <p>  remove 1 fraction from Scheme 1 (min 1)\n"
      "  <f>  increase dose/fraction by 0.1 Gy (Scheme 1)\n"
      "  <b>  decrease dose/fraction by 0.1 Gy (Scheme 1)\n"
      "  <q>  quit window\n\n"))

    (dolist (sch radonc--schemes)
      (pcase-let ((`(,idx ,n ,d) sch))
        (let ((D (* n d)))
          (insert (format "Scheme %d\nNumber of fractions: %g\nDose per fraction: %.2f Gy\nTotal Dose: %.2f Gy\n"
                          idx n d D))
          (insert "-------------------------------------\n")
          (dolist (ab radonc--ab-list)
            (let* ((alpha-beta (car ab))
                   (color (cdr ab))
                   ;; BED = nd * (1 + d/(α/β))
                   (bed (* D (+ 1.0 (/ (float d) alpha-beta))))
                   ;; EQD2 = BED / (1 + 2/(α/β))
                   (eqd2 (/ bed (+ 1.0 (/ 2.0 alpha-beta))))
                   (start (point)))
              (insert (format "α/β = %g Gy: \t | BED = %6.2f Gy  | \t EQD2 = %6.2f Gy\n"
                              alpha-beta bed eqd2))
              (add-text-properties start (point)
                                   `(face (:foreground ,color)))))
          (insert "\n"))))
    (goto-char (point-min))))

(defun radonc--update-scheme1-fractions (delta)
  "Change Scheme 1 fractions by DELTA; never go below 1; then re-render."
  (unless radonc--schemes
    (user-error "No schemes in this buffer"))
  (let* ((s1 (car radonc--schemes))
         (idx (nth 0 s1))
         (n   (nth 1 s1))
         (d   (nth 2 s1))
         (n2  (max 1 (+ n delta))))
    (setcar radonc--schemes (list idx n2 d))
    (radonc--render-results-buffer)))

(defun radonc-eqd2-scheme1-inc-fractions ()
  "Increase number of fractions in Scheme 1 by 1."
  (interactive)
  (radonc--update-scheme1-fractions +1))

(defun radonc-eqd2-scheme1-dec-fractions ()
  "Decrease number of fractions in Scheme 1 by 1 (min 1)."
  (interactive)
  (radonc--update-scheme1-fractions -1))

(defun radonc--update-scheme1-dose-per-fraction (delta)
  "Change Scheme 1 dose per fraction by DELTA (Gy); then re-render."
  (unless radonc--schemes
    (user-error "No schemes in this buffer"))
  (let* ((s1 (car radonc--schemes))
         (idx (nth 0 s1))
         (n   (nth 1 s1))
         (d   (nth 2 s1))
         (d2  (max 0.0 (+ (float d) delta)))) ; prevent negative dose
    (setcar radonc--schemes (list idx n d2))
    (radonc--render-results-buffer)))

(defun radonc-eqd2-scheme1-inc-dose-per-fraction ()
  "Increase Scheme 1 dose per fraction by 0.1 Gy."
  (interactive)
  (radonc--update-scheme1-dose-per-fraction 0.1))

(defun radonc-eqd2-scheme1-dec-dose-per-fraction ()
  "Decrease Scheme 1 dose per fraction by 0.1 Gy."
  (interactive)
  (radonc--update-scheme1-dose-per-fraction -0.1))

;;;###autoload
(defun radonc-eqd2-calc ()
  "BED/EQD2 calculator for any number of fractionation schemes.

Keeps prompting for schemes until you press RET on an empty
\"Number of fractions\" prompt.

Results go to `radonc-eqd2-results-buffer` with a creation timestamp.
Buffer is read-only and shown via `pop-to-buffer`."
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
  (radonc-eqd2-results-mode)

  ;; Ensure keys work even if other minor modes steal them
  (local-set-key (kbd "n") #'radonc-eqd2-scheme1-inc-fractions)
  (local-set-key (kbd "p") #'radonc-eqd2-scheme1-dec-fractions)
  (local-set-key (kbd "f") #'radonc-eqd2-scheme1-inc-dose-per-fraction)
  (local-set-key (kbd "b") #'radonc-eqd2-scheme1-dec-dose-per-fraction)

  (setq radonc--schemes schemes
        radonc--ab-list ab-list
        radonc--created-at (format-time-string "%Y-%m-%d %H:%M:%S %Z"))
  (radonc--render-results-buffer)
  (pop-to-buffer (current-buffer)))))

(provide 'radonc)
;;; radonc.el ends here
