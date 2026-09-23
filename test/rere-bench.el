;;; rere-bench.el --- Benchmarks for rere.el  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Szymon Wilczek
;; License: GPL-3.0-or-later

;;; Commentary:
;; Measure rendering and review operations on a large synthetic diff.
;; Run with `make bench'.
;; Size is controlled by environment variables:
;;   RERE_BENCH_FILES  number of files      (default 200)
;;   RERE_BENCH_HUNKS  hunks per file       (default 20)
;;   RERE_BENCH_RUNS   repetitions per op   (default 5)

;;; Code:

(require 'rere)
(require 'cl-lib)

(defun rere-bench--env (name default)
  "Return integer value of environment variable NAME or DEFAULT."
  (let ((v (getenv name)))
    (if (and v (not (string-empty-p v)))
        (string-to-number v)
      default)))

(defun rere-bench--make-diff (nfiles nhunks)
  "Return a synthetic diff with NFILES files of NHUNKS hunks each.
Every hunk has 6 context, 2 removed and 4 added lines."
  (with-temp-buffer
    (dotimes (f nfiles)
      (insert (format "diff --git a/src/file%d.el b/src/file%d.el\n"
                      f f)
              "index 1111111..2222222 100644\n"
              (format "--- a/src/file%d.el\n+++ b/src/file%d.el\n"
                      f f))
      (dotimes (h nhunks)
        (let ((o (1+ (* h 40)))
              (n (1+ (* h 42))))
          (insert (format "@@ -%d,8 +%d,10 @@ (defun fn%d ()\n" o n h)
                  "   (let ((x 1))\n"
                  "     (setq y 2)\n"
                  "     (foo x)\n"
                  (format "-    (old-call %d %d)\n" f h)
                  "-    (old-other)\n"
                  (format "+    (new-call %d %d)\n" f h)
                  "+    (new-other)\n"
                  (format "+    (added-a %d)\n" h)
                  (format "+    (added-b %d)\n" h)
                  "     (bar)\n"
                  "     (baz)\n"
                  "     (qux)))\n"))))
    (buffer-string)))

(defun rere-bench--time (fn runs)
  "Call FN RUNS times and return the median duration in ms."
  (let ((times nil))
    (dotimes (_ runs)
      (garbage-collect)
      (let ((t0 (float-time)))
        (funcall fn)
        (push (* 1000.0 (- (float-time) t0)) times)))
    (nth (/ runs 2) (sort times #'<))))

(defun rere-bench--report (name ms)
  "Print benchmark result NAME with duration MS."
  (princ (format "%-34s %9.1f ms\n" name ms)))

(defun rere-bench--setup (raw)
  "Prepare a fresh rere buffer for RAW diff and return it."
  (let ((buf (get-buffer-create "*rere-bench*")))
    (with-current-buffer buf
      (unless (eq major-mode 'rere-mode)
        (rere-mode))
      (setq rere--diff-files (rere--parse-diff raw)
            rere--reviewed (make-hash-table :test 'equal)
            rere--flagged (make-hash-table :test 'equal)
            rere--diffstat-cache nil)
      (rere--render-buffer))
    buf))

(defun rere-bench--at-first-pending (fn)
  "Move to the first pending line and call FN."
  (lambda ()
    (rere--goto-first-pending)
    (funcall fn)))

(defun rere-bench--at-middle-pending (fn)
  "Move to a pending line in the middle of the buffer and call FN."
  (lambda ()
    (goto-char (/ (point-max) 2))
    (goto-char (or (text-property-any (point) (point-max)
                                      'rere-pending t)
                   (point-min)))
    (funcall fn)))

(defun rere-bench--at-first-hunk (fn)
  "Move to the first pending hunk heading and call FN."
  (lambda ()
    (rere--goto-first-pending)
    (goto-char (oref (oref (magit-current-section) parent) start))
    (unless (rere-hunk-p (oref (magit-current-section) value))
      (goto-char (oref (magit-current-section) start)))
    (funcall fn)))

(defun rere-bench-run ()
  "Run all benchmarks and print results."
  (let* ((nfiles (rere-bench--env "RERE_BENCH_FILES" 200))
         (nhunks (rere-bench--env "RERE_BENCH_HUNKS" 20))
         (runs (rere-bench--env "RERE_BENCH_RUNS" 5))
         (raw (rere-bench--make-diff nfiles nhunks))
         (inhibit-message t)
         (buf nil))
    (princ (format "rere benchmark: %d files x %d hunks, %d runs\n"
                   nfiles nhunks runs))
    (rere-bench--report
     "parse diff"
     (rere-bench--time (lambda () (rere--parse-diff raw)) runs))
    (setq buf (rere-bench--setup raw))
    (with-current-buffer buf
      (princ (format "changed lines: %d, buffer lines: %d\n"
                     rere--total-lines
                     (count-lines (point-min) (point-max))))
      (rere-bench--report
       "full render"
       (rere-bench--time (lambda () (rere--render-buffer)) runs))
      (rere-bench--report
       "accept line (top)"
       (rere-bench--time
        (rere-bench--at-first-pending #'rere-smart-accept) runs))
      (rere-bench--report
       "accept line (middle)"
       (rere-bench--time
        (rere-bench--at-middle-pending #'rere-smart-accept) runs))
      (rere-bench--report
       "accept hunk"
       (rere-bench--time
        (rere-bench--at-first-hunk #'rere-smart-accept) runs))
      (rere-bench--report
       "flag line (middle)"
       (rere-bench--time
        (rere-bench--at-middle-pending #'rere-toggle-flag) runs))
      (rere-bench--report
       "full render (partly reviewed)"
       (rere-bench--time (lambda () (rere--render-buffer)) runs)))))

(provide 'rere-bench)
;;; rere-bench.el ends here
