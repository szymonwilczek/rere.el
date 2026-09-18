;;; rere-test.el --- Tests for rere.el  -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2026 Szymon Wilczek
;; License: GPL-3.0-or-later

;;; Commentary:
;; ERT test suite for the rere.el package.

;;; Code:

(require 'ert)
(require 'rere)

;;;; Test fixtures

(defconst rere-test--sample-diff
  "diff --git a/foo.el b/foo.el
index 1234567..abcdef0 100644
--- a/foo.el
+++ b/foo.el
@@ -1,3 +1,4 @@
 (defun foo ()
-  (message \"old\")
+  (message \"new\")
+  (message \"added\")
   nil)
diff --git a/bar.el b/bar.el
index 2345678..bcdef01 100644
--- a/bar.el
+++ b/bar.el
@@ -10,4 +10,3 @@
 (defun bar ()
-  (removed-call)
   (keep-this)
   t)
"
  "Sample diff output for testing.")

(defconst rere-test--simple-diff
  "diff --git a/f.el b/f.el
index 0000000..1111111 100644
--- a/f.el
+++ b/f.el
@@ -1,2 +1,3 @@
 ctx
+added-line
 ctx2
"
  "Minimal diff with one added line.")

;;;; Diff parsing tests

(ert-deftest rere-test-parse-diff-file-count ()
  "Parse sample diff and check file count."
  (let ((files (rere--parse-diff
                rere-test--sample-diff)))
    (should (= (length files) 2))))

(ert-deftest rere-test-parse-diff-filenames ()
  "Parse sample diff and check filenames."
  (let ((files (rere--parse-diff
                rere-test--sample-diff)))
    (should (equal (rere-file-diff-filename
                    (nth 0 files))
                   "foo.el"))
    (should (equal (rere-file-diff-filename
                    (nth 1 files))
                   "bar.el"))))

(ert-deftest rere-test-parse-diff-hunk-count ()
  "Each file has exactly one hunk."
  (let ((files (rere--parse-diff
                rere-test--sample-diff)))
    (should (= (length
                (rere-file-diff-hunks
                 (nth 0 files))) 1))
    (should (= (length
                (rere-file-diff-hunks
                 (nth 1 files))) 1))))

(ert-deftest rere-test-parse-diff-line-types ()
  "Check parsed line types in first hunk."
  (let* ((files (rere--parse-diff
                 rere-test--sample-diff))
         (hunk (car (rere-file-diff-hunks
                     (car files))))
         (lines (rere-hunk-lines hunk))
         (types (mapcar #'rere-diff-line-type
                        lines)))
    (should (equal types
                   '(context removed
                             added added
                             context)))))

(ert-deftest rere-test-parse-diff-line-content ()
  "Check content of parsed diff lines."
  (let* ((files (rere--parse-diff
                 rere-test--sample-diff))
         (hunk (car (rere-file-diff-hunks
                     (car files))))
         (lines (rere-hunk-lines hunk)))
    (should (equal
             (rere-diff-line-content (nth 1 lines))
             "  (message \"old\")"))
    (should (equal
             (rere-diff-line-content (nth 2 lines))
             "  (message \"new\")"))))

(ert-deftest rere-test-parse-diff-line-numbers ()
  "Check line numbers are assigned correctly."
  (let* ((files (rere--parse-diff
                 rere-test--simple-diff))
         (hunk (car (rere-file-diff-hunks
                     (car files))))
         (lines (rere-hunk-lines hunk)))
    ;; context: old=1, new=1
    (should (= (rere-diff-line-old-line
                (nth 0 lines)) 1))
    (should (= (rere-diff-line-new-line
                (nth 0 lines)) 1))
    ;; added: old=nil, new=2
    (should (null (rere-diff-line-old-line
                   (nth 1 lines))))
    (should (= (rere-diff-line-new-line
                (nth 1 lines)) 2))
    ;; context: old=2, new=3
    (should (= (rere-diff-line-old-line
                (nth 2 lines)) 2))
    (should (= (rere-diff-line-new-line
                (nth 2 lines)) 3))))

(ert-deftest rere-test-parse-diff-hashes-unique ()
  "Each diff line gets a unique content hash."
  (let* ((files (rere--parse-diff
                 rere-test--sample-diff))
         (all-hashes '()))
    (dolist (file files)
      (dolist (hunk (rere-file-diff-hunks file))
        (dolist (dl (rere-hunk-lines hunk))
          (push (rere-diff-line-hash dl)
                all-hashes))))
    ;; all hashes must be unique
    (should (= (length all-hashes)
               (length
                (delete-dups
                 (copy-sequence all-hashes)))))))

(ert-deftest rere-test-parse-empty-diff ()
  "Parsing an empty diff returns empty list."
  (should (null (rere--parse-diff ""))))

;;;; Line classification tests

(ert-deftest rere-test-reviewable-p ()
  "Only added and removed lines are reviewable."
  (let* ((files (rere--parse-diff
                 rere-test--sample-diff))
         (hunk (car (rere-file-diff-hunks
                     (car files))))
         (lines (rere-hunk-lines hunk)))
    ;; context line is not reviewable
    (should-not
     (rere--reviewable-p (nth 0 lines)))
    ;; removed line is reviewable
    (should (rere--reviewable-p (nth 1 lines)))
    ;; added line is reviewable
    (should (rere--reviewable-p (nth 2 lines)))))

;;;; Review state tests

(ert-deftest rere-test-accept-line ()
  "Accepting a line marks it as reviewed."
  (let* ((files (rere--parse-diff
                 rere-test--simple-diff))
         (hunk (car (rere-file-diff-hunks
                     (car files))))
         (dl (nth 1 (rere-hunk-lines hunk)))
         (reviewed (make-hash-table
                    :test 'equal)))
    ;; simulate buffer-local variable
    (should (rere--reviewable-p dl))
    (puthash (rere-diff-line-hash dl)
             t reviewed)
    (should (gethash (rere-diff-line-hash dl)
                     reviewed))))

(ert-deftest rere-test-unaccept-line ()
  "Unaccepting a line removes it from reviewed."
  (let* ((files (rere--parse-diff
                 rere-test--simple-diff))
         (hunk (car (rere-file-diff-hunks
                     (car files))))
         (dl (nth 1 (rere-hunk-lines hunk)))
         (reviewed (make-hash-table
                    :test 'equal)))
    (puthash (rere-diff-line-hash dl)
             t reviewed)
    (remhash (rere-diff-line-hash dl) reviewed)
    (should-not (gethash
                 (rere-diff-line-hash dl)
                 reviewed))))

(ert-deftest rere-test-accept-hunk ()
  "Accepting a hunk marks all reviewable lines."
  (let* ((files (rere--parse-diff
                 rere-test--sample-diff))
         (hunk (car (rere-file-diff-hunks
                     (car files))))
         (reviewed (make-hash-table
                    :test 'equal)))
    (dolist (dl (rere-hunk-lines hunk))
      (when (rere--reviewable-p dl)
        (puthash (rere-diff-line-hash dl)
                 t reviewed)))
    ;; 3 reviewable lines: 1 removed + 2 added
    (should (= (hash-table-count reviewed) 3))))

(ert-deftest rere-test-smart-accept-category ()
  "Smart accept on Pending review heading accepts all pending lines."
  (with-temp-buffer
    (rere-mode)
    (setq rere--diff-files
          (rere--parse-diff rere-test--sample-diff))
    (setq rere--reviewed (make-hash-table :test 'equal))
    (rere--render-buffer)
    (goto-char (point-min))
    (search-forward "Pending review")
    (rere-smart-accept)
    (should (= rere--reviewed-count rere--total-lines))
    (should (= rere--reviewed-count 4))))

(ert-deftest rere-test-unaccept-category ()
  "Unaccept on Reviewed changes heading restores all lines to pending."
  (with-temp-buffer
    (rere-mode)
    (setq rere--diff-files
          (rere--parse-diff rere-test--sample-diff))
    (setq rere--reviewed (make-hash-table :test 'equal))
    (rere--render-buffer)
    (goto-char (point-min))
    (search-forward "Pending review")
    (rere-smart-accept)
    (should (= rere--reviewed-count 4))
    (goto-char (point-min))
    (search-forward "Reviewed changes")
    (rere-unaccept)
    (should (= rere--reviewed-count 0))))

;;;; Counting tests

(ert-deftest rere-test-count-lines ()
  "Counting reflects total reviewable lines."
  (with-temp-buffer
    (rere-mode)
    (setq rere--diff-files
          (rere--parse-diff
           rere-test--sample-diff))
    (setq rere--reviewed
          (make-hash-table :test 'equal))
    (rere--count-lines)
    ;; foo.el: 1 removed + 2 added = 3
    ;; bar.el: 1 removed = 1
    ;; total: 4
    (should (= rere--total-lines 4))
    (should (= rere--reviewed-count 0))))

(ert-deftest rere-test-count-after-accept ()
  "Count updates after accepting lines."
  (with-temp-buffer
    (rere-mode)
    (setq rere--diff-files
          (rere--parse-diff
           rere-test--sample-diff))
    (setq rere--reviewed
          (make-hash-table :test 'equal))
    ;; accept one line
    (let* ((hunk (car (rere-file-diff-hunks
                       (car rere--diff-files))))
           (dl (nth 1 (rere-hunk-lines hunk))))
      (rere--accept-line dl))
    (rere--count-lines)
    (should (= rere--total-lines 4))
    (should (= rere--reviewed-count 1))))

;;;; Refresh persistence tests

(ert-deftest rere-test-refresh-preserves-reviewed ()
  "Refresh preserves reviewed status for unchanged lines."
  (with-temp-buffer
    (rere-mode)
    ;; first parse
    (setq rere--diff-files
          (rere--parse-diff
           rere-test--sample-diff))
    (setq rere--reviewed
          (make-hash-table :test 'equal))
    ;; accept line
    (let* ((hunk (car (rere-file-diff-hunks
                       (car rere--diff-files))))
           (dl (nth 2 (rere-hunk-lines hunk)))
           (hash (rere-diff-line-hash dl)))
      (rere--accept-line dl)
      (should (gethash hash rere--reviewed))
      ;; simulate refresh with same diff
      (let ((old-reviewed
             (copy-hash-table rere--reviewed)))
        (setq rere--diff-files
              (rere--parse-diff
               rere-test--sample-diff))
        (clrhash rere--reviewed)
        (dolist (file rere--diff-files)
          (dolist (h (rere-file-diff-hunks file))
            (dolist (l (rere-hunk-lines h))
              (when (and (rere--reviewable-p l)
                         (gethash
                          (rere-diff-line-hash l)
                          old-reviewed))
                (puthash
                 (rere-diff-line-hash l)
                 t rere--reviewed)))))
        ;; the same line should still be reviewed
        (should
         (gethash hash rere--reviewed))))))

(ert-deftest rere-test-refresh-clears-changed ()
  "Refresh clears reviewed status for changed lines."
  (with-temp-buffer
    (rere-mode)
    (setq rere--diff-files
          (rere--parse-diff
           rere-test--sample-diff))
    (setq rere--reviewed
          (make-hash-table :test 'equal))
    ;; accept line from foo.el
    (let* ((hunk (car (rere-file-diff-hunks
                       (car rere--diff-files))))
           (dl (nth 2 (rere-hunk-lines hunk)))
           (hash (rere-diff-line-hash dl)))
      (rere--accept-line dl)
      ;; refresh with DIFFERENT diff
      (let ((old-reviewed
             (copy-hash-table rere--reviewed)))
        (setq rere--diff-files
              (rere--parse-diff
               rere-test--simple-diff))
        (clrhash rere--reviewed)
        (dolist (file rere--diff-files)
          (dolist (h (rere-file-diff-hunks file))
            (dolist (l (rere-hunk-lines h))
              (when (and (rere--reviewable-p l)
                         (gethash
                          (rere-diff-line-hash l)
                          old-reviewed))
                (puthash
                 (rere-diff-line-hash l)
                 t rere--reviewed)))))
        ;; line no longer in diff -> not reviewed
        (should-not
         (gethash hash rere--reviewed))))))

;;;; Git dir helper tests

(ert-deftest rere-test-git-dir-returns-path ()
  "Git dir returns a path in a git repo."
  (let ((default-directory
         "/home/wolfie/Dokumenty/GitHub/rere.el/"))
    (should (rere--git-dir))))

;;;; Rebase guard test

(ert-deftest rere-test-rebase-guard-no-rebase ()
  "Rebase guard returns nil outside rebase."
  ;; I test in the rere.el repo which is not
  ;; in a rebase right now :)
  (let ((default-directory
         "/home/wolfie/Dokumenty/GitHub/rere.el/"))
    (should-not (rere--rebase-in-progress-p))))

;;;; Target finding tests

(ert-deftest rere-test-find-next-target-middle ()
  "Target finder picks next line when removing a middle line."
  (let* ((files (rere--parse-diff rere-test--sample-diff))
         (hunk (car (rere-file-diff-hunks (car files))))
         (lines (cl-remove-if-not #'rere--reviewable-p
                                  (rere-hunk-lines hunk)))
         (target (rere--find-next-target (list (nth 0 lines))
                                         lines)))
    (should (equal target
                   (rere-diff-line-hash (nth 1 lines))))))

(ert-deftest rere-test-find-next-target-block ()
  "Target finder picks line after block for visual accept."
  (let* ((files (rere--parse-diff rere-test--sample-diff))
         (hunk (car (rere-file-diff-hunks (car files))))
         (lines (cl-remove-if-not #'rere--reviewable-p
                                  (rere-hunk-lines hunk)))
         (target (rere--find-next-target (list (nth 0 lines)
                                               (nth 1 lines))
                                         lines)))
    (should (equal target
                   (rere-diff-line-hash (nth 2 lines))))))

(ert-deftest rere-test-find-next-target-last ()
  "Target finder picks previous line when removing last line."
  (let* ((files (rere--parse-diff rere-test--sample-diff))
         (hunk (car (rere-file-diff-hunks (car files))))
         (lines (cl-remove-if-not #'rere--reviewable-p
                                  (rere-hunk-lines hunk)))
         (target (rere--find-next-target (list (nth 2 lines))
                                         lines)))
    (should (equal target
                   (rere-diff-line-hash (nth 1 lines))))))

(ert-deftest rere-test-find-next-target-all ()
  "Target finder returns nil when removing all lines."
  (let* ((files (rere--parse-diff rere-test--sample-diff))
         (hunk (car (rere-file-diff-hunks (car files))))
         (lines (cl-remove-if-not #'rere--reviewable-p
                                  (rere-hunk-lines hunk)))
         (target (rere--find-next-target lines lines)))
    (should (null target))))

;;;; Toggle section tests

(ert-deftest rere-test-toggle-file-from-line ()
  "Toggling section from a diff line collapses enclosing file."
  (with-temp-buffer
    (rere-mode)
    (setq rere--diff-files
          (rere--parse-diff rere-test--sample-diff))
    (setq rere--reviewed
          (make-hash-table :test 'equal))
    (rere--render-buffer)
    (goto-char (point-min))
    (search-forward "+  (message \"new\")")
    (rere-toggle-section)
    (let ((sec (magit-current-section)))
      (should (oref sec hidden)))))

;;;; Persistence tests

(ert-deftest rere-test-save-and-load-state ()
  "Saving and loading reviewed state persists across sessions."
  (let ((tmp-dir (make-temp-file "rere-test-rebase-" t)))
    (unwind-protect
        (cl-letf (((symbol-function 'rere--rebase-dir)
                   (lambda () tmp-dir)))
          (let ((rere--commit-info '(:sha "abc1234"))
                (rere--reviewed (make-hash-table :test 'equal)))
            (puthash "hash1" t rere--reviewed)
            (puthash "hash2" t rere--reviewed)
            (rere--save-reviewed-state)
            (let ((loaded (rere--load-reviewed-state)))
              (should (= (hash-table-count loaded) 2))
              (should (gethash "hash1" loaded))
              (should (gethash "hash2" loaded)))))
      (delete-directory tmp-dir t))))

(ert-deftest rere-test-cleanup-old-states ()
  "Switching commits cleans up previous review state files."
  (let ((tmp-dir (make-temp-file "rere-test-rebase-" t)))
    (unwind-protect
        (cl-letf (((symbol-function 'rere--rebase-dir)
                   (lambda () tmp-dir)))
          (let ((old-file (expand-file-name "rere-reviewed-old111"
                                            tmp-dir))
                (cur-file (expand-file-name "rere-reviewed-cur222"
                                            tmp-dir)))
            (with-temp-file old-file (insert "hash1\n"))
            (with-temp-file cur-file (insert "hash2\n"))
            (rere--cleanup-old-reviewed-states "cur222")
            (should-not (file-exists-p old-file))
            (should (file-exists-p cur-file))))
      (delete-directory tmp-dir t))))

(ert-deftest rere-test-reviewed-section-hidden-overlay ()
  "Reviewed section is hidden with invisible overlay by default."
  (with-temp-buffer
    (rere-mode)
    (setq rere--diff-files
          (rere--parse-diff rere-test--sample-diff))
    (setq rere--reviewed (make-hash-table :test 'equal))
    (let* ((hunk (car (rere-file-diff-hunks (car rere--diff-files))))
           (dl (car (rere-hunk-lines hunk))))
      (puthash (rere-diff-line-hash dl) t rere--reviewed))
    (rere--render-buffer)
    (let ((rev-sec (cl-find 'rere-reviewed
                            (oref magit-root-section children)
                            :key (lambda (s) (oref s type)))))
      (should (oref rev-sec hidden))
      (should (get-char-property (oref rev-sec content)
                                 'invisible)))))

;;;; Context and diff navigation tests

(ert-deftest rere-test-collect-file-lines-includes-context ()
  "Collecting lines for display includes surrounding context."
  (let* ((files (rere--parse-diff rere-test--sample-diff))
         (foo-file (car files))
         (rere--reviewed (make-hash-table :test 'equal))
         (collected (rere--collect-file-lines
                     foo-file #'rere--pending-p))
         (hunk-lines (cdar collected))
         (types (mapcar #'rere-diff-line-type hunk-lines)))
    (should (equal types
                   '(context removed added added context)))))

(ert-deftest rere-test-collect-excludes-hunk-without-matches ()
  "Hunk with no matching reviewable lines is omitted."
  (let* ((files (rere--parse-diff rere-test--simple-diff))
         (f-file (car files))
         (hunk (car (rere-file-diff-hunks f-file)))
         (added-line (nth 1 (rere-hunk-lines hunk)))
         (rere--reviewed (make-hash-table :test 'equal)))
    ;; mark the only added line as reviewed
    (puthash (rere-diff-line-hash added-line) t rere--reviewed)
    ;; when collecting pending lines, hunk has no pending lines
    (should (null (rere--collect-file-lines
                   f-file #'rere--pending-p)))))

(ert-deftest rere-test-goto-first-pending ()
  "Moves point to first reviewable line, skipping context."
  (with-temp-buffer
    (rere-mode)
    (setq rere--diff-files
          (rere--parse-diff rere-test--sample-diff))
    (setq rere--reviewed (make-hash-table :test 'equal))
    (rere--render-buffer)
    (goto-char (point-min))
    (should (rere--goto-first-pending))
    (let* ((sec (magit-current-section))
           (val (oref sec value)))
      (should (rere-diff-line-p val))
      (should (eq (rere-diff-line-type val) 'removed))
      (should (equal (rere-diff-line-content val)
                     "  (message \"old\")")))))

(ert-deftest rere-test-next-and-previous-diff-line ()
  "Navigation commands skip context lines and headings."
  (with-temp-buffer
    (rere-mode)
    (setq rere--diff-files
          (rere--parse-diff rere-test--sample-diff))
    (setq rere--reviewed (make-hash-table :test 'equal))
    (rere--render-buffer)
    (rere--goto-first-pending)
    ;; initially on first reviewable line: - (message "old")
    (let ((val1 (oref (magit-current-section) value)))
      (should (eq (rere-diff-line-type val1) 'removed)))
    ;; next diff line -> + (message "new")
    (rere-next-diff-line)
    (let ((val2 (oref (magit-current-section) value)))
      (should (eq (rere-diff-line-type val2) 'added))
      (should (equal (rere-diff-line-content val2)
                     "  (message \"new\")")))
    ;; next diff line -> + (message "added")
    (rere-next-diff-line)
    (let ((val3 (oref (magit-current-section) value)))
      (should (eq (rere-diff-line-type val3) 'added))
      (should (equal (rere-diff-line-content val3)
                     "  (message \"added\")")))
    ;; next diff line jumps over context to next file: - (removed-call)
    (rere-next-diff-line)
    (let ((val4 (oref (magit-current-section) value)))
      (should (eq (rere-diff-line-type val4) 'removed))
      (should (equal (rere-diff-line-content val4)
                     "  (removed-call)")))
    ;; previous diff line jumps back to + (message "added")
    (rere-previous-diff-line)
    (let ((val5 (oref (magit-current-section) value)))
      (should (eq (rere-diff-line-type val5) 'added))
      (should (equal (rere-diff-line-content val5)
                     "  (message \"added\")")))
    ;; next diff line at boundary stays in place
    (rere-next-diff-line)
    (let ((pos (point)))
      (rere-next-diff-line)
      (should (= (point) pos)))
    ;; previous diff line at start boundary stays in place
    (rere--goto-first-pending)
    (let ((pos (point)))
      (rere-previous-diff-line)
      (should (= (point) pos)))))

(provide 'rere-test)
;;; rere-test.el ends here
