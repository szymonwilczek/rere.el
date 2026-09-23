;;; rere-test.el --- Tests for rere.el  -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2026 Szymon Wilczek
;; License: GPL-3.0-or-later

;;; Commentary:
;; ERT test suite for the rere.el package.

;;; Code:

(require 'ert)
(require 'rere)
(require 'rere-bench)

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

(ert-deftest rere-test-unaccept-stinky-category ()
  "Unaccept on Stinky changes heading restores all flagged lines to pending."
  (with-temp-buffer
    (rere-mode)
    (setq rere--diff-files
          (rere--parse-diff rere-test--sample-diff))
    (setq rere--flagged (make-hash-table :test 'equal))
    (rere--render-buffer)
    (rere--goto-first-pending)
    (rere-toggle-flag)
    (should (= (rere--flagged-count) 1))
    (goto-char (point-min))
    (search-forward "Stinky changes")
    (rere-unaccept)
    (should (= (rere--flagged-count) 0))))

(ert-deftest rere-test-unaccept-on-pending-errors ()
  "Unaccept on a pending diff line or Pending heading signals user-error."
  (with-temp-buffer
    (rere-mode)
    (setq rere--diff-files
          (rere--parse-diff rere-test--sample-diff))
    (rere--render-buffer)
    ;; on pending heading
    (goto-char (point-min))
    (search-forward "Pending review")
    (should-error (rere-unaccept) :type 'user-error)
    ;; on pending line
    (rere--goto-first-pending)
    (should-error (rere-unaccept) :type 'user-error)))

(ert-deftest rere-test-unaccept-file ()
  "Unaccept on a file heading in Reviewed changes restores file to pending."
  (with-temp-buffer
    (rere-mode)
    (setq rere--diff-files
          (rere--parse-diff rere-test--sample-diff))
    (setq rere--reviewed (make-hash-table :test 'equal))
    ;; accept all lines
    (dolist (file rere--diff-files)
      (rere--accept-file-lines file))
    (rere--render-buffer)
    (goto-char (point-min))
    (search-forward "Reviewed changes")
    ;; expand reviewed section
    (rere-toggle-section)
    (search-forward "modified   foo.el")
    (rere-unaccept)
    ;; foo.el had 3 lines, bar.el has 1
    (should (= rere--reviewed-count 1))))

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

;;;; Line identity tests

(defconst rere-test--shift-diff-before
  "diff --git a/k.el b/k.el
index 0000000..1111111 100644
--- a/k.el
+++ b/k.el
@@ -1,2 +1,6 @@
+;; comment line one
+;; comment line two
 ctx
+(code-one)
 ctx2
+(code-two)
"
  "Diff with a comment and two code lines.")

(defconst rere-test--shift-diff-after
  "diff --git a/k.el b/k.el
index 0000000..1111111 100644
--- a/k.el
+++ b/k.el
@@ -1,2 +1,5 @@
+;; reworded comment
 ctx
+(code-one)
 ctx2
+(code-two)
"
  "Same diff after shortening the comment, shifting line numbers.")

(defun rere-test--line-by-content (files content)
  "Return the first diff line in FILES whose content is CONTENT."
  (cl-loop for file in files
           thereis (cl-loop for hunk in (rere-file-diff-hunks file)
                            thereis (cl-find content
                                             (rere-hunk-lines hunk)
                                             :key #'rere-diff-line-content
                                             :test #'equal))))

(ert-deftest rere-test-identity-survives-line-shift ()
  "Editing an earlier line must not change identity of later lines."
  (let* ((before (rere--parse-diff rere-test--shift-diff-before))
         (after (rere--parse-diff rere-test--shift-diff-after))
         (b1 (rere-test--line-by-content before "(code-one)"))
         (a1 (rere-test--line-by-content after "(code-one)")))
    (should-not (equal (rere-diff-line-new-line b1)
                       (rere-diff-line-new-line a1)))
    (should (equal (rere-diff-line-hash b1)
                   (rere-diff-line-hash a1)))))

(ert-deftest rere-test-identity-distinguishes-duplicates ()
  "Identical lines in one file get distinct identities."
  (let* ((files (rere--parse-diff
                 "diff --git a/d.el b/d.el
--- a/d.el
+++ b/d.el
@@ -1,1 +1,3 @@
+}
 x
+}
"))
         (lines (cl-remove-if-not
                 #'rere--reviewable-p
                 (rere-hunk-lines
                  (car (rere-file-diff-hunks (car files)))))))
    (should (= (length lines) 2))
    (should-not (equal (rere-diff-line-hash (nth 0 lines))
                       (rere-diff-line-hash (nth 1 lines))))))

(ert-deftest rere-test-refresh-keeps-review-after-line-shift ()
  "Reviewed lines stay reviewed after an unrelated edit above them."
  (with-temp-buffer
    (rere-mode)
    (setq rere--diff-files
          (rere--parse-diff rere-test--shift-diff-before)
          rere--reviewed (make-hash-table :test 'equal)
          rere--flagged (make-hash-table :test 'equal))
    (rere--accept-line
     (rere-test--line-by-content rere--diff-files "(code-one)"))
    (cl-letf (((symbol-function 'rere--get-raw-diff)
               (lambda () rere-test--shift-diff-after))
              ((symbol-function 'rere--save-reviewed-state-now)
               #'ignore))
      (rere-refresh))
    (should (rere--reviewed-p
             (rere-test--line-by-content rere--diff-files
                                         "(code-one)")))
    (should (= rere--reviewed-count 1))))

(ert-deftest rere-test-migrate-legacy-state ()
  "State saved with line-number hashes is translated on load."
  (with-temp-buffer
    (rere-mode)
    (setq rere--diff-files
          (rere--parse-diff rere-test--shift-diff-before))
    (let* ((dl (rere-test--line-by-content rere--diff-files
                                           "(code-one)"))
           (table (make-hash-table :test 'equal)))
      (puthash (rere--legacy-line-hash dl) t table)
      (puthash "unknown-hash" t table)
      (rere--migrate-state table)
      (should (= (hash-table-count table) 1))
      (should (gethash (rere-diff-line-hash dl) table)))))

;;;; Git dir helper tests

(ert-deftest rere-test-git-dir-returns-path ()
  "Git dir returns a path in a git repo."
  (let ((default-directory
         (file-name-as-directory
          (locate-dominating-file
           (or load-file-name default-directory) ".git"))))
    (should (rere--git-dir))))

;;;; Rebase guard test

(ert-deftest rere-test-rebase-guard-no-rebase ()
  "Rebase guard returns nil outside rebase."
  ;; I test in the rere.el repo which is not
  ;; in a rebase right now :)
  (let ((default-directory
         (file-name-as-directory
          (locate-dominating-file
           (or load-file-name default-directory) ".git"))))
    (should-not (rere--rebase-in-progress-p))))

;;;; Target finding tests

(defun rere-test--content-at-point ()
  "Return the content of the diff line at point."
  (rere-diff-line-content (rere--section-diff-line)))

(ert-deftest rere-test-accept-moves-to-next-line ()
  "After accepting a line point moves to the next pending line."
  (with-temp-buffer
    (rere-test--setup-sample)
    (rere--goto-first-pending)
    (rere-smart-accept)
    (should (equal (rere-test--content-at-point)
                   "  (message \"new\")"))))

(ert-deftest rere-test-accept-block-skips-to-next-hunk ()
  "Accepting a whole hunk moves to the first pending line after it."
  (with-temp-buffer
    (rere-test--setup-sample)
    (rere--goto-first-pending)
    (goto-char (oref (magit-current-section) start))
    (rere-smart-accept)
    (should (equal (rere-test--content-at-point) "  (removed-call)"))))

(ert-deftest rere-test-accept-last-moves-to-previous-line ()
  "Accepting the last pending line moves back to the previous one."
  (with-temp-buffer
    (rere-test--setup-sample)
    (goto-char (point-min))
    (search-forward "(removed-call)")
    (rere-smart-accept)
    (should (equal (rere-test--content-at-point)
                   "  (message \"added\")"))))

(ert-deftest rere-test-undo-stays-within-reviewed ()
  "Undoing in Reviewed moves to the next reviewed line, not Pending."
  (with-temp-buffer
    (rere-test--setup-sample)
    (goto-char (point-min))
    (re-search-forward "^Pending review")
    (rere-smart-accept)
    (re-search-forward "^Reviewed changes")
    (rere-toggle-section)
    (search-forward "(message \"new\")")
    (rere-unaccept)
    (should (equal (rere-test--content-at-point)
                   "  (message \"added\")"))
    (should (eq (rere--section-category (magit-current-section))
                'rere-reviewed))))

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

(defun rere-test--visible-lines ()
  "Return the visible lines of the current buffer as a list of strings."
  (let ((lines nil))
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (unless (invisible-p (point))
          (push (buffer-substring-no-properties
                 (line-beginning-position) (line-end-position))
                lines))
        (forward-line 1)))
    (nreverse lines)))

(defun rere-test--setup-sample ()
  "Render the sample diff in the current buffer with empty state."
  (rere-mode)
  (setq rere--diff-files (rere--parse-diff rere-test--sample-diff)
        rere--reviewed (make-hash-table :test 'equal)
        rere--flagged (make-hash-table :test 'equal)
        rere--diffstat-cache nil)
  (rere--render-buffer))

(ert-deftest rere-test-line-highlight-covers-one-line ()
  "The current line highlight spans exactly the line at point."
  (with-temp-buffer
    (rere-test--setup-sample)
    (rere--goto-first-pending)
    (rere--update-line-highlight)
    (should (= (overlay-start rere--line-overlay)
               (line-beginning-position)))
    (should (= (overlay-end rere--line-overlay)
               (line-beginning-position 2)))))

(ert-deftest rere-test-accept-on-context-line-errors ()
  "Accepting on a context line never accepts the enclosing hunk."
  (with-temp-buffer
    (rere-test--setup-sample)
    (goto-char (point-min))
    (search-forward "(defun foo ()")
    (should (eq (rere-diff-line-type (rere--section-diff-line)) 'context))
    (should-error (rere-smart-accept) :type 'user-error)
    (should (= (hash-table-count rere--reviewed) 0))))

(ert-deftest rere-test-visibility-survives-render ()
  "Collapsed file sections stay collapsed after a re-render."
  (with-temp-buffer
    (rere-test--setup-sample)
    (goto-char (point-min))
    (re-search-forward "^  modified   bar.el")
    (rere-toggle-section)
    (rere--render-buffer)
    (goto-char (point-min))
    (re-search-forward "^  modified   bar.el")
    (should (oref (magit-current-section) hidden))
    (should-not (member "  (removed-call)"
                        (rere-test--visible-lines)))))

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
    (let ((val (rere--section-diff-line)))
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
    (let ((val1 (rere--section-diff-line)))
      (should (eq (rere-diff-line-type val1) 'removed)))
    ;; next diff line -> + (message "new")
    (rere-next-diff-line)
    (let ((val2 (rere--section-diff-line)))
      (should (eq (rere-diff-line-type val2) 'added))
      (should (equal (rere-diff-line-content val2)
                     "  (message \"new\")")))
    ;; next diff line -> + (message "added")
    (rere-next-diff-line)
    (let ((val3 (rere--section-diff-line)))
      (should (eq (rere-diff-line-type val3) 'added))
      (should (equal (rere-diff-line-content val3)
                     "  (message \"added\")")))
    ;; next diff line jumps over context to next file: - (removed-call)
    (rere-next-diff-line)
    (let ((val4 (rere--section-diff-line)))
      (should (eq (rere-diff-line-type val4) 'removed))
      (should (equal (rere-diff-line-content val4)
                     "  (removed-call)")))
    ;; previous diff line jumps back to + (message "added")
    (rere-previous-diff-line)
    (let ((val5 (rere--section-diff-line)))
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

(ert-deftest rere-test-next-and-previous-file ()
  "File navigation with [ and ] jumps between file sections."
  (with-temp-buffer
    (rere-mode)
    (setq rere--diff-files
          (rere--parse-diff rere-test--sample-diff))
    (setq rere--reviewed (make-hash-table :test 'equal))
    (rere--render-buffer)
    (goto-char (point-min))
    (search-forward "modified   foo.el")
    ;; on foo.el -> jump to next file bar.el
    (rere-next-file)
    (let ((sec (magit-current-section)))
      (should (eq (oref sec type) 'rere-file-section))
      (should (equal (rere-file-diff-filename (oref sec value))
                     "bar.el")))
    ;; from diff line in bar.el -> [ jumps to bar.el header
    (search-forward "removed-call")
    (rere-previous-file)
    (let ((sec (magit-current-section)))
      (should (eq (oref sec type) 'rere-file-section))
      (should (equal (rere-file-diff-filename (oref sec value))
                     "bar.el")))
    ;; from bar.el header -> [ jumps to foo.el header
    (rere-previous-file)
    (let ((sec (magit-current-section)))
      (should (eq (oref sec type) 'rere-file-section))
      (should (equal (rere-file-diff-filename (oref sec value))
                     "foo.el")))))

;;;; Word refinement tests

(ert-deftest rere-test-diff-word-ranges ()
  "Word difference computes accurate token ranges."
  (let* ((s1 "  (defun foo (x y))")
         (s2 "  (defun foo (x y z))")
         (ranges (rere--diff-word-ranges s1 s2)))
    ;; s1 has no highlights, s2 highlights " z"
    (should (null (car ranges)))
    (should (equal (cdr ranges) '((17 . 19))))
    (should (equal (substring s2 17 19) " z")))
  (let* ((s1 "const x = calculate_sum(a, b);")
         (s2 "const x = compute_total(a, b);")
         (ranges (rere--diff-word-ranges s1 s2)))
    (should (equal (car ranges) '((10 . 23))))
    (should (equal (cdr ranges) '((10 . 23))))
    (should (equal (substring s1 10 23) "calculate_sum"))
    (should (equal (substring s2 10 23) "compute_total"))))

(ert-deftest rere-test-refine-hunk-highlights ()
  "Parsing diff assigns word highlight ranges to paired lines."
  (let* ((raw (concat "diff --git a/test.el b/test.el\n"
                      "--- a/test.el\n"
                      "+++ b/test.el\n"
                      "@@ -1,2 +1,2 @@\n"
                      "-(defun foo (old-arg))\n"
                      "+(defun foo (new-arg))\n"
                      " (context-line)\n"))
         (files (rere--parse-diff raw))
         (hunk (car (rere-file-diff-hunks (car files))))
         (lines (rere-hunk-lines hunk))
         (r-line (nth 0 lines))
         (a-line (nth 1 lines)))
    (should (equal (rere-diff-line-highlights r-line)
                   '((12 . 15))))
    (should (equal (rere-diff-line-highlights a-line)
                   '((12 . 15))))
    (should (equal (substring (rere-diff-line-content r-line) 12 15)
                   "old"))
    (should (equal (substring (rere-diff-line-content a-line) 12 15)
                   "new"))))

(ert-deftest rere-test-render-with-word-refinement ()
  "Rendering single line applies highlight faces to refined tokens."
  (let* ((raw (concat "diff --git a/test.el b/test.el\n"
                      "--- a/test.el\n"
                      "+++ b/test.el\n"
                      "@@ -1,2 +1,2 @@\n"
                      "-(defun foo (old-arg))\n"
                      "+(defun foo (new-arg))\n"
                      " (context-line)\n"))
         (files (rere--parse-diff raw))
         (hunk (car (rere-file-diff-hunks (car files))))
         (lines (rere-hunk-lines hunk))
         (a-line (nth 1 lines)))
    (with-temp-buffer
      (rere-mode)
      (let ((inhibit-read-only t))
        (rere--insert-single-line a-line))
      ;; buffer content:
      ;; "  +(defun foo (new-arg))\n"
      ;; prefix "  +" has base face
      (should (eq (get-text-property 1 'font-lock-face)
                  'magit-diff-added))
      ;; content start: "(defun foo (" at pos 4
      (should (eq (get-text-property 4 'font-lock-face)
                  'magit-diff-added))
      ;; highlighted "new" starting at pos 4 + 12 = 16
      (should (eq (get-text-property 16 'font-lock-face)
                  (rere--added-highlight-face))))))

(ert-deftest rere-test-diff-word-ranges-low-similarity ()
  "Completely different lines should not compute word highlights."
  (let* ((s1 "  (delete-process gh-radar-process--notifications)")
         (s2 "  (if force")
         (ranges (rere--diff-word-ranges s1 s2)))
    (should (null (car ranges)))
    (should (null (cdr ranges)))))

(ert-deftest rere-test-refine-hunk-pure-addition ()
  "Hunk with only additions should not compute word highlights."
  (let* ((raw (concat "diff --git a/test.el b/test.el\n"
                      "--- a/test.el\n"
                      "+++ b/test.el\n"
                      "@@ -1,0 +1,3 @@\n"
                      "+(defun new-func ())\n"
                      "+  (message \"hello\"))\n"))
         (files (rere--parse-diff raw))
         (hunk (car (rere-file-diff-hunks (car files))))
         (lines (rere-hunk-lines hunk)))
    (dolist (line lines)
      (should (null (rere-diff-line-highlights line))))))

(ert-deftest rere-test-refine-hunk-pure-removal ()
  "Hunk with only removals should not compute word highlights."
  (let* ((raw (concat "diff --git a/test.el b/test.el\n"
                      "--- a/test.el\n"
                      "+++ b/test.el\n"
                      "@@ -1,3 +1,0 @@\n"
                      "-(defun old-func ())\n"
                      "-  (message \"bye\"))\n"))
         (files (rere--parse-diff raw))
         (hunk (car (rere-file-diff-hunks (car files))))
         (lines (rere-hunk-lines hunk)))
    (dolist (line lines)
      (should (null (rere-diff-line-highlights line))))))

(ert-deftest rere-test-render-releases-old-markers ()
  "Section boundaries are markers and a re-render releases the old ones."
  (with-temp-buffer
    (rere-test--setup-sample)
    (let ((old-root magit-root-section))
      (should (markerp (oref old-root start)))
      (should (markerp (oref old-root end)))
      (rere--render-buffer)
      (should-not (marker-buffer (oref old-root start)))
      (should (eq (marker-buffer (oref magit-root-section start))
                  (current-buffer))))))

(ert-deftest rere-test-diffstat-graph ()
  "Diffstat graph generates proper +/- bars with faces."
  (let ((g (rere--diffstat-graph 3 1 10)))
    (should (equal (substring-no-properties g) "+++-"))
    (should (eq (get-text-property 0 'font-lock-face g)
                'magit-diff-added))
    (should (eq (get-text-property 3 'font-lock-face g)
                'magit-diff-removed))))

(ert-deftest rere-test-diffstat-left-aligned-numbers ()
  "Diffstat numbers are left-aligned and graphs align to first plus."
  (let ((s1 (rere--format-file-diffstat "foo.el" 1 1 0 2 10 3))
        (s2 (rere--format-file-diffstat "bar.el" 200 78 0 278 10 3)))
    (should (string-match-p "| 2   \\+-" (substring-no-properties s1)))
    (should (string-match-p "| 278 \\+" (substring-no-properties s2)))))

(ert-deftest rere-test-diffstat-aligned-reviewed-stats ()
  "Diffstat [reviewed/total] stats align at the same column across files."
  (let* ((s1 (rere--format-file-diffstat "foo.el" 1 1 0 2 10 3))
         (s2 (rere--format-file-diffstat "bar.el" 200 78 0 278 10 3))
         (s3 (rere--format-file-diffstat "baz.el" 6 4 0 10 10 3))
         (pos1 (string-match "\\[" s1))
         (pos2 (string-match "\\[" s2))
         (pos3 (string-match "\\[" s3)))
    (should (= pos1 pos2))
    (should (= pos2 pos3))))

(ert-deftest rere-test-diffstat-section-rendered ()
  "Buffer rendering includes Files changed section."
  (with-temp-buffer
    (rere-mode)
    (setq rere--diff-files
          (rere--parse-diff rere-test--sample-diff))
    (setq rere--reviewed (make-hash-table :test 'equal))
    (rere--render-buffer)
    (goto-char (point-min))
    (should (search-forward "Files changed (2)" nil t))
    (should (search-forward "foo.el" nil t))
    (should (search-forward "bar.el" nil t))))

(ert-deftest rere-test-diffstat-smart-accept-file ()
  "Pressing s on a diffstat file line accepts all lines of that file."
  (with-temp-buffer
    (rere-mode)
    (setq rere--diff-files
          (rere--parse-diff rere-test--sample-diff))
    (setq rere--reviewed (make-hash-table :test 'equal))
    (rere--render-buffer)
    (goto-char (point-min))
    (search-forward "foo.el")
    (rere-smart-accept)
    ;; foo.el has 3 reviewable lines (1 removed, 2 added)
    ;; total 3 lines accepted
    (should (= (hash-table-count rere--reviewed) 3))))

;;;; 100% completion tests

(ert-deftest rere-test-100-percent-banner ()
  "Buffer rendering displays completion banner at 100% review."
  (with-temp-buffer
    (rere-mode)
    (setq rere--commit-info
          '(:sha "abcdef123456" :title "Test commit"
                 :step 1 :total 1))
    (setq rere--diff-files
          (rere--parse-diff rere-test--sample-diff))
    (setq rere--reviewed (make-hash-table :test 'equal))
    ;; mark all lines as reviewed
    (dolist (file rere--diff-files)
      (dolist (hunk (rere-file-diff-hunks file))
        (dolist (dl (rere-hunk-lines hunk))
          (when (rere--reviewable-p dl)
            (puthash (rere-diff-line-hash dl) t rere--reviewed)))))
    (rere--render-buffer)
    (goto-char (point-min))
    (should (search-forward "100%]" nil t))
    (should (search-forward "All changes reviewed for commit" nil t))
    (should (search-forward "100% reviewed! Press 'q' to return" nil t))))

(ert-deftest rere-test-100-percent-message ()
  "Render buffer emits notification message when 100% is reached."
  (with-temp-buffer
    (rere-mode)
    (setq rere--commit-info
          '(:sha "abcdef123456" :title "Test commit"
                 :step 1 :total 1))
    (setq rere--diff-files
          (rere--parse-diff rere-test--sample-diff))
    (setq rere--reviewed (make-hash-table :test 'equal))
    (dolist (file rere--diff-files)
      (dolist (hunk (rere-file-diff-hunks file))
        (dolist (dl (rere-hunk-lines hunk))
          (when (rere--reviewable-p dl)
            (puthash (rere-diff-line-hash dl) t rere--reviewed)))))
    (let ((msg nil))
      (cl-letf (((symbol-function 'message)
                 (lambda (fmt &rest args)
                   (setq msg (apply #'format fmt args)))))
        (rere--render-buffer))
      (should (string-match-p "100% reviewed" msg)))))

(ert-deftest rere-test-debounced-save ()
  "Debounced save schedules timer and immediate save executes now."
  (let ((tmp-dir (make-temp-file "rere-test-debounce-" t)))
    (unwind-protect
        (cl-letf (((symbol-function 'rere--rebase-dir)
                   (lambda () tmp-dir)))
          (with-temp-buffer
            (rere-mode)
            (setq rere--commit-info '(:sha "deb123"))
            (setq rere--reviewed (make-hash-table :test 'equal))
            (puthash "h1" t rere--reviewed)
            (rere--schedule-save-reviewed-state)
            (should (timerp rere--save-state-timer))
            (rere--save-reviewed-state-now)
            (should-not rere--save-state-timer)
            (let ((file (expand-file-name "rere-reviewed-deb123" tmp-dir)))
              (should (file-exists-p file)))))
      (delete-directory tmp-dir t))))

(ert-deftest rere-test-reviewed-section-washer ()
  "Reviewed section uses lazy washer to render content on demand."
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
      ;; washer is set when hidden
      (should (oref rev-sec washer))
      ;; showing section washes content
      (let ((inhibit-read-only t))
        (magit-section-show rev-sec))
      (should-not (oref rev-sec washer)))))

(ert-deftest rere-test-accept-line-updates-in-place ()
  "Accepting a line updates counts, headings and diffstat."
  (with-temp-buffer
    (rere-test--setup-sample)
    (rere--goto-first-pending)
    (let* ((dl (rere--section-diff-line))
           (hash (rere-diff-line-hash dl)))
      (rere-smart-accept)
      (should (= rere--reviewed-count 1))
      (should-not (text-property-any (point-min) (point-max)
                                     'rere-line-hash hash))
      (goto-char (point-min))
      (should (re-search-forward "^Progress: 1/4 lines reviewed" nil t))
      (should (re-search-forward "^  foo.el .*\\[1/3\\]" nil t))
      (should (re-search-forward "^Pending review (3)" nil t))
      (should (re-search-forward "^Reviewed changes (1)" nil t))
      ;; point moved to the next pending line
      (goto-char (point-min))
      (rere--goto-first-pending)
      (should (equal (rere-diff-line-content (rere--section-diff-line))
                     "  (message \"new\")")))))

(ert-deftest rere-test-toggle-reviewed-after-inplace-accept ()
  "Toggling Reviewed changes works after in-place line accept."
  (with-temp-buffer
    (rere-mode)
    (setq rere--diff-files
          (rere--parse-diff
           rere-test--sample-diff))
    (setq rere--reviewed
          (make-hash-table :test 'equal))
    (rere--render-buffer)
    ;; accept single line in-place
    (rere--goto-first-pending)
    (let ((dl (rere--section-diff-line)))
      (rere-smart-accept)
      ;; heading should retain magit-section property
      (goto-char (point-min))
      (should (re-search-forward
               "^Reviewed changes (1)" nil t))
      (let ((sec (magit-current-section)))
        (should sec)
        (should (eq (oref sec type) 'rere-reviewed))
        (should (oref sec hidden))
        ;; toggle should expand and render reviewed line
        (rere-toggle-section)
        (let ((rev-sec (cl-find 'rere-reviewed
                                (oref magit-root-section children)
                                :key (lambda (s) (oref s type)))))
          (should-not (oref rev-sec hidden))
          ;; reviewed lines should now be present
          (goto-char (oref rev-sec start))
          (should (search-forward
                   (rere-diff-line-content dl)
                   (oref rev-sec end) t))
          ;; toggle again should collapse
          (goto-char (point-min))
          (re-search-forward "^Reviewed changes (1)" nil t)
          (rere-toggle-section)
          (let ((collapsed-sec
                 (cl-find 'rere-reviewed
                          (oref magit-root-section children)
                          :key (lambda (s) (oref s type)))))
            (should (oref collapsed-sec hidden))))))))


(ert-deftest rere-test-toggle-focus ()
  "Focus mode shows only the selected file diff."
  (with-temp-buffer
    (rere-mode)
    (setq rere--diff-files
          (rere--parse-diff rere-test--sample-diff))
    (rere--render-buffer)
    ;; point is on foo.el
    (rere--goto-first-pending)
    (rere-toggle-focus)
    (should (equal rere--focused-file "foo.el"))
    (goto-char (point-min))
    (should (search-forward "Focus: foo.el (press 'f' to show all)" nil t))
    ;; foo.el should be rendered
    (should (search-forward "modified   foo.el" nil t))
    ;; bar.el should NOT be rendered in diff lines
    (goto-char (point-min))
    (should (search-forward "Pending review" nil t))
    (should-not (search-forward "modified   bar.el" nil t))
    ;; toggle focus again clears it
    (rere-toggle-focus)
    (should-not rere--focused-file)
    (goto-char (point-min))
    (should-not (search-forward "Focus:" nil t))
    (should (search-forward "modified   bar.el" nil t))))

(ert-deftest rere-test-toggle-context ()
  "Context toggle shows or hides context lines."
  (with-temp-buffer
    (rere-mode)
    (setq rere--diff-files
          (rere--parse-diff rere-test--simple-diff))
    (rere--render-buffer)
    ;; context lines are visible initially
    (goto-char (point-min))
    (should (search-forward "  ctx" nil t))
    ;; toggle context hides them
    (rere-toggle-context)
    (should-not rere--show-context)
    (goto-char (point-min))
    (should-not (search-forward "  ctx" nil t))
    (should (search-forward "+added-line" nil t))
    ;; toggle again restores them
    (rere-toggle-context)
    (should rere--show-context)
    (goto-char (point-min))
    (should (search-forward "  ctx" nil t))))

(ert-deftest rere-test-flag-line-and-stinky-section ()
  "Flagging lines moves them to Stinky changes with warning face."
  (with-temp-buffer
    (rere-mode)
    (setq rere--diff-files
          (rere--parse-diff rere-test--simple-diff))
    (rere--render-buffer)
    ;; initially no stinky section
    (goto-char (point-min))
    (should-not (search-forward "Stinky changes" nil t))
    ;; move to diff line and flag it
    (rere--goto-first-pending)
    (let* ((dl (rere--section-diff-line))
           (hash (rere-diff-line-hash dl)))
      (rere-toggle-flag)
      (should (rere--flagged-p dl))
      (goto-char (point-min))
      (should (search-forward "Stinky changes (1)" nil t))
      ;; line should have rere-flagged-line face and property
      (let ((pos (text-property-any (point-min) (point-max)
                                    'rere-line-hash hash)))
        (should pos)
        (should (get-text-property pos 'rere-flagged))
        (should (eq (get-text-property pos 'font-lock-face)
                    'rere-flagged-line)))
      ;; unflagging line removes stinky section
      (goto-char (text-property-any (point-min) (point-max)
                                    'rere-line-hash hash))
      (rere-toggle-flag)
      (should-not (rere--flagged-p dl))
      (goto-char (point-min))
      (should-not (search-forward "Stinky changes" nil t)))))

(ert-deftest rere-test-flag-advances-to-next-pending ()
  "Flagging a line moves point to next pending line, not to stinky section."
  (with-temp-buffer
    (rere-mode)
    (setq rere--diff-files
          (rere--parse-diff rere-test--sample-diff))
    (rere--render-buffer)
    ;; move to first pending line (in foo.el)
    (rere--goto-first-pending)
    (let* ((first-dl (rere--section-diff-line))
           (pending-lines (rere--pending-diff-lines))
           (second-dl (cadr pending-lines))
           (second-hash (rere-diff-line-hash second-dl)))
      ;; flag the first line
      (rere-toggle-flag)
      ;; first line is now flagged
      (should (rere--flagged-p first-dl))
      ;; point must be on the SECOND pending line in Pending review
      (should (equal (rere--line-hash-at-point) second-hash))
      (should (get-text-property (point) 'rere-pending))
      (should-not (get-text-property (point) 'rere-flagged)))))

(ert-deftest rere-test-flag-blocks-100-percent ()
  "Flagged lines block 100% review even if all other lines are reviewed."
  (with-temp-buffer
    (rere-mode)
    (setq rere--diff-files
          (rere--parse-diff rere-test--sample-diff))
    (setq rere--reviewed (make-hash-table :test 'equal))
    (setq rere--flagged (make-hash-table :test 'equal))
    ;; review all lines except the last one, which is flagged
    (let ((all-rev-lines '()))
      (dolist (file rere--diff-files)
        (dolist (hunk (rere-file-diff-hunks file))
          (dolist (dl (rere-hunk-lines hunk))
            (when (rere--reviewable-p dl)
              (push dl all-rev-lines)))))
      (let ((flag-dl (car all-rev-lines))
            (rev-dls (cdr all-rev-lines)))
        (puthash (rere-diff-line-hash flag-dl) t rere--flagged)
        (dolist (dl rev-dls)
          (puthash (rere-diff-line-hash dl) t rere--reviewed))))
    (rere--render-buffer)
    (goto-char (point-min))
    ;; 100% banner should NOT appear
    (should-not (search-forward "100%]" nil t))
    (should-not (search-forward "All changes reviewed for commit" nil t))
    ;; Progress should show (1 flagged)
    (goto-char (point-min))
    (should (search-forward "(1 flagged)" nil t))
    ;; Stinky section is present
    (goto-char (point-min))
    (should (search-forward "Stinky changes (1)" nil t))))

(ert-deftest rere-test-next-flagged ()
  "Jump to next flagged line using rere-next-flagged."
  (with-temp-buffer
    (rere-mode)
    (setq rere--diff-files
          (rere--parse-diff rere-test--sample-diff))
    (setq rere--flagged (make-hash-table :test 'equal))
    ;; flag the second reviewable line
    (let ((rev-lines '()))
      (dolist (file rere--diff-files)
        (dolist (hunk (rere-file-diff-hunks file))
          (dolist (dl (rere-hunk-lines hunk))
            (when (rere--reviewable-p dl)
              (push dl rev-lines)))))
      (let ((target-dl (nth 1 (nreverse rev-lines))))
        (puthash (rere-diff-line-hash target-dl) t rere--flagged)
        (rere--render-buffer)
        (goto-char (point-min))
        (rere-next-flagged)
        (should (get-text-property (point) 'rere-flagged))
        (should (equal (get-text-property (point) 'rere-line-hash)
                       (rere-diff-line-hash target-dl)))))))

(ert-deftest rere-test-next-and-previous-pending-file ()
  "Navigate between files with pending changes using { and }."
  (with-temp-buffer
    (rere-mode)
    (setq rere--diff-files
          (rere--parse-diff rere-test--sample-diff))
    (setq rere--reviewed (make-hash-table :test 'equal))
    ;; accept all lines in foo.el, bar.el remains pending
    (rere--accept-file-lines (nth 0 rere--diff-files))
    (rere--render-buffer)
    (goto-char (point-min))
    ;; } should jump to bar.el in pending section
    (rere-next-pending-file)
    (let ((sec (magit-current-section)))
      (should (eq (oref sec type) 'rere-file-section))
      (should (equal (rere-file-diff-filename (oref sec value))
                     "bar.el")))
    ;; { from bottom should jump back to bar.el
    (goto-char (point-max))
    (rere-previous-pending-file)
    (let ((sec (magit-current-section)))
      (should (eq (oref sec type) 'rere-file-section))
      (should (equal (rere-file-diff-filename (oref sec value))
                     "bar.el")))))

(ert-deftest rere-test-save-and-load-flagged-state ()
  "Save and load flagged state to/from rebase dir."
  (let ((tmp-dir (make-temp-file "rere-test-flagged-" t)))
    (unwind-protect
        (cl-letf (((symbol-function 'rere--rebase-dir)
                   (lambda () tmp-dir)))
          (with-temp-buffer
            (rere-mode)
            (setq rere--commit-info '(:sha "flag123"))
            (setq rere--reviewed (make-hash-table :test 'equal))
            (setq rere--flagged (make-hash-table :test 'equal))
            (puthash "h-flagged" t rere--flagged)
            (rere--save-reviewed-state)
            (let ((loaded (rere--load-flagged-state)))
              (should (gethash "h-flagged" loaded)))))
      (delete-directory tmp-dir t))))


;;;; Incremental update equivalence

(defun rere-test--section-tree (section)
  "Return a comparable description of SECTION and its descendants."
  (list (oref section type)
        (let ((v (oref section value)))
          (cond ((rere-file-diff-p v) (rere-file-diff-filename v))
                ((rere-hunk-p v) (rere-hunk-header v))
                (t v)))
        (marker-position (oref section start))
        (and (oref section content)
             (if (markerp (oref section content))
                 (marker-position (oref section content))
               (oref section content)))
        (marker-position (oref section end))
        (oref section hidden)
        (mapcar #'rere-test--section-tree (oref section children))))

(defun rere-test--snapshot ()
  "Return a comparable snapshot of the current rere buffer."
  (let ((props nil)
        (pos (point-min)))
    (while (< pos (point-max))
      (let ((sec (get-text-property pos 'magit-section)))
        (push (list pos
                    (get-text-property pos 'font-lock-face)
                    (get-text-property pos 'rere-line-hash)
                    (get-text-property pos 'rere-pending)
                    (get-text-property pos 'rere-flagged)
                    (get-text-property pos 'rere-reviewable)
                    (and sec (oref sec type))
                    (and sec (marker-position (oref sec start))))
              props))
      (setq pos (next-property-change pos nil (point-max))))
    (list (buffer-substring-no-properties (point-min) (point-max))
          (nreverse props)
          (rere-test--visible-lines)
          (rere-test--section-tree magit-root-section))))

(defun rere-test--full-render-snapshot (source)
  "Fully render the review state of buffer SOURCE and snapshot it."
  (let ((files (buffer-local-value 'rere--diff-files source))
        (reviewed (buffer-local-value 'rere--reviewed source))
        (flagged (buffer-local-value 'rere--flagged source))
        (visibility (buffer-local-value 'rere--visibility source)))
    (with-temp-buffer
      (rere-mode)
      (setq rere--diff-files files
            rere--reviewed (copy-hash-table reviewed)
            rere--flagged (copy-hash-table flagged)
            rere--visibility (and visibility
                                  (copy-hash-table visibility)))
      (rere--render-buffer)
      (rere-test--snapshot))))

(defun rere-test--random-line (prop)
  "Move point to a random visible line having text property PROP."
  (let ((positions nil)
        (pos (point-min)))
    (while (setq pos (text-property-not-all pos (point-max) prop nil))
      (unless (invisible-p pos)
        (push pos positions))
      (setq pos (save-excursion (goto-char pos)
                                (line-beginning-position 2))))
    (when positions
      (goto-char (nth (random (length positions)) positions)))))

(defun rere-test--random-heading (type)
  "Move point to a random visible heading of a section of TYPE."
  (let ((starts nil))
    (magit-map-sections
     (lambda (s)
       (when (and (eq (oref s type) type)
                  (not (invisible-p (oref s start))))
         (push (marker-position (oref s start)) starts))))
    (when starts
      (goto-char (nth (random (length starts)) starts)))))

(ert-deftest rere-test-incremental-matches-full-render ()
  "Incremental updates always produce the same buffer as a full render."
  (random "rere")
  (with-temp-buffer
    (rere-mode)
    (setq rere--diff-files
          (rere--parse-diff
           (concat rere-test--sample-diff
                   (rere-bench--make-diff 4 3)))
          rere--reviewed (make-hash-table :test 'equal)
          rere--flagged (make-hash-table :test 'equal))
    (cl-letf (((symbol-function 'rere--schedule-save-reviewed-state)
               #'ignore))
      (rere--render-buffer)
      (dotimes (step 150)
        (let ((op (random 8)))
          (ignore-errors
            (pcase op
              (0 (when (rere-test--random-line 'rere-pending)
                   (rere-smart-accept)))
              (1 (when (rere-test--random-heading 'rere-hunk-section)
                   (rere-smart-accept)))
              (2 (when (rere-test--random-heading 'rere-file-section)
                   (rere-smart-accept)))
              (3 (when (rere-test--random-line 'rere-reviewable)
                   (rere-toggle-flag)))
              (4 (when (rere-test--random-line 'rere-reviewable)
                   (rere-unaccept)))
              (6 (when (rere-test--random-heading 'rere-file-section)
                   (rere-toggle-section)))
              (7 (when (rere-test--random-heading 'rere-hunk-section)
                   (rere-unaccept))))))
        (let ((incremental (rere-test--snapshot))
              (full (rere-test--full-render-snapshot
                     (current-buffer))))
          (unless (equal incremental full)
            (ert-fail (list :step step
                            :text-equal (equal (car incremental)
                                               (car full))
                            :incremental (car incremental)
                            :full (car full)))))))))

(provide 'rere-test)
;;; rere-test.el ends here
