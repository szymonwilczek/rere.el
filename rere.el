;;; rere.el --- Review git rebase diffs line by line -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Szymon Wilczek

;; Author:  Szymon Wilczek <swilczek.lx@gmail.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1") (magit "3.0") (magit-section "3.0"))
;; Keywords: vc tools
;; URL: https://github.com/szymonwilczek/rere.el

;; rere.el is free software: you can redistribute it
;; and/or modify it under the terms of the GNU General
;; Public License as published by the Free Software
;; Foundation, either version 3 of the License, or (at
;; your option) any later version.

;; rere.el is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the
;; implied warranty of MERCHANTABILITY or FITNESS FOR A
;; PARTICULAR PURPOSE.  See the GNU General Public License
;; for more details.

;; You should have received a copy of the GNU General
;; Public License along with rere.el.  If not, see
;; <https://www.gnu.org/licenses/>.

;;; Commentary:

;; rere.el (Rebase Review) is a passive, read-only code review tool
;; for interactive git rebase sessions.
;;
;; During `git rebase -i`, when you stop at a commit marked with `edit`,
;; rere.el lets you review the diff line-by-line in a Magit-like interface
;; without touching the git index.
;;
;; Usage:
;;   M-x rere    (only works during interactive rebase)
;;
;; Key bindings:
;;   s / S - smart accept (category/file/hunk/line)
;;   u     - undo accept (move back to Pending)
;;   n / p - jump to next/previous diff line
;;   ] / [ - jump to next/previous file
;;   RET   - open source file at diff line
;;   TAB   - toggle section visibility
;;   r     - refresh diff
;;   q     - quit rere buffer

;;; Code:

(require 'magit-section)
(require 'magit-diff)
(require 'cl-lib)
(require 'subr-x)

(declare-function evil-define-key "evil-core"
                  (state keymap key def &rest bindings))
(declare-function evil-local-set-key "evil-core"
                  (state key def))
(declare-function evil-set-initial-state "evil-core"
                  (mode state))
(declare-function evil-normal-state "evil-states" ())
(declare-function evil-visual-state-p "evil-states"
                  (&optional state))

;;;; Customization

(defgroup rere nil
  "Rebase Review for GNU Emacs."
  :group 'tools
  :prefix "rere-")

(defcustom rere-buffer-name "*rere*"
  "Name of the rere review buffer."
  :type 'string
  :group 'rere)

(defcustom rere-refine-highlight t
  "Whether to highlight word-level differences in diff lines."
  :type 'boolean
  :group 'rere)

(defcustom rere-show-diffstat t
  "Whether to display the file diffstat summary section."
  :type 'boolean
  :group 'rere)

;;;; Faces

(defface rere-flagged-line
  '((t :inherit (magit-diff-base diff-changed) :extend t))
  "Face for flagged (stinky) diff lines."
  :group 'rere)

(defface rere-flagged-heading
  '((t :inherit (warning magit-section-heading)))
  "Face for flagged section heading."
  :group 'rere)

;;;; Data structures

(cl-defstruct rere-diff-line
  "A single line in a diff."
  type        ; `added', `removed', `context'
  content     ; raw text without +/- prefix
  raw         ; full raw line from diff
  file        ; filename this line belongs to
  hunk-header ; hunk header string
  old-line    ; line number in old file (or nil)
  new-line    ; line number in new file (or nil)
  hash        ; content hash for persistence
  highlights) ; list of (beg . end) word highlight offsets

(cl-defstruct rere-hunk
  "A diff hunk containing lines."
  header     ; hunk header (e.g. @@ -1,3 +1,5 @@)
  lines      ; list of `rere-diff-line'
  file)      ; parent filename

(cl-defstruct rere-file-diff
  "Diff for a single file."
  filename   ; file path
  hunks      ; list of `rere-hunk'
  header)    ; raw diff header lines

;;;; Internal state

(defvar-local rere--diff-files nil
  "Parsed diff: list of `rere-file-diff'.")

(defvar-local rere--reviewed nil
  "Hash table of reviewed line hashes.")

(defvar-local rere--flagged nil
  "Hash table of flagged line hashes.")

(defvar-local rere--focused-file nil
  "Filename currently focused, or nil if showing all files.")

(defvar-local rere--show-context t
  "Whether to display context lines in diff hunks.")

(defvar-local rere--commit-info nil
  "Plist with :sha :title :step :total.")

(defvar-local rere--saved-window-config nil
  "Window configuration before entering rere.")

(defvar-local rere--total-lines 0
  "Total number of reviewable diff lines.")

(defvar-local rere--reviewed-count 0
  "Number of reviewed diff lines.")

;;;; Rebase guard and environment

(defun rere--git-dir ()
  "Return the .git directory for the current repo."
  (let ((dir (locate-dominating-file
              default-directory ".git")))
    (when dir
      (expand-file-name ".git" dir))))

(defun rere--rebase-dir ()
  "Return the active rebase directory, or nil."
  (when-let* ((git-dir (rere--git-dir)))
    (cond
     ((file-directory-p
       (expand-file-name "rebase-merge" git-dir))
      (expand-file-name "rebase-merge" git-dir))
     ((file-directory-p
       (expand-file-name "rebase-apply" git-dir))
      (expand-file-name "rebase-apply" git-dir)))))

(defun rere--rebase-in-progress-p ()
  "Return non-nil if an interactive rebase is active."
  (not (null (rere--rebase-dir))))

;;;; State persistence

(defun rere--save-reviewed-state ()
  "Save reviewed and flagged hashes to the rebase state directory."
  (when-let* ((rebase-dir (rere--rebase-dir))
              (sha (plist-get rere--commit-info :sha)))
    (let ((rev-file (expand-file-name
                     (format "rere-reviewed-%s" sha)
                     rebase-dir))
          (flag-file (expand-file-name
                      (format "rere-flagged-%s" sha)
                      rebase-dir))
          (rev-hashes '())
          (flag-hashes '())
          (write-region-inhibit-fsync t))
      (when rere--reviewed
        (maphash (lambda (k _v) (push k rev-hashes))
                 rere--reviewed))
      (with-temp-file rev-file
        (dolist (h (nreverse rev-hashes))
          (insert h "\n")))
      (when rere--flagged
        (maphash (lambda (k _v) (push k flag-hashes))
                 rere--flagged))
      (with-temp-file flag-file
        (dolist (h (nreverse flag-hashes))
          (insert h "\n"))))))

(defvar-local rere--save-state-timer nil
  "Timer for debounced saving of reviewed state.")

(defun rere--schedule-save-reviewed-state ()
  "Schedule saving of reviewed state after a short idle delay."
  (when (timerp rere--save-state-timer)
    (cancel-timer rere--save-state-timer))
  (setq rere--save-state-timer
        (run-with-idle-timer 0.25 nil
                             (lambda (buf)
                               (when (buffer-live-p buf)
                                 (with-current-buffer buf
                                   (rere--save-reviewed-state)
                                   (setq rere--save-state-timer nil))))
                             (current-buffer))))

(defun rere--save-reviewed-state-now ()
  "Save reviewed state immediately and cancel any pending timer."
  (when (timerp rere--save-state-timer)
    (cancel-timer rere--save-state-timer)
    (setq rere--save-state-timer nil))
  (rere--save-reviewed-state))

(defun rere--load-reviewed-state ()
  "Load reviewed hashes from the rebase state directory.
Return a hash table of reviewed hashes."
  (let ((table (make-hash-table :test 'equal)))
    (when-let* ((rebase-dir (rere--rebase-dir))
                (sha (plist-get rere--commit-info :sha)))
      (let ((file (expand-file-name
                   (format "rere-reviewed-%s" sha)
                   rebase-dir)))
        (when (file-exists-p file)
          (with-temp-buffer
            (insert-file-contents file)
            (dolist (line (split-string (buffer-string) "\n" t))
              (puthash (string-trim line) t table))))))
    table))

(defun rere--load-flagged-state ()
  "Load flagged hashes from the rebase state directory.
Return a hash table of flagged hashes."
  (let ((table (make-hash-table :test 'equal)))
    (when-let* ((rebase-dir (rere--rebase-dir))
                (sha (plist-get rere--commit-info :sha)))
      (let ((file (expand-file-name
                   (format "rere-flagged-%s" sha)
                   rebase-dir)))
        (when (file-exists-p file)
          (with-temp-buffer
            (insert-file-contents file)
            (dolist (line (split-string (buffer-string) "\n" t))
              (puthash (string-trim line) t table))))))
    table))

(defun rere--cleanup-old-reviewed-states (current-sha)
  "Delete review state files from previous commits in REBASE-DIR."
  (when-let* ((rebase-dir (rere--rebase-dir)))
    (dolist (prefix '("rere-reviewed-*" "rere-flagged-*"))
      (dolist (f (file-expand-wildcards
                  (expand-file-name prefix rebase-dir)))
        (unless (or (equal (file-name-nondirectory f)
                           (format "rere-reviewed-%s" current-sha))
                    (equal (file-name-nondirectory f)
                           (format "rere-flagged-%s" current-sha)))
          (ignore-errors (delete-file f)))))))

;;;; Commit info

(defun rere--read-commit-info ()
  "Read current commit info during rebase.
Return a plist with :sha :title :step :total."
  (let* ((rebase-dir (rere--rebase-dir))
         (sha (and rebase-dir
                   (rere--read-file-trimmed
                    (expand-file-name
                     "stopped-sha" rebase-dir))))
         (msgnum (and rebase-dir
                      (rere--read-file-trimmed
                       (expand-file-name
                        "msgnum" rebase-dir))))
         (end (and rebase-dir
                   (rere--read-file-trimmed
                    (expand-file-name "end" rebase-dir))))
         (title (string-trim
                 (shell-command-to-string
                  (format "git log -1 --format=%%s %s"
                          (or sha "HEAD"))))))
    (list :sha (or sha
                   (string-trim
                    (shell-command-to-string
                     "git rev-parse --short HEAD")))
          :title title
          :step (if msgnum
                    (string-to-number msgnum)
                  1)
          :total (if end
                     (string-to-number end)
                   1))))

(defun rere--read-file-trimmed (path)
  "Read file at PATH and return trimmed content.
Return nil if file does not exist."
  (when (file-exists-p path)
    (string-trim
     (with-temp-buffer
       (insert-file-contents path)
       (buffer-string)))))

;;;; Diff parsing

(defun rere--get-raw-diff ()
  "Run git diff HEAD~1 and return output as string."
  (shell-command-to-string "git diff HEAD~1"))

(defun rere--parse-diff (raw-diff)
  "Parse RAW-DIFF string into list of `rere-file-diff'.
Return structured representation of the diff."
  (let ((files '())
        (current-file nil)
        (current-hunk nil)
        (current-filename nil)
        (old-line 0)
        (new-line 0))
    (dolist (line (split-string raw-diff "\n"))
      (cond
       ;; new file diff header
       ((string-match
         "^diff --git a/\\(.+\\) b/\\(.+\\)" line)
        (let ((new-filename (match-string 2 line)))
          ;; save previous hunk/file
          (when current-hunk
            (when current-file
              (push (rere--finalize-hunk current-hunk)
                    (rere-file-diff-hunks current-file))))
          (when current-file
            (setf (rere-file-diff-hunks current-file)
                  (nreverse
                   (rere-file-diff-hunks current-file)))
            (push current-file files))
          (setq current-filename new-filename)
          (setq current-file
                (make-rere-file-diff
                 :filename current-filename
                 :hunks '()
                 :header line))
          (setq current-hunk nil)))

       ;; Hunk header
       ((string-match
         "^@@[ \t]+\\(-[0-9]+\\(?:,[0-9]+\\)?\\)[ \t]+\
\\(\\+[0-9]+\\(?:,[0-9]+\\)?\\)[ \t]+@@\\(.*\\)"
         line)
        (let* ((old-spec (match-string 1 line))
               (new-spec (match-string 2 line)))
          ;; save previous hunk
          (when (and current-hunk current-file)
            (push (rere--finalize-hunk current-hunk)
                  (rere-file-diff-hunks current-file)))
          (setq old-line
                (abs (string-to-number old-spec)))
          (setq new-line
                (string-to-number new-spec))
          (setq current-hunk
                (make-rere-hunk
                 :header line
                 :lines '()
                 :file current-filename))))

       ;; Diff lines
       ((and current-hunk
             (string-match "^\\([-+ ]\\)" line))
        (let* ((prefix (match-string 1 line))
               (content (substring line 1))
               (type (pcase prefix
                       ("+" 'added)
                       ("-" 'removed)
                       (_ 'context)))
               (ol (unless (eq type 'added)
                     (prog1 old-line
                       (cl-incf old-line))))
               (nl (unless (eq type 'removed)
                     (prog1 new-line
                       (cl-incf new-line))))
               (dl (make-rere-diff-line
                    :type type
                    :content content
                    :raw line
                    :file current-filename
                    :hunk-header
                    (rere-hunk-header current-hunk)
                    :old-line ol
                    :new-line nl
                    :hash (md5
                           (format "%s:%s:%s:%s"
                                   current-filename
                                   (or ol "")
                                   (or nl "")
                                   content)))))
          (push dl (rere-hunk-lines current-hunk))))))
    ;; finalize last hunk/file
    (when current-hunk
      (when current-file
        (push (rere--finalize-hunk current-hunk)
              (rere-file-diff-hunks current-file))))
    (when current-file
      (setf (rere-file-diff-hunks current-file)
            (nreverse
             (rere-file-diff-hunks current-file)))
      (push current-file files))
    (nreverse files)))

(defun rere--added-highlight-face ()
  "Return face for added word highlights."
  (cond
   ((facep 'magit-diff-added-highlight) 'magit-diff-added-highlight)
   ((facep 'diff-refine-added) 'diff-refine-added)
   (t 'highlight)))

(defun rere--removed-highlight-face ()
  "Return face for removed word highlights."
  (cond
   ((facep 'magit-diff-removed-highlight) 'magit-diff-removed-highlight)
   ((facep 'diff-refine-removed) 'diff-refine-removed)
   (t 'highlight)))

(defun rere--separator-token-p (tok)
  "Return non-nil if TOK is whitespace or punctuation."
  (let ((text (aref tok 0)))
    (not (string-match-p "\\`[a-zA-Z0-9_]" text))))

(defun rere--tokenize-line (str)
  "Tokenize STR into a vector of [token-str beg end]."
  (let ((pos 0)
        (len (length str))
        (tokens nil))
    (while (< pos len)
      (let* ((ch (aref str pos))
             (end (cond
                   ((or (and (<= ?a ch) (<= ch ?z))
                        (and (<= ?A ch) (<= ch ?Z))
                        (and (<= ?0 ch) (<= ch ?9))
                        (= ch ?_))
                    (string-match "[a-zA-Z0-9_]+" str pos)
                    (match-end 0))
                   ((or (= ch ?\s) (= ch ?\t))
                    (string-match "[ \t]+" str pos)
                    (match-end 0))
                   (t (1+ pos)))))
        (push (vector (substring str pos end) pos end)
              tokens)
        (setq pos end)))
    (vconcat (nreverse tokens))))

(defun rere--merge-ranges (ranges)
  "Merge contiguous or overlapping character RANGES."
  (when ranges
    (let ((cur (car ranges))
          (res nil))
      (dolist (r (cdr ranges))
        (if (= (cdr cur) (car r))
            (setq cur (cons (car cur) (cdr r)))
          (push cur res)
          (setq cur r)))
      (push cur res)
      (nreverse res))))

(defun rere--range-total-length (ranges)
  "Return the total character span of RANGES."
  (apply #'+ (mapcar (lambda (r) (- (cdr r) (car r))) ranges)))

(defun rere--diff-word-ranges (s1 s2)
  "Compute word differences between S1 and S2.
Return cons (RANGES1 . RANGES2) where each is a list of (beg . end)."
  (if (or (> (length s1) 1000) (> (length s2) 1000))
      (cons nil nil)
    (let* ((v1 (rere--tokenize-line s1))
           (v2 (rere--tokenize-line s2))
           (n (length v1))
           (m (length v2)))
      (if (or (zerop n) (zerop m))
          (cons nil nil)
        ;; quick check: count common substantive words
        (let ((w1 (cl-remove-if #'rere--separator-token-p v1))
              (w2 (cl-remove-if #'rere--separator-token-p v2)))
          (if (or (null w1) (null w2))
              (cons nil nil)
            (let* ((words1 (mapcar (lambda (v) (aref v 0)) w1))
                   (words2 (mapcar (lambda (v) (aref v 0)) w2))
                   (common (cl-intersection words1 words2 :test #'equal))
                   (sim (/ (* 2.0 (length common))
                           (+ (length words1) (length words2)))))
              (if (< sim 0.4)
                  (cons nil nil)
                ;; run DP
                (let* ((w (1+ m))
                       (dp (make-vector (* (1+ n) w) 0)))
                  (dotimes (i n)
                    (let ((tok1 (aref (aref v1 i) 0))
                          (row-curr (* (1+ i) w))
                          (row-prev (* i w)))
                      (dotimes (j m)
                        (let ((tok2 (aref (aref v2 j) 0)))
                          (aset dp (+ row-curr (1+ j))
                                (if (equal tok1 tok2)
                                    (1+ (aref dp (+ row-prev j)))
                                  (max (aref dp (+ row-curr j))
                                       (aref dp (+ row-prev (1+ j))))))))))
                  (let ((i n) (j m)
                        (diff1 nil)
                        (diff2 nil))
                    (while (or (> i 0) (> j 0))
                      (cond
                       ((and (> i 0) (> j 0)
                             (equal (aref (aref v1 (1- i)) 0)
                                    (aref (aref v2 (1- j)) 0)))
                        (cl-decf i)
                        (cl-decf j))
                       ((and (> j 0)
                             (or (zerop i)
                                 (>= (aref dp (+ (* i w) (1- j)))
                                     (aref dp (+ (* (1- i) w) j)))))
                        (let ((tok (aref v2 (1- j))))
                          (push (cons (aref tok 1) (aref tok 2)) diff2))
                        (cl-decf j))
                       ((> i 0)
                        (let ((tok (aref v1 (1- i))))
                          (push (cons (aref tok 1) (aref tok 2)) diff1))
                        (cl-decf i))))
                    (let ((r1 (rere--merge-ranges diff1))
                          (r2 (rere--merge-ranges diff2)))
                      ;; if either side is 100% changed, do not treat as word
                      ;; refinement (it is a completely replaced line)
                      (if (or (and r1 (= (rere--range-total-length r1)
                                         (length s1)))
                              (and r2 (= (rere--range-total-length r2)
                                         (length s2))))
                          (cons nil nil)
                        (cons r1 r2)))))))))))))

(defun rere--refine-hunk (hunk)
  "Compute word-level diff refinement for paired lines in HUNK."
  (let ((lines (rere-hunk-lines hunk)))
    (when (and (cl-some (lambda (l) (eq (rere-diff-line-type l) 'removed))
                        lines)
               (cl-some (lambda (l) (eq (rere-diff-line-type l) 'added))
                        lines))
      (save-match-data
        (let ((rem-block nil)
              (add-block nil))
          (cl-labels ((flush ()
                        (when (and rem-block add-block)
                          (let ((rems (nreverse rem-block))
                                (adds (nreverse add-block)))
                            (dotimes (i (min (length rems) (length adds)))
                              (let* ((r-line (nth i rems))
                                     (a-line (nth i adds))
                                     (ranges
                                      (rere--diff-word-ranges
                                       (rere-diff-line-content r-line)
                                       (rere-diff-line-content a-line))))
                                (setf (rere-diff-line-highlights r-line)
                                      (car ranges))
                                (setf (rere-diff-line-highlights a-line)
                                      (cdr ranges))))))
                        (setq rem-block nil
                              add-block nil)))
            (dolist (line lines)
              (pcase (rere-diff-line-type line)
                ('removed
                 (when add-block (flush))
                 (push line rem-block))
                ('added
                 (if rem-block
                     (push line add-block)
                   nil))
                (_ (flush))))
            (flush)))))))

(defun rere--finalize-hunk (hunk)
  "Finalize HUNK by reversing lines and computing refinement."
  (setf (rere-hunk-lines hunk)
        (nreverse (rere-hunk-lines hunk)))
  (when rere-refine-highlight
    (rere--refine-hunk hunk))
  hunk)

;;;; Line classification helpers

(defun rere--reviewable-p (diff-line)
  "Return non-nil if DIFF-LINE is reviewable.
Only added and removed lines are reviewable."
  (memq (rere-diff-line-type diff-line)
        '(added removed)))

(defun rere--reviewed-p (diff-line)
  "Return non-nil if DIFF-LINE has been reviewed."
  (and rere--reviewed
       (gethash (rere-diff-line-hash diff-line)
                rere--reviewed)))

(defun rere--flagged-p (diff-line)
  "Return non-nil if DIFF-LINE is flagged."
  (and rere--flagged
       (gethash (rere-diff-line-hash diff-line)
                rere--flagged)))

(defun rere--flagged-count ()
  "Return the number of flagged diff lines."
  (if rere--flagged
      (hash-table-count rere--flagged)
    0))

(defun rere--pending-p (diff-line)
  "Return non-nil if DIFF-LINE is pending review."
  (and (rere--reviewable-p diff-line)
       (not (rere--reviewed-p diff-line))
       (not (rere--flagged-p diff-line))))

(defun rere--file-has-pending-p (file-diff)
  "Return non-nil if FILE-DIFF has any pending lines."
  (cl-some
   (lambda (hunk)
     (cl-some #'rere--pending-p (rere-hunk-lines hunk)))
   (rere-file-diff-hunks file-diff)))

;;;; Counting

(defun rere--count-lines ()
  "Count total and reviewed lines, update state."
  (setq rere--total-lines 0)
  (dolist (file rere--diff-files)
    (dolist (hunk (rere-file-diff-hunks file))
      (dolist (dl (rere-hunk-lines hunk))
        (when (rere--reviewable-p dl)
          (cl-incf rere--total-lines)))))
  (setq rere--reviewed-count
        (if rere--reviewed
            (hash-table-count rere--reviewed)
          0)))

;;;; In-place buffer patching

(defun rere--patch-accept-lines (lines)
  "Accept LINES in-place without full buffer rebuild.
Hide accepted lines with invisible overlays, update
heading counters and diffstat, and move cursor to the
next pending line.  Returns t if patch was performed."
  (let ((inhibit-read-only t))
    ;; 1) mark in hash table
    (dolist (dl lines)
      (rere--accept-line dl))
    ;; 2) update counts
    (setq rere--reviewed-count
          (hash-table-count rere--reviewed))
    ;; 3) hide lines and clear pending property
    (dolist (dl lines)
      (let ((hash (rere-diff-line-hash dl)))
        (rere--patch-hide-line hash)))
    ;; 4) hide empty parent sections
    (rere--patch-hide-empty-sections)
    ;; 5) update headings in-place
    (rere--patch-update-headings)
    ;; 6) update diffstat reviewed counts
    (rere--patch-update-diffstat lines)
    ;; 7) move to next pending line
    (let ((next (save-excursion
                  (forward-line 1)
                  (text-property-any
                   (point) (point-max)
                   'rere-pending t))))
      (if next
          (goto-char next)
        ;; try backward
        (let ((prev (save-excursion
                      (rere--patch-find-prev-pending))))
          (if prev
              (goto-char prev)
            (rere--goto-first-pending)
            (unless (get-text-property
                     (point) 'rere-pending)
              (rere--goto-pending-section))))))
    ;; 8) check 100%
    (when (and (> rere--total-lines 0)
               (= rere--reviewed-count
                  rere--total-lines)
               (zerop (rere--flagged-count)))
      (rere--render-buffer)
      (message
       "[rere] 100%% reviewed! \
Press 'q' to return, then continue in Magit."))
    t))

(defun rere--patch-hide-line (hash)
  "Hide the buffer line with HASH using an invisible overlay."
  (when-let* ((pos (text-property-any
                    (point-min) (point-max)
                    'rere-line-hash hash)))
    (save-excursion
      (goto-char pos)
      (let* ((beg (line-beginning-position))
             (end (line-beginning-position 2))
             (ov (make-overlay beg end nil t nil)))
        (overlay-put ov 'invisible t)
        (overlay-put ov 'rere-accepted t)
        ;; clear pending property so nav skips it
        (remove-text-properties
         beg end '(rere-pending nil))))))

(defun rere--section-has-visible-pending-p (section)
  "Return non-nil if SECTION has any visible pending lines in its range."
  (when (and (oref section start) (oref section end))
    (let ((pos (oref section start))
          (limit (oref section end))
          (found nil))
      (while (and (not found) pos (< pos limit))
        (when (and (get-text-property pos 'rere-pending)
                   (not (invisible-p pos)))
          (setq found t))
        (setq pos (next-single-property-change pos 'rere-pending nil limit)))
      found)))

(defun rere--patch-hide-empty-sections ()
  "Hide empty hunk/file headings under Pending after in-place accept."
  (when-let* ((pending-sec
               (and (bound-and-true-p magit-root-section)
                    (cl-find-if
                     (lambda (s) (eq (oref s type) 'rere-pending))
                     (oref magit-root-section children)))))
    (dolist (file-sec (oref pending-sec children))
      (when (eq (oref file-sec type) 'rere-file-section)
        (let ((file-has-pending nil))
          (dolist (hunk-sec (oref file-sec children))
            (when (eq (oref hunk-sec type) 'rere-hunk-section)
              (if (rere--section-has-visible-pending-p hunk-sec)
                  (setq file-has-pending t)
                ;; hide the empty hunk heading
                (let ((beg (oref hunk-sec start))
                      (end (oref hunk-sec end)))
                  (when (and beg end)
                    (let ((ov (make-overlay beg end nil t nil)))
                      (overlay-put ov 'invisible t)
                      (overlay-put ov 'rere-empty-section t)))))))
          (unless file-has-pending
            ;; hide the empty file heading
            (let ((beg (oref file-sec start))
                  (end (oref file-sec end)))
              (when (and beg end)
                (let ((ov (make-overlay beg end nil t nil)))
                  (overlay-put ov 'invisible t)
                  (overlay-put ov 'rere-empty-section t))))))))))

(defun rere--patch-find-prev-pending ()
  "Find position of the previous pending line."
  (let ((pos (point)))
    (while (and pos (> pos (point-min))
                (not (get-text-property
                      pos 'rere-pending)))
      (setq pos (previous-single-property-change
                 pos 'rere-pending)))
    (when (and pos (> pos (point-min))
               (get-text-property
                pos 'rere-pending))
      pos)))

(defun rere--patch-update-headings ()
  "Update Progress, Pending, and Reviewed headings in-place."
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward
           "^Progress: [0-9]+/[0-9]+ lines reviewed \\[[0-9]+%\\]"
           nil t)
      (let* ((pct (if (> rere--total-lines 0)
                      (/ (* 100 rere--reviewed-count)
                         rere--total-lines)
                    100))
             (new (format
                   "Progress: %d/%d lines reviewed [%d%%]"
                   rere--reviewed-count
                   rere--total-lines pct))
             (beg (match-beginning 0))
             (end (match-end 0)))
        (goto-char beg)
        (delete-region beg end)
        (insert (propertize
                 new 'font-lock-face
                 (if (and (= rere--reviewed-count
                             rere--total-lines)
                          (zerop (rere--flagged-count)))
                     'magit-diff-added
                   'magit-section-heading)))))
    (goto-char (point-min))
    (when (re-search-forward
           "^Pending review (\\([0-9]+\\))" nil t)
      (let* ((pending (- rere--total-lines
                         rere--reviewed-count
                         (rere--flagged-count)))
             (new (number-to-string pending))
             (beg (match-beginning 1))
             (end (match-end 1))
             (props (text-properties-at beg)))
        (goto-char beg)
        (delete-region beg end)
        (insert (apply #'propertize new props))))
    (goto-char (point-min))
    (when (re-search-forward
           "^Reviewed changes (\\([0-9]+\\))" nil t)
      (let* ((new (number-to-string rere--reviewed-count))
             (beg (match-beginning 1))
             (end (match-end 1))
             (props (text-properties-at beg)))
        (goto-char beg)
        (delete-region beg end)
        (insert (apply #'propertize new props))))))

(defun rere--patch-update-diffstat (lines)
  "Update diffstat [reviewed/total] for files touched by LINES."
  (let ((files-touched (make-hash-table :test 'equal)))
    (dolist (dl lines)
      (puthash (rere-diff-line-file dl) t files-touched))
    (when (and rere--diffstat-cache
               (> (hash-table-count files-touched) 0))
      (pcase-let
          ((`(,_ml ,_md ,entries) rere--diffstat-cache))
        (dolist (entry entries)
          (pcase-let
              ((`(,file ,_a ,_r ,total ,_ ,rl) entry))
            (when (gethash
                   (rere-file-diff-filename file)
                   files-touched)
              (let* ((fname
                      (rere-file-diff-filename file))
                     (reviewed
                      (cl-count-if
                       #'rere--reviewed-p rl))
                     (new-str
                      (if (= reviewed total)
                          (propertize
                           (format "[%d/%d]"
                                   reviewed total)
                           'font-lock-face
                           'magit-diff-added)
                        (propertize
                         (format "[%d/%d]"
                                 reviewed total)
                         'font-lock-face
                         'magit-dimmed))))
                (save-excursion
                  (goto-char (point-min))
                  (when (search-forward
                         fname nil t)
                    (let ((le (line-end-position)))
                      (when (re-search-forward
                             "\\[\\([0-9]+\\)/\
\\([0-9]+\\)\\]"
                             le t)
                        (replace-match
                         new-str t t)))))))))))))


;;;; Review operations

(defun rere--accept-line (diff-line)
  "Mark DIFF-LINE as reviewed."
  (when (rere--reviewable-p diff-line)
    (when rere--flagged
      (remhash (rere-diff-line-hash diff-line)
               rere--flagged))
    (unless rere--reviewed
      (setq rere--reviewed
            (make-hash-table :test 'equal)))
    (puthash (rere-diff-line-hash diff-line)
             t rere--reviewed)))

(defun rere--unaccept-line (diff-line)
  "Mark DIFF-LINE as not reviewed (pending)."
  (when rere--flagged
    (remhash (rere-diff-line-hash diff-line)
             rere--flagged))
  (when rere--reviewed
    (remhash (rere-diff-line-hash diff-line)
             rere--reviewed)))

(defun rere--flag-line (diff-line)
  "Mark DIFF-LINE as flagged."
  (when (rere--reviewable-p diff-line)
    (when rere--reviewed
      (remhash (rere-diff-line-hash diff-line)
               rere--reviewed))
    (unless rere--flagged
      (setq rere--flagged
            (make-hash-table :test 'equal)))
    (puthash (rere-diff-line-hash diff-line)
             t rere--flagged)))

(defun rere--unflag-line (diff-line)
  "Remove flag from DIFF-LINE."
  (when rere--flagged
    (remhash (rere-diff-line-hash diff-line)
             rere--flagged)))

(defun rere--accept-hunk-lines (hunk)
  "Mark all reviewable lines in HUNK as reviewed."
  (dolist (dl (rere-hunk-lines hunk))
    (rere--accept-line dl)))

(defun rere--accept-file-lines (file-diff)
  "Mark all lines in FILE-DIFF as reviewed."
  (dolist (hunk (rere-file-diff-hunks file-diff))
    (rere--accept-hunk-lines hunk)))

;;;; Buffer rendering

(defun rere--pending-diff-lines ()
  "Return list of all diff lines currently pending review."
  (let ((lines nil))
    (dolist (file rere--diff-files)
      (dolist (hunk (rere-file-diff-hunks file))
        (dolist (dl (rere-hunk-lines hunk))
          (when (rere--pending-p dl)
            (push dl lines)))))
    (nreverse lines)))

(defun rere--flagged-diff-lines ()
  "Return list of all diff lines currently flagged."
  (let ((lines nil))
    (dolist (file rere--diff-files)
      (dolist (hunk (rere-file-diff-hunks file))
        (dolist (dl (rere-hunk-lines hunk))
          (when (rere--flagged-p dl)
            (push dl lines)))))
    (nreverse lines)))

(defun rere--reviewed-diff-lines ()
  "Return list of all diff lines currently reviewed."
  (let ((lines nil))
    (dolist (file rere--diff-files)
      (dolist (hunk (rere-file-diff-hunks file))
        (dolist (dl (rere-hunk-lines hunk))
          (when (rere--reviewed-p dl)
            (push dl lines)))))
    (nreverse lines)))

(defun rere--region-elements (beg end)
  "Collect all reviewable diff lines between BEG and END.
Expands any hunks or file sections intersecting the region."
  (let ((b (min beg end))
        (e (max beg end))
        (lines '()))
    (save-excursion
      (goto-char b)
      (while (< (point) e)
        (when-let* ((section (magit-current-section)))
          (let ((val (oref section value)))
            (cond
             ((rere-diff-line-p val)
              (unless (memq val lines)
                (push val lines)))
             ((rere-hunk-p val)
              (dolist (dl (rere-hunk-lines val))
                (unless (memq dl lines)
                  (push dl lines))))
             ((rere-file-diff-p val)
              (dolist (h (rere-file-diff-hunks val))
                (dolist (dl (rere-hunk-lines h))
                  (unless (memq dl lines)
                    (push dl lines))))))))
        (forward-line 1)))
    (nreverse lines)))

(defun rere--find-next-target (to-remove all-lines)
  "Find hash of the line to focus after removing TO-REMOVE from ALL-LINES."
  (when (and to-remove all-lines)
    (let* ((to-remove-table (make-hash-table :test 'equal))
           (_ (dolist (dl to-remove)
                (puthash (rere-diff-line-hash dl) t to-remove-table)))
           (last-dl (car (last to-remove)))
           (first-dl (car to-remove))
           (tail (cdr (cl-member (rere-diff-line-hash last-dl)
                                 all-lines
                                 :key #'rere-diff-line-hash
                                 :test #'equal)))
           (next-dl
            (cl-find-if-not
             (lambda (dl)
               (gethash (rere-diff-line-hash dl) to-remove-table))
             tail)))
      (if next-dl
          (rere-diff-line-hash next-dl)
        (let* ((pos (cl-position
                     (rere-diff-line-hash first-dl)
                     all-lines
                     :key #'rere-diff-line-hash
                     :test #'equal))
               (head (and pos (> pos 0)
                          (cl-subseq all-lines 0 pos)))
               (prev-dl
                (and head
                     (cl-find-if-not
                      (lambda (dl)
                        (gethash (rere-diff-line-hash dl)
                                 to-remove-table))
                      (reverse head)))))
          (when prev-dl
            (rere-diff-line-hash prev-dl)))))))

(defun rere--render-buffer (&optional target-hash)
  "Render the rere review buffer content.
If TARGET-HASH is provided, move point to that line.
Otherwise, try to preserve cursor position."
  (let ((inhibit-read-only t)
        (saved-section-path (rere--current-section-path))
        (saved-line-hash (or target-hash
                             (rere--line-hash-at-point))))
    (remove-overlays (point-min) (point-max))
    (erase-buffer)
    (rere--count-lines)
    (magit-insert-section (magit-root-section)
      (rere--insert-header)
      (rere--insert-diffstat-section)
      (rere--insert-pending-section)
      (rere--insert-stinky-section)
      (rere--insert-reviewed-section)
      (rere--insert-footer))
    (magit-section-show magit-root-section)
    (when (and (> rere--total-lines 0)
               (= rere--reviewed-count rere--total-lines)
               (zerop (rere--flagged-count)))
      (message
       "[rere] 100%% reviewed! Press 'q' to return, then continue in Magit."))
    ;; restore position
    (or (and saved-line-hash
             (rere--goto-line-hash saved-line-hash))
        (rere--goto-section-path saved-section-path)
        (rere--goto-first-pending)
        (rere--goto-first-flagged)
        (rere--goto-pending-section)
        (goto-char (point-min)))))

(defun rere--goto-pending-section ()
  "Move point to the Pending review section."
  (let ((pos nil))
    (save-excursion
      (goto-char (point-min))
      (while (and (not pos) (not (eobp)))
        (when-let* ((section (magit-current-section)))
          (when (eq (oref section type) 'rere-pending)
            (setq pos (oref section start))))
        (forward-line 1)))
    (when pos
      (goto-char pos)
      t)))

(defun rere--goto-first-pending ()
  "Move point to the first pending reviewable diff line.
Return t if found, nil otherwise."
  (when-let* ((pos (text-property-any (point-min) (point-max)
                                      'rere-pending t)))
    (goto-char pos)
    t))

(defun rere--goto-first-flagged ()
  "Move point to the first flagged diff line.
Return t if found, nil otherwise."
  (when-let* ((pos (text-property-any (point-min) (point-max)
                                      'rere-flagged t)))
    (goto-char pos)
    t))

(defun rere--current-section-path ()
  "Return path identifier for current section."
  (when-let* ((section (magit-current-section)))
    (magit-section-ident section)))

(defun rere--line-hash-at-point ()
  "Return the diff-line hash at point, if any."
  (or (get-text-property (point) 'rere-line-hash)
      (when-let* ((section (magit-current-section)))
        (let ((value (oref section value)))
          (when (rere-diff-line-p value)
            (rere-diff-line-hash value))))))

(defun rere--goto-line-hash (hash)
  "Move point to the line with HASH.  Return t if found."
  (when hash
    (when-let* ((pos (text-property-any (point-min) (point-max)
                                        'rere-line-hash hash)))
      (goto-char pos)
      t)))

(defun rere--goto-section-path (path)
  "Move point to section identified by PATH.
Return t if found."
  (when path
    (ignore-errors
      (when-let* ((section
                   (magit-get-section path)))
        (goto-char (oref section start))
        t))))

(defun rere--insert-header ()
  "Insert the rere header with commit info."
  (let* ((sha (or (plist-get rere--commit-info :sha)
                  "unknown"))
         (title (or (plist-get
                     rere--commit-info :title)
                    "unknown"))
         (step (or (plist-get
                    rere--commit-info :step) 0))
         (total (or (plist-get
                     rere--commit-info :total) 0))
         (flagged-count (rere--flagged-count))
         (done (and (> rere--total-lines 0)
                    (= rere--reviewed-count rere--total-lines)
                    (zerop flagged-count)))
         (pct (if (> rere--total-lines 0)
                  (/ (* 100 rere--reviewed-count)
                     rere--total-lines)
                100))
         (short-sha (if (> (length sha) 7)
                        (substring sha 0 7)
                      sha)))
    (magit-insert-section (rere-header)
      (insert
       (propertize
        (format "Rebasing: %s %s (step %d/%d)\n"
                short-sha title step total)
        'font-lock-face 'magit-section-heading))
      (insert
       (propertize
        (format "Progress: %d/%d lines reviewed [%d%%]%s\n"
                rere--reviewed-count
                rere--total-lines pct
                (if (> flagged-count 0)
                    (format " (%d flagged)" flagged-count)
                  ""))
        'font-lock-face (if done
                            'magit-diff-added
                          (if (> flagged-count 0)
                              'warning
                            'magit-section-heading))))
      (when rere--focused-file
        (insert
         (propertize
          (format "Focus: %s (press 'f' to show all)\n"
                  rere--focused-file)
          'font-lock-face 'magit-diff-file-heading)))
      (when done
        (insert
         (propertize
          (format
           "\nAll changes reviewed for commit %s!\n  \
Press 'q' to return, then amend or continue in Magit.\n"
           short-sha)
          'font-lock-face 'magit-diff-added)))
      (insert "\n"))))

(defun rere--diffstat-graph (added removed max-width)
  "Return propertized diffstat graph string for ADDED and REMOVED lines.
MAX-WIDTH is the maximum length of the +/- bar."
  (let* ((total (+ added removed))
         (width (if (<= total max-width)
                    total
                  max-width))
         (add-bar (if (zerop total) 0
                    (round (* (/ (float added) total) width))))
         (rem-bar (if (zerop total) 0
                    (- width add-bar))))
    (concat
     (propertize (make-string add-bar ?+)
                 'font-lock-face 'magit-diff-added)
     (propertize (make-string rem-bar ?-)
                 'font-lock-face 'magit-diff-removed))))

(defun rere--format-file-diffstat (filename added removed reviewed total
                                            max-len &optional max-digits)
  "Format a single file diffstat line for FILENAME.
ADDED, REMOVED, REVIEWED, and TOTAL are line counts.
MAX-LEN is the maximum filename display width.
MAX-DIGITS is the maximum width of the total diff count column."
  (let* ((disp-fn (if (> (length filename) 35)
                      (concat "..." (substring filename
                                               (- (length filename) 32)))
                    filename))
         (padding (make-string (max 0 (- max-len (length disp-fn))) ?\s))
         (tot-diff (+ added removed))
         (digits (or max-digits
                     (length (number-to-string tot-diff))))
         (num-fmt (format "%%-%dd " digits))
         (max-graph-width 15)
         (graph (rere--diffstat-graph added removed max-graph-width))
         (graph-pad (make-string
                     (max 2 (+ (- max-graph-width (length graph)) 2))
                     ?\s))
         (rev-part (if (= reviewed total)
                       (propertize (format "[%d/%d]" reviewed total)
                                   'font-lock-face 'magit-diff-added)
                     (propertize (format "[%d/%d]" reviewed total)
                                 'font-lock-face 'magit-dimmed))))
    (concat (propertize (concat "  " disp-fn)
                        'font-lock-face 'magit-diff-file-heading)
            padding
            (propertize " | " 'font-lock-face 'magit-dimmed)
            (propertize (format num-fmt tot-diff)
                        'font-lock-face 'magit-dimmed)
            graph
            graph-pad
            rev-part
            "\n")))

(defvar-local rere--diffstat-cache nil
  "Cached metadata for diffstat rendering: (max-len max-digits entries).")

(defun rere--compute-diffstat-cache ()
  "Compute and cache static diffstat metadata for `rere--diff-files'."
  (if (null rere--diff-files)
      (setq rere--diffstat-cache nil)
    (let* ((entries
            (mapcar
             (lambda (file)
               (let ((added 0)
                     (removed 0)
                     (total 0)
                     (rl nil))
                 (dolist (hunk (rere-file-diff-hunks file))
                   (dolist (dl (rere-hunk-lines hunk))
                     (when (rere--reviewable-p dl)
                       (cl-incf total)
                       (if (eq (rere-diff-line-type dl) 'added)
                           (cl-incf added)
                         (cl-incf removed))
                       (push dl rl))))
                 (list file added removed total (+ added removed)
                       (nreverse rl))))
             rere--diff-files))
           (names
            (mapcar (lambda (f)
                      (let ((fn (rere-file-diff-filename f)))
                        (if (> (length fn) 35)
                            (concat "..." (substring fn (- (length fn) 32)))
                          fn)))
                    rere--diff-files))
           (max-len (min 35 (max 10 (apply #'max (mapcar #'length names)))))
           (max-digits
            (apply #'max (mapcar (lambda (e)
                                   (length (number-to-string (nth 4 e))))
                                 entries))))
      (setq rere--diffstat-cache (list max-len max-digits entries)))))

(defun rere--insert-diffstat-section ()
  "Insert diffstat section listing changed files and review stats."
  (when (and rere-show-diffstat rere--diff-files)
    (unless rere--diffstat-cache
      (rere--compute-diffstat-cache))
    (magit-insert-section (rere-diffstat nil nil)
      (magit-insert-heading
        (format "Files changed (%d)\n" (length rere--diff-files)))
      (pcase-let ((`(,max-len ,max-digits ,entries) rere--diffstat-cache))
        (dolist (entry entries)
          (pcase-let ((`(,file ,added ,removed ,total ,_ ,rl) entry))
            (let ((reviewed (cl-count-if #'rere--reviewed-p rl)))
              (magit-insert-section (rere-file-stat file nil)
                (insert
                 (rere--format-file-diffstat
                  (rere-file-diff-filename file)
                  added removed reviewed total max-len max-digits)))))))
      (insert "\n"))))

(defun rere--insert-pending-section ()
  "Insert the Pending review section."
  (let ((pending-count
         (- rere--total-lines
            rere--reviewed-count
            (rere--flagged-count))))
    (magit-insert-section (rere-pending nil nil)
      (magit-insert-heading
        (format "Pending review (%d)\n" pending-count))
      (if (zerop pending-count)
          (insert
           (propertize "  All changes reviewed.\n"
                       'font-lock-face
                       'magit-dimmed))
        (rere--insert-diff-lines #'rere--pending-p))
      (insert "\n"))))

(defun rere--insert-stinky-section ()
  "Insert the Stinky (flagged) changes section if any lines are flagged."
  (let ((count (rere--flagged-count)))
    (when (> count 0)
      (magit-insert-section (rere-stinky nil nil)
        (magit-insert-heading
          (propertize
           (format "Stinky changes (%d)\n" count)
           'font-lock-face 'rere-flagged-heading))
        (rere--insert-diff-lines #'rere--flagged-p)
        (insert "\n")))))

(defun rere--insert-reviewed-section ()
  "Insert the Reviewed changes section."
  (let* ((prev-sec
          (and (bound-and-true-p magit-root-section)
               (cl-find-if
                (lambda (s) (eq (oref s type) 'rere-reviewed))
                (oref magit-root-section children))))
         (hidden (if prev-sec (oref prev-sec hidden) t)))
    (magit-insert-section
        (rere-reviewed nil hidden
                       :washer (lambda ()
                                 (rere--insert-diff-lines #'rere--reviewed-p)))
      (magit-insert-heading
        (format "Reviewed changes (%d)\n"
                rere--reviewed-count))
      (unless hidden
        (when (> rere--reviewed-count 0)
          (rere--insert-diff-lines #'rere--reviewed-p)))
      (insert "\n"))))

(defun rere--insert-diff-lines (pred)
  "Insert diff lines matching PRED grouped by file/hunk."
  (let ((files (if rere--focused-file
                   (cl-remove-if-not
                    (lambda (f)
                      (equal (rere-file-diff-filename f)
                             rere--focused-file))
                    rere--diff-files)
                 rere--diff-files)))
    (dolist (file files)
      (let ((file-lines
             (rere--collect-file-lines file pred)))
        (when file-lines
          (magit-insert-section
              (rere-file-section file nil)
            (magit-insert-heading
              (propertize
               (format "  modified   %s\n"
                       (rere-file-diff-filename file))
               'font-lock-face
               'magit-diff-file-heading))
            (dolist (hunk-data file-lines)
              (let ((hunk (car hunk-data))
                    (lines (cdr hunk-data)))
                (magit-insert-section
                    (rere-hunk-section hunk nil)
                  (magit-insert-heading
                    (propertize
                     (concat "  "
                             (rere-hunk-header hunk)
                             "\n")
                     'font-lock-face
                     'magit-diff-hunk-heading))
                  (dolist (dl lines)
                    (rere--insert-single-line
                     dl)))))))))))

(defun rere--collect-file-lines (file-diff pred)
  "Collect lines from FILE-DIFF matching PRED with surrounding context.
Return alist of (hunk . lines-to-render)."
  (let ((result '()))
    (dolist (hunk (rere-file-diff-hunks file-diff))
      (let ((lines '())
            (has-matching nil))
        (dolist (dl (rere-hunk-lines hunk))
          (cond
           ((funcall pred dl)
            (setq has-matching t)
            (push dl lines))
           ((and rere--show-context
                 (eq (rere-diff-line-type dl) 'context))
            (push dl lines))))
        (when has-matching
          (push (cons hunk (nreverse lines)) result))))
    (nreverse result)))

(defun rere--insert-single-line (dl)
  "Insert a single diff line DL with proper face and word refinement."
  (let* ((flagged (rere--flagged-p dl))
         (type (rere-diff-line-type dl))
         (face (cond
                (flagged 'rere-flagged-line)
                ((eq type 'added) 'magit-diff-added)
                ((eq type 'removed) 'magit-diff-removed)
                ((eq type 'context) 'magit-diff-context)
                (t 'default)))
         (hl-face (unless flagged
                    (when rere-refine-highlight
                      (pcase type
                        ('added (rere--added-highlight-face))
                        ('removed (rere--removed-highlight-face))))))
         (prefix (pcase type
                   ('added "+")
                   ('removed "-")
                   (_ " ")))
         (content (rere-diff-line-content dl))
         (highlights (unless flagged
                       (when rere-refine-highlight
                         (rere-diff-line-highlights dl))))
         (hash (rere-diff-line-hash dl))
         (reviewable (rere--reviewable-p dl))
         (pending (rere--pending-p dl)))
    (magit-insert-section (rere-line dl)
      (let ((beg (point)))
        (insert "  " prefix content "\n")
        (add-text-properties
         beg (point)
         `(font-lock-face ,face
                          rere-line-hash ,hash
                          rere-diff-line ,dl
                          ,@(when reviewable '(rere-reviewable t))
                          ,@(when pending '(rere-pending t))
                          ,@(when flagged '(rere-flagged t))))
        (when (and hl-face highlights)
          (let ((offset (+ beg 3)))
            (dolist (hl highlights)
              (put-text-property (+ offset (car hl))
                                 (+ offset (cdr hl))
                                 'font-lock-face hl-face))))))))

(defun rere--insert-footer ()
  "Insert footer with keybinding hints."
  (when (and (> rere--total-lines 0)
             (= rere--reviewed-count rere--total-lines)
             (zerop (rere--flagged-count)))
    (insert
     (propertize
      (concat "\n100% reviewed! Press 'q' to return, "
              "then amend or continue in Magit.\n")
      'font-lock-face 'magit-diff-added)))
  (insert
   (propertize
    (concat "\n"
            "s accept  u undo  m/M flag  f focus  c context\n"
            "n/p diff  [/] file  {/} pending file  TAB toggle  "
            "RET edit  r refresh  q quit\n")
    'font-lock-face 'magit-dimmed)))

;;;; Interactive commands

(defun rere--section-diff-line ()
  "Return the diff-line at point, or nil."
  (or (get-text-property (point) 'rere-diff-line)
      (when-let* ((section (magit-current-section)))
        (let ((val (oref section value)))
          (when (rere-diff-line-p val) val)))))

(defun rere--section-hunk ()
  "Return the hunk at point, or nil."
  (when-let* ((section (magit-current-section)))
    (let ((val (oref section value)))
      (cond
       ((rere-hunk-p val) val)
       ((rere-diff-line-p val)
        ;; walk up to parent hunk section
        (when-let* ((parent
                     (oref section parent)))
          (let ((pval (oref parent value)))
            (when (rere-hunk-p pval) pval))))))))

(defun rere--section-file ()
  "Return the file-diff at point, or nil."
  (when-let* ((section (magit-current-section)))
    (let ((val (oref section value)))
      (cond
       ((rere-file-diff-p val) val)
       (t
        ;; walk up to find file section
        (let ((s section))
          (while (and s
                      (not (rere-file-diff-p
                            (oref s value))))
            (setq s (oref s parent)))
          (when s (oref s value))))))))

(defalias 'rere-accept-line #'rere-smart-accept)

(defun rere-smart-accept ()
  "Smart accept: region, category, file, hunk, or line at point.
In visual mode or when region is active, accept selected lines.
On Pending review heading, accept all pending changes.
On a file heading, accept entire file.
On a hunk heading, accept entire hunk.
On a diff line, accept that line."
  (interactive)
  (let* ((in-visual (and (bound-and-true-p evil-mode)
                         (evil-visual-state-p)))
         (has-region (or in-visual (use-region-p)))
         (to-accept nil)
         (target-hash nil))
    (cond
     (has-region
      (setq to-accept
            (cl-remove-if-not
             #'rere--pending-p
             (rere--region-elements
              (region-beginning) (region-end))))
      (when in-visual
        (evil-normal-state))
      (deactivate-mark))
     ((when-let* ((dl (rere--section-diff-line)))
        (when (rere--pending-p dl)
          (setq to-accept (list dl))
          (setq target-hash
                (or (save-excursion
                      (forward-line 1)
                      (when-let* ((pos (text-property-any
                                        (point) (point-max)
                                        'rere-pending t)))
                        (get-text-property pos 'rere-line-hash)))
                    (let ((pos (point)))
                      (while (and pos (> pos (point-min))
                                  (not (get-text-property
                                        pos 'rere-pending)))
                        (setq pos (previous-single-property-change
                                   pos 'rere-pending)))
                      (when (and pos (> pos (point-min)))
                        (get-text-property pos 'rere-line-hash))))))))
     (t
      (when-let* ((section (magit-current-section)))
        (let ((val (oref section value)))
          (cond
           ((eq (oref section type) 'rere-pending)
            (setq to-accept (rere--pending-diff-lines)))
           ((rere-file-diff-p val)
            (setq to-accept
                  (cl-remove-if-not
                   #'rere--pending-p
                   (cl-mapcan
                    (lambda (h)
                      (copy-sequence (rere-hunk-lines h)))
                    (rere-file-diff-hunks val))))
            (setq target-hash
                  (or (save-excursion
                        (goto-char (oref section end))
                        (when-let* ((pos (text-property-any
                                          (point) (point-max)
                                          'rere-pending t)))
                          (get-text-property pos 'rere-line-hash)))
                      (let ((pos (oref section start)))
                        (while (and pos (> pos (point-min))
                                    (not (get-text-property
                                          pos 'rere-pending)))
                          (setq pos (previous-single-property-change
                                     pos 'rere-pending)))
                        (when (and pos (> pos (point-min)))
                          (get-text-property pos 'rere-line-hash))))))
           ((rere-hunk-p val)
            (setq to-accept
                  (cl-remove-if-not
                   #'rere--pending-p
                   (copy-sequence (rere-hunk-lines val))))
            (setq target-hash
                  (or (save-excursion
                        (goto-char (oref section end))
                        (when-let* ((pos (text-property-any
                                          (point) (point-max)
                                          'rere-pending t)))
                          (get-text-property pos 'rere-line-hash)))
                      (let ((pos (oref section start)))
                        (while (and pos (> pos (point-min))
                                    (not (get-text-property
                                          pos 'rere-pending)))
                          (setq pos (previous-single-property-change
                                     pos 'rere-pending)))
                        (when (and pos (> pos (point-min)))
                          (get-text-property pos 'rere-line-hash)))))))))))
    (unless to-accept
      (user-error "[rere] No pending changes to accept"))
    (rere--schedule-save-reviewed-state)
    (let ((rev-sec
           (and (bound-and-true-p magit-root-section)
                (cl-find-if
                 (lambda (s) (eq (oref s type) 'rere-reviewed))
                 (oref magit-root-section children)))))
      ;; fast path: single-line in-place patch when reviewed is collapsed
      (if (and (= (length to-accept) 1)
               (not has-region)
               (or (null rev-sec) (oref rev-sec hidden)))
          (rere--patch-accept-lines to-accept)
        ;; bulk accept or reviewed section expanded: full render
        (unless target-hash
          (setq target-hash
                (rere--find-next-target
                 to-accept
                 (rere--pending-diff-lines))))
        (dolist (dl to-accept)
          (rere--accept-line dl))
        (rere--render-buffer target-hash)))))

(defun rere-unaccept ()
  "Undo acceptance of region, category, line, hunk, or file at point.
Move items back from Reviewed to Pending."
  (interactive)
  (let* ((in-visual (and (bound-and-true-p evil-mode)
                         (evil-visual-state-p)))
         (has-region (or in-visual (use-region-p)))
         (to-unaccept nil))
    (if has-region
        (progn
          (setq to-unaccept
                (cl-remove-if-not
                 #'rere--reviewed-p
                 (rere--region-elements
                  (region-beginning) (region-end))))
          (when in-visual
            (evil-normal-state))
          (deactivate-mark))
      (when-let* ((section (magit-current-section)))
        (let ((val (oref section value)))
          (setq to-unaccept
                (cond
                 ((eq (oref section type) 'rere-reviewed)
                  (rere--reviewed-diff-lines))
                 ((rere-diff-line-p val)
                  (when (rere--reviewed-p val)
                    (list val)))
                 ((rere-hunk-p val)
                  (cl-remove-if-not
                   #'rere--reviewed-p
                   (copy-sequence (rere-hunk-lines val))))
                 ((rere-file-diff-p val)
                  (cl-remove-if-not
                   #'rere--reviewed-p
                   (cl-mapcan
                    (lambda (h)
                      (copy-sequence (rere-hunk-lines h)))
                    (rere-file-diff-hunks val))))
                 (t nil))))))
    (unless to-unaccept
      (user-error "[rere] No reviewed changes to unaccept"))
    (let ((target-hash
           (rere--find-next-target to-unaccept (rere--reviewed-diff-lines))))
      (dolist (dl to-unaccept)
        (rere--unaccept-line dl))
      (rere--schedule-save-reviewed-state)
      (rere--render-buffer target-hash))))

(defun rere-toggle-section ()
  "Toggle section visibility.
If on Pending, Stinky, or Reviewed header, toggle that category.
If on a hunk header, toggle that hunk.
If on a file header or diff line, toggle that file."
  (interactive)
  (let ((sec (magit-current-section)))
    (unless sec
      (user-error "[rere] No section at point"))
    (let ((target-sec
           (cond
            ((memq (oref sec type)
                   '(rere-pending rere-reviewed rere-stinky))
             sec)
            ((eq (oref sec type) 'rere-hunk-section)
             sec)
            (t
             (let ((file-sec sec))
               (while (and file-sec
                           (not (eq (oref file-sec type)
                                    'rere-file-section)))
                 (setq file-sec (oref file-sec parent)))
               (or file-sec sec))))))
      (magit-section-toggle target-sec)
      (when (and (oref target-sec hidden)
                 (oref target-sec content)
                 (> (point) (oref target-sec content)))
        (goto-char (oref target-sec start))))))

(defun rere-toggle-focus ()
  "Toggle focus mode on the file at point.
When active, only changes for this file are displayed.
Pressing 'f' again restores the full view."
  (interactive)
  (if rere--focused-file
      (progn
        (setq rere--focused-file nil)
        (rere--render-buffer)
        (message "[rere] Focus cleared. Showing all files."))
    (let* ((dl (rere--section-diff-line))
           (file-diff (rere--section-file))
           (filename (cond
                      (dl (rere-diff-line-file dl))
                      (file-diff (rere-file-diff-filename file-diff))
                      (t nil))))
      (unless filename
        (user-error "[rere] No file at point to focus"))
      (setq rere--focused-file filename)
      (rere--render-buffer)
      (message "[rere] Focused on %s. Press 'f' to unfocus." filename))))

(defun rere-toggle-context ()
  "Toggle visibility of context lines in diff hunks."
  (interactive)
  (setq rere--show-context (not rere--show-context))
  (rere--render-buffer)
  (message "[rere] Context lines %s."
           (if rere--show-context "shown" "hidden")))

(defun rere-toggle-flag ()
  "Toggle flag on diff line at point or in active region.
Flagged lines move to the 'Stinky changes' section and must be
resolved before 100% review can be reached."
  (interactive)
  (let* ((in-visual (and (bound-and-true-p evil-mode)
                         (evil-visual-state-p)))
         (has-region (or in-visual (use-region-p)))
         (lines (if has-region
                    (cl-remove-if-not
                     #'rere--reviewable-p
                     (rere--region-elements
                      (region-beginning) (region-end)))
                  (when-let* ((dl (rere--section-diff-line)))
                    (when (rere--reviewable-p dl)
                      (list dl))))))
    (when in-visual (evil-normal-state))
    (when has-region (deactivate-mark))
    (unless lines
      (user-error "[rere] No reviewable diff line at point"))
    (let* ((all-flagged (cl-every #'rere--flagged-p lines))
           (target-hash
            (if all-flagged
                (rere--find-next-target
                 lines (rere--flagged-diff-lines))
              (rere--find-next-target
               lines (rere--pending-diff-lines)))))
      (dolist (dl lines)
        (if all-flagged
            (rere--unflag-line dl)
          (rere--flag-line dl)))
      (rere--schedule-save-reviewed-state)
      (rere--render-buffer target-hash)
      (message "[rere] %s %d line(s)."
               (if all-flagged "Unflagged" "Flagged")
               (length lines)))))

(defun rere-next-flagged ()
  "Jump to the next flagged (stinky) diff line."
  (interactive)
  (let ((found nil)
        (orig (point)))
    (save-excursion
      (forward-line 1)
      (while (and (not found) (not (eobp)))
        (if (and (get-text-property (point) 'rere-flagged)
                 (not (invisible-p (point))))
            (setq found (point))
          (forward-line 1))))
    (unless found
      (save-excursion
        (goto-char (point-min))
        (while (and (not found) (< (point) orig))
          (if (and (get-text-property (point) 'rere-flagged)
                   (not (invisible-p (point))))
              (setq found (point))
            (forward-line 1)))))
    (if found
        (goto-char found)
      (message "[rere] No flagged lines found"))))

(defun rere-next-diff-line ()
  "Move point to next reviewable diff line, skipping context."
  (interactive)
  (let ((found nil)
        (orig (point)))
    (save-excursion
      (forward-line 1)
      (while (and (not found) (not (eobp)))
        (if (and (get-text-property (point) 'rere-reviewable)
                 (not (invisible-p (point))))
            (setq found (point))
          (forward-line 1))))
    (if found
        (goto-char found)
      (message "[rere] No further reviewable diff lines below")
      (goto-char orig))))

(defun rere-previous-diff-line ()
  "Move point to previous reviewable diff line, skipping context."
  (interactive)
  (let ((found nil)
        (orig (point)))
    (save-excursion
      (forward-line -1)
      (while (and (not found) (not (bobp)))
        (if (and (get-text-property (point) 'rere-reviewable)
                 (not (invisible-p (point))))
            (setq found (point))
          (forward-line -1))))
    (if found
        (goto-char found)
      (message "[rere] No previous reviewable diff lines above")
      (goto-char orig))))

(defun rere-next-file ()
  "Move point to next file heading."
  (interactive)
  (let ((found nil)
        (orig (point))
        (cur-end (line-end-position)))
    (save-excursion
      (forward-line 1)
      (while (and (not found) (not (eobp)))
        (unless (invisible-p (point))
          (when-let* ((section (magit-current-section)))
            (let ((s section))
              (while (and s (not (eq (oref s type)
                                     'rere-file-section)))
                (setq s (oref s parent)))
              (when (and s (> (oref s start) cur-end))
                (setq found (oref s start))))))
        (unless found
          (forward-line 1))))
    (if found
        (goto-char found)
      (message "[rere] No further files below")
      (goto-char orig))))

(defun rere-previous-file ()
  "Move point to previous file heading."
  (interactive)
  (let ((found nil)
        (orig (point))
        (cur-beg (line-beginning-position)))
    (save-excursion
      (forward-line -1)
      (while (and (not found) (not (bobp)))
        (unless (invisible-p (point))
          (when-let* ((section (magit-current-section)))
            (let ((s section))
              (while (and s (not (eq (oref s type)
                                     'rere-file-section)))
                (setq s (oref s parent)))
              (when (and s (< (oref s start) cur-beg))
                (setq found (oref s start))))))
        (unless found
          (forward-line -1))))
    (if found
        (goto-char found)
      (message "[rere] No previous files above")
      (goto-char orig))))

(defun rere-next-pending-file ()
  "Move point to next file heading with pending changes."
  (interactive)
  (let ((found nil)
        (orig (point))
        (cur-end (line-end-position)))
    (save-excursion
      (forward-line 1)
      (while (and (not found) (not (eobp)))
        (unless (invisible-p (point))
          (when-let* ((section (magit-current-section)))
            (let ((s section))
              (while (and s (not (eq (oref s type) 'rere-file-section)))
                (setq s (oref s parent)))
              (when (and s (> (oref s start) cur-end))
                (let ((file-diff (oref s value)))
                  (when (and (rere-file-diff-p file-diff)
                             (rere--file-has-pending-p file-diff))
                    (setq found (oref s start))))))))
        (unless found
          (forward-line 1))))
    (if found
        (goto-char found)
      (message "[rere] No further pending files below")
      (goto-char orig))))

(defun rere-previous-pending-file ()
  "Move point to previous file heading with pending changes."
  (interactive)
  (let ((found nil)
        (orig (point))
        (cur-beg (line-beginning-position)))
    (save-excursion
      (forward-line -1)
      (while (and (not found) (not (bobp)))
        (unless (invisible-p (point))
          (when-let* ((section (magit-current-section)))
            (let ((s section))
              (while (and s (not (eq (oref s type) 'rere-file-section)))
                (setq s (oref s parent)))
              (when (and s (< (oref s start) cur-beg))
                (let ((file-diff (oref s value)))
                  (when (and (rere-file-diff-p file-diff)
                             (rere--file-has-pending-p file-diff))
                    (setq found (oref s start))))))))
        (unless found
          (forward-line -1))))
    (if found
        (goto-char found)
      (message "[rere] No previous pending files above")
      (goto-char orig))))

(defun rere-open-file ()
  "Open the source file at the diff line or file heading at point."
  (interactive)
  (let ((dl (rere--section-diff-line))
        (file-diff (rere--section-file)))
    (unless (or dl file-diff)
      (user-error
       "[rere] No diff line or file at point"))
    (let* ((file (if dl
                     (rere-diff-line-file dl)
                   (rere-file-diff-filename file-diff)))
           (line-num (if dl
                         (or (rere-diff-line-new-line dl)
                             (rere-diff-line-old-line dl)
                             1)
                       1))
           (full-path
            (expand-file-name
             file
             (rere--repo-root))))
      (unless (file-exists-p full-path)
        (user-error
         "[rere] File not found: %s" full-path))
      (find-file full-path)
      (goto-char (point-min))
      (forward-line (1- line-num)))))

(defun rere--repo-root ()
  "Return the repository root directory."
  (string-trim
   (shell-command-to-string
    "git rev-parse --show-toplevel")))

(defun rere-refresh ()
  "Refresh the diff, preserving reviewed and flagged state.
Lines that were reviewed and still exist unchanged
remain in Reviewed.  Flagged lines remain in Stinky changes.
New or modified lines appear in Pending."
  (interactive)
  (let ((old-reviewed
         (copy-hash-table rere--reviewed))
        (old-flagged
         (and rere--flagged (copy-hash-table rere--flagged))))
    (setq rere--diff-files
          (rere--parse-diff (rere--get-raw-diff)))
    ;; rebuild reviewed and flagged sets:
    ;; keep only hashes that still exist in the new diff
    (clrhash rere--reviewed)
    (when old-flagged
      (clrhash rere--flagged))
    (dolist (file rere--diff-files)
      (dolist (hunk (rere-file-diff-hunks file))
        (dolist (dl (rere-hunk-lines hunk))
          (when (rere--reviewable-p dl)
            (let ((h (rere-diff-line-hash dl)))
              (cond
               ((and old-flagged (gethash h old-flagged))
                (puthash h t rere--flagged))
               ((gethash h old-reviewed)
                (puthash h t rere--reviewed))))))))
    (rere--count-lines)
    (rere--save-reviewed-state-now)
    (setq rere--diffstat-cache nil)
    (rere--render-buffer)
    (message "[rere] Diff refreshed. %d/%d reviewed."
             rere--reviewed-count
             rere--total-lines)))

(defun rere-quit ()
  "Quit the rere buffer and restore windows."
  (interactive)
  (rere--save-reviewed-state-now)
  (let ((config rere--saved-window-config))
    (setq rere--saved-window-config nil)
    (kill-buffer (current-buffer))
    (when config
      (set-window-configuration config))))

;;;; Keymap

(defvar rere-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map magit-section-mode-map)
    (define-key map (kbd "s") #'rere-smart-accept)
    (define-key map (kbd "S") #'rere-smart-accept)
    (define-key map (kbd "u") #'rere-unaccept)
    (define-key map (kbd "RET") #'rere-open-file)
    (define-key map (kbd "r") #'rere-refresh)
    (define-key map (kbd "TAB") #'rere-toggle-section)
    (define-key map (kbd "<tab>") #'rere-toggle-section)
    (define-key map (kbd "n") #'rere-next-diff-line)
    (define-key map (kbd "p") #'rere-previous-diff-line)
    (define-key map (kbd "]") #'rere-next-file)
    (define-key map (kbd "[") #'rere-previous-file)
    (define-key map (kbd "}") #'rere-next-pending-file)
    (define-key map (kbd "{") #'rere-previous-pending-file)
    (define-key map (kbd "f") #'rere-toggle-focus)
    (define-key map (kbd "c") #'rere-toggle-context)
    (define-key map (kbd "m") #'rere-toggle-flag)
    (define-key map (kbd "M") #'rere-next-flagged)
    (define-key map (kbd "q") #'rere-quit)
    map)
  "Keymap for `rere-mode'.")

;;;; Evil integration

(defun rere--setup-evil-buffer ()
  "Install buffer-local Evil overrides so global maps do not steal keys."
  (when (fboundp 'evil-local-set-key)
    (dolist (state '(normal motion))
      (evil-local-set-key state (kbd "[") #'rere-previous-file)
      (evil-local-set-key state (kbd "]") #'rere-next-file)
      (evil-local-set-key state (kbd "{") #'rere-previous-pending-file)
      (evil-local-set-key state (kbd "}") #'rere-next-pending-file)
      (evil-local-set-key state (kbd "f") #'rere-toggle-focus)
      (evil-local-set-key state (kbd "c") #'rere-toggle-context)
      (evil-local-set-key state (kbd "m") #'rere-toggle-flag)
      (evil-local-set-key state (kbd "M") #'rere-next-flagged)
      (evil-local-set-key state (kbd "s") #'rere-smart-accept)
      (evil-local-set-key state (kbd "S") #'rere-smart-accept)
      (evil-local-set-key state (kbd "u") #'rere-unaccept)
      (evil-local-set-key state (kbd "r") #'rere-refresh)
      (evil-local-set-key state (kbd "q") #'rere-quit)
      (evil-local-set-key state (kbd "RET") #'rere-open-file)
      (evil-local-set-key state (kbd "TAB") #'rere-toggle-section)
      (evil-local-set-key state (kbd "<tab>") #'rere-toggle-section)
      (evil-local-set-key state (kbd "n") #'rere-next-diff-line)
      (evil-local-set-key state (kbd "p") #'rere-previous-diff-line))
    (evil-local-set-key 'visual (kbd "s") #'rere-smart-accept)
    (evil-local-set-key 'visual (kbd "S") #'rere-smart-accept)
    (evil-local-set-key 'visual (kbd "u") #'rere-unaccept)
    (evil-local-set-key 'visual (kbd "m") #'rere-toggle-flag)))

(defun rere--setup-evil ()
  "Set up Evil keybindings for `rere-mode'."
  (when (bound-and-true-p evil-mode)
    (evil-set-initial-state 'rere-mode 'normal)
    (evil-define-key 'normal rere-mode-map
      (kbd "s") #'rere-smart-accept
      (kbd "S") #'rere-smart-accept
      (kbd "u") #'rere-unaccept
      (kbd "r") #'rere-refresh
      (kbd "q") #'rere-quit
      (kbd "RET") #'rere-open-file
      (kbd "TAB") #'rere-toggle-section
      (kbd "<tab>") #'rere-toggle-section
      (kbd "n") #'rere-next-diff-line
      (kbd "p") #'rere-previous-diff-line
      (kbd "]") #'rere-next-file
      (kbd "[") #'rere-previous-file
      (kbd "}") #'rere-next-pending-file
      (kbd "{") #'rere-previous-pending-file
      (kbd "f") #'rere-toggle-focus
      (kbd "c") #'rere-toggle-context
      (kbd "m") #'rere-toggle-flag
      (kbd "M") #'rere-next-flagged
      (kbd "g g") #'beginning-of-buffer
      (kbd "G") #'end-of-buffer)
    (evil-define-key 'visual rere-mode-map
      (kbd "s") #'rere-smart-accept
      (kbd "S") #'rere-smart-accept
      (kbd "u") #'rere-unaccept
      (kbd "m") #'rere-toggle-flag)))

(with-eval-after-load 'evil
  (rere--setup-evil))

;; Also run now if evil is already loaded
(when (featurep 'evil)
  (rere--setup-evil))

;;;; Major mode

(define-derived-mode rere-mode magit-section-mode
  "Rere"
  "Major mode for rebase review.
\\{rere-mode-map}"
  (setq-local magit-section-inhibit-markers t)
  (add-hook 'kill-buffer-hook #'rere--save-reviewed-state-now nil t)
  (setq-local revert-buffer-function
              (lambda (&rest _) (rere-refresh)))
  (when (fboundp 'evil-local-set-key)
    (rere--setup-evil-buffer)))

;;;; Entry point

;;;###autoload
(defun rere ()
  "Start a Rebase Review session.
Only works during an interactive git rebase."
  (interactive)
  (unless (rere--rebase-in-progress-p)
    (user-error
     "[rere] Not currently in an interactive rebase"))
  (let* ((repo-dir default-directory)
         (config (current-window-configuration))
         (buf (get-buffer-create rere-buffer-name)))
    (switch-to-buffer buf)
    (delete-other-windows)
    (setq default-directory repo-dir)
    (unless (eq major-mode 'rere-mode)
      (rere-mode))
    (setq rere--saved-window-config config)
    (let* ((new-info (rere--read-commit-info))
           (new-sha (plist-get new-info :sha)))
      (rere--cleanup-old-reviewed-states new-sha)
      (setq rere--commit-info new-info)
      (setq rere--reviewed (rere--load-reviewed-state))
      (setq rere--flagged (rere--load-flagged-state)))
    (setq rere--diff-files
          (rere--parse-diff (rere--get-raw-diff)))
    (setq rere--diffstat-cache nil)
    (rere--render-buffer)
    (or (rere--goto-first-pending)
        (goto-char (point-min)))))

(provide 'rere)
;;; rere.el ends here
