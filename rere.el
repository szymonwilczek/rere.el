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
  "Save reviewed hashes to the rebase state directory."
  (when-let* ((rebase-dir (rere--rebase-dir))
              (sha (plist-get rere--commit-info :sha)))
    (let ((file (expand-file-name
                 (format "rere-reviewed-%s" sha)
                 rebase-dir))
          (hashes '())
          (write-region-inhibit-fsync t))
      (when rere--reviewed
        (maphash (lambda (k _v) (push k hashes))
                 rere--reviewed))
      (with-temp-file file
        (dolist (h (nreverse hashes))
          (insert h "\n"))))))

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

(defun rere--cleanup-old-reviewed-states (current-sha)
  "Delete review state files from previous commits in REBASE-DIR."
  (when-let* ((rebase-dir (rere--rebase-dir)))
    (dolist (f (file-expand-wildcards
                (expand-file-name "rere-reviewed-*" rebase-dir)))
      (unless (equal (file-name-nondirectory f)
                     (format "rere-reviewed-%s" current-sha))
        (ignore-errors (delete-file f))))))

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

(defun rere--pending-p (diff-line)
  "Return non-nil if DIFF-LINE is pending review."
  (and (rere--reviewable-p diff-line)
       (not (rere--reviewed-p diff-line))))

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

;;;; Review operations

(defun rere--accept-line (diff-line)
  "Mark DIFF-LINE as reviewed."
  (when (rere--reviewable-p diff-line)
    (unless rere--reviewed
      (setq rere--reviewed
            (make-hash-table :test 'equal)))
    (puthash (rere-diff-line-hash diff-line)
             t rere--reviewed)))

(defun rere--unaccept-line (diff-line)
  "Mark DIFF-LINE as not reviewed (pending)."
  (when rere--reviewed
    (remhash (rere-diff-line-hash diff-line)
             rere--reviewed)))

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
      (rere--insert-reviewed-section)
      (rere--insert-footer))
    (magit-section-show magit-root-section)
    (when (and (> rere--total-lines 0)
               (= rere--reviewed-count rere--total-lines))
      (message
       "[rere] 100%% reviewed! Press 'q' to return, then continue in Magit."))
    ;; restore position
    (or (and saved-line-hash
             (rere--goto-line-hash saved-line-hash))
        (rere--goto-section-path saved-section-path)
        (rere--goto-first-pending)
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

(defun rere--current-section-path ()
  "Return path identifier for current section."
  (when-let* ((section (magit-current-section)))
    (magit-section-ident section)))

(defun rere--line-hash-at-point ()
  "Return the diff-line hash at point, if any."
  (when-let* ((section (magit-current-section)))
    (let ((value (oref section value)))
      (when (rere-diff-line-p value)
        (rere-diff-line-hash value)))))

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
         (done (and (> rere--total-lines 0)
                    (= rere--reviewed-count rere--total-lines)))
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
        (format "Progress: %d/%d lines reviewed [%d%%]\n"
                rere--reviewed-count
                rere--total-lines pct)
        'font-lock-face (if done
                            'magit-diff-added
                          'magit-section-heading)))
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
                                            max-len)
  "Format a single file diffstat line for FILENAME.
ADDED, REMOVED, REVIEWED, and TOTAL are line counts.
MAX-LEN is the maximum filename display width."
  (let* ((disp-fn (if (> (length filename) 35)
                      (concat "..." (substring filename
                                               (- (length filename) 32)))
                    filename))
         (padding (make-string (max 0 (- max-len (length disp-fn))) ?\s))
         (tot-diff (+ added removed))
         (graph (rere--diffstat-graph added removed 15))
         (rev-part (if (= reviewed total)
                       (propertize (format "  [%d/%d]" reviewed total)
                                   'font-lock-face 'magit-diff-added)
                     (propertize (format "  [%d/%d]" reviewed total)
                                 'font-lock-face 'magit-dimmed))))
    (concat (propertize (concat "  " disp-fn)
                        'font-lock-face 'magit-diff-file-heading)
            padding
            (propertize " | " 'font-lock-face 'magit-dimmed)
            (propertize (format "%2d " tot-diff)
                        'font-lock-face 'magit-dimmed)
            graph
            rev-part
            "\n")))

(defun rere--insert-diffstat-section ()
  "Insert diffstat section listing changed files and review stats."
  (when (and rere-show-diffstat rere--diff-files)
    (magit-insert-section (rere-diffstat nil nil)
      (magit-insert-heading
        (format "Files changed (%d)\n" (length rere--diff-files)))
      (let* ((names (mapcar (lambda (f)
                              (let ((fn (rere-file-diff-filename f)))
                                (if (> (length fn) 35)
                                    (concat "..."
                                            (substring
                                             fn (- (length fn) 32)))
                                  fn)))
                            rere--diff-files))
             (max-len (min 35 (max 10 (apply #'max
                                             (mapcar #'length names))))))
        (dolist (file rere--diff-files)
          (let ((added 0)
                (removed 0)
                (reviewed 0)
                (total 0))
            (dolist (hunk (rere-file-diff-hunks file))
              (dolist (dl (rere-hunk-lines hunk))
                (when (rere--reviewable-p dl)
                  (cl-incf total)
                  (if (eq (rere-diff-line-type dl) 'added)
                      (cl-incf added)
                    (cl-incf removed))
                  (when (rere--reviewed-p dl)
                    (cl-incf reviewed)))))
            (magit-insert-section (rere-file-stat file nil)
              (insert
               (rere--format-file-diffstat
                (rere-file-diff-filename file)
                added removed reviewed total max-len))))))
      (insert "\n"))))

(defun rere--insert-pending-section ()
  "Insert the Pending review section."
  (let ((pending-count
         (- rere--total-lines rere--reviewed-count)))
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

(defun rere--insert-reviewed-section ()
  "Insert the Reviewed changes section."
  (magit-insert-section (rere-reviewed nil t)
    (magit-insert-heading
      (format "Reviewed changes (%d)\n"
              rere--reviewed-count))
    (when (> rere--reviewed-count 0)
      (rere--insert-diff-lines #'rere--reviewed-p))
    (insert "\n")))

(defun rere--insert-diff-lines (pred)
  "Insert diff lines matching PRED grouped by file/hunk."
  (dolist (file rere--diff-files)
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
                   dl))))))))))

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
           ((eq (rere-diff-line-type dl) 'context)
            (push dl lines))))
        (when has-matching
          (push (cons hunk (nreverse lines)) result))))
    (nreverse result)))

(defun rere--insert-single-line (dl)
  "Insert a single diff line DL with proper face and word refinement."
  (let* ((type (rere-diff-line-type dl))
         (face (pcase type
                 ('added 'magit-diff-added)
                 ('removed 'magit-diff-removed)
                 ('context 'magit-diff-context)
                 (_ 'default)))
         (hl-face (when rere-refine-highlight
                    (pcase type
                      ('added (rere--added-highlight-face))
                      ('removed (rere--removed-highlight-face)))))
         (prefix (pcase type
                   ('added "+")
                   ('removed "-")
                   (_ " ")))
         (content (rere-diff-line-content dl))
         (highlights (when rere-refine-highlight
                       (rere-diff-line-highlights dl)))
         (hash (rere-diff-line-hash dl))
         (reviewable (rere--reviewable-p dl))
         (pending (rere--pending-p dl)))
    (magit-insert-section (rere-line dl)
      (let ((beg (point)))
        (insert "  " prefix content "\n")
        (put-text-property beg (point) 'font-lock-face face)
        (put-text-property beg (point) 'rere-line-hash hash)
        (when reviewable
          (put-text-property beg (point) 'rere-reviewable t))
        (when pending
          (put-text-property beg (point) 'rere-pending t))
        (when (and hl-face highlights)
          (let ((offset (+ beg 3)))
            (dolist (hl highlights)
              (put-text-property (+ offset (car hl))
                                 (+ offset (cdr hl))
                                 'font-lock-face hl-face))))))))

(defun rere--insert-footer ()
  "Insert footer with keybinding hints."
  (when (and (> rere--total-lines 0)
             (= rere--reviewed-count rere--total-lines))
    (insert
     (propertize
      (concat "\n100% reviewed! Press 'q' to return, "
              "then amend or continue in Magit.\n")
      'font-lock-face 'magit-diff-added)))
  (insert
   (propertize
    (concat "\n"
            "s accept  u undo  "
            "n/p diff  [/] file  TAB toggle  "
            "RET edit  r refresh  q quit\n")
    'font-lock-face 'magit-dimmed)))

;;;; Interactive commands

(defun rere--section-diff-line ()
  "Return the diff-line at point, or nil."
  (when-let* ((section (magit-current-section)))
    (let ((val (oref section value)))
      (when (rere-diff-line-p val) val))))

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
         (all-pending (rere--pending-diff-lines)))
    (if has-region
        (progn
          (setq to-accept
                (cl-remove-if-not
                 #'rere--pending-p
                 (rere--region-elements
                  (region-beginning) (region-end))))
          (when in-visual
            (evil-normal-state))
          (deactivate-mark))
      (when-let* ((section (magit-current-section)))
        (let ((val (oref section value)))
          (setq to-accept
                (cond
                 ((eq (oref section type) 'rere-pending)
                  all-pending)
                 ((rere-file-diff-p val)
                  (cl-remove-if-not
                   #'rere--pending-p
                   (cl-mapcan
                    (lambda (h)
                      (copy-sequence (rere-hunk-lines h)))
                    (rere-file-diff-hunks val))))
                 ((rere-hunk-p val)
                  (cl-remove-if-not
                   #'rere--pending-p
                   (copy-sequence (rere-hunk-lines val))))
                 ((rere-diff-line-p val)
                  (when (rere--pending-p val)
                    (list val)))
                 (t nil))))))
    (unless to-accept
      (user-error "[rere] No pending changes to accept"))
    (let ((target-hash
           (rere--find-next-target to-accept all-pending)))
      (dolist (dl to-accept)
        (rere--accept-line dl))
      (rere--save-reviewed-state)
      (rere--render-buffer target-hash))))

(defun rere-unaccept ()
  "Undo acceptance of region, category, line, hunk, or file at point.
Move items back from Reviewed to Pending."
  (interactive)
  (let* ((in-visual (and (bound-and-true-p evil-mode)
                         (evil-visual-state-p)))
         (has-region (or in-visual (use-region-p)))
         (to-unaccept nil)
         (all-reviewed (rere--reviewed-diff-lines)))
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
                  all-reviewed)
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
           (rere--find-next-target to-unaccept all-reviewed)))
      (dolist (dl to-unaccept)
        (rere--unaccept-line dl))
      (rere--save-reviewed-state)
      (rere--render-buffer target-hash))))

(defun rere-toggle-section ()
  "Toggle section visibility.
If on Pending or Reviewed header, toggle that category.
If on a hunk header, toggle that hunk.
If on a file header or diff line, toggle that file."
  (interactive)
  (let ((sec (magit-current-section)))
    (unless sec
      (user-error "[rere] No section at point"))
    (let ((target-sec
           (cond
            ((memq (oref sec type) '(rere-pending rere-reviewed))
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
  "Refresh the diff, preserving reviewed state.
Lines that were reviewed and still exist unchanged
remain in Reviewed.  New or modified lines appear in
Pending."
  (interactive)
  (let ((old-reviewed
         (copy-hash-table rere--reviewed)))
    (setq rere--diff-files
          (rere--parse-diff (rere--get-raw-diff)))
    ;; rebuild reviewed set:
    ;; keep only hashes that still exist in the new diff
    (clrhash rere--reviewed)
    (dolist (file rere--diff-files)
      (dolist (hunk (rere-file-diff-hunks file))
        (dolist (dl (rere-hunk-lines hunk))
          (when (and (rere--reviewable-p dl)
                     (gethash
                      (rere-diff-line-hash dl)
                      old-reviewed))
            (puthash (rere-diff-line-hash dl)
                     t rere--reviewed)))))
    (rere--count-lines)
    (rere--save-reviewed-state)
    (rere--render-buffer)
    (message "[rere] Diff refreshed. %d/%d reviewed."
             rere--reviewed-count
             rere--total-lines)))

(defun rere-quit ()
  "Quit the rere buffer and restore windows."
  (interactive)
  (rere--save-reviewed-state)
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
    (define-key map (kbd "q") #'rere-quit)
    map)
  "Keymap for `rere-mode'.")

;;;; Evil integration

(defun rere--setup-evil ()
  "Set up Evil keybindings for `rere-mode'.
Bind review keys in normal and visual states so Evil does not
shadow them."
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
      (kbd "g g") #'beginning-of-buffer
      (kbd "G") #'end-of-buffer)
    (evil-define-key 'visual rere-mode-map
      (kbd "s") #'rere-smart-accept
      (kbd "S") #'rere-smart-accept
      (kbd "u") #'rere-unaccept)))

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
  (add-hook 'kill-buffer-hook #'rere--save-reviewed-state nil t)
  (setq-local revert-buffer-function
              (lambda (&rest _) (rere-refresh))))

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
      (setq rere--reviewed (rere--load-reviewed-state)))
    (setq rere--diff-files
          (rere--parse-diff (rere--get-raw-diff)))
    (rere--render-buffer)
    (or (rere--goto-first-pending)
        (goto-char (point-min)))))

(provide 'rere)
;;; rere.el ends here
