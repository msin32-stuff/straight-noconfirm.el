;;; straight-noconfirm.el --- Never-prompting straight-pull-all  -*- lexical-binding: t; -*-

;; Requires: straight.el (load this file after straight is bootstrapped)

;;; Commentary:

;; This file adds `straight-pull-all-noconfirm', a version of
;; `straight-pull-all' that never asks you anything.
;;
;; Every question straight.el can ask while pulling goes through one
;; function, `straight--popup-raw'.  While the command runs, that
;; function is replaced by one that looks up a preconfigured answer
;; (a key such as "d") and runs the matching action, exactly as if you
;; had pressed the key.  Nested confirmations ("Discard all local
;; changes permanently?") are answered automatically too.
;;
;; Answers are looked up in three layers, first match wins:
;;
;;   1. `straight-noconfirm-package-answers' - for specific packages
;;   2. `straight-noconfirm-answers'         - for any package
;;   3. built-in defaults, chosen to never destroy data
;;
;; Questions (see `straight-noconfirm-question-patterns'):
;;
;;   remote-url-mismatch  "In repository X, remote Y has URL ..."
;;                        keys: r d  (R and D need typed input; unusable)
;;   dirty-worktree       "Repository X has a dirty worktree"   keys: z d
;;   unfinished-rebase    "Repository X has an unfinished rebase"  key: a
;;   merge-conflict       "Repository X has a merge conflict"      key: a
;;   branch-missing       "... branch Y does not exist."           key: c
;;   head-diverged        "... HEAD on default branch Y has diverged from Z"
;;                        keys: m r x  (and c/f depending on the case)
;;   head-ahead           "... HEAD on Y is ahead of ..."          keys: c f
;;   head-behind          "... HEAD ... is behind ..."             keys: f c
;;   head-even            "... HEAD on Y is even with default branch Z"
;;   status-failure       "... failed to get status"      (no usable key)
;;   confirm              any nested yes/no "are you sure?"        key: y
;;   processing-error     "While processing repository X, an error occurred"
;;                        keys: c s SPC C-g
;;   processing-paused    "Processing of repository X paused at your request"
;;   unknown              any question this file does not recognize
;;
;; An answer is one of:
;;
;;   "d" or ?d           press that key
;;   ("x" "f")           press the first of these that is offered
;;   fail                answer nothing: give up on this repository (it
;;                       then goes through `processing-error', whose
;;                       built-in answer "c" cancels it)
;;   a function          called with a plist (:question :prompt :keys
;;                       :package :repo); returns any of the above, or
;;                       nil to fall through to the next layer
;;
;; Examples:
;;
;;   ;; Any package: discard a dirty worktree (no second prompt), and when
;;   ;; the default branch has diverged, reset it to the remote.
;;   (setq straight-noconfirm-answers
;;         '((dirty-worktree     . "d")
;;           (remote-url-mismatch . "d")
;;           (head-diverged      . "x")
;;           (processing-error   . "c")))
;;
;;   ;; Only for slime: rename the bad remote aside instead of deleting it.
;;   (straight-noconfirm-set-answer 'remote-url-mismatch "r" "slime")
;;
;;   ;; Only cancel when the error is a submodule failure, else skip & retry.
;;   (setq straight-noconfirm-answers
;;         (cons (cons 'processing-error
;;                     (lambda (ctx)
;;                       (if (string-match-p "git submodule"
;;                                           (plist-get ctx :prompt))
;;                           "c"
;;                         "s")))
;;               straight-noconfirm-answers))
;;
;; Then run M-x straight-pull-all-noconfirm (or call it from batch Emacs).
;; Every automatic answer is reported with `message' and kept in
;; `straight-noconfirm-last-log'.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'straight)

(defgroup straight-noconfirm nil
  "Run straight.el operations without ever prompting."
  :group 'straight
  :prefix "straight-noconfirm-")

;;;; Configuration

(defcustom straight-noconfirm-answers nil
  "Answers to use for any package.
Alist of (QUESTION . ANSWER).  See the Commentary of
straight-noconfirm.el for the questions and the forms an ANSWER can
take.  Questions not listed here use the built-in default.
Per-package settings in `straight-noconfirm-package-answers' take
precedence."
  :group 'straight-noconfirm
  :type '(alist :key-type symbol :value-type sexp))

(defcustom straight-noconfirm-package-answers nil
  "Answers to use for specific packages.
Alist of (NAME . ((QUESTION . ANSWER) ...)).  NAME is a package name
or a local repository name (both strings); an entry for any package
that lives in the repository being processed applies.  These take
precedence over `straight-noconfirm-answers'."
  :group 'straight-noconfirm
  :type '(alist :key-type string
                :value-type (alist :key-type symbol :value-type sexp)))

(defcustom straight-noconfirm-stash-message
  "straight-pull-all-noconfirm: automatic stash"
  "Message for stashes made when a dirty worktree is answered with \"z\".
The empty string lets Git pick its default message."
  :group 'straight-noconfirm
  :type 'string)

(defcustom straight-noconfirm-max-repeats 5
  "How often one repository may get the same answer to the same question.
straight.el re-checks a repository after every action and asks again
if the problem persists, so an answer that does not fix the problem
would loop forever.  Past this many repeats the repository is given up
on (or, for `processing-error', canceled)."
  :group 'straight-noconfirm
  :type 'integer)

(defconst straight-noconfirm--qstr
  "\"\\(?:[^\"\\]\\|\\\\.\\)*\""
  "Regexp matching a string as printed by `%S'.")

(defvar straight-noconfirm-question-patterns
  (let ((q straight-noconfirm--qstr))
    (list
     (cons 'unfinished-rebase
           (concat "\\`Repository " q " has an unfinished rebase"))
     (cons 'merge-conflict
           (concat "\\`Repository " q " has a merge conflict"))
     (cons 'dirty-worktree
           (concat "\\`Repository " q " has a dirty worktree"))
     (cons 'remote-url-mismatch
           (concat "\\`In repository " q ", remote " q " has URL"))
     (cons 'branch-missing
           (concat "\\`In repository " q ", +branch " q " does not exist\\."))
     (cons 'status-failure
           (concat "\\`In repository " q ", failed to get status:"))
     (cons 'head-even
           (concat "\\`In repository " q ", \\(?:detached HEAD\\|HEAD on "
                   q "\\) is even with default branch"))
     (cons 'head-diverged
           (concat "\\`In repository " q ", \\(?:detached HEAD\\|HEAD on "
                   "\\(?:default branch \\)?" q "\\) has diverged from"))
     (cons 'head-ahead
           (concat "\\`In repository " q ", \\(?:detached HEAD\\|HEAD on "
                   "\\(?:default branch \\)?" q "\\) is ahead of"))
     (cons 'head-behind
           (concat "\\`In repository " q ", \\(?:detached HEAD\\|HEAD on "
                   "\\(?:default branch \\)?" q "\\) is behind"))
     (cons 'processing-error
           (concat "\\`While processing repository " q
                   ", an error occurred:"))
     (cons 'processing-paused
           (concat "\\`Processing of repository " q
                   " paused at your request"))))
  "Alist of (QUESTION . REGEXP) used to recognize straight.el prompts.
Questions are matched in order against the prompt text.  Nested
yes/no confirmations are recognized by their keys instead and are
called `confirm'.  Anything unmatched is `unknown'.")

(defconst straight-noconfirm--builtin-answers
  '((remote-url-mismatch . "r")     ; rename the bad remote aside, re-add
    (unfinished-rebase   . "a")     ; abort it
    (merge-conflict      . "a")     ; abort it
    (dirty-worktree      . "z")     ; stash, including untracked files
    (branch-missing      . "c")     ; create it
    (head-diverged       . fail)    ; needs a human decision
    (head-ahead          . "c")     ; check out default branch, keep other
    (head-behind         . ("f" "c"))
    (head-even           . "c")
    (status-failure      . fail)
    (confirm             . "y")     ; the action was already chosen above
    (processing-error    . "c")     ; cancel this repository
    (processing-paused   . "C-g")   ; user hit C-g: stop everything
    (unknown             . fail))
  "Built-in answers, used when nothing else is configured.
They are chosen to never destroy data: nothing is discarded or reset,
and anything that needs a human decision cancels that repository.")

(defconst straight-noconfirm--fallback-keys
  '((processing-error . "c") (processing-paused . "C-g"))
  "Keys used when a question that cannot `fail' has no usable answer.
`processing-error' and `processing-paused' are asked outside the
error handling of `straight--map-repos-interactively', so signaling
an error there would abort the whole run.")

(defconst straight-noconfirm--unusable-keys '("e" "g" "l" "R" "D")
  "Keys that are never pressed automatically.
\"e\", \"g\" and \"l\" open recursive edits; \"R\" and \"D\" ask for typed
input after partly modifying the repository.")

;;;; State

(defvar straight-noconfirm--package nil
  "Package currently being processed by `straight-pull-all-noconfirm'.")
(defvar straight-noconfirm--counts nil
  "Hash table counting answers per (REPO QUESTION KEY).")
(defvar straight-noconfirm--log nil
  "Answers given during the current run, newest first.")
(defvar straight-noconfirm--in-action nil
  "Non-nil while an automatically chosen action is running.")

(defvar straight-noconfirm-last-log nil
  "Answers given by the last `straight-pull-all-noconfirm', oldest first.
Each element is a plist with :repo :package :question :key :desc.
A :key of nil means no answer was given and the repository was failed.")

;;;; Helpers

(defun straight-noconfirm-set-answer (question answer &optional package)
  "Set the ANSWER to QUESTION, for PACKAGE or (if nil) for any package.
PACKAGE is a package or local repository name.  See the Commentary of
straight-noconfirm.el for the forms an ANSWER can take."
  (if package
      (let ((entry (assoc package straight-noconfirm-package-answers)))
        (unless entry
          (setq entry (list package))
          (push entry straight-noconfirm-package-answers))
        (setcdr entry (cons (cons question answer)
                            (assq-delete-all question (cdr entry)))))
    (setq straight-noconfirm-answers
          (cons (cons question answer)
                (assq-delete-all question straight-noconfirm-answers))))
  answer)

(defun straight-noconfirm--identify (prompt actions)
  "Return the question symbol for PROMPT with ACTIONS."
  (let ((keys (mapcar #'car actions))
        (case-fold-search nil))
    (cond
     ((and (member "y" keys) (member "n" keys)) 'confirm)
     ((cl-loop for (id . re) in straight-noconfirm-question-patterns
               when (string-match-p re prompt) return id))
     (t 'unknown))))

(defun straight-noconfirm--prompt-repo (prompt)
  "Return the repository name mentioned in PROMPT, or nil."
  (let ((case-fold-search nil))
    (when (string-match
           (concat "[Rr]epository \\(" straight-noconfirm--qstr "\\)") prompt)
      (let ((s (ignore-errors
                 (car (read-from-string (match-string 1 prompt))))))
        (and (stringp s) s)))))

(defun straight-noconfirm--recipe-repo (package)
  "Return the local repository of PACKAGE."
  (plist-get (gethash package straight--recipe-cache) :local-repo))

(defun straight-noconfirm--repo-packages (repo)
  "Return the names of all known packages living in local repository REPO."
  (let (pkgs)
    (maphash (lambda (pkg recipe)
               (when (equal (plist-get recipe :local-repo) repo)
                 (push pkg pkgs)))
             straight--recipe-cache)
    (sort pkgs #'string<)))

(defun straight-noconfirm--context (prompt actions)
  "Return a plist describing the question PROMPT with ACTIONS."
  (let* ((parsed (straight-noconfirm--prompt-repo prompt))
         (pkg straight-noconfirm--package)
         (pkg-repo (and pkg (straight-noconfirm--recipe-repo pkg)))
         (repo (or parsed pkg-repo))
         ;; The package is only trustworthy if it belongs to the
         ;; repository the question is about.
         (pkg (and (or (null parsed) (equal parsed pkg-repo)) pkg)))
    (list :question (straight-noconfirm--identify prompt actions)
          :prompt prompt
          :keys (mapcar #'car actions)
          :package pkg
          :repo repo
          :subjects (delete-dups
                     (delq nil (append (list pkg repo)
                                       (and repo (straight-noconfirm--repo-packages
                                                  repo))))))))

(defun straight-noconfirm--specs (question subjects)
  "Return configured answer specs for QUESTION, most specific first."
  (append
   (cl-loop for subject in subjects
            for cell = (assq question
                             (cdr (assoc subject
                                         straight-noconfirm-package-answers)))
            when cell collect (cdr cell))
   (let ((cell (assq question straight-noconfirm-answers)))
     (and cell (list (cdr cell))))
   (let ((cell (assq question straight-noconfirm--builtin-answers)))
     (and cell (list (cdr cell))))))

(defun straight-noconfirm--keys-of (spec)
  "Normalize answer SPEC to a list of key strings."
  (cond ((integerp spec) (list (char-to-string spec)))
        ((stringp spec) (list spec))
        ((listp spec)
         (mapcar (lambda (k) (if (integerp k) (char-to-string k) k)) spec))
        (t (error "Invalid answer %S" spec))))

(defun straight-noconfirm--decide (ctx actions)
  "Pick the key to press for CTX.
Return (KEY) on success, or (nil . REASON) if the question cannot be
answered."
  (let* ((question (plist-get ctx :question))
         (spec (cl-loop for s in (straight-noconfirm--specs
                                  question (plist-get ctx :subjects))
                        for resolved = (if (and (not (eq s 'fail))
                                                (functionp s))
                                           (funcall s ctx)
                                         s)
                        when resolved return resolved))
         (offered (mapcar #'car actions)))
    (cond
     ((null spec)
      (cons nil (format "no answer configured for %s" question)))
     ((eq spec 'fail)
      (cons nil (format "%s is configured to fail" question)))
     (t
      (let* ((wanted (straight-noconfirm--keys-of spec))
             (key (cl-find-if
                   (lambda (k)
                     (and (member k offered)
                          (not (member k straight-noconfirm--unusable-keys))))
                   wanted)))
        (if key
            (list key)
          (cons nil (format "answer %s to %s is not usable here (offered: %s)"
                            (mapconcat (lambda (k) (format "%S" k)) wanted " or ")
                            question
                            (mapconcat
                             (lambda (k) (format "%S" k))
                             (cl-remove-if
                              (lambda (k)
                                (member k straight-noconfirm--unusable-keys))
                              offered)
                             ", ")))))))))

(defun straight-noconfirm--give-up (ctx reason)
  "Fail the repository of CTX because of REASON."
  (push (list :repo (plist-get ctx :repo) :package (plist-get ctx :package)
              :question (plist-get ctx :question) :key nil :desc reason)
        straight-noconfirm--log)
  (message "straight-pull-all-noconfirm: %s: %s; giving up on it"
           (or (plist-get ctx :repo) "?") reason)
  (error "straight-noconfirm: %s" reason))

;;;; The replacement popup

(defun straight-noconfirm--popup (prompt actions)
  "Answer the question PROMPT with ACTIONS from the configuration.
This replaces `straight--popup-raw' while a noconfirm command runs and
has the same calling convention and return value."
  (let* ((ctx (straight-noconfirm--context prompt actions))
         (question (plist-get ctx :question))
         (repo (plist-get ctx :repo))
         (decision (straight-noconfirm--decide ctx actions))
         (key (car decision))
         (reason (cdr decision)))
    ;; Loop guard.
    (when key
      (let* ((counter (list repo question key))
             (n (1+ (gethash counter straight-noconfirm--counts 0))))
        (puthash counter n straight-noconfirm--counts)
        (when (> n straight-noconfirm-max-repeats)
          (setq reason (format "answered %S to %s %d times in a row without fixing it"
                               key question (1- n))
                key nil))))
    (unless key
      (let ((fallback (cdr (assq question straight-noconfirm--fallback-keys))))
        (if (and fallback (assoc fallback actions))
            (progn
              (message "straight-pull-all-noconfirm: %s: %s; using %S instead"
                       (or repo "?") reason fallback)
              (setq key fallback))
          (straight-noconfirm--give-up ctx reason))))
    (let ((action (cl-find key actions :key #'car :test #'equal :from-end t)))
      (cl-destructuring-bind (_key desc func . args) action
        (push (list :repo repo :package (plist-get ctx :package)
                    :question question :key key :desc desc)
              straight-noconfirm--log)
        (message "straight-pull-all-noconfirm: %s: %s -> %s%s%s"
                 (or repo "?") question key
                 (if (stringp desc) (format " (%s)" desc) "")
                 (if (eq question 'processing-error)
                     (format "; error was: %s"
                             (string-trim
                              (replace-regexp-in-string
                               "\\`.*an error occurred:" "" prompt)))
                   ""))
        ;; `straight--popup-raw' returns the value of the action.
        (let ((straight-noconfirm--in-action t))
          (apply func args))))))

(defun straight-noconfirm--read-string-advice (orig prompt &rest args)
  "Around advice for `read-string': never ask during an automatic action.
ORIG is the original function; PROMPT and ARGS are its arguments."
  (cond
   ((not straight-noconfirm--in-action) (apply orig prompt args))
   ((string-prefix-p "Optional stash message" prompt)
    straight-noconfirm-stash-message)
   (t (error "straight-noconfirm: refusing to ask for input (%s)" prompt))))

;;;; Command

;;;###autoload
(defun straight-pull-all-noconfirm (&optional from-upstream predicate)
  "Like `straight-pull-all', but never prompts.
Every question is answered from `straight-noconfirm-package-answers',
`straight-noconfirm-answers' or the built-in defaults; see the
Commentary of straight-noconfirm.el.  A repository whose question has
no usable answer is canceled and not retried.

With prefix argument FROM-UPSTREAM, pull from upstream remotes too.
PREDICATE, if non-nil, is called with a package name and should return
non-nil for packages that should be pulled.

Return the recipes of repositories that were canceled.  Repositories
canceled while fetching are not merged."
  (interactive "P")
  (let ((straight-noconfirm--counts (make-hash-table :test #'equal))
        (straight-noconfirm--package nil)
        (straight-noconfirm--log nil)
        (straight-noconfirm--in-action nil)
        (straight-display-subprocess-prompts nil)
        (process-environment (cons "GIT_TERMINAL_PROMPT=0"
                                   process-environment))
        (canceled nil))
    (advice-add 'straight--popup-raw :override #'straight-noconfirm--popup)
    (advice-add 'read-string :around #'straight-noconfirm--read-string-advice)
    (unwind-protect
        (dolist (phase '(fetch merge))
          (let ((canceled-repos (mapcar (lambda (r) (plist-get r :local-repo))
                                        canceled)))
            (setq canceled
                  (append
                   canceled
                   (straight--map-existing-repos-interactively
                    (lambda (package)
                      (setq straight-noconfirm--package package)
                      (if (eq phase 'fetch)
                          (straight-fetch-package package from-upstream)
                        (straight-merge-package package from-upstream)))
                    (lambda (package)
                      (and (not (member (straight-noconfirm--recipe-repo package)
                                        canceled-repos))
                           (or (null predicate) (funcall predicate package)))))))))
      (advice-remove 'straight--popup-raw #'straight-noconfirm--popup)
      (advice-remove 'read-string #'straight-noconfirm--read-string-advice)
      (setq straight-noconfirm-last-log (reverse straight-noconfirm--log)))
    (let ((answered (cl-count-if (lambda (e) (plist-get e :key))
                                 straight-noconfirm-last-log)))
      (message "straight-pull-all-noconfirm: done; %d answer%s given automatically%s"
               answered (if (= answered 1) "" "s")
               (if canceled
                   (format "; canceled: %s"
                           (mapconcat (lambda (r) (format "%s" (plist-get r :local-repo)))
                                      canceled ", "))
                 "")))
    canceled))

(provide 'straight-noconfirm)
;;; straight-noconfirm.el ends here
