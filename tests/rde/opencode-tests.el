;;; opencode-tests.el --- Tests for opencode.el -*- lexical-binding: t; -*-

(require 'ert)
(load (expand-file-name "../../src/rde/features/opencode.el"
                        (file-name-directory load-file-name))
      nil t)

(defvar opencode-server-url)
(defvar opencode-server-instances)
(defvar opencode-agent-alist)
(defvar opencode-agent-current)
(defvar opencode--instance-discovery-failures)
(defvar opencode-diff-line-markers)
(defvar majutsu-buffer-diff-range)
(defvar magit-buffer-range)
(defvar magit-buffer-typearg)
(defvar magit-buffer-diff-type)
(defvar magit-buffer-revision)
(defvar magit-buffer-revision-hash)
(defvar magit-buffer-diff-args)
(declare-function opencode--choose-agent "opencode" (cross-project))
(declare-function opencode--compose-buffer "opencode" (selection))
(declare-function opencode--discover-instances "opencode" ())
(declare-function opencode--instance-candidates "opencode" ())
(declare-function opencode--pick-agent
                  "opencode"
                  (server-url instance-label query-directory
                              &optional project-label expected-workspace
                               workspace-catalog))
(declare-function opencode--pick-instance "opencode" ())
(declare-function opencode--pick-instance-project-agent "opencode" (instance))
(declare-function opencode--pick-selection
                  "opencode" (server-url selections &optional project-label))
(declare-function opencode--render-context "opencode" (capture &optional previous))
(declare-function opencode--render-diff-records "opencode" (records))
(declare-function opencode--diff-line-records "opencode" (hunk start end))
(declare-function opencode--majutsu-revision-context "opencode" (root))
(declare-function opencode--magit-revision-context "opencode" (root hunk))
(declare-function opencode--revision-side "opencode" (vcs root expression))
(declare-function opencode--git-empty-tree-hash "opencode" (root))
(declare-function opencode--semantic-diff-file-p "opencode" (section majutsu))
(declare-function opencode--selection-for-root "opencode" ())
(declare-function opencode--send "opencode" (selection text))

(defun opencode-test--selection (server-url &optional log-id created)
  "Return a complete test selection for SERVER-URL."
  (list :server-url server-url
        :instance-label "test"
        :log-id (or log-id 42)
        :created (or created 1000)
        :project-id "project-id"
        :workspace-id "workspace-id"
        :label "project / agent"
        :directory "/project"
        :worktree "/project"))

(defun opencode-test--logs-response (&optional log-id created project-id)
  "Return a matching /agent/logs response for tests."
  `((project . ((projectID . ,(or project-id "project-id"))
                (directory . "/project")
                (worktree . "/project")))
    (logs . ,(vector `((logID . ,(or log-id 42))
                       (title . "agent")
                       (created . ,(or created 1000))
                       (updated . 2000))))))

(defclass opencode-test-hunk ()
  ((content :initarg :content)
   (end :initarg :end)
   (from-range :initarg :from-range)
   (to-range :initarg :to-range)
   (combined :initarg :combined :initform nil)))

(defclass opencode-test-section ()
  ((type :initarg :type)
   (parent :initarg :parent :initform nil)
   (value :initarg :value :initform nil)
   (header :initarg :header :initform nil)
   (binary :initarg :binary :initform nil)
   (children :initarg :children :initform nil)))

(ert-deftest opencode-instance-candidates-normalize-and-deduplicate ()
  (let ((opencode-server-url "http://localhost:4098/")
        (opencode-server-instances
         '(("duplicate" . "http://localhost:4098")
           ("isolated" . "http://localhost:4097/"))))
    (should
     (equal (opencode--instance-candidates)
            '((:name "default" :server-url "http://localhost:4098")
              (:name "isolated" :server-url "http://localhost:4097"))))))

(ert-deftest opencode-discover-instances-isolates-failures ()
  (let ((opencode-server-url "http://default")
        (opencode-server-instances '(("other" . "http://other"))))
    (cl-letf (((symbol-function 'opencode--fetch-workspaces)
               (lambda (server-url &optional _timeout)
                 (if (equal server-url "http://default")
                     (signal 'opencode-error '("offline"))
                   '(((projectID . "other-project")
                      (workspaceID . "other-workspace")
                      (directory . "/other")
                      (worktree . "/other")))))))
      (should
       (equal (opencode--discover-instances)
              '((:name "other"
                 :server-url "http://other"
                 :workspaces (((projectID . "other-project")
                               (workspaceID . "other-workspace")
                               (directory . "/other")
                               (worktree . "/other"))))))))))

(ert-deftest opencode-discover-instances-reports-aggregate-failure ()
  (let ((opencode-server-url "http://default")
        (opencode-server-instances '(("other" . "http://other"))))
    (cl-letf (((symbol-function 'opencode--fetch-workspaces)
               (lambda (server-url &optional _timeout)
                 (signal 'opencode-error (list (concat server-url " offline"))))))
      (let ((error (should-error (opencode--discover-instances)
                                 :type 'user-error)))
        (should (string-match-p "http://default offline"
                                (error-message-string error)))
        (should (string-match-p "http://other offline"
                                (error-message-string error)))))))

(ert-deftest opencode-pick-instance-prompts-only-for-multiple ()
  (let ((opencode--instance-discovery-failures nil)
        (prompt-count 0)
        (one '((:name "one" :server-url "http://one" :workspaces nil)))
        (two '((:name "one" :server-url "http://one" :workspaces nil)
               (:name "two" :server-url "http://two" :workspaces nil))))
    (cl-letf (((symbol-function 'opencode--discover-instances)
               (lambda () one))
              ((symbol-function 'completing-read)
               (lambda (&rest _args) (setq prompt-count (1+ prompt-count)))))
      (should (equal (plist-get (opencode--pick-instance) :name) "one"))
      (should (= prompt-count 0)))
    (cl-letf (((symbol-function 'opencode--discover-instances)
               (lambda () two))
              ((symbol-function 'completing-read)
               (lambda (_prompt choices &rest _args)
                 (setq prompt-count (1+ prompt-count))
                 (caar (last choices)))))
      (should (equal (plist-get (opencode--pick-instance) :name) "two"))
      (should (= prompt-count 1)))))

(ert-deftest opencode-send-uses-captured-instance ()
  (let ((opencode-server-url "http://changed")
        (opencode-server-instances '(("selected" . "http://selected")))
        captured)
    (cl-letf (((symbol-function 'opencode--fetch-logs)
               (lambda (&rest _args) (opencode-test--logs-response)))
              ((symbol-function 'opencode--fetch-workspaces)
               (lambda (&rest _args)
                 '(((projectID . "project-id")
                    (workspaceID . "workspace-id")
                    (directory . "/project")
                    (worktree . "/project")))))
              ((symbol-function 'opencode--post-json)
               (lambda (server-url path payload)
                 (setq captured (list server-url path payload)))))
      (opencode--send (opencode-test--selection "http://selected") "hello")
      (should (equal (seq-take captured 2)
                      '("http://selected" "/log-id/42/message"))))))

(ert-deftest opencode-send-rejects-selection-without-instance ()
  (should-error
   (opencode--send '(:log-id 42 :label "old selection") "hello")
   :type 'user-error))

(ert-deftest opencode-send-rejects-reused-log-id ()
  (let ((opencode-server-url "http://selected"))
    (cl-letf (((symbol-function 'opencode--fetch-logs)
               (lambda (&rest _args)
                 (opencode-test--logs-response 42 2000)))
              ((symbol-function 'opencode--fetch-workspaces)
               (lambda (&rest _args)
                 '(((projectID . "project-id")
                    (workspaceID . "workspace-id")
                    (directory . "/project")
                    (worktree . "/project"))))))
      (should-error
       (opencode--send (opencode-test--selection "http://selected" 42 1000)
                       "hello")
       :type 'user-error))))

(ert-deftest opencode-plain-selection-ignores-additional-instances ()
  (let ((opencode-server-url "http://default/")
        (opencode-server-instances '(("other" . "http://other")))
        captured)
    (cl-letf (((symbol-function 'opencode--root) (lambda () "/project"))
              ((symbol-function 'opencode--pick-instance)
               (lambda () (ert-fail "plain selection probed instances")))
              ((symbol-function 'opencode--pick-agent)
               (lambda (server-url instance-label directory
                        &optional _label _workspace _catalog)
                 (setq captured (list server-url instance-label directory))
                 (opencode-test--selection "http://default" 1))))
      (opencode--choose-agent nil)
      (should (equal captured '("http://default" "default" "/project"))))))

(ert-deftest opencode-plain-selection-rejects-malformed-default-url ()
  (let ((opencode-server-url 42))
    (cl-letf (((symbol-function 'opencode--root) (lambda () "/project")))
      (should-error (opencode--choose-agent nil) :type 'user-error))))

(ert-deftest opencode-prefix-selection-picks-instance-project-then-agent ()
  (let* ((opencode-agent-alist nil)
         (opencode-agent-current nil)
         (workspace '((projectID . "project-id")
                      (workspaceID . "workspace-id")
                      (directory . "/project")
                      (worktree . "/project")))
         (picked (opencode-test--selection "http://selected"))
         captured)
    (cl-letf (((symbol-function 'opencode--root) (lambda () "/source"))
              ((symbol-function 'opencode--pick-instance)
               (lambda () (list :name "selected"
                                 :server-url "http://selected"
                                 :workspaces (list workspace))))
              ((symbol-function 'opencode--pick-instance-project-agent)
               (lambda (instance)
                 (setq captured instance)
                 picked)))
      (let ((selection (opencode--choose-agent t)))
        (should (equal (plist-get captured :server-url) "http://selected"))
        (should (equal (plist-get captured :workspaces) (list workspace)))
        (should (eq selection picked))
        (should (eq selection opencode-agent-current))
        (should (eq selection (cdr (assoc "/source" opencode-agent-alist))))))))

(ert-deftest opencode-instance-project-agent-picker-orders-prompts ()
  (let* ((first-workspace '((projectID . "project-one")
                            (workspaceID . "workspace-one")
                            (directory . "/one")
                            (worktree . "/one")
                            (name . "one")))
         (second-workspace '((projectID . "project-two")
                             (workspaceID . "workspace-two")
                             (directory . "/two")
                             (worktree . "/two")
                             (name . "two")))
         (instance (list :name "selected"
                         :server-url "http://selected"
                         :workspaces (list first-workspace second-workspace)))
         prompts)
    (cl-letf (((symbol-function 'completing-read)
               (lambda (prompt choices &rest _args)
                 (push prompt prompts)
                 (if (string-prefix-p "Project" prompt)
                     (caar (last choices))
                   (caar choices))))
              ((symbol-function 'opencode--agent-selections)
               (lambda (_server-url _instance-label directory &rest _args)
                 (should (equal directory "/two"))
                 (list (opencode-test--selection "http://selected" 2)))))
      (let ((selection (opencode--pick-instance-project-agent instance)))
        (should (equal (nreverse prompts)
                       '("Project on http://selected: "
                         "Agent in two on http://selected: ")))
        (should (= (plist-get selection :log-id) 2))))))

(ert-deftest opencode-agent-choice-displays-directory ()
  (let ((selection (opencode-test--selection "http://selected" 42))
        prompt choices)
    (cl-letf (((symbol-function 'completing-read)
               (lambda (actual-prompt actual-choices &rest _args)
                 (setq prompt actual-prompt
                       choices actual-choices)
                 (caar actual-choices))))
      (should (eq (opencode--pick-selection
                   "http://selected" (list selection))
                  selection))
      (should (equal prompt "Agent on http://selected: "))
      (should (string-match-p "/project" (caar choices))))))

(ert-deftest opencode-diff-gutters-use-compact-old-new-columns ()
  (should
   (equal
    (opencode--render-diff-records
     '((:kind removed :old 14 :new nil :text "GXD_SRC_LOAD_PATH=-L ./src/guile")
       (:kind removed :old 15 :new nil :text "TEST_SRC_LOAD_PATH=...")
       (:kind removed :old 16 :new nil :text "DEV_SRC_LOAD_PATH=...")
       (:kind added :old nil :new 11 :text "GUIX_CHANNELS_FILE = ...")
       (:kind context :old 17 :new 12 :text "preserved context")))
    (string-join
     '("14    - GXD_SRC_LOAD_PATH=-L ./src/guile"
       "15    - TEST_SRC_LOAD_PATH=..."
       "16    - DEV_SRC_LOAD_PATH=..."
       "   11 + GUIX_CHANNELS_FILE = ..."
       "17 12   preserved context")
     "\n"))))

(ert-deftest opencode-diff-records-preserve-exact-selected-content ()
  (with-temp-buffer
    (insert "-removed exactly  \n+added exactly\n preserved exactly\n")
    (let* ((hunk (make-instance 'opencode-test-hunk
                                :content (point-min)
                                :end (point-max)
                                :from-range '(14 2)
                                :to-range '(11 2)))
           (selection (opencode--diff-line-records
                       hunk (point-min) (point-max)))
           (records (plist-get selection :records)))
      (should
       (equal records
              '((:kind removed :old 14 :new nil :text "removed exactly  ")
                (:kind added :old nil :new 11 :text "added exactly")
                (:kind context :old 15 :new 12 :text "preserved exactly"))))
      (should (plist-get selection :ends-with-newline)))))

(ert-deftest opencode-diff-records-distinguish-final-newline-boundary ()
  (with-temp-buffer
    (insert "+added\n")
    (let ((hunk (make-instance 'opencode-test-hunk
                               :content (point-min)
                               :end (point-max)
                               :from-range '(1 0)
                               :to-range '(1 1))))
      (should-not
       (plist-get (opencode--diff-line-records hunk (point-min) (1- (point-max)))
                  :ends-with-newline))
      (should
       (plist-get (opencode--diff-line-records hunk (point-min) (point-max))
                  :ends-with-newline)))))

(ert-deftest opencode-diff-records-reject-no-newline-metadata ()
  (with-temp-buffer
    (insert "+added\n\\ No newline at end of file\n")
    (let ((hunk (make-instance 'opencode-test-hunk
                               :content (point-min)
                               :end (point-max)
                               :from-range '(1 0)
                               :to-range '(1 1))))
      (should-error
       (opencode--diff-line-records hunk (point-min) (point-max))
       :type 'user-error))))

(ert-deftest opencode-diff-gutters-use-configurable-markers ()
  (let ((opencode-diff-line-markers
         '((removed . "<") (added . ">") (context . "="))))
    (should
     (equal (opencode--render-diff-records
             '((:kind context :old 1 :new 1 :text "same")))
            "1 1 = same"))))

(ert-deftest opencode-compose-context-deduplicates-project-and-commit ()
  (let* ((first '(:diff t
                  :project-key "/p"
                  :project-header "Project: ~/p"
                  :revision-key (git commit "abc")
                  :revision-header "Commit: abc [git]"
                  :body "one.scm:1\n```diff\n1 1   one\n```\n\n"))
         (same '(:diff t
                 :project-key "/p"
                 :project-header "Project: ~/p"
                 :revision-key (git commit "abc")
                 :revision-header "Commit: abc [git]"
                 :body "two.scm:2\n```diff\n2 2   two\n```\n\n"))
         (next '(:diff t
                 :project-key "/p"
                 :project-header "Project: ~/p"
                 :revision-key (git commit "def")
                 :revision-header "Commit: def [git]"
                 :body "one.scm:3\n```diff\n3 3   three\n```\n\n"))
         (other '(:diff t
                  :project-key "/q"
                  :project-header "Project: ~/q"
                  :revision-key (jj commit "ghi")
                  :revision-header "Commit: ghi [jj]"
                  :body "x.clj:4\n```diff\n4 4   four\n```\n\n")))
    (should (string-prefix-p "Project: ~/p\nCommit: abc [git]"
                             (opencode--render-context first)))
    (should (equal (opencode--render-context same first)
                   (plist-get same :body)))
    (should (string-prefix-p "Commit: def [git]\n\n"
                             (opencode--render-context next same)))
    (should (string-prefix-p "Project: ~/q\nCommit: ghi [jj]\n\n"
                             (opencode--render-context other next)))))

(ert-deftest opencode-jj-revision-key-includes-both-range-sides ()
  (cl-letf (((symbol-function 'majutsu-jj--parse-diff-range)
             (lambda (range)
               (if (member "--from=A" range) '("A" . "B") '("C" . "B"))))
            ((symbol-function 'majutsu-jj-lines)
             (lambda (&rest args)
               (pcase (nth (1+ (cl-position "-r" args :test #'equal)) args)
                 ("A" '("commit-a\tchange-a"))
                 ("B" '("commit-b\tchange-b"))
                 ("C" '("commit-c\tchange-c"))))))
    (setq majutsu-buffer-diff-range '("--from=A" "--to=B"))
    (unwind-protect
      (let ((ab (opencode--majutsu-revision-context default-directory)))
        (setq majutsu-buffer-diff-range '("--from=C" "--to=B"))
        (let ((cb (opencode--majutsu-revision-context default-directory)))
          (should-not (equal (plist-get ab :key) (plist-get cb :key)))
          (should (equal (plist-get ab :header)
                         "Diff: commit-a -> commit-b [jj]; change change-b"))))
      (makunbound 'majutsu-buffer-diff-range))))

(ert-deftest opencode-jj-multi-revision-key-includes-total-diff-boundaries ()
  (setq majutsu-buffer-diff-range '("--revisions=A" "--revisions=B"))
  (unwind-protect
      (cl-letf (((symbol-function 'opencode--revision-side)
                 (lambda (_vcs _root expression)
                   (cond
                    ((equal expression "(A) | (B)")
                     '(:identity
                       (resolved
                        ("commit-b" "change-b")
                        ("commit-a" "change-a"))
                       :entries
                       (("commit-b" "change-b")
                        ("commit-a" "change-a"))))
                    ((equal expression "roots((A) | (B))-")
                     '(:identity (resolved ("commit-parent" "change-parent"))
                       :display "commit-p"))
                    ((equal expression "heads((A) | (B))")
                     '(:identity (resolved ("commit-b" "change-b"))
                       :display "commit-b" :change "change-b"))))))
        (let ((context
               (opencode--majutsu-revision-context default-directory)))
          (should (equal (plist-get context :header)
                         "Diff: commit-p -> commit-b [jj]; change change-b"))
          (should (equal (cadr (plist-get context :key))
                         '(("--revisions=A" "--revisions=B")
                            (resolved
                             ("commit-b" "change-b")
                              ("commit-a" "change-a")))))))
    (makunbound 'majutsu-buffer-diff-range)))

(ert-deftest opencode-magit-reverse-swaps-revision-pair ()
  (let ((hunk (make-instance 'opencode-test-section :type 'hunk)))
    (setq magit-buffer-range "A..B"
          magit-buffer-typearg nil
          magit-buffer-diff-type 'committed
          magit-buffer-revision nil
          magit-buffer-revision-hash nil
          magit-buffer-diff-args nil)
    (unwind-protect
    (cl-letf (((symbol-function 'magit-diff-type) (lambda (_hunk) 'committed))
              ((symbol-function 'magit-split-range)
               (lambda (_range) '("A" . "B")))
              ((symbol-function 'opencode--revision-side)
               (lambda (_vcs _root expression)
                 (list :identity expression :display expression))))
      (let ((normal (opencode--magit-revision-context default-directory hunk)))
        (setq magit-buffer-diff-args '("-R"))
        (let ((reversed (opencode--magit-revision-context default-directory hunk)))
          (should (equal (plist-get normal :header) "Diff: A -> B [git]"))
          (should (equal (plist-get reversed :header) "Diff: B -> A [git]"))
          (should-not (equal (plist-get normal :key)
                             (plist-get reversed :key))))))
      (mapc (lambda (symbol)
              (makunbound symbol))
            '(magit-buffer-range magit-buffer-typearg magit-buffer-diff-type
              magit-buffer-revision magit-buffer-revision-hash
              magit-buffer-diff-args)))))

(ert-deftest opencode-magit-root-commit-uses-empty-tree-identity ()
  (let ((hunk (make-instance 'opencode-test-section :type 'hunk)))
    (setq magit-buffer-range "ROOT^..ROOT"
          magit-buffer-typearg nil
          magit-buffer-diff-type 'committed
          magit-buffer-revision "ROOT"
          magit-buffer-revision-hash "root-full"
          magit-buffer-diff-args nil)
    (unwind-protect
        (cl-letf (((symbol-function 'magit-diff-type)
                   (lambda (_hunk) 'committed))
                  ((symbol-function 'magit-rev-verify)
                   (lambda (revision)
                     (cond
                      ((equal revision "ROOT") "root-full")
                      ((equal revision "root-full") "root-full"))))
                  ((symbol-function 'magit-rev-parse)
                   (lambda (revision)
                     (and (member revision '("ROOT" "root-full"))
                          "root-full")))
                  ((symbol-function 'opencode--git-empty-tree-hash)
                   (lambda (_root) "empty-tree-full"))
                  ((symbol-function 'opencode--revision-side)
                   (lambda (_vcs _root expression)
                     (list :identity expression :display expression))))
          (let ((context
                 (opencode--magit-revision-context default-directory hunk)))
            (should (equal (plist-get context :header)
                           "Commit: root-ful [git]"))
            (should (equal (nthcdr 2 (plist-get context :key))
                           '("empty-tree-full" "root-full")))))
      (mapc (lambda (symbol) (makunbound symbol))
            '(magit-buffer-range magit-buffer-typearg magit-buffer-diff-type
              magit-buffer-revision magit-buffer-revision-hash
              magit-buffer-diff-args)))))

(ert-deftest opencode-magit-stash-subdiffs-have-distinct-pairs ()
  (let* ((commit (make-instance 'opencode-test-section
                                :type 'commit :value "stash^2"))
         (file (make-instance 'opencode-test-section
                              :type 'file :parent commit))
         (hunk (make-instance 'opencode-test-section
                              :type 'hunk :parent file)))
    (setq magit-buffer-range "stash^..stash"
          magit-buffer-typearg nil
          magit-buffer-diff-type 'committed
          magit-buffer-revision "stash"
          magit-buffer-revision-hash "stash-full"
          magit-buffer-diff-args nil)
    (unwind-protect
        (cl-letf (((symbol-function 'derived-mode-p)
                   (lambda (&rest modes) (memq 'magit-stash-mode modes)))
                  ((symbol-function 'magit-diff-type)
                   (lambda (_hunk) 'committed))
                  ((symbol-function 'opencode--revision-side)
                   (lambda (_vcs _root expression)
                     (list :identity expression :display expression))))
          (let ((staged
                 (opencode--magit-revision-context default-directory hunk)))
            (setf (slot-value commit 'value) "stash")
            (let ((unstaged
                   (opencode--magit-revision-context default-directory hunk)))
              (setf (slot-value commit 'value) "stash^3")
              (let ((untracked
                     (opencode--magit-revision-context default-directory hunk)))
                (should (equal (plist-get staged :header)
                               "Diff: stash^ -> stash^2 [git]"))
                (should (equal (plist-get unstaged :header)
                               "Diff: stash^2 -> stash [git]"))
                (should (equal (plist-get untracked :header)
                               "Diff: stash^ -> stash^3 [git]"))
                (should (= (length (delete-dups
                                    (list (plist-get staged :key)
                                          (plist-get unstaged :key)
                                          (plist-get untracked :key))))
                           3))))))
      (mapc (lambda (symbol) (makunbound symbol))
            '(magit-buffer-range magit-buffer-typearg magit-buffer-diff-type
              magit-buffer-revision magit-buffer-revision-hash
              magit-buffer-diff-args)))))

(ert-deftest opencode-magit-no-index-rejects-revision-context ()
  (let ((hunk (make-instance 'opencode-test-section :type 'hunk)))
    (setq magit-buffer-range nil
          magit-buffer-typearg "--no-index"
          magit-buffer-diff-type 'undefined
          magit-buffer-revision nil
          magit-buffer-revision-hash nil
          magit-buffer-diff-args nil)
    (unwind-protect
        (cl-letf (((symbol-function 'magit-diff-type)
                   (lambda (_hunk) 'undefined)))
          (should-error
           (opencode--magit-revision-context default-directory hunk)
           :type 'user-error))
      (mapc (lambda (symbol) (makunbound symbol))
            '(magit-buffer-range magit-buffer-typearg magit-buffer-diff-type
              magit-buffer-revision magit-buffer-revision-hash
              magit-buffer-diff-args)))))

(ert-deftest opencode-status-list-file-is-not-semantic-diff-context ()
  (let* ((plain (make-instance 'opencode-test-section :type 'file))
         (hunk (make-instance 'opencode-test-section :type 'hunk))
         (diff (make-instance 'opencode-test-section
                              :type 'file :header "diff --git a/x b/x\n"))
         (washed (make-instance 'opencode-test-section
                                :type 'file :children (list hunk))))
    (should-not (opencode--semantic-diff-file-p plain nil))
    (should (opencode--semantic-diff-file-p diff nil))
    (should (opencode--semantic-diff-file-p washed nil))
    (should (opencode--semantic-diff-file-p plain t))))

(ert-deftest opencode-pick-agent-rejects-workspace-mismatch ()
  (let ((workspace '((projectID . "project-a")
                     (workspaceID . "workspace-a")
                     (directory . "/project")
                     (worktree . "/project"))))
    (cl-letf (((symbol-function 'opencode--fetch-logs)
               (lambda (&rest _args)
                 (opencode-test--logs-response 42 1000 "project-b"))))
      (should-error
       (opencode--pick-agent
        "http://selected" "selected" "/project" "project-a" workspace
        (list workspace))
       :type 'user-error))))

(ert-deftest opencode-stale-local-selection-falls-through-to-global ()
  (let* ((opencode-server-url "http://selected")
         (root "/source")
         (global (opencode-test--selection "http://selected"))
         (opencode-agent-alist `((,root . (:log-id 1 :label "old"))))
         (opencode-agent-current global))
    (cl-letf (((symbol-function 'opencode--root) (lambda () root)))
      (should (eq (opencode--selection-for-root) global))
      (should-not (assoc root opencode-agent-alist)))))

(ert-deftest opencode-compose-identity-includes-instance ()
  (let* ((first (opencode-test--selection "http://one"))
         (second (opencode-test--selection "http://two"))
         (first-buffer (opencode--compose-buffer first))
         (same-buffer (opencode--compose-buffer first))
         (second-buffer (opencode--compose-buffer second)))
    (unwind-protect
        (progn
          (should (eq first-buffer same-buffer))
          (should-not (eq first-buffer second-buffer)))
      (kill-buffer first-buffer)
      (kill-buffer second-buffer))))

(ert-deftest opencode-compose-identity-rejects-same-url-reused-log-id ()
  (let* ((first (opencode-test--selection "http://one" 42 1000))
         (replacement (opencode-test--selection "http://one" 42 2000))
         (first-buffer (opencode--compose-buffer first))
         (replacement-buffer (opencode--compose-buffer replacement)))
    (unwind-protect
        (should-not (eq first-buffer replacement-buffer))
      (kill-buffer first-buffer)
      (kill-buffer replacement-buffer))))

(provide 'opencode-tests)

;;; opencode-tests.el ends here
