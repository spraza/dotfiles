;; Don't see startup message everytime
(setq inhibit-startup-message t)

;; Always highlight paren couples
(show-paren-mode 1)

;; Keep refreshing buffers
(global-auto-revert-mode t)

;; Hide menu bar (at the top)
(menu-bar-mode 0)

;; Install packages from "package"
;; NOTE: Not all packages in this file are installed this way
;;       Some are installed manually (see later sections of the file)
(require 'package)
;; List of packages
(setq package-list '(company flycheck))
;; Package location(s)
(add-to-list 'package-archives
	     '("MELPA Stable" . "https://stable.melpa.org/packages/") t)
;; Activate all the packages (in particular autoloads)
(package-initialize)
;; Fetch the list of packages available
(unless package-archive-contents
  (package-refresh-contents))
;; Install the packages if nt already installed
(dolist (package package-list)
  (unless (package-installed-p package)
        (package-install package)))


;; helm, async setup
;; https://github.com/emacs-helm/helm
(add-to-list 'load-path "~/git-repos/emacs-async")
(add-to-list 'load-path "~/git-repos/helm")
(require 'helm-config)
;; Utility function - asks for root directory for search + calls helm-find
(defun helm-find-with-prefix-arg ()
  (interactive)
  (setq current-prefix-arg '(4)) ; C-u
    (call-interactively 'helm-find))

;; helm-ag setup
;; https://github.com/syohex/emacs-helm-ag
(add-to-list 'load-path "~/git-repos/emacs-helm-ag")
(load-file "~/git-repos/emacs-helm-ag/helm-ag.el")

;; Smooth scrolling
;; https://github.com/aspiers/smooth-scrolling
(load-file "~/git-repos/smooth-scrolling/smooth-scrolling.el")
(require 'smooth-scrolling)
(smooth-scrolling-mode 1)

;; rtags setup
(add-to-list 'load-path "~/git-repos/rtags/build/src")
(load-file "~/git-repos/rtags/build/src/rtags.el")
(require 'rtags)
(require 'company)
(require 'flycheck-rtags)
(require 'rtags-helm)
;; (setq rtags-use-helm t) ;; uncomment if helm rtags integration is required
(setq rtags-autostart-diagnostics t)
(setq rtags-completions-enabled t)
(rtags-diagnostics)
(push 'company-rtags company-backends)
(global-company-mode)
;; Start rdm (rtags server) automatically in C/C++ modes
(add-hook 'c-mode-common-hook 'rtags-start-process-unless-running)
(add-hook 'c++-mode-common-hook 'rtags-start-process-unless-running)

;; Key Bindings
(global-set-key (kbd "C-M-l") 'linum-mode)
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x f") 'helm-find-with-prefix-arg)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "M-s") 'helm-occur)
(global-set-key (kbd "C-c m i") 'helm-do-ag)
(global-set-key (kbd "C-x b") 'helm-mini)
(define-key c-mode-base-map (kbd "C-u") (function company-complete))
(define-key c-mode-base-map (kbd "C-i") (function rtags-find-symbol-at-point))

;; Customize emacs mode line
;; using setq-default; applies to /all/ modes

;; total number of lines
;; source: http://stackoverflow.com/questions/8190277/how-do-i-display-the-total-number-of-lines-in-the-emacs-modeline
(defvar my-mode-line-buffer-line-count nil)
(make-variable-buffer-local 'my-mode-line-buffer-line-count)
(defun my-mode-line-count-lines ()
  (setq my-mode-line-buffer-line-count (int-to-string (count-lines (point-min) (point-max)))))
(add-hook 'find-file-hook 'my-mode-line-count-lines)
(add-hook 'after-save-hook 'my-mode-line-count-lines)
(add-hook 'after-revert-hook 'my-mode-line-count-lines)
(add-hook 'dired-after-readin-hook 'my-mode-line-count-lines)

(setq-default mode-line-format
	      (list
	       ;; buffer name
	       '(:eval (propertize "%b " 'face '(:foreground "black")
				   'help-echo (buffer-file-name)))


	       ;; current line number / total number of lines
	       "("
	       '(:eval (when line-number-mode
			 (let ((str "%l"))
			   (when (and (not (buffer-modified-p)) my-mode-line-buffer-line-count)
			     (setq str (concat str "/" my-mode-line-buffer-line-count)))
			   str)))       
	       ") "

	       "["
	       ;; was this buffer modified since the last save?
	       '(:eval (when (buffer-modified-p)
			 (concat ""  (propertize "*"
						 'face '(:foreground "black")
						 'help-echo "Buffer has been modified"))))
	       ;; is this buffer read-only?
	       '(:eval (when buffer-read-only
			 (concat ","  (propertize "ReadOnly"
						  'face '(:foreground "black")
						  'help-echo "Buffer is read-only"))))
	       "] "
	       
	       ;; file path
	       '(:eval (propertize "%f " 'face '(:foreground "black")
				   'help-echo (buffer-file-name)))       

	       ;; size of file
	       "["
	       (propertize "%I" 'face '(:foreground "black")) ;; size
	       "] "
	       
	       ;; the current major mode for the buffer.
	       "["
	       '(:eval (propertize "%m" 'face '(:foreground "black")
				   'help-echo buffer-file-coding-system))
	       "] "
	       
	       "--"
	       ;; Show minor modes list
	       minor-mode-alist
	       
	       "%-" ;; fill with '-'
	       ))
