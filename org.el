(require 'org)
(require 'org-superstar)
(setq org-directory "~/Documents/programming/org")
(setq org-default-notes-file (concat org-directory "/notes.org"))
(add-hook 'org-mode-hook (lambda () (org-superstar-mode 1)))

(setq org-hide-emphasis-markers t)
;(setq org-agenda-files (quote ("~/Documents/org/agenda.org"
							 ;                "~/Document/org/semesterSched.org")))
(setq org-agenda-files (quote ("g:/Notes/org/actualtodo.org"
                               "g:/Notes/Org/excitement.org")))
(add-hook 'org-mode-hook
          (lambda ()
            (local-unset-key (kbd "C-c ["))
            (local-unset-key (kbd "C-c ]"))))
;;TOC 
(if (require 'toc-org nil t)
    (add-hook 'org-mode-hook 'toc-org-mode)

  ;; enable in markdown, too
  (add-hook 'markdown-mode-hook 'toc-org-mode)
  (define-key markdown-mode-map (kbd "\C-c\C-o") 'toc-org-markdown-follow-thing-at-point)
  (warn "toc-org not found"))

;; todo capturing
(require 'org)(setq org-capture-templates '(
			       ("m"
				"Melee"
				entry
				(file+headline "g:/Notes/Org/actualtodo.org" "Melee")
				"* TODO %? \n :PROPERTIES:\n :Source: %a \n :END:\n %i"
				:prepend nil
				:empty-lines 1
				:create t
				:kill-buffer t)
                                ("d"
				"Drawing"
				entry
				(file+headline "g:/Notes/Org/actualtodo.org" "Drawing")
				"* TODO %?"
				:prepend nil
				:empty-lines 1
				:create t
				:kill-buffer t)
			       )
      )
(global-set-key (kbd "C-c c") 'org-capture)
(require 'ox-md)

(defun org-toggle-emphasis ()
  "Toggle hiding/showing of org emphasize markers."
  (interactive)
  (if org-hide-emphasis-markers
      (set-variable 'org-hide-emphasis-markers nil)
    (set-variable 'org-hide-emphasis-markers t)))
(define-key org-mode-map (kbd "C-c e") 'org-toggle-emphasis)

(defun org-agenda-current-buffer ()
   (interactive)
   (let ((org-agenda-files (list (buffer-file-name (current-buffer)))))
      (org-agenda)))

(define-key global-map (kbd "C-c C-a") #'org-agenda-current-buffer)
(define-key global-map (kbd "C-c a") #'org-agenda)

(require 'epa-file)
(epa-file-enable)