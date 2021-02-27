;;; ivy-lsp-current-buffer-symbols.el --- Ivy interface for lsp workspace symbols scoped to current buffer -*- lexical-binding: t -*-

;; Copyright (C) 2020 Jean-Hadrien Chabran <jh@chabran.fr>

;; Author: Jean-Hadrien Chabran <jh@chabran.fr>
;; URL: https://github.com/jhchabran/ivy-lsp-current-buffer-symbols
;; Version: 0.1
;; Package-Requires: ((emacs "25.1") (dash "2.14.1") (lsp-mode "6.2.1") (ivy "0.13.0"))

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This package provides ivy as the interface to pick a LSP workspace symbol scoped to the current file

;;; Setup

;; stuff

;;; Code:
(require 'ivy)
(require 'dash)

(require 'lsp-protocol)
(require 'lsp-mode)

(defgroup ivy-lsp-current-buffer-symbols nil
  "Select lsp workspace results scoped to current buffer"
  :prefix "ivy-lsp-current-buffer-symbols"
  :group 'ivy
  :link '(url-link :tag "Github" "https://github.com/jhchabran/ivy-lsp-current-buffer-symbols"))


(defun ivy-lsp-current-buffer-symbols-jump--candidates (lsp-symbols)
  "Generate a list of candidates from LSP-SYMBOLS."
  (let* ((symbols-names (mapcar (lambda (h) (gethash "name" h)) lsp-symbols))
         (max-type-width
          (cl-loop for kind in lsp--symbol-kind maximize (length (cdr kind))))
         (max-name-width
          (cl-loop for name in symbols-names maximize (length name)))
         (max-width (max (+ max-name-width 3) 25)))
    (cl-loop
     with fmt = (format "%%-%ds %%-%ds %%-%ds" max-name-width max-type-width max-width)
     for symbol in lsp-symbols
     collect
     (list (format fmt
                   (gethash "name" symbol)
                   (propertize (cdr (assq (gethash "kind" symbol) lsp--symbol-kind)) 'face 'font-lock-comment-face)
                   (propertize (gethash "detail" symbol) 'face 'font-lock-constant-face))
           symbol))))

(defun ivy-lsp-current-buffer-symbols-jump ()
  "Show the list of lsp symbols in the current workspace filtered to the current buffer via ivy."
  (interactive)
  (with-lsp-workspaces (lsp-workspaces)
        (let* ((filtered-symbols (lsp--imenu-filter-symbols (lsp--get-document-symbols)))
               (symbol-markers (-> filtered-symbols
                                      lsp--collect-lines-and-cols
                                      lsp--convert-line-col-to-points-batch))
               (candidates (ivy-lsp-current-buffer-symbols-jump--candidates filtered-symbols)))
          (ivy-read "current buffer symbols:"
                    candidates
                    :require-match t
                    :re-builder #'ivy--regex-plus ; Using fuzzy yields really poor choices with the symbol details
                    :action (lambda (candidate)
                              (let* ((symbol (cadr candidate))
                                     (line-col (lsp--get-line-and-col symbol))
                                     (marker (gethash line-col symbol-markers)))
                                (goto-char  marker)))))))

(provide 'ivy-lsp-current-buffer-symbols)
;;; ivy-lsp-current-buffer-symbols ends here
