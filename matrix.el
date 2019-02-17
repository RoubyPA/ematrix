;;; matrix.el --- The Martrix monitor               -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Pierre-Antoine Rouby

;; Author: Pierre-Antoine Rouby <contact@parouby.fr>
;; Keywords: games

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; The Matrix monitor for emacs.

;;; Code:

;;; Matrix config

(require 'cl)

(defgroup matrix nil
  "Matrix configuration."
  :group 'application)

(defcustom matrix-list-char
  '("A" "B" "C" "D" "E" "F" "G" "H"
    "I" "J" "K" "L" "M" "N" "O" "P"
    "Q" "R" "S" "T" "U" "V" "W" "X"
    "Y" "Z" "0" "1" "2" "3" "4" "5"
    "6" "7" "8" "9" "λ" "+" "*" "#")
  "Possible char for `matrix'."
  :type 'list :group 'matrix)

(defcustom matrix-speed 0.9
  "Matrix scrolling speed."
  :type 'float :group 'matrix)

(defcustom matrix-buffer-name "*matrix*"
  "Buffer name."
  :type 'string :group 'matrix)

;;; Faces

(defgroup matrix-faces nil
  "Faces for `matrix'."
  :group 'matrix)

(defface matrix-char-face
  '((t (:foreground "green")))
  "Head color."
  :group 'matrix-faces)

;;; Variable

(defvar matrix-buffer-height nil
  "Matrix height.")

(defvar matrix-buffer-width nil
  "Matrix width.")

(cl-defstruct column x y)               ;Column structure

(defvar matrix-list-column nil
  "Matrix list of current column")

;;; Functions
(defun matrix-get-buffer ()
  (get-buffer matrix-buffer-name))

(defun matrix-update-vars ()
  "Update Matrix variable."
  (let ((window (get-buffer-window matrix-buffer-name)))
    (setq matrix-buffer-height (window-height window))
    (setq matrix-buffer-width  (window-width  window))))

(defun matrix-prepare-buffer ()
  "Init `Matrix' buffer."
  (set-buffer (matrix-get-buffer))
  (erase-buffer)
  (mapcar (lambda (c)
            (insert-char c matrix-buffer-width)
            (newline))
          (make-list matrix-buffer-height ?\s)))

(defun matrix-insert (x y)
  "Insert new char"
  (with-current-buffer (matrix-get-buffer)
    (let* ((r (random (length matrix-list-char)))
           (c (nth r matrix-list-char)))
      (when (< y (- matrix-buffer-height 1))
        (goto-char (+ 1 x (* y (+ 1 matrix-buffer-width))))
        (delete-char 1)
        (insert (propertize c 'face 'matrix-char-face)))
      (when (and (<= 0 (- y 10))
                 (< (- y 10) (- matrix-buffer-height 1)))
        (goto-char (+ 1 x (* (- y 10) (+ 1 matrix-buffer-width))))
        (delete-char 1)
        (insert-char ?\s)))))

(defun matrix-make-new-column (n)
  "Create N new column."
  (if (= 0 n)
      '()
    (cons (make-column :x (random matrix-buffer-width)
                       :y 0)
          (matrix-make-new-column (- n 1)))))

(defun matrix-main ()
  "Matrix main main function."
  (with-current-buffer (matrix-get-buffer)
    (read-only-mode 0)
    (setq matrix-list-column
          (append
           (matrix-make-new-column 2)
           (remove 'nil
                   (mapcar (lambda (c)
                             (let ((x (column-x c))
                                   (y (column-y c)))
                               (matrix-insert x y)
                               (if (< y (+ 10 matrix-buffer-height))
                                   (make-column :x x :y (+ 1 y))
                                 nil)))
                           matrix-list-column))))
    (goto-char (point-min))
    (read-only-mode t)))

;;;###autoload
(defun matrix ()
  "Run matrix buffer"
  (interactive)
  (let ((buffer (get-buffer-create matrix-buffer-name)))
    (matrix-update-vars)
    (matrix-prepare-buffer)
    (add-hook 'window-size-change-functions
              'matrix-update-vars nil t)
    (setq matrix-list-column (matrix-make-new-column 2))
    (switch-to-buffer buffer)
    (run-with-timer 1 (- 1 matrix-speed)
                    'matrix-main)))

(defun matrix-off ()
  "Put out the fire."
  (interactive)
  (cancel-function-timers 'matrix-main)
  (when (matrix-get-buffer)
    (kill-buffer (matrix-get-buffer))))

(provide 'matrix)
;;; matrix.el ends here
