;;; uni-alphanums.el --- Easily insert Unicode alphanumerical alternatives              -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Erik Sjöstrand
;; MIT License

;; Author: Erik Sjöstrand <sjostrand.erik@gmail.com>
;; URL: https://github.com/Kungsgeten/uni-alphanums.git
;; Keywords: convenience, i18n
;; Package-Requires: ((emacs "28.2"))
;; Version: 0.1
;;; Commentary:

;; Use your A-Z and 0-9 keys to insert a UTF-8 version of that character.
;; Use `uni-alphanums-insert' to start inserting characters.
;; Change "font" with `uni-alphanums-set-alphas' or `uni-alphanums-set-nums'.

;;; Code:

(defvar uni-alphanums-alpha-sets
  '(("Circled" . (:small ?ⓐ :capital ?Ⓐ :example "Ⓒⓘⓡⓒⓛⓔⓓ"))
    ("Fraktur" . (:small ?𝔞 :capital ?𝔄 :example "𝔉𝔯𝔞𝔨𝔱𝔲𝔯"))
    ("Fraktur Bold" . (:small ?𝖆 :capital ?𝕬 :example "𝕱𝖗𝖆𝖐𝖙𝖚𝖗 𝕭𝖔𝖑𝖉"))
    ("Script" . (:small ?𝒶 :capital ?𝒜 :missing ((?g . ?ℊ)) :example "𝒮𝒸𝓇𝒾𝓅𝓉"))
    ("Script Bold" . (:small ?𝓪 :capital ?𝓐 :example "𝓢𝓬𝓻𝓲𝓹𝓽 𝓑𝓸𝓵𝓭"))
    ("Doublestruck" . (:small ?𝕒 :capital ?𝔸 :example "𝔻𝕠𝕦𝕓𝕝𝕖𝕤𝕥𝕣𝕦𝕔𝕜" :missing ((?C . ?ℂ)
                                                                               (?H . ?ℍ)
                                                                               (?N . ?ℕ)
                                                                               (?P . ?ℙ)
                                                                               (?Q . ?ℚ)
                                                                               (?R . ?ℝ)
                                                                               (?Z . ?ℤ)))))
  "Available character sets used for the A-Z keys.
An alist where the `car' is the name of the set, and the `cdr' is a plist:

- :small   The char in the set representing latin small letter a.
- :capital The char in the set representing latin capital letter A.
- :example An example string to be shown during completion.
- :missing An alist of characters and which are missing in a-z or A-Z.
           Can also be used if both :small and :capital are omitted.
           The `car' should be the normal latin char and the `cdr' is
           the unicode replacement.")

(defvar uni-alphanums-alphas (car uni-alphanums-alpha-sets)
  "The current active value from `uni-alphanums-alpha-sets'.
Chars from this set will be inserted using `uni-alphanums-insert'.")

(defvar uni-alphanums-num-sets
  '(("Circled" . (:positive ?① :zero ?⓪ :example "①②③④⑤⑥⑦⑧⑨⓪"))
    ("Circled Sans" . (:positive ?➀ :example "➀➁➂➃➄➅➆➇➈"))
    ("Circled Black" . (:positive ?❶ :example "❶❷❸❹❺❻❼❽❾"))
    ("Circled Black Sans" . (:positive ?➊ :example "➊➋➌➍➎➏➐➑➒"))
    ("Doublestruck" . (:positive ?𝟙 :zero ?𝟘 :example "𝟙𝟚𝟛𝟜𝟝𝟞𝟟𝟠𝟡𝟘"))
    ("Roman" . (:positive ?Ⅰ :example "Ⅰ Ⅱ Ⅲ Ⅳ Ⅴ Ⅵ Ⅶ Ⅷ Ⅸ"))
    ("Roman Small" . (:positive ?ⅰ :example "ⅰ ⅱ ⅲ ⅳ ⅴ ⅵ ⅶ ⅷ ⅸ")))
  "Available character sets used for the 0-9 keys.
An alist where the `car' is the name of the set, and the `cdr' is a plist:

- :positive The char in the set representing digit 1.
- :zero     The char in the set representing digit 0.
- :example  An example string to be shown during completion.")

(defvar uni-alphanums-nums (car uni-alphanums-num-sets)
  "The current active value from `uni-alphanums-num-sets'.
Chars from this set will be inserted using `uni-alphanums-insert'.")

(defvar uni-alphanums-repeat t
  "If `uni-alphanums-insert' should insert more than one character per call.")

(defvar uni-alphanums-start-insertion-after-alphanum-set t
  "If `uni-alphanums-insert' should be called after changing alphanums.")

(defun uni-alphanums--alphas-annotation (str)
  "Get the example annotation from STR in `uni-alphanums-alpha-sets'."
  (when-let ((example (plist-get (alist-get str uni-alphanums-alpha-sets nil nil #'string-equal) :example)))
    (concat (make-string (- 50 (length str)) 32) example)))

(defun uni-alphanums--alphas-collection (str pred flag)
  "Collection function for `uni-alphanums-set-alphas'."
  (pcase flag
    ('nil (try-completion str uni-alphanums-alpha-sets pred))
    ('t (all-completions str uni-alphanums-alpha-sets pred))
    ('lambda (test-completion str uni-alphanums-alpha-sets pred))
    (`(boundaries . ,suffix)
     (completion-boundaries str uni-alphanums-alpha-sets pred suffix))
    ('metadata
     '(metadata . ((annotation-function . uni-alphanums--alphas-annotation))))))

(defun uni-alphanums-set-alphas (type)
  "Set the character TYPE of the A-Z keys."
  (interactive (list (completing-read "Set alphas: " #'uni-alphanums--alphas-collection nil t)))
  (setq uni-alphanums-alphas (assoc type uni-alphanums-alpha-sets #'string-equal))
  (if (and uni-alphanums-start-insertion-after-alphanum-set
           (called-interactively-p 'any))
      (call-interactively #'uni-alphanums-insert)))

(defun uni-alphanums--nums-annotation (str)
  "Get the example annotation from STR in `uni-alphanums-num-sets'."
  (when-let ((example (plist-get (alist-get str uni-alphanums-num-sets nil nil #'string-equal) :example)))
    (concat (make-string (- 50 (length str)) 32) example)))

(defun uni-alphanums--nums-collection (str pred flag)
  "Collection function for `uni-alphanums-set-nums'."
  (pcase flag
    ('nil (try-completion str uni-alphanums-num-sets pred))
    ('t (all-completions str uni-alphanums-num-sets pred))
    ('lambda (test-completion str uni-alphanums-num-sets pred))
    (`(boundaries . ,suffix)
     (completion-boundaries str uni-alphanums-num-sets pred suffix))
    ('metadata
     '(metadata . ((annotation-function . uni-alphanums--nums-annotation))))))

(defun uni-alphanums-set-nums (type)
  "Set the character TYPE of the 0-9 keys."
  (interactive (list (completing-read "Set numbers: " #'uni-alphanums--nums-collection nil t)))
  (setq uni-alphanums-nums (assoc type uni-alphanums-num-sets #'string-equal))
  (if (and uni-alphanums-start-insertion-after-alphanum-set
           (called-interactively-p 'any))
      (call-interactively #'uni-alphanums-insert)))

(defun uni-alphanums-toggle-repeat ()
  "Toggle if `uni-alphanums-insert' should insert more than one character per call."
  (interactive)
  (setq uni-alphanums-repeat (not uni-alphanums-repeat))
  (message "Unicode alphanums repeat %s"
           (if uni-alphanums-repeat "on" "off")))

(defun uni-alphanums-insert (char)
  "Insert the unicode variant of CHAR.
The variant of A-Z can be changed with `uni-alphanums-set-alphas'.
The variant of 0-9 can be changed with `uni-alphanums-set-nums'."
  (interactive "c")
  (when-let ((base-difference
              (pcase char
                ;; Small letters
                ((and (pred (<= ?a))
                      (pred (>= ?z)))
                 (or (when-let ((missing (assoc char (plist-get (cdr uni-alphanums-alphas) :missing))))
                       (- (cdr missing) (car missing)))
                     (- (or (plist-get (cdr uni-alphanums-alphas) :small)
                            (plist-get (cdr uni-alphanums-alphas) :capital)
                            (error "Neither :small nor :capital in %s" (car uni-alphanums-alphas)))
                        ?a)))
                ;; Capital letters
                ((and (pred (<= ?A))
                      (pred (>= ?Z)))
                 (or (when-let ((missing (assoc char (plist-get (cdr uni-alphanums-alphas) :missing))))
                       (- (cdr missing) (car missing)))
                     (- (or (plist-get (cdr uni-alphanums-alphas) :capital)
                            (plist-get (cdr uni-alphanums-alphas) :small)
                            (error "Neither :capital nor :small in %s" (car uni-alphanums-alphas)))
                        ?A)))
                ;; Zero
                (?0 (- (or (plist-get (cdr uni-alphanums-nums) :zero)
                           (error "No :zero found in %s" (car uni-alphanums-nums)))
                       ?0))
                ;; Positive numbers
                ((and (pred (<= ?1))
                      (pred (>= ?9)))
                 (- (or (plist-get (cdr uni-alphanums-nums) :positive)
                        (error "No :positive found in %s" (car uni-alphanums-nums)))
                    ?1))
                ;; Space is inserted as is
                (32 0)
                ;; Backspace
                (127 'bspc))))
    (if (eq base-difference 'bspc)
        (delete-char -1)
      (insert (char-to-string (+ base-difference char))))
    (if (and uni-alphanums-repeat (called-interactively-p 'any))
        (call-interactively #'uni-alphanums-insert))))

(provide 'uni-alphanums)
;;; uni-alphanums.el ends here
