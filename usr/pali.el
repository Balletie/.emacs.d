;;; pali.el -- Highlight Pāḷi syllables for chanting -*- lexical-binding: t -*-

;; Copyright © 2020 Taan Skip

;; Author: Taan Skip
;; Maintainer: Taan Skip
;; Version: 1.0
;; Keywords: pali chant

;;; Code:
(require 'generator)
(require 'dash-functional)
(require 'ox)

(defun pali--at-pali-block-p (start end)
  (save-excursion
    (goto-char start)
    (org-in-block-p '("pali"))))

(defvar pali-fontify-predicates '(pali--at-pali-block-p)
  "Predicates used to highlight syllables.")

(defconst pali-category-table (make-category-table))
(define-category ?c "Pali consonant" pali-category-table)
(define-category ?d "Pali consonant cluster (two characters, char 1)" pali-category-table)
(define-category ?e "Pali consonant cluster (two characters, char 2)" pali-category-table)
(define-category ?v "Pali short vowel" pali-category-table)
(define-category ?V "Pali long vowel" pali-category-table)
(define-category ?W "Pali long vowel (two characters, char 1)" pali-category-table)
(define-category ?X "Pali long vowel (two characters, char 2)" pali-category-table)

(defun pali--short-vowel-p (str)
  (member (downcase str) pali-short-vowel-list))

(defun pali--long-vowel-p (str)
  (member (downcase str) pali-long-vowel-list))

(defun pali--unstopped-syllable-p (vowel &optional end-consonant)
  (if (null end-consonant)
      t
    (let* ((small-con (downcase end-consonant))
           (end-consonant-props (cdr (assoc small-con pali-consonant-alist))))
    (or (member 'unstopped end-consonant-props)
        (member 'nasal end-consonant-props)))))

(defvar pali--tone-quirk nil
  "Variable indicating that an initial 'm' was followed by a
  final 's'.")

(defun pali--set-tone-quirk (end-consonant following-consonant)
  (setq pali--tone-quirk (and (string= end-consonant "s")
                              (string= following-consonant "m"))))

(defun pali--unset-tone-quirk ()
  (let ((temp-holder pali--tone-quirk))
    (setq pali--tone-quirk nil)
    temp-holder))

(defun pali--high-determining-consonant-p (consonant)
  (unless (null consonant)
    (let* ((small-con (downcase consonant))
           (consonant-props (cdr (assoc small-con pali-consonant-alist))))
      (or (pali--unset-tone-quirk)
          (member 'high-det consonant-props)))))

(defun pali--high-or-base-tone (consonant vowel &optional end-consonant)
  (or (and (pali--unstopped-syllable-p vowel end-consonant)
           (pali--high-determining-consonant-p consonant)
           'high)
      'base))

(defun pali--build-category-table (str-list cat-char &optional no-two-cat)
  (dolist (el str-list)
    (let* ((char (aref el 0))
           (capital (upcase char))
	   (inc 0))
      (when (and (> (length el) 1) (not no-two-cat))
	(setq inc 1)
        (modify-category-entry (aref el 1) (+ 1 inc cat-char) pali-category-table))
      (modify-category-entry char (+ inc cat-char) pali-category-table)
      (modify-category-entry capital (+ inc cat-char) pali-category-table))))

(defconst pali-long-vowel-list
  '("ā" 
    "e" 
    "ī" 
    "o" 
    "ū" 
    "ay"))

(pali--build-category-table pali-long-vowel-list ?V)

(defconst pali-short-vowel-list
  '("a"
    "i"
    "u"))

(pali--build-category-table pali-short-vowel-list ?v)
  
(defconst pali-consonant-alist
  '(("b"  stop)
    ("bh")

    ("c"  stop)
    ("ch" stop high-det)

    ("d"  stop)
    ("ḍ"  stop)
    ("dh")
    ("ḍh")

    ("g"  stop)
    ("gh")

    ("h"  vowel-semi high-det)

    ("j"  stop)
    ("jh")

    ("k"  stop)
    ("kh" high-det)

    ("l"  unstopped vowel-semi)
    ("ḷ"  unstopped vowel-semi)

    ("m"  unstopped)
    ("ṁ"  nasal)

    ("n"  unstopped)
    ("ṅ"  unstopped)
    ("ṇ"  unstopped special)
    ("ñ"  unstopped)

    ("p"  stop)
    ("ph" high-det)

    ("r"  vowel-semi)

    ("s"  stop high-det)

    ("t"  stop)
    ("ṭ"  stop)
    ("th" high-det)
    ("ṭh" high-det)

    ("v"  vowel-semi)
    ("y"  unstopped vowel-semi)))

(pali--build-category-table (mapcar 'car pali-consonant-alist) ?c)

(defconst pali-syllable-pattern
  "\\W*\\(?1:\\cd\\ce\\|\\cc\\)?\\W*\\(?2:\\cW\\cX\\|\\cV\\|\\cv\\)\\S-*?\\(?3:\\cd\\ce\\|\\(?4:\\cc\\)\\W*\\(?5:\\cd\\ce\\|\\cc\\)\\|\\cc\\)?"
  "Regex for matching a Pali syllable. Simplified version:
(de|c)?(WX|V|v)(de|(c)(de|c)|c)?, where each letter in the regex
stands for the category of that letter. Includes non-whitespace
non-word-constituent characters after each consonant or vowel.")

(defun pali--find-single-syllable (from &optional to str)
  ;; Yanked from lao-util.el: sets up match object for str arg or current buffer.
  (with-category-table pali-category-table
    (if str
        (if (setq from (string-match pali-syllable-pattern str from))
	    (if (and to (>= from to))
		(setq from nil)
	      (setq to (match-end 0))))
      (save-excursion
        (goto-char from)
        (if (setq to (re-search-forward pali-syllable-pattern to t))
	    (setq from (match-beginning 0))
	  (setq from nil)))))
  (if from
      (let* ((consonant (match-string 1 str))
             (vowel (match-string 2 str))
             (end-consonant-group (match-string 3 str))
             (end-consonant (or (match-string 4 str) end-consonant-group))
             (following-consonant (match-string 5 str))
             (start (or (match-beginning 1) (match-beginning 2) (match-beginning 0)))
             (end (match-end 0))
             type tone)
	;; (message "---------------------")
        ;; (message (match-string 0 str))
        ;; (message "consonant:           %s" consonant)
        ;; (message "vowel:               %s" vowel)
        ;; (message "end-consonant-group: %s" end-consonant-group)
        ;; (message "end-consonant:       %s" end-consonant)
        ;; (message "following-consonant: %s" following-consonant)
        (cond
         ((or following-consonant (string= end-consonant "ṁ"))
	  ;; Matched a syllable that ends with a consonant and
	  ;; precedes a syllable that starts with a consonant.
          (setq end (or (match-end 4) (match-end 3))
                type 'long))

	 ((and (not end-consonant) (= (length vowel) 2))
	  ;; Handle the case of an 'ay' combination that's not followed by a
	  ;; consonant: the 'y' counts as a consonant instead.
	  (setq end (1- (match-end 2))
		end-consonant "y"
		type 'short))

         ((pali--long-vowel-p vowel)
          (setq end (match-end 2)
                end-consonant nil
                type 'long))

         ((pali--short-vowel-p vowel)
          (setq end (match-end 2)
                end-consonant nil
                type 'short)))

        (if (eq type 'short)
            (setq tone 'base)
          (setq tone (pali--high-or-base-tone consonant vowel end-consonant)))
        ;; Set tone quirk; _must_ be done after evaluating tone in line above.
        (pali--set-tone-quirk end-consonant following-consonant)

        (list start end type tone))))

(iter-defun pali-iter-syllables (&optional from to str yield-non-syllable)
  (let ((from (or from (if str 0 (point))))
	(to (or to (when str (length str))))
        val)
    (while (setq val (pali--find-single-syllable from to str))
      (let* ((start (car val))
             (end (cadr val))
             (type (caddr val))
             (tone (cadddr val)))
        (when (and yield-non-syllable (> start from))
          (iter-yield (list `(,from ,start) nil nil)))
        (iter-yield (list `(,start ,end) type tone))
        (setq from end)))
    (when (and to (< from to))
        (iter-yield (list `(,from ,to) nil nil)))))

(defun pali-syllables-str (&optional str from to)
  "Given a string, return a list of triplets
containing (SYLLABLE-OR-STR TYPE TONE), with TYPE and TONE
potentially nil."
  (let ((pali-syllables (list))
	(substring-f (if str (-partial 'substring-no-properties str)
		       'buffer-substring-no-properties)))
    (iter-do (syllable (pali-iter-syllables from to str t))
      ;; Replace range with syllable text
      (setcar syllable (apply substring-f (car syllable)))
      ;; (message (car syllable))
      (setq pali-syllables (append pali-syllables (list syllable))))
    pali-syllables))

(defun pali-annotate-buffer ()
  "Create a new buffer from the current buffer as an annotated
HTML file."
  (interactive)
  (let* ((name (concat (buffer-name) ".html"))
	 (file (buffer-file-name))
	 (buf (generate-new-buffer name)))
    (copy-to-buffer buf (point-min) (point-max))
    (with-current-buffer buf
      (setq buffer-file-name
	    (if file (concat file ".html")
	      (expand-file-name name)))
      (iter-do (syllable (pali-iter-syllables))
	)
      buf)))

;;;###autoload
(defun pali-font-lock-set-syllable-faces (&optional to)
  (iter-do (syllable (pali-iter-syllables (point) to))
    (let* ((range (car syllable))
	   (start (car range))
	   (end   (cadr range))
           (type  (cadr syllable))
           (tone  (caddr syllable)))
      (when (-some? (-rpartial 'funcall start end)
                      pali-fontify-predicates)
        (add-text-properties start end
                             (list 'type type 'tone tone
                                   'font-lock-face (list (and (eq tone 'high) 'underline)
                                                         (and (eq type 'long) 'highlight))
                                   'font-lock-fontified t))))))

;; (pali-syllables-str "[Evam-me sutaṁ.] Ekaṁ samayaṁ Bhagavā")
;; (pali-syllables-str "Samma-sambuddho")
;; (pali-syllables-str "bhikkhū gārayhā")
;; (pali-syllables-str "Aham-aderena taṁ")
;; (pali-syllables-str "Phalaṁ mayhaṁ")
;; (pali-syllables-str "Saṁvatteyya")
;; (pali-syllables-str "Arahaṁ samma-sambuddho bhagavā")
;; (pali-syllables-str "Buddh'vārahanta")
;; (pali-syllables-str "Yathā nhārū")
;; (pali-syllables-str "Saṇhāhi vācāhi upavhayantā")
;; (pali-syllables-str "Kapilavatthusmiṁ")
;; (pali-syllables-str "nhārū")
;; (pali-syllables-str "n'atthi")
;; (pali-syllables-str "appaccakkhāya")
;; (pali-syllables-str "Pāṭimokkha")
;; (pali-syllables-str "Uddiṭṭhā dve aniyatā dhammā")
;; (pali-syllables-str "Dvitti-patta-pūre")
;; (pali-syllables-str "bhikkhu-sammatiyā")
;; (with-temp-buffer (insert "bhikkhu-sammatiyā") (goto-char 0) (pali-syllables-str))

(defun pali--latex-encode (s info)
  (let ((res-string "")
	prev-type)
    (iter-do (syllable (pali-iter-syllables nil nil s t))
      (pcase-let* ((`((,start ,end) ,type ,tone) syllable)
		   (syllable (substring s start end)))
	(when (eq tone 'high)
	  (setq syllable (format "{\\ul{%s}}" syllable)))
	(when (eq type 'long)
	  (setq syllable (format "\\hl{%s}" syllable)))
	(setq res-string (concat res-string (when (and type prev-type) "\\-") syllable))
	(setq prev-type type)))
    res-string))

(defun pali-latex-plain-text (text backend info)
  ;; Filter has to be on plain-text, because if it's a paragraph
  ;; filter, the text parameter might already have LaTeX commands in
  ;; it.
  (let* ((par (org-element-lineage text '(paragraph)))
	 ;; Note: `par' and `parent-block' may be nil.
	 (parent-block (org-element-lineage par '(special-block)))
	 (parent-block-type (org-element-property :type parent-block))
	 (pali-p (equal "pali" parent-block-type)))
    (if (and par pali-p)
	(cond
	 ((org-export-derived-backend-p backend 'latex)
	  (pali--latex-encode text info))
	 ((org-export-derived-backend-p backend 'html)
	  text))
      text)))

(defun pali-latex-item (item backend info)
  (if (org-export-derived-backend-p backend 'latex)
      (when-let* ((match (string-match "\\\\begin{\\(pali\\|english\\)}\n"
				       item))
		  (prec-text (substring-no-properties item 0 match)))
	;; Swap \begin{pali|english} with preceding \item command
	(store-substring item 0 (match-string 0 item))
	(store-substring item (- (match-end 0) match) prec-text)
	;; Insert \item inside 'english' environment
	(replace-regexp-in-string "\\(\\\\begin{english}\n\\)\\(?:.\\|\n\\)*\\'" "\\&\\\\item " item nil nil 1))
    item))

(defun pali-latex-plain-list (list backend info)
  (if (org-export-derived-backend-p backend 'latex)
      ;; Insert \synccounter{enumi} to sync enumerate counters in both columns
      (replace-regexp-in-string "\\\\begin{enumerate}\\(\\(?:.\\|\n\\)*\\)\\\\end{enumerate}"
				"\\\\end{bilingual}\n\\\\begin{enumerate}\n\\\\begin{bilingual}\\1\\\\end{bilingual}\\\\end{enumerate}\\\\begin{bilingual}" list)
    list))

(defconst pali-latex-header "\\usepackage{paracol}
\\usepackage{soulutf8}
\\colorlet{shadecolor}{gray!20}
\\sethlcolor{shadecolor}
\\renewcommand{\\ul}{\\uline}
\\newenvironment{bilingual}{%
  \\begin{paracol}[1]*{2}%
}{%
  \\end{paracol}%
}

\\newenvironment{pali}{%
  \\begin{leftcolumn*}%
}{%
  \\end{leftcolumn*}%
}

\\newenvironment{english}{%
  \\begin{rightcolumn}%
}{%
  \\end{rightcolumn}%
}\n")

(defun pali-latex-options (opts backend)
  (if (org-export-derived-backend-p backend 'latex)
      (plist-put opts :latex-header
		 (concat pali-latex-header
			 (plist-get opts :latex-header)))
      opts))

(add-to-list 'org-export-filter-plain-text-functions 'pali-latex-plain-text)
(add-to-list 'org-export-filter-plain-list-functions 'pali-latex-plain-list)
(add-to-list 'org-export-filter-item-functions 'pali-latex-item)
(add-to-list 'org-export-filter-options-functions 'pali-latex-options)

(defun pali--add-to-extra-keywords ()
  (let ((keywords '((pali-font-lock-set-syllable-faces))))
    (font-lock-add-keywords nil keywords t)))

;;;###autoload
(define-minor-mode pali-mode
  "Toggle Pāḷi minor-mode for Org-mode."
  :init-value nil
  :lighter " Pāḷi"
  (when pali-mode
    (pali--add-to-extra-keywords)))

(provide 'pali)
