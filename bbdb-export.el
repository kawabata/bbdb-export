;;; bbdb-export.el --- Versatile Exporter of BBDB records to various formats

;; Filename: bbdb-export.el
;; Description: Versatile Exporter of BBDB records to various formats
;; Author: KAWABATA, Taichi <kawabata.taichi_at_gmail.com>
;; Created: 2008-05-10
;; Keywords: bbdb
;; Version: 1.130913
;; URL: http://github.com/kawabata/bbdb-export/

;;; Commentary:

;; This code exports BBDB data to vCard (M-x bbdb-export-vcard-v3) and
;; other formats.

;; This exporter processes three functions.
;;
;;   +----------------------+
;;   | bbdb-export-p        |  This determines which BBDB records to be exported.
;;   +----------+-----------+
;;              |
;;              v              This converts structured BBDB fields to
;;   +----------+-----------+  flat alist with string.  You can add new field
;;   | bbdb-export-info     |  if you need.  `:name-format' determines whether
;;   +----------+-----------+  name is Asian ("last-first") or European ("first-last")
;;              |              style.
;;              v
;;   +----------+-----------+
;;   | bbdb-export-template |  This exports flat alist to specified format.
;;   +----------------------+
;;
;; For bbdb-import, following-code may be used.
;;  (let ((fresh-record (make-vector bbdb-record-length nil)))
;;    (bbdb-record-set-cache fresh-record
;;                           (make-vector bbdb-cache-length nil))
;;    (if vcard-rev            ; For fresh records,
;;        (bbdb-record-putprop ; set creation-date from vcard-rev
;;         fresh-record 'creation-date vcard-rev)
;;      (run-hook-with-args 'bbdb-create-hook fresh-record))
;;    fresh-record)))

(require 'bbdb-com)
(require 'cl-lib)
(require 'qp)

;;; Variables

(defgroup bbdb-export nil
  "Export BBDB records to various formats"
  :prefix "bbeb-export-"
  :group 'bbdb)

(defcustom bbdb-export-file
  "~/.emacs.d/bbdb.vcf"
  "Filename for export."
  :type 'string
  :group 'bbdb-export)

(defcustom bbdb-export-p
  'listp
  "Function to decide whether specific BBDB records to be exported."
  :type 'function
  :group 'bbdb-export)

(defcustom bbdb-export-info
  'bbdb-export-info
  "Function to convert BBDB record to alist."
  :type 'function
  :group 'bbdb-export)

(defcustom bbdb-export-template
  'bbdb-export-template-vcard-v3
  "Function to convert BBDB alist to text data."
  :type 'function
  :group 'bbdb-export)

(defvar bbdb-name-format) ;; defined in bbdb.el

;;; Main Function

(defun bbdb-export ()
  "Export BBDB data that satisfies CONDITION to FILE with specified TEMPLATE.
See `bbdb-export-vcard-template' for example of template format."
  (interactive)
  (with-temp-file bbdb-export-file
    (dolist (record (bbdb-search (bbdb-records) "."))
      (when (funcall bbdb-export-p record)
        (let ((info (funcall bbdb-export-info record)))
          (insert (funcall bbdb-export-template info)))))))

;;;; Pre-defined functions

(defun bbdb-export-vcard-v3 ()
  (interactive)
  (let ((bbdb-export-file "~/.emacs.d/bbdb.vcf")
        (bbdb-export-p (lambda (record) (bbdb-record-field record 'furigana)))
        (bbdb-export-template 'bbdb-export-template-vcard-v3))
    (bbdb-export)))

(defun bbdb-export-csv ()
  (interactive)
  (let ((bbdb-export-file "~/.emacs.d/bbdb.csv")
        (bbdb-export-p (lambda (record) (bbdb-record-field record 'nenga)))
        (bbdb-export-template 'bbdb-export-template-csv))
    (bbdb-export)))

;;; BBDB Field extraction functions

(defun bbdb-export--label (record call-func label-func label)
  (elt (cl-find-if (lambda (item) (equal label (funcall label-func item)))
                   (funcall call-func record)) 1))

(defun bbdb-export--phone-label (record label)
  (let ((phone (bbdb-export--label record 'bbdb-record-phone 'bbdb-phone-label label)))
    (and phone
         (or (and (stringp phone) phone)
             (bbdb-phone-string phone)))))

(defun bbdb-export--address-label (record label)
  (elt
   (bbdb-export--label record 'bbdb-record-address 'bbdb-address-label label) 1))

(defun bbdb-export--email-work (mails)
  (cl-find-if (lambda (mail)
                (or (string-match "\\.co\\.jp$" mail)
                    (string-match "\\.ac\\.jp$" mail)
                    (string-match "\\.go\\.jp$" mail)
                    (string-match "\\.com$" mail)))
              mails))

(defun bbdb-export--email-cell (mails)
  (cl-find-if (lambda (mail)
                (or (string-match "docomo\\.ne\\.jp$" mail)
                    (string-match "i\\.softbank\\.jp$" mail)
                    (string-match "ezweb\\.ne\\.jp$" mail)
                    (string-match "auone\\.jp$" mail)
                    (string-match "jp-t\\.ne\\.jp$" mail)))
              mails))

(defun bbdb-export--email-home (mails exclusions)
  (cl-find-if (lambda (mail)
                (not (memq mail exclusions)))
              mails))

;;;; Misc

(defun bbdb-export--remove-hyphens (string)
  (if (null string) ""
    (replace-regexp-in-string "-" "" string)))

(defun bbdb-export--quote (string)
  "Quote string to MIME quoted-printable format."
  (if (listp string) (setq string (apply 'concat string)))
  (quoted-printable-encode-string (encode-coding-string string 'utf-8)))
  ;;string) ;; for debugging, replace above line with this.


;;; Record to Alist

(defun bbdb-export-info (record)
  (let* ((furigana     (bbdb-record-field record 'furigana))
         (lastname     (bbdb-record-lastname record))
         (firstname    (bbdb-record-firstname record))
         (name-format  (bbdb-record-field record 'name-format))
         (mails        (bbdb-record-mail record))
         (email-work   (bbdb-export--email-work mails))
         (email-cell   (bbdb-export--email-cell mails))
         (email-home   (bbdb-export--email-home mails (list email-work
                                                           email-cell))))
    `((:firstname      . ,firstname)
      (:lastname       . ,lastname)
      (:name-format    . ,(if (stringp name-format) name-format
                            (symbol-name bbdb-name-format)))
      (:organization   . ,(bbdb-record-organization record))

      (:furigana       . ,furigana)
      (:furigana-last  . ,(and furigana (car (split-string furigana))))
      (:furigana-first . ,(and furigana (apply 'concat (cdr (split-string furigana)))))

      (:phone-work     . ,(bbdb-export--phone-label record "work"))
      (:phone-work2    . ,(bbdb-export--phone-label record "work2"))
      (:phone-home     . ,(bbdb-export--phone-label record "home"))
      (:phone-cell     . ,(bbdb-export--phone-label record "cell"))

      (:email-home     . ,email-home)
      (:email-work     . ,email-work)
      (:email-cell     . ,email-cell)

      (:address-home   . ,(bbdb-export--address-label record "home"))
      (:address-work   . ,(bbdb-export--address-label record "work"))
      (:address-jikka  . ,(bbdb-export--address-label record "実家"))

      ;;(attribution    . ,(bbdb-record-field record 'attribution))
      (:notes          . ,(bbdb-record-field record 'notes))
      (:birthday       . ,(bbdb-record-field record 'birthday))
      (:nenga          . ,(bbdb-record-field record 'nenga))
      (:www            . ,(bbdb-record-field record 'www)))))

;;; Templates

(defun bbdb-export-template-vcard-v2 (info)
  "BBDB export template for vCard v2."
  (cl-flet ((\, (key) (assoc-default key info)))
    (concat
     "BEGIN:VCARD\n"
     "VERSION:2.1\n"
     "N;CHARSET=UTF-8;ENCODING=QUOTED-PRINTABLE:"
         (bbdb-export-quote ,:lastname) ";"
         (bbdb-export-quote ,:firstname) ";;;\n"
     "FN;CHARSET=UTF-8;ENCODING=QUOTED-PRINTABLE:"
         (if (equal :name-format "last-first")
           (concat (bbdb-export-quote ,:lastname) " "
                   (bbdb-export-quote ,:firstname))
           (concat (bbdb-export-quote ,:firstname) " "
                   (bbdb-export-quote ,:lastname))) "\n"
     "X-PHONETIC-LAST-NAME;CHARSET=UTF-8;ENCODING=QUOTED-PRINTABLE:"
     (bbdb-export-quote (ucs-normalize-NFKC-string,:furigana-last)) "\n"
     "X-PHONETIC-FIRST-NAME;CHARSET=UTF-8;ENCODING=QUOTED-PRINTABLE:"
     (bbdb-export-quote (ucs-normalize-NFKC-string ,:furigana-first)) "\n"
     (when ,:phone-work
       (concat "TEL;WORK;VOICE:" (bbdb-export--remove-hyphens ,:phone-work) "\n"))
     (when ,:phone-home
       (concat "TEL;HOME;VOICE:" (bbdb-export--remove-hyphens ,:phone-home) "\n"))
     (when ,:phone-cell
       (concat "TEL;CELL:" (bbdb-export--remove-hyphens ,:phone-cell) "\n"))
     (when ,:phone-work2
       (concat "TEL;WORK;VOICE:" (bbdb-export--remove-hyphens ,:phone-work2) "\n"))
     (when ,:email-work   (concat "EMAIL;WORK:" ,:email-work "\n"))
     (when ,:email-home   (concat "EMAIL;HOME:" ,:email-home "\n"))
     (when ,:email-cell   (concat "EMAIL;CELL:" ,:email-cell "\n"))
     (when ,:address-work (concat "ADR;WORK;CHARSET=UTF-8;ENCODING=QUOTED-PRINTABLE:;"
                                  (bbdb-export-quote ,:address-work) "\n"))
     (when ,:address-home (concat "ADR;HOME;CHARSET=UTF-8;ENCODING=QUOTED-PRINTABLE:;"
                                  (bbdb-export-quote ,:address-home) "\n"))
     (when ,:www (concat "URL:" ,:www "\n"))
     (when ,:birthday (concat "BDAY:" ,:birthday "\n"))
     "X-CLASS:PUBLIC\n"
     "END:VCARD\n")))

(defun bbdb-export-template-vcard-v3 (info)
  "BBDB export template for vCard v3."
  (cl-flet ((\, (key) (assoc-default key info)))
    (concat
     "BEGIN:VCARD\n"
     "VERSION:3.0\n"
     "N:" ,:lastname ";" ,:firstname ";;;\n"
     "FN:" (if (equal ,:name-format "last-first")
               (concat ,:lastname " " ,:firstname)
             (concat ,:firstname " " ,:lastname)) "\n"
     "X-PHONETIC-LAST-NAME:" (ucs-normalize-NFKC-string,:furigana-last) "\n"
     "X-PHONETIC-FIRST-NAME:"(ucs-normalize-NFKC-string ,:furigana-first) "\n"
     (when ,:phone-work
       (concat "TEL;TYPE=WORK,VOICE:" (bbdb-export--remove-hyphens ,:phone-work) "\n"))
     (when ,:phone-home
       (concat "TEL;TYPE=HOME,VOICE:" (bbdb-export--remove-hyphens ,:phone-home) "\n"))
     (when ,:phone-cell
       (concat "TEL;TYPE=CELL:" (bbdb-export--remove-hyphens ,:phone-cell) "\n"))
     (when ,:phone-work2
       (concat "TEL;TYPE=WORK,VOICE:" (bbdb-export--remove-hyphens ,:phone-work2) "\n"))
     (when ,:email-work   (concat "EMAIL;TYPE=WORK:" ,:email-work "\n"))
     (when ,:email-home   (concat "EMAIL;TYPE=HOME:" ,:email-home "\n"))
     (when ,:email-cell   (concat "EMAIL;TYPE=CELL:" ,:email-cell "\n"))
     (when ,:address-work (concat "ADR;TYPE=WORK:" ,:address-work "\n"))
     (when ,:address-home (concat "ADR;TYPE=HOME:" ,:address-home "\n"))
     (when ,:www (concat "URL:" ,:www "\n"))
     (when ,:birthday (concat "BDAY:" ,:birthday "\n"))
     "X-CLASS:PUBLIC\n"
     "END:VCARD\n")))

(defvar bbdb-export-template-csv
  '(lastname " " firstname ","
    (substring (car addr) 1) "," ;; 郵便番号から「〒」を抜いたもの
    (elt addr 1) ","
    (elt addr 2) "\n"))

(provide 'bbdb-export)

;;; bbdb-export.el ends here.

;; Local Variables:
;; lexical-binding: t
;; outline-minor-mode: t
;; time-stamp-pattern: "10/Version:\\\\?[ \t]+1.%02y%02m%02d\\\\?\n"
;; eval: (hide-sublevels 5)
;; End:
