(require 'ert)


(ert-deftest ert-test-org-contacts-property-email-value-extracting-regexp ()
  "Testing org-contacts property `EMAIL' value extracting regexp rule."
  (let ((regexp-rule
         ;; "\\[\\[mailto:\\(.*\\)\\]\\(\\[.*\\]\\)\\]" ; valid
         "\\[\\[mailto:\\(.*\\)\\]\\(\\[.*\\]\\)\\]\\(,\\ *\\[\\[mailto:\\(.*\\)\\]\\(\\[.*\\]\\)\\]\\)" ; valid
         ))
    (let ((pvalue "huangtc@outlook.com")) ; normal email
      (if (string-match regexp-rule pvalue)
          (should (string-equal (match-string 1 pvalue) "yantar92@posteo.net"))
        pvalue))

    (let ((pvalue "huangtc@outlook.com,")) ; has comma separator
      (if (string-match regexp-rule pvalue)
          (should (string-equal (match-string 1 pvalue) "yantar92@posteo.net"))
        pvalue))

    (let ((pvalue "huangtc@outlook.com, tristan.j.huang@gmail.com,"))
      (if (string-match regexp-rule pvalue)
          (should (string-equal (match-string 1 pvalue) "yantar92@posteo.net"))
        pvalue))

    (let ((pvalue "[[mailto:yantar92@posteo.net]]"))
      (if (string-match regexp-rule pvalue)
          (should (string-equal (match-string 1 pvalue) "yantar92@posteo.net"))
        pvalue))

    (let ((pvalue "[[mailto:yantar92@posteo.net][yantar92@posteo.net]]"))
      (if (string-match regexp-rule pvalue)
          (should (string-equal (match-string 1 pvalue) "yantar92@posteo.net"))
        pvalue))

    (let ((pvalue "[[mailto:yantar92@posteo.net][yantar92@posteo.net]], [[mailto:yantar92@gmail.com][yantar92@gmail.com]]"))
      (if (string-match regexp-rule pvalue)
          (should (string-equal (match-string 1 pvalue) "yantar92@posteo.net"))
        pvalue))
    ))

;;; literal testing

;; (let ((regexp-rule "\\[\\[mailto:\\(.*\\)\\]\\(\\[.*\\]\\)\\]")
;;       (pvalue "[[mailto:yantar92@posteo.net][yantar92@posteo.net]]"))
;;   (if (string-match regexp-rule pvalue)
;;       (match-string 1 pvalue)
;;     pvalue))

;; (let ((regexp-rule "\\[\\[mailto:\\(.*\\)\\]\\(\\[.*\\]\\)\\]\\(,\\ *\\[\\[mailto:\\(.*\\)\\]\\(\\[.*\\]\\)\\]\\)")
;;       (pvalue "[[mailto:yantar92@posteo.net][yantar92@posteo.net]], [[mailto:yantar92@gmail.com][yantar92@gmail.com]]"))
;;   (if (string-match regexp-rule pvalue)
;;       (match-string 1 pvalue)
;;     pvalue))
