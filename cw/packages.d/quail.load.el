(quail-define-package
 "french-cw-postfix" "French" "C<" t
 "French (Français) input method with postfix modifiers based on
 french-alt-postfix."
 nil t nil nil nil nil nil nil nil nil t)
(quail-define-rules

 ("<<" ["« "])      ("<<<" ["<<"])
 (">>" [" »"])      (">>>" [">>"])
 ("``" ["“"])       ("```" ["``"])
 ("`'" ["”"])       ("`''" ["`'"])

 ("o^" ?°)          ("o^^" ["o^"])
 ("1^" ?¹)          ("1^^" ["1^"])
 ("2^" ?²)          ("2^^" ["2^"])
 ("3^" ?³)          ("3^^" ["3^"])
 ("4^" ?⁴)          ("4^^" ["4^"])
 ("5^" ?⁵)          ("5^^" ["5^"])
 ("6^" ?⁶)          ("6^^" ["6^"])
 ("7^" ?⁷)          ("7^^" ["7^"])
 ("8^" ?⁸)          ("8^^" ["8^"])
 ("9^" ?⁹)          ("9^^" ["9^"])
 ("0^" ?⁰)          ("0^^" ["0^"])

 ;; Magic letters
 ("ae" ?æ)          ("aee"  ["ae"])
 ("oe" ?œ)          ("oee"  ["oe"])
 ("AE" ?Æ)          ("AEE"  ["AE"])
 ("OE" ?Œ)          ("OEE"  ["OE"])

 ("|:" ?¦)          ("|::"  ["|:"])
 ("p/" ?§)          ("p//"  ["p/"])
 ("p|" ?¶)          ("p||"  ["p|"])
 ("..." ?…)         ("...." ["..."])

 (":))" ?☺)         (":)))" [":))"]);; ☻ or ☺?
 (":((" ?☹)         (":(((" [":(("])

 ("=~" ?≈)          ("=~~"  ["=~"])
 ("=/" ?≠)          ("=//"  ["=/"])

 ;; non breaking space
 ("?" [" ?"])       ("??" ["?"])
 ("!" [" !"])       ("!!" ["!"])
 (";" [" ;"])       (";;" [";"])
 (":" [" :"])       ("::" [":"])
 )

;;;###autoload
(defun cw:toggle-input-method ()
  "Toggle between `french-cw-postfix' method  and nil"
  (interactive)
  (if (string= current-input-method "french-cw-postfix")
      (inactivate-input-method)
    (set-input-method "french-cw-postfix")))

(global-set-key (kbd "<f9>") 'cw:toggle-input-method)
