(defvar conf/prag-ligs-general
  '("[INFO]" "[WARN]" "[PASS]" "[VERBOSE]" "[KO]" "[OK]" "[PASS]" "[ERROR]" "[DEBUG]" "[INFO]" "[WARN]" "[WARNING]" "[ERR]" "[FATAL]" "[TRACE]" "[FIXME]" "[TODO]" "[BUG]" "[NOTE]" "[HACK]" "[MARK]" "[FAIL]" "// ERROR" "// DEBUG" "// INFO" "// WARN" "// WARNING" "// ERR" "// FATAL" "// TRACE" "// FIXME" "// TODO" "// BUG" "// NOTE" "// HACK" "// MARK" "// FAIL" "# ERROR" "# DEBUG" "# INFO" "# WARN" "# WARNING" "# ERR" "# FATAL" "# TRACE" "# FIXME" "# TODO" "# BUG" "# NOTE" "# HACK" "# MARK" "# FAIL" "#_" "#{" "#?" "##" "#_(" "#[")
  "List of general purpose ligatures in Pragamta.")

(defvar conf/prag-ligs-prog
  '("!=" "!==" "!=<" "#(" "%=" "&%" "&&" "&+" "&-" "&/" "&=" "&&&" "$>" "(|" "*>" "++" "+++" "+=" "+>" "++=" "--" "-<" "-<<" "-=" "->" "->>" "---" "-->" "-+-" "-\\/" "-|>" "-<|" "->-" "-<-" "-|" "-||" "-|:" ".=" "//=" "/=" "/==" "/-\\" "/-:" "/->" "/=>" "/-<" "/=<" "/=:" ":=" ":=>" ":-\\" ":=\\" ":-/" ":=/" ":-|" ":=|" ":|-" ":|=" "<$>" "<*" "<*>" "<+>" "<-" "<<=" "<=>" "<>" "<|>" "<<-" "<|" "<=<" "<~" "<~~" "<<~" "<$" "<+" "<!>" "<@>" "<#>" "<%>" "<^>" "<&>" "<?>" "<.>" "</>" "<\\>" "<\">" "<:>" "<~>" "<**>" "<<^" "<=" "<->" "<!--" "<--" "<~<" "<==>" "<|-" "<||" "<<|" "<-<" "<-->" "<<==" "<==" "<-\\" "<-/" "<=\\" "<=/" "=<<" "==" "===" "==>" "=>" "=~" "=>>" "=~=" "==>>" "=>=" "=<=" "=<" "==<" "=<|" "=/" "=/=" "=/<" "=|" "=||" "=|:" ">-" ">>-" ">>=" ">=>" ">>^" ">>|" ">!=" ">->" ">==" ">=" ">/=" ">-|" ">=|" ">-\\" ">=\\" ">-/" ">=/" ">λ=" "?." "^=" "^^" "^<<" "^>>" "\\=" "\\==" "\\/-" "\\-/" "\\-:" "\\->" "\\=>" "\\-<" "\\=<" "\\=:" "|=" "|>=" "|>" "|+|" "|->" "|-->" "|=>" "|==>" "|>-" "|<<" "||>" "|>>" "|-" "||-" "||=" "|)" "|]" "|-:" "|=:" "|-<" "|=<" "|--<" "|==<" "~=" "~>" "~~>" "~>>" "[[" "[|" "_|_" "]]")
  "List of programming ligatures in Pragmata.")

(defvar conf/comfy-ligs
  '("<---" "<--" "<<-" "<-" "->" "-->" "--->" "<->" "<-->" "<--->" "<---->" "<!--"
    "<==" "<===" "<=" "=>" "=>>" "==>" "===>" ">=" "<=>" "<==>" "<===>" "<====>" "<!---"
    "<~~" "<~" "~>" "~~>" "::" ":::" "==" "!=" "===" "!=="
    ":=" ":-" ":+" "<*" "<*>" "*>" "<|" "<|>" "|>" "+:" "-:" "=:" "<******>" "++" "+++")
  "List of ligatures in Iosevka")

(use-package ligature
  :straight t
  :config
  (global-ligature-mode t))

(provide 'ligate)
