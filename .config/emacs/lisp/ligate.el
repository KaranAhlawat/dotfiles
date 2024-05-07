;;; ligate.el --- True ligatures in Emacs	 -*- lexical-binding: t; -*-
;;; Commentary:
;; Just uses ligature.el to enable Fira ligatures
;;; Code:
(defvar pragmata-whitespace-ligatures '("[INFO]" "[WARN]" "[PASS]" "[VERBOSE]" "[KO]" "[OK]" "[PASS]" "[ERROR]" "[DEBUG]" "[INFO]" "[WARN]"
																				"[WARNING]" "[ERR]" "[FATAL]" "[TRACE]" "[FIXME]" "[TODO]" "[BUG]" "[NOTE]" "[HACK]" "[MARK]" "[FAIL]"
																				"// ERROR" "// DEBUG" "// INFO" "// WARN" "// WARNING" "// ERR" "// FATAL" "// TRACE" "// FIXME" "// TODO"
																				"// BUG" "// NOTE" "// HACK" "// MARK" "// FAIL" "# ERROR" "# DEBUG" "# INFO" "# WARN" "# WARNING" "# ERR"
																				"# FATAL" "# TRACE" "# FIXME" "# TODO" "# BUG" "# NOTE" "# HACK" "# MARK" "# FAIL"))


(defvar pragmata-coding-ligatures '("!=" "!==" "!≡" "!≡≡" "!=<" "#(" "#_" "#{" "#?" "##" "#_(" "#[" "%=" "&%" "&&" "&+" "&-" "&/"
																		"&=" "&&&" "$>" "(|" "*>" "++" "+++" "+=" "+>" "++=" "--" "-<" "-<<" "-=" "->" "->>" "---" "-->"
																		"-+-" "-\\/" "-|>" "-<|" "->-" "-<-" "-|" "-||" "-|:" ".=" "//=" "/=" "/==" "/-\\" "/-:" "/->" "/=>"
																		"/-<" "/=<" "/=:" ":=" ":=" ":=>" ":-\\" ":=\\" ":-/" ":=/" ":-|" ":=|" ":|-" ":|=" "<$>" "<*" "<*>"
																		"<+>" "<-" "<<=" "<=" "<=>" "<>" "<|>" "<<-" "<|" "<=<" "<~" "<~~" "<<~" "<$" "<+" "<!>" "<@>" "<#>"
																		"<%>" "<^>" "<&>" "<?>" "<.>" "</>" "<\\>" "<\">" "<:>" "<~>" "<**>" "<<^" "<=" "<->" "<!--" "<--" "<~<"
																		"<==>" "<|-" "<||" "<<|" "<-<" "<-->" "<<==" "<==" "<-\\" "<-/" "<=\\" "<=/" "=<<" "==" "===" "==>" "=>"
																		"=~" "=>>" "=~=" "==>>" "=>=" "=<=" "=<" "==<" "=<|" "=/" "=/=" "=/<" "=|" "=||" "=|:" ">-" ">=" ">>-"
																		">>=" ">=>" ">>^" ">>|" ">!=" ">->" ">==" ">=" ">/=" ">-|" ">=|" ">-\\" ">=\\" ">-/" ">=/" ">λ=" "?." "^="
																		"^^" "^<<" "^>>" "\\=" "\\==" "\\/-" "\\-/" "\\-:" "\\->" "\\=>" "\\-<" "\\=<" "\\=:" "|=" "|>=" "|>" "|+|"
																		"|->" "|-->" "|=>" "|==>" "|>-" "|<<" "||>" "|>>" "|-" "||-" "||=" "|)" "|]" "|-:" "|=:" "|-<" "|=<" "|--<"
																		"|==<" "~=" "~>" "~~>" "~>>" "[[" "[|" "_|_" "]]"))

(defvar fira-code-coding-ligatures '(;; == === ==== => =| =>>=>=|=>==>> ==< =/=//=// =~
																		 ;; =:= =!=
																		 ("=" (rx (+ (or ">" "<" "|" "/" "~" ":" "!" "="))))
																		 ;; ;; ;;;
																		 (";" (rx (+ ";")))
																		 ;; && &&&
																		 ("&" (rx (+ "&")))
																		 ;; !! !!! !. !: !!. != !== !~
																		 ("!" (rx (+ (or "=" "!" "\." ":" "~"))))
																		 ;; ?? ??? ?:	 ?=	 ?.
																		 ("?" (rx (or ":" "=" "\." (+ "?"))))
																		 ;; %% %%%
																		 ("%" (rx (+ "%")))
																		 ;; |> ||> |||> ||||> |] |} || ||| |-> ||-||
																		 ;; |->>-||-<<-| |- |== ||=||
																		 ;; |==>>==<<==<=>==//==/=!==:===>
																		 ("|" (rx (+ (or ">" "<" "|" "/" ":" "!" "}" "\]"
																										 "-" "=" ))))
																		 ;; \\ \\\ \/
																		 ("\\" (rx (or "/" (+ "\\"))))
																		 ;; ++ +++ ++++ +>
																		 ("+" (rx (or ">" (+ "+"))))
																		 ;; :: ::: :::: :> :< := :// ::=
																		 (":" (rx (or ">" "<" "=" "//" ":=" (+ ":"))))
																		 ;; // /// //// /\ /* /> /===:===!=//===>>==>==/
																		 ("/" (rx (+ (or ">"	"<" "|" "/" "\\" "\*" ":" "!"
																										 "="))))
																		 ;; .. ... .... .= .- .? ..= ..<
																		 ("\." (rx (or "=" "-" "\?" "\.=" "\.<" (+ "\."))))
																		 ;; -- --- ---- -~ -> ->> -| -|->-->>->--<<-|
																		 ("-" (rx (+ (or ">" "<" "|" "~" "-"))))
																		 ;; *> */ *)	** *** ****
																		 ("*" (rx (or ">" "/" ")" (+ "*"))))
																		 ;; www wwww
																		 ("w" (rx (+ "w")))
																		 ;; <> <!-- <|> <: <~ <~> <~~ <+ <* <$ </	 <+> <*>
																		 ;; <$> </> <|	<||	 <||| <|||| <- <-| <-<<-|-> <->>
																		 ;; <<-> <= <=> <<==<<==>=|=>==/==//=!==:=>
																		 ;; << <<< <<<<
																		 ("<" (rx (+ (or "\+" "\*" "\$" "<" ">" ":" "~"	 "!"
																										 "-"	"/" "|" "="))))
																		 ;; >: >- >>- >--|-> >>-|-> >= >== >>== >=|=:=>>
																		 ;; >> >>> >>>>
																		 (">" (rx (+ (or ">" "<" "|" "/" ":" "=" "-"))))
																		 ;; #: #= #! #( #? #[ #{ #_ #_( ## ### #####
																		 ("#" (rx (or ":" "=" "!" "(" "\?" "\[" "{" "_(" "_"
																									(+ "#"))))
																		 ;; ~~ ~~~ ~=	 ~-	 ~@ ~> ~~>
																		 ("~" (rx (or ">" "=" "-" "@" "~>" (+ "~"))))
																		 ;; __ ___ ____ _|_ __|____|_
																		 ("_" (rx (+ (or "_" "|"))))
																		 ;; Fira code: 0xFF 0x12
																		 ("0" (rx (and "x" (+ (in "A-F" "a-f" "0-9")))))
																		 ;; Fira code:
																		 "Fl"	 "Tl"	 "fi"	 "fj"	 "fl"	 "ft"
																		 ;; The few not covered by the regexps.
																		 "{|"	 "[|"	 "]#"	 "(*"	 "}#"	 "$>"	 "^="))

(use-package ligature
	:straight t
	:config

	(ligature-set-ligatures 't '("www"))
	(ligature-set-ligatures 'prog-mode fira-code-coding-ligatures)

	(global-ligature-mode 't))

(provide 'ligate)
;;; ligate.el ends here
