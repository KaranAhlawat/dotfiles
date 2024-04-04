(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(connection-local-criteria-alist
   '(((:application tramp :protocol "adb")
      tramp-adb-connection-local-default-shell-profile tramp-adb-connection-local-default-ps-profile)
     ((:application tramp)
      tramp-connection-local-default-system-profile tramp-connection-local-default-shell-profile)
     ((:application eshell)
      eshell-connection-default-profile)))
 '(connection-local-profile-alist
   '((tramp-adb-connection-local-default-ps-profile
      (tramp-process-attributes-ps-args)
      (tramp-process-attributes-ps-format
       (user . string)
       (pid . number)
       (ppid . number)
       (vsize . number)
       (rss . number)
       (wchan . string)
       (pc . string)
       (state . string)
       (args)))
     (tramp-adb-connection-local-default-shell-profile
      (shell-file-name . "/system/bin/sh")
      (shell-command-switch . "-c"))
     (tramp-connection-local-darwin-ps-profile
      (tramp-process-attributes-ps-args "-acxww" "-o" "pid,uid,user,gid,comm=abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ" "-o" "state=abcde" "-o" "ppid,pgid,sess,tty,tpgid,minflt,majflt,time,pri,nice,vsz,rss,etime,pcpu,pmem,args")
      (tramp-process-attributes-ps-format
       (pid . number)
       (euid . number)
       (user . string)
       (egid . number)
       (comm . 52)
       (state . 5)
       (ppid . number)
       (pgrp . number)
       (sess . number)
       (ttname . string)
       (tpgid . number)
       (minflt . number)
       (majflt . number)
       (time . tramp-ps-time)
       (pri . number)
       (nice . number)
       (vsize . number)
       (rss . number)
       (etime . tramp-ps-time)
       (pcpu . number)
       (pmem . number)
       (args)))
     (tramp-connection-local-busybox-ps-profile
      (tramp-process-attributes-ps-args "-o" "pid,user,group,comm=abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ" "-o" "stat=abcde" "-o" "ppid,pgid,tty,time,nice,etime,args")
      (tramp-process-attributes-ps-format
       (pid . number)
       (user . string)
       (group . string)
       (comm . 52)
       (state . 5)
       (ppid . number)
       (pgrp . number)
       (ttname . string)
       (time . tramp-ps-time)
       (nice . number)
       (etime . tramp-ps-time)
       (args)))
     (tramp-connection-local-bsd-ps-profile
      (tramp-process-attributes-ps-args "-acxww" "-o" "pid,euid,user,egid,egroup,comm=abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ" "-o" "state,ppid,pgid,sid,tty,tpgid,minflt,majflt,time,pri,nice,vsz,rss,etimes,pcpu,pmem,args")
      (tramp-process-attributes-ps-format
       (pid . number)
       (euid . number)
       (user . string)
       (egid . number)
       (group . string)
       (comm . 52)
       (state . string)
       (ppid . number)
       (pgrp . number)
       (sess . number)
       (ttname . string)
       (tpgid . number)
       (minflt . number)
       (majflt . number)
       (time . tramp-ps-time)
       (pri . number)
       (nice . number)
       (vsize . number)
       (rss . number)
       (etime . number)
       (pcpu . number)
       (pmem . number)
       (args)))
     (tramp-connection-local-default-shell-profile
      (shell-file-name . "/bin/sh")
      (shell-command-switch . "-c"))
     (tramp-connection-local-default-system-profile
      (path-separator . ":")
      (null-device . "/dev/null"))
     (eshell-connection-default-profile
      (eshell-path-env-list))))
 '(custom-safe-themes
   '("6631f884f5f43e9d8eee42f5bcf8522a7f791688d2d2667ec135c129066be243" "4c7228157ba3a48c288ad8ef83c490b94cb29ef01236205e360c2c4db200bb18" "02f57ef0a20b7f61adce51445b68b2a7e832648ce2e7efb19d217b6454c1b644" "afa47084cb0beb684281f480aa84dab7c9170b084423c7f87ba755b15f6776ef" "a9abd706a4183711ffcca0d6da3808ec0f59be0e8336868669dc3b10381afb6f" "b99e334a4019a2caa71e1d6445fc346c6f074a05fcbb989800ecbe54474ae1b0" "c865644bfc16c7a43e847828139b74d1117a6077a845d16e71da38c8413a5aaa" "5f128efd37c6a87cd4ad8e8b7f2afaba425425524a68133ac0efd87291d05874" "60ada0ff6b91687f1a04cc17ad04119e59a7542644c7c59fc135909499400ab8" "2721b06afaf1769ef63f942bf3e977f208f517b187f2526f0e57c1bd4a000350" "da75eceab6bea9298e04ce5b4b07349f8c02da305734f7c0c8c6af7b5eaa9738" "aec7b55f2a13307a55517fdf08438863d694550565dee23181d2ebd973ebd6b8" "7ea883b13485f175d3075c72fceab701b5bf76b2076f024da50dff4107d0db25" "7e068da4ba88162324d9773ec066d93c447c76e9f4ae711ddd0c5d3863489c52" "8d3ef5ff6273f2a552152c7febc40eabca26bae05bd12bc85062e2dc224cde9a" "1a1ac598737d0fcdc4dfab3af3d6f46ab2d5048b8e72bc22f50271fd6d393a00" "545ab1a535c913c9214fe5b883046f02982c508815612234140240c129682a68" "805b96a0f453029e5d3061630da6291e2831c1b927d72ea451ab40e7b3e8d05c" "bbdefca19798136ead944525aa6bca77f6877baacf9ac62b801aceebfeb7539c" "3e4f8d6cf201b550f64b3f549bad6ed53eff7b74b80f028f229005ce8707a091" "03adef25678d624333371e34ec050db1ad7d13c9db92995df5085ebb82978671" "affa3b550675b926959756e1fb920418e005660e13bc1c0d9690f92f8e16f55b" "ef95afda294e0b27a9f3e578642783f9b6e8a7e33546df46edbbb9138b94a8a0" "279f74e365ba5aade8bc702e0588f0c90b5dee6cf04cf61f9455661700a6ebeb" "9fad628c15f1e94af44e07b00ebe3c15109be28f4d73adf4a9e22090845cbce9" default))
 '(package-vc-selected-packages
   '((scala-ts-mode :vc-backend Git :url "https://github.com/KaranAhlawat/scala-ts-mode")))
 '(safe-local-variable-values
   '((lsp-enabled-clients ts-ls)
     (cider-shadow-watched-builds "app")
     (cider-default-cljs-repl . shadow)
     (cider-preferred-build-tool . shadow)))
'(smtpmail-smtp-server "smtp.gmail.com")
'(smtpmail-smtp-service 25))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(fringe ((t :background "#ffffff")))
 '(header-line ((t :box (:line-width 4 :color "#efefef" :style nil))))
 '(header-line-highlight ((t :box (:color "#202020"))))
 '(keycast-key ((t)))
 '(line-number ((t :background "#ffffff")))
 '(mode-line ((t :box (:line-width 6 :color "#b7c7ff" :style nil))))
 '(mode-line-active ((t :box (:line-width 6 :color "#b7c7ff" :style nil))))
 '(mode-line-highlight ((t :box (:color "#202020"))))
 '(mode-line-inactive ((t :box (:line-width 6 :color "#dbdbdb" :style nil))))
 '(tab-bar-tab ((t :box (:line-width 4 :color "#ffffff" :style nil))))
 '(tab-bar-tab-inactive ((t :box (:line-width 4 :color "#b3b3b3" :style nil))))
 '(vertical-border ((t :background "#ffffff" :foreground "#ffffff")))
 '(window-divider ((t (:background "#ffffff" :foreground "#ffffff"))))
 '(window-divider-first-pixel ((t (:background "#ffffff" :foreground "#ffffff"))))
 '(window-divider-last-pixel ((t (:background "#ffffff" :foreground "#ffffff")))))
