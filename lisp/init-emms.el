;;;; ems
(eye/use-package 'emms
                 :urls '(("emms" . "http://git.savannah.gnu.org/git/emms.git")) ;; 需要手动下载, https报错：SSL certificate problem: certificate has expired
                 :load-path "emms"
                 :command '(emms)
                 :ensure nil
                 :config
                 (progn
                   (require 'emms-setup)
                   (emms-all)
                   ;; (emms-default-players)

                   ;; (setq emms-source-file-default-directory "e:/music")
                   (setq emms-source-file-default-directory "y:/music/music")

                   ;; custom player, https://www.gnu.org/software/emms/manual/#New-Player
                   (require 'emms-player-simple)
                   ;; (define-emms-simple-player play '(file) "\\.mp3$" "D:\\emacs_env\\mpv\\mpv.exe") ;; ok

                   ;; https://mpv.io/manual/master/
                   ;; (define-emms-simple-player play '(file) "\\.mp3$" "mpv.exe --volume=50") ;; wrong
                   ;; (define-emms-simple-player play '(file) "\\.mp3$" "mpv.exe" "--volume=50") ;; 注意：参数不能和命令写在一起
                   ;; https://stackoverflow.com/questions/9147823/emms-error-dont-know-how-to-play-track
                   ;; https://epkg.vercel.app/emms-player-simple-mpv
                   ;; debug on emms-player-get
                   ;; maybe use (regexp-opt ".mp3" ".ape")
                   (define-emms-simple-player mympv '(file)
                     (regexp-opt
                      '(".ogg" ".mp3" ".wav" ".mpg" ".mpeg" ".wmv" ".wma"
                        ".mov" ".avi" ".divx" ".ogm" ".asf" ".mkv" "http://" "mms://"
                        ".rm" ".rmvb" ".mp4" ".flac" ".vob" ".m4a" ".flv" ".ogv" ".pls"))
                     "mpv.exe" "--no-terminal" "--force-window=no" "--volume=50")


                   (setq emms-setup-default-player-list '(emms-player-mympv)) ;; emms-player-mplayer emms-player-vlc
                   (setq emms-player-list '(emms-player-mympv))

                   )
                 )


(provide 'init-emms)
