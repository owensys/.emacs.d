#!/usr/bin/bash

cd ~/.emacs.d/packages



# args: name url commit
function inst()
{
    echo "Install $1..."
    if [ ! -d "$1" ]; then
        outclone=$(git clone --depth 1 $2 $1 > /dev/null)
        # error check
        if [[ $outclone =~ "failed" ]]; then
            echo "Clone failed: $1"
            return
        fi
    fi


    pushd $1 > /dev/null
    deps=10
    while :
    do
        logall=$(git log --oneline --all|grep $3)
        if [[ $logall != "" ]]; then
            echo "found commit id, break"
            break
        else
            echo "not found commit id $3 , fetch $deps ..."
            outfetch=$(git fetch --depth $deps)
            # error check
            if [[ $outfetch =~ "failed" ]]; then
                break
            fi
            deps=$(($deps + 10))
        fi
    done

    git checkout $3 > /dev/null

    # check log commit
    output=$(git log --oneline -1)
    if [[ $output =~ $3 ]]; then
        echo "Install $1 ok"
    else
        echo "Install $1 failed!"
    fi
    popd > /dev/null
}

inst "ace-window" "https://github.com/abo-abo/ace-window" "77115af"
inst "emacs-which-key" "https://github.com/justbur/emacs-which-key.git" "bd34ede"
#inst "all-the-icons" "https://github.com/domtronn/all-the-icons.el.git" "f75c1130b"
inst "avy" "https://github.com/abo-abo/avy.git" "be61211"
inst "avy-zap" "https://github.com/cute-jumper/avy-zap.git" "7c8d1f4"
#inst "aweshell" "https://github.com/manateelazycat/aweshell.git" "6c10c8c"
inst "awesome-tray" "https://github.com/manateelazycat/awesome-tray.git" "edd150"
inst "benchmark-init" "https://github.com/dholm/benchmark-init-el.git" "0243556"
inst "bing-dict" "https://github.com/cute-jumper/bing-dict.el.git" "1d581aa"
inst "bm" "https://github.com/joodland/bm.git" "9a31c61"
inst "cfrs" "https://github.com/Alexander-Miller/cfrs" "f3a21f2"
inst "chinese-word-at-point" "https://github.com/xuchunyang/chinese-word-at-point.el.git" "8223d74"
inst "color-rg" "https://github.com/manateelazycat/color-rg.git" "a0f1e36"
inst "company-posframe" "https://github.com/tumashu/company-posframe.git" "18d6641"
inst "company-quickhelp" "https://github.com/expez/company-quickhelp.git" "9505fb0"
#inst "compat" "https://github.com/emacs-compat/compat" "be1d94d"
inst "counsel-etags" "https://github.com/redguardtoo/counsel-etags.git" "a65c03d"
inst "dash" "https://github.com/magnars/dash.el.git" "96eaba0"
inst "deft" "https://github.com/jrblevin/deft.git" "28be94d"
inst "denote" "https://github.com/protesilaos/denote" "5b395c2"
inst "doom-modeline" "https://github.com/seagle0128/doom-modeline.git" "41dff2c"
inst "edit-at-point" "https://github.com/enoson/edit-at-point.el.git" "3b800c1"
inst "eldoc-eval" "https://github.com/thierryvolpiatto/eldoc-eval.git" "e918005"
inst "elfeed" "https://github.com/skeeto/elfeed.git" "162d7d5"
inst "elisp" "https://github.com/zenitani/elisp.git" "dfa6a49"
inst "elisp-refs" "https://github.com/Wilfred/elisp-refs.git" "bf3cca8"
inst "emacs-application-framework" "https://github.com/manateelazycat/emacs-application-framework.git" "6705cadd"
inst "emacs-async" "https://github.com/jwiegley/emacs-async.git" "34feabe"
inst "emacs-backward-forward" "https://gitlab.com/vancan1ty/emacs-backward-forward.git" "5848995"
inst "emacs-ctable" "https://github.com/kiwanami/emacs-ctable.git" "48b7374"
inst "emacs-dashboard" "https://github.com/emacs-dashboard/emacs-dashboard.git" "34a0076"
inst "emacs-db" "https://github.com/nicferrier/emacs-db.git" "b3a423f"
inst "emacs-deferred" "https://github.com/kiwanami/emacs-deferred.git" "2239671"
inst "emacs-epc" "https://github.com/kiwanami/emacs-epc.git" "e1bfa5c"
inst "emacs-htmlize" "https://github.com/hniksic/emacs-htmlize.git" "dd27bc3"
inst "emacs-kv" "https://github.com/nicferrier/emacs-kv.git" "7211484"
#inst "emacs-memoize" "https://github.com/skeeto/emacs-memoize.git" "51b0759"
inst "emacs-password-genarator" "https://github.com/zargener/emacs-password-genarator.git" "c1da979"
inst "emacs-web-server" "https://github.com/skeeto/emacs-web-server.git" "22ce66e"
inst "emacs-websocket" "https://github.com/ahyatt/emacs-websocket.git" "1a08093"
# inst "emacsql" "https://github.com/skeeto/emacsql.git" "6d8cd93"
inst "emacsql" "https://github.com/magit/emacsql" "640122"
cp bin/emacsql-sqlite.exe  packages/emacsql/sqlite/
inst "emacsql-sqlite3" "https://github.com/cireu/emacsql-sqlite3.git" "2113618"
#inst "emms" "https://git.savannah.gnu.org/git/emms.git" "271427f"
inst "eno" "https://github.com/enoson/eno.el.git" "40075bb"
inst "f" "https://github.com/rejeep/f.el.git" "29531ac"
inst "ghub" "https://github.com/magit/ghub.git" "0fd648f"
inst "global-readonly" "https://github.com/owensys/global-readonly.git" "d804186"
inst "graphql" "https://github.com/vermiculus/graphql.el.git" "67237f2"
inst "helm" "https://github.com/emacs-helm/helm.git" "11867f8"
inst "helm-dash" "https://github.com/areina/helm-dash.git" "7f853bd"
inst "helpful" "https://github.com/Wilfred/helpful.git" "32cb28b"
inst "highlight-numbers" "https://github.com/Fanael/highlight-numbers.git" "8b4744c"
inst "hungry-delete" "https://github.com/nflath/hungry-delete.git" "d919e55"
inst "ht" "https://github.com/emacsmirror/ht.git" "3c1677f"
inst "hydra" "https://github.com/abo-abo/hydra.git" "317e1de"
inst "idle-require" "https://github.com/nschum/idle-require.el.git" "33592bb"
inst "ivy-posframe" "https://github.com/tumashu/ivy-posframe.git" "533a8e3"
inst "ivy-rich" "https://github.com/Yevgnen/ivy-rich.git" "aff9b6b"
inst "ivy-yasnippet" "https://github.com/mkcms/ivy-yasnippet.git" "83402d9"
inst "json-mode" "https://github.com/joshwnj/json-mode.git" "eedb456"
inst "json-snatcher" "https://github.com/Sterlingg/json-snatcher.git" "b28d1c0"
inst "keyfreq" "https://github.com/dacap/keyfreq.git" "dd88193"
inst "lsp-bridge" "https://github.com/manateelazycat/lsp-bridge" "e9ff0"
inst "lua-mode" "https://github.com/immerrr/lua-mode.git" "ad639c6"
inst "magit" "https://github.com/magit/magit.git" "5d91aad"
inst "magit-popup" "https://github.com/magit/magit-popup.git" "d8585fa"
inst "markdown-mode" "https://github.com/jrblevin/markdown-mode.git" "5d98592"
inst "mpvi" "https://github.com/lorniu/mpvi.git" "34b5c76"
inst "multiple-cursors" "https://github.com/magnars/multiple-cursors.el.git" "6956e8e"
inst "names" "https://github.com/Malabarba/names.git" "45a272f"
inst "nerd-icons" "https://github.com/rainstormstudio/nerd-icons.el" "a7321dd"
inst "nursery" "https://github.com/chrisbarrett/nursery.git" "ed35d99"
inst "orderless" "https://github.com/oantolin/orderless.git" "e678402"
inst "org-bullets" "https://github.com/integral-dw/org-bullets" "b70ac2e"
inst "org-journal" "https://github.com/bastibe/org-journal.git" "18df4d5"
inst "org-present" "https://github.com/rlister/org-present.git" "4ec04e1"
inst "org-ql" "https://github.com/alphapapa/org-ql" "2c09854"
inst "org-roam" "https://github.com/org-roam/org-roam.git" "5c06471"
inst "org-roam-ui" "https://github.com/org-roam/org-roam-ui.git" "5ac7496"
inst "org-super-agenda" "https://github.com/alphapapa/org-super-agenda.git" "f4f5289"
inst "ov" "https://github.com/emacsmirror/ov.git" "c5b9aa4"
inst "ox-hugo" "https://github.com/kaushalmodi/ox-hugo.git" "9544656"
inst "page-break-lines" "https://github.com/purcell/page-break-lines.git" "79eca86"
inst "parent-mode" "https://github.com/Fanael/parent-mode.git" "db692cf"
inst "pcre2el" "https://github.com/joddie/pcre2el.git" "b941ed8"
inst "peg" "https://github.com/emacsmirror/peg.git" "f55ca24"
inst "pfuture" "https://github.com/Alexander-Miller/pfuture.git" "19b53ae"
inst "plantuml-mode" "https://github.com/skuro/plantuml-mode" "ea45a13"
inst "popup" "https://github.com/auto-complete/popup-el.git" "71cede0"
inst "pos-tip" "https://github.com/pitkali/pos-tip.git" "bfe7420"
inst "posframe" "https://github.com/tumashu/posframe.git" "3b97dc1"
inst "rainbow-delimiters" "https://github.com/Fanael/rainbow-delimiters.git" "a32b39b"
inst "rainbow-mode" "https://github.com/emacsmirror/rainbow-mode.git" "5c6c4b1"
inst "rg" "https://github.com/dajva/rg.el.git" "e9dc4ed"
inst "s" "https://github.com/magnars/s.el.git" "dda84d3"
inst "shrink-path" "https://gitlab.com/bennya/shrink-path.el.git" "c14882c"
inst "shut-up" "https://github.com/cask/shut-up.git" "ff6f06f"
inst "smex" "https://github.com/nonsequitur/smex.git" "55aaebe"
inst "super-save" "https://github.com/bbatsov/super-save.git" "6f6512b"
inst "swiper" "https://github.com/abo-abo/swiper.git" "d28225e"
inst "symbol-overlay" "https://github.com/wolray/symbol-overlay.git" "72ff963"
# Emacs-Lisp Library for converting S-expressions to TOML
inst "tomelr" "https://github.com/kaushalmodi/tomelr.git" "670e0a0" #Needed by ox-hugo
inst "transient" "https://github.com/magit/transient.git" "b412784"
inst "treemacs" "https://github.com/Alexander-Miller/treemacs.git" "a7a877a"
inst "treepy" "https://github.com/volrath/treepy.el.git" "7c4a0d2"
inst "ts" "https://github.com/alphapapa/ts.el.git" "5529360"
inst "vertico" "https://github.com/minad/vertico.git" "926234a"
inst "vertico-posframe" "https://github.com/tumashu/vertico-posframe.git" "7da6d64"
inst "visual-fill-column" "https://github.com/joostkremers/visual-fill-column.git" "577fd2d"
inst "w32-browser" "https://github.com/emacsorphanage/w32-browser" "e5c60ea"
inst "web-mode" "https://github.com/fxbois/web-mode.git" "57856ba"
inst "wgrep" "https://github.com/mhayashi1120/Emacs-wgrep.git" "3132abd"
inst "with-editor" "https://github.com/magit/with-editor.git" "bce8d1b"
inst "writeroom-mode" "https://github.com/joostkremers/writeroom-mode.git" "1fd5284"
inst "ws-butler" "https://github.com/lewang/ws-butler.git" "e3a38d9"
inst "xr" "https://github.com/mattiase/xr.git" "6200de8"
inst "yankpad" "https://github.com/Kungsgeten/yankpad.git" "927e6d2"
inst "yasnippet" "https://github.com/joaotavora/yasnippet.git" "5cbdbf0"
inst "citre" "https://github.com/universal-ctags/citre.git" "4626ada"
inst "ef-themes" "https://github.com/protesilaos/ef-themes.git" "e45e89"
inst "go-mode" "https://github.com/dominikh/go-mode.el.git" "f2134"
inst "powerline" "https://github.com/milkypostman/powerline.git" "c35c35b"
inst "centaur-tabs" "https://github.com/ema2159/centaur-tabs.git" "0bb1aa1"
inst "good-scroll" "https://github.com/io12/good-scroll.el.git" "a7ffd5"
inst "ctrlf" "https://github.com/raxod502/ctrlf.git" "9b4cf"
inst "popper" "https://github.com/karthink/popper.git" "a93ff3"
inst "holo-layer" "https://github.com/manateelazycat/holo-layer" "94ba9"


echo "All package install finished."
