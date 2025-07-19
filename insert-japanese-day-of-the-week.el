;;; insert-japanese-day-of-the-week.el --- 日付文字列の直後に曜日を挿入 -*- lexical-binding: t; -*-
;; Copyright (C) 2025 Jun Morimoto
;; Author: Jun Morimoto <morimoto@gmail.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "24.1"))
;; Keywords: convenience, lisp, date
;; URL: https://github.com/mrmt/insert-japanese-day-of-the-week
;;
;;; Commentary:
;; このパッケージは、日付文字列の直後に日本語の曜日を挿入するものです
;; 以下の例で、★にpointを置いて実行すると、実行後のようになります。
;; 今日は2025年7月5日★ → 今日は2025年7月5日(土)
;; 今日は2025/07/05★ → 今日は2025/07/05(土)
;; 今日は2025.7.5★ → 今日は2025.7.5(土)
;;
;;; Code:
(require 'cl-lib)

(defun insert-japanese-day-of-the-week ()
  "日付文字列の直後に曜日を挿入します."
  (interactive "")
  (save-excursion
    (save-restriction
      (let* ((saved-point (point))
             (parsed-time
              (cl-block parse-date
                (let ((date-parse-mode nil)
                      (year (nth 5 (decode-time)))
                      (month 0)
                      (day 0)
                      (s)
                      (backward-and-get-char
                       (lambda ()
                         (backward-char)
                         (buffer-substring-no-properties (point) (1+ (point))))))

                  ;; 1文字後退して文字を取得
                  (setq s (funcall backward-and-get-char))

                  ;; 「日」なら日本語モードにセットして後退・取得
                  (when (string-match-p "日" s)
                    (setq date-parse-mode 'japanese)
                    (setq s (funcall backward-and-get-char)))

                  ;; 日の1の位の数字ならセットし、後退・取得
                  (if (string-match-p "[0-9]" s)
                      (progn
                        (setq day (string-to-number s))
                        (setq s (funcall backward-and-get-char)))
                    (cl-return-from parse-date nil))

                  ;; 日の10の位の数字なら日付に加算し、後退・取得
                  (when (string-match-p "[0-9]" s)
                    (setq day (+ day (* 10 (string-to-number s))))
                    (setq s (funcall backward-and-get-char)))

                  ;; 月と日のセパレータでなければならない
                  (cond
                   ;; スラッシュ区切りの日付表記のようだ
                   ((and (string-match-p "/" s)
                         (not (eq date-parse-mode 'japanese)))
                    (setq date-parse-mode 'slash))
                   ;; ドット区切りの日付表記のようだ
                   ((and (string-match-p "\\." s)
                         (not (eq date-parse-mode 'japanese)))
                    (setq date-parse-mode 'dot))
                   ;; 「月」「日」区切りの日付表記のようだ
                   ((and (string-match-p "月" s)
                         (eq date-parse-mode 'japanese))
                    t)
                   ;; いずれでもない。異常
                   (t 
                    (cl-return-from parse-date nil)))

                  ;; 後退・取得
                  (setq s (funcall backward-and-get-char))

                  ;; 月の1の位の数字ならセット、後退・取得
                  (if (string-match-p "[0-9]" s)
                      (progn
                        (setq month (string-to-number s))
                        (setq s (funcall backward-and-get-char)))
                    (cl-return-from parse-date nil))

                  ;; 月の10の位の数字なら日付に加算し、後退・取得
                  (when (string-match-p "[0-9]" s)
                    (setq month (+ month (* 10 (string-to-number s))))
                    (setq s (funcall backward-and-get-char)))

                  ;; 年と月のセパレータか、あるいは日付文字列の終わり
                  (cond
                   ;; スラッシュ区切りの日付表記のようだ
                   ((and (string-match-p "/" s)
                         (eq date-parse-mode 'slash))
                    t)
                   ;; ドット区切りの日付表記のようだ
                   ((and (string-match-p "\\." s)
                         (eq date-parse-mode 'dot))
                    t)
                   ;; 「年」「月」「日」区切りの日付表記のようだ
                   ((and (string-match-p "年" s)
                         (eq date-parse-mode 'japanese))
                    t)
                   ;; いずれでもない。日付文字列の終わり。
                   (t
                    (cl-return-from parse-date
                      (encode-time 0 0 0 day month year))))

                  ;; 年の1の位の数字なら日付に加算し、後退・取得
                  (setq s (funcall backward-and-get-char))
                  (if (string-match-p "[0-9]" s)
                      (setq year (string-to-number s))
                    (cl-return-from parse-date nil))

                  ;; 年の10の位の数字なら日付に加算し、後退・取得
                  (setq s (funcall backward-and-get-char))
                  (if (string-match-p "[0-9]" s)
                      (setq year (+ year (* 10 (string-to-number s))))
                    (cl-return-from parse-date nil))

                  ;; 年の100の位の数字なら日付に加算し、後退・取得
                  (setq s (funcall backward-and-get-char))
                  (if (string-match-p "[0-9]" s)
                      (setq year (+ year (* 100 (string-to-number s))))
                    (cl-return-from parse-date nil))

                  ;; 年の1000の位の数字なら日付に加算し、後退・取得
                  (setq s (funcall backward-and-get-char))
                  (if (string-match-p "[0-9]" s)
                      (setq year (+ year (* 1000 (string-to-number s))))
                    (cl-return-from parse-date nil))

                  (cl-return-from parse-date
                    (encode-time 0 0 0 day month year))))))

        (if parsed-time
            (let* ((tbl '((0 . "日")
                          (1 . "月")
                          (2 . "火")
                          (3 . "水")
                          (4 . "木")
                          (5 . "金")
                          (6 . "土")))
                   (decoded-time (decode-time parsed-time))
                   (wday (cdr (assoc (decoded-time-weekday decoded-time) tbl))))
              (message "%s/%s/%sは%s曜日です"
                       (decoded-time-year decoded-time)
                       (decoded-time-month decoded-time)
                       (decoded-time-day decoded-time)
                       wday)
              (goto-char saved-point)
              (insert (format "(%s)" wday)))
          (message "日付が見つかりませんでした"))))))

(provide 'insert-japanese-day-of-the-week)

;;; insert-japanese-day-of-the-week.el ends here
