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

(defun insert-japanese-day-of-the-week ()
  "日付文字列の直後に曜日を挿入します."
  (interactive "")
  (let* ((tbl '((0 . "日")
                (1 . "月")
                (2 . "火")
                (3 . "水")
                (4 . "木")
                (5 . "金")
                (6 . "土")))
         ;; デフォルトの年は今年
         (year (nth 5 (decode-time)))
         month
         day
         (hour 0)
         (minute 0)
         (second 0)
         time)
    (save-excursion
      (save-restriction
        (let ((end (point)))
          ;; 行頭からカーソル位置までnarrow
          (beginning-of-line)
          (narrow-to-region (point) end)
          (goto-char end)
          (cond
           ;; yyyy年mm月dd日
           ((re-search-backward
             "\\b\\([0-9]\\{4\\}\\)年\\([0-9]\\{1,2\\}\\)月\\([0-9]\\{1,2\\}\\)日"
             nil t)
            (setq year  (string-to-number (match-string 1))
                  month (string-to-number (match-string 2))
                  day   (string-to-number (match-string 3))))
           ;; mm月dd日
           ((re-search-backward
             "\\b\\([0-9]\\{1,2\\}\\)月\\([0-9]\\{1,2\\}\\)日"
             nil t)
            (setq month (string-to-number (match-string 1))
                  day   (string-to-number (match-string 2))))
           ;; yyyy/mm/dd
           ((re-search-backward
             "\\b\\([0-9]\\{4\\}\\)/\\([0-9]\\{1,2\\}\\)/\\([0-9]\\{1,2\\}\\)"
             nil t)
            (setq year  (string-to-number (match-string 1))
                  month (string-to-number (match-string 2))
                  day   (string-to-number (match-string 3))))
           ;; mm/dd
           ((re-search-backward
             "\\b\\([0-9]\\{1,2\\}\\)/\\([0-9]\\{1,2\\}\\)"
             nil t)
            (setq month (string-to-number (match-string 1))
                  day   (string-to-number (match-string 2))))
           ;; yyyy.mm.dd
           ((re-search-backward
             "\\b\\([0-9]\\{4\\}\\)\\.\\([0-9]\\{1,2\\}\\)\\.\\([0-9]\\{1,2\\}\\)"
             nil t)
            (setq year  (string-to-number (match-string 1))
                  month (string-to-number (match-string 2))
                  day   (string-to-number (match-string 3))))
           ;; mm.dd
           ((re-search-backward
             "\\b\\([0-9]\\{1,2\\}\\)\\.\\([0-9]\\{1,2\\}\\)"
             nil t)
            (setq month (string-to-number (match-string 1))
                  day   (string-to-number (match-string 2)))))
          (if (and month day)
              (setq time (encode-time second minute hour day month year))))))
    (if time
        (insert
         (format "(%s)"
                 (cdr (assoc
                       (nth 6 (decode-time time))
                       tbl))))
      (message "日付が見つかりませんでした"))))


(provide 'insert-japanese-day-of-the-week)

;;; insert-japanese-day-of-the-week.el ends here
