;;; test.el --- test insert-japanese-day-of-the-week.el -*- lexical-binding: t; -*-
;; Copyright (C) 2025 Jun Morimoto
;; Author: Jun Morimoto <morimoto@gmail.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "24.1"))
;; Keywords: convenience, lisp, date
;; URL: https://github.com/mrmt/insert-japanese-day-of-the-week
;;
;;; Commentary:
;;; test insert-japanese-day-of-the-week.el
;;
;;; Code:

(require 'ert)
(ert-delete-all-tests)

(load-file "../insert-japanese-day-of-the-week.el")

(defun my-fixed-decode-time (&optional time zone)
  "2025年7月5日 00:00:00 土曜日 を返すモック."
  ;; (秒 分 時 日 月 年 曜日 DST tz)
  (list 0 0 0 5 7 2025 6 nil nil))

(defmacro my-deftest (test-name source-string result-string)
  "テストを生成するマクロ.
Argument TEST-NAME テスト名称.
Argument SOURCE-STRING テストしたい日付文字列.
Argument RESULT-STRING 望まれる結果文字列."
  `(ert-deftest ,test-name ()
     ",test-name形式に曜日が付くことをテスト"
     (cl-letf (((symbol-function 'decode-time) #'my-fixed-decode-time))
       (with-temp-buffer
         (insert ,source-string)
         (goto-char (point-max))
         (insert-japanese-day-of-the-week)
         (should (string= (buffer-string) ,result-string))))))

(my-deftest yyyy-mm-dd-slash
            "今日は2025/07/05"
            "今日は2025/07/05(土)")
(my-deftest yyyy-mm-d-slash
            "今日は2025/07/5"
            "今日は2025/07/5(土)")
(my-deftest yyyy-m-dd-slash
            "今日は2025/7/05"
            "今日は2025/7/05(土)")
(my-deftest mm-dd-slash
            "今日は07/05"
            "今日は07/05(土)")
(my-deftest mm-d-slash
            "今日は07/5"
            "今日は07/5(土)")
(my-deftest m-dd-slash
            "今日は7/05"
            "今日は7/05(土)")
(my-deftest m-d-slash
            "今日は7/5"
            "今日は7/5(土)")

(my-deftest yyyy-mm-dd-dot
            "今日は2025.07.05"
            "今日は2025.07.05(土)")
(my-deftest yyyy-mm-d-dot
            "今日は2025.07.5"
            "今日は2025.07.5(土)")
(my-deftest yyyy-m-dd-dot
            "今日は2025.7.05"
            "今日は2025.7.05(土)")
(my-deftest mm-dd-dot
            "今日は07.05"
            "今日は07.05(土)")
(my-deftest mm-d-dot
            "今日は07.5"
            "今日は07.5(土)")
(my-deftest m-dd-dot
            "今日は7.05"
            "今日は7.05(土)")
(my-deftest m-d-dot
            "今日は7.5"
            "今日は7.5(土)")

(my-deftest yyyy-mm-dd-japanese
            "今日は2025年07月05日"
            "今日は2025年07月05日(土)")
(my-deftest yyyy-mm-d-japanese
            "今日は2025年07月5日"
            "今日は2025年07月5日(土)")
(my-deftest yyyy-m-dd-japanese
            "今日は2025年7月05日"
            "今日は2025年7月05日(土)")
(my-deftest mm-dd-japanese
            "今日は07月05日"
            "今日は07月05日(土)")
(my-deftest mm-d-japanese
            "今日は07月5日"
            "今日は07月5日(土)")
(my-deftest m-dd-japanese
            "今日は7月05日"
            "今日は7月05日(土)")
(my-deftest m-d-japanese
            "今日は7月5日"
            "今日は7月5日(土)")

;; 全テスト実行方法
;; M-x eval-buffer RET
;; M-x ert RET t RET

(provide 'test)

;;; test.el ends here
