;;; far.el --- minimal variance paragraph filling.

;; Package-Requires: (s dash)

;;; Commentary:

;; Reimplementation of far a dynamic programming based paragraph filler

;;; Code:

(require 's)
(require 'dash)
(require 'cl-lib)

(defun far--get-tokens (lines prefix)
  "Split LINES into tokens after skipping PREFIX."
  (-flatten (--map (split-string (substring it (length prefix))
                                 " \\|\n" t)
                   lines)))

(defun far--parse-prefix (lines)
  "Find the common prefix between LINES in a set of symbols."
  (let ((prefix nil)
        (smallest (-min (-map #'length lines)))
        (prefixes '(?# ?\; ?> ?: ?- ?* ?$ ?% ?/ ? )))
    (cl-loop for ch across (substring (-first-item lines) 0 smallest)
             with prefix = nil
             if (and (-contains? prefixes ch)
                     (--all? (string-prefix-p
                              (s-append (string ch) prefix) it)
                             lines))
             do (setq prefix (s-append (string ch) prefix))
             else return prefix)))

(defun far--get-lines (par width)
  "Forward greedy search on PAR word list, WIDTH is an int."
  (when (> (-max (-map #'length par)) width)
    (error "Longest word is larger than the width"))
  (cl-loop while (< i (length par))
           with i = 0
           and count = 0
           and lines = '(0)
           and v = 0
           do (setq v (length (elt par i)))
           do (cl-incf count)
           do (cl-loop while (and (< (+ i 1) (length par))
                                  (<= v width))
                       do (cl-incf i)
                       do (setq v (+ v 1 (length (elt par i))))
                       do (push count lines))
           if (<= v width)
           do (progn (cl-incf i)
                     (push count lines))
           finally return (reverse lines)))

(defun far--vardp (par lines width)
  "Generate the dp table from PAR, word list.
Uses LINES from get-lines and WIDTH constraint."
  (cl-loop for i from 1 to (length par)
           with dp = (make-vector (+ 1 (length par)) nil)
           with k and best and sum-x2 and sum-x and x
           initially do (setf (elt dp 0) '(0 0 0 0))
           do (setq k 0
                    best 9999999
                    sum-x2 0
                    sum-x 0
                    x 0)
           do (cl-loop for j from (- i 1) downto 0
                       with v and n
                       and sum-x2j and sum-xj
                       and mean and var
                       do (setq v (+ x
                                     (if (= x 0) 0 1)
                                     (length (elt par j))))
                       if (<= v width)
                       do (progn
                            (setq x v
                                  sum-x2j (-third-item (elt dp j))
                                  sum-xj (-fourth-item (elt dp j))
                                  n (+ 1 (elt lines j)))
                            (setq sum-x2j (+ sum-x2j (* x x))
                                  sum-xj (+ sum-xj x))
                            (setq mean (/ sum-xj (float n)))
                            (setq var (- (/ sum-x2j n) (* mean mean)))
                            (when (and (< var best)
                                       (= n (elt lines i)))
                              (setq k j
                                    best var
                                    sum-x2 sum-x2j
                                    sum-x sum-xj)))
                       else return nil)
           do (setf (elt dp i) (list k best sum-x2 sum-x))
           finally return dp))

(defun far--compute-k (par lines dp)
  "Computes the start point for refilling.
Uses PAR, word list, LINES, from get-lines, and DP, the dp table."
  (if (<= (-last-item lines) 3)
      (length par)
    (cl-loop for i from (- (length par) 1) downto 0
             with best = [1.0e+INF 1.0e+INF]
             and k = [0 0] and x = 0 and b
             do (setq x (+ x (if (= x 0) 0 1) (length (elt par i))))
             if (> x width)
             return (if (/= (elt best 1) 1.0e+INF) (elt k 1) (elt k 0))
             do (setq b
                      (if (<= x (/ (-last-item (elt dp i))
                                   (elt lines i)))
                          1 0))
             if (and (= (+ 1 (elt lines i)) (-last-item lines))
                     (< (-second-item (elt dp i)) (elt best b)))
             do (setf (elt best b) (-second-item (elt dp i))
                      (elt k b) i)
             finally return (if (/= (elt best 1) 1.0e+INF)
                                (elt k 1)
                              (elt k 0)))))

(defun far--process (text width)
  "Takes a raw string TEXT and return the wrapped string to WIDTH."
  (let* ((lines-raw (s-split "\n" text t))
         (prefix (far--parse-prefix lines-raw))
         (par (far--get-tokens lines-raw prefix))
         (lines (far--get-lines par width))
         (dp (far--vardp par lines width))
         (k (far--compute-k par lines dp))
         (out nil))
    (if (< k (length par))
        (push (s-join " " (-slice par k)) out))
    (cl-loop while (> i 0)
             with i = k and j
             do (setq j (-first-item (elt dp i)))
             do (push (s-join " " (-slice par j i)) out)
             do (setq i j))
    (s-join "\n" (--map (concat prefix it) out))))

(defvar far-fill-paragraph-width 80
  "Width to fill paragraphs.")

;;;###autoload
(defun far-fill-paragraph ()
  "Fills paragraph at point to far-fill-paragraph-width chars."
  (interactive)
  (let* ((para-end (save-excursion
                     (backward-paragraph)
                     (set-mark (point))
                     (forward-paragraph)
                     (backward-char)
                     (point)))
         (para-start (region-beginning))
         (para (buffer-substring-no-properties para-start para-end))
         (rewrapped (far--process para far-fill-paragraph-width)))
    ;; hack to make sure indentation is preserved correctly
    (replace-region-contents (+ (if (s-prefix? "\n" para) 1 0) para-start)
                             para-end (lambda () rewrapped))))

(provide 'far)
;;; far.el ends here