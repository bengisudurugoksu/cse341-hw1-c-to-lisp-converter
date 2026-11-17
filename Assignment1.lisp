(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))

(quicklisp:quickload "cl-ppcre")
(use-package :cl-ppcre)

(defun split-string (regex string)
  (cl-ppcre:split regex string))

(defun split-sequence (delimiter string)
  "Recursively split STRING by DELIMITER into a list of substrings."
  (let ((pos (search delimiter string)))
    (if pos
        (cons (subseq string 0 pos)
              (split-sequence delimiter (subseq string (+ pos (length delimiter)))))
        (list string))))


(defun string-prefix-p (prefix str)
  "Check if PREFIX is a prefix of STR."
  (and (<= (length prefix) (length str))
       (string= prefix (subseq str 0 (length prefix)))))

(defun replace-string (string old new)
  "Replace OLD substring with NEW substring in STRING immutably."
  (labels ((replace-helper (str start)
             (let ((pos (position old str :start start)))
               (if pos
                   (concatenate 'string
                                (subseq str start pos)
                                new
                                (replace-helper str (+ pos (length old))))
                   (subseq str start)))))
    (replace-helper string 0)))



(defun replace-in-string (str replacements)
  "Replace substrings in STR according to REPLACEMENTS."
  (reduce (lambda (s pair)
            (replace-string s (first pair) (second pair)))
          replacements
          :initial-value str))



(defun line-type (line)
  "Determine the type of a C line and return a corresponding symbol."
  (let ((trimmed-line (string-trim '(#\Space #\Tab) line)))
    (cond
      ((string-prefix-p "//" trimmed-line) 'comment)
      ((string-prefix-p "#" trimmed-line) 'preprocessor)
      ((string= trimmed-line "}") 'close-brace)
      ((and (string-prefix-p "if" trimmed-line) (search "(" trimmed-line)) 'if-statements)
      ((and (string-prefix-p "for" trimmed-line) (search "(" trimmed-line)) 'for-loops)
      ((and (string-prefix-p "while" trimmed-line) (search "(" trimmed-line)) 'while-loops)
      ((string-prefix-p "printf" trimmed-line) 'print-statement)

      ;; Check for definition-by-function-return pattern
      ((cl-ppcre:scan "^\\s*(int|float|double|char|long)\\s+\\w+\\s*=\\s*\\w+\\s*\\(.*\\);" trimmed-line)
       'definition-by-function-return)
      
      ;; Check for assignment-by-function-return pattern
      ((and (search "=" trimmed-line)
            (search "(" trimmed-line)
            (search ")" trimmed-line)
            (search ";" trimmed-line))
       'assignment-by-function-return)

      ;; Variable assignment pattern
      ((cl-ppcre:scan "^\\s*\\w+\\s*=.*;" trimmed-line) 'variable-assignment) 

      ;; Increment or decrement
      ((or (search "++" trimmed-line)
           (search "--" trimmed-line)) 'increment-decrement)

      ;; Variable definition with type
      ((and (search "=" trimmed-line) 
            (search ";" trimmed-line) 
            (search " " trimmed-line)) 'variable-definition)

      ;; Function definition
      ((and (search " " trimmed-line)
            (search "(" trimmed-line)
            (search "{" trimmed-line)) 'function-definition)

      ;; Function prototype
      ((and (search " " trimmed-line)
            (search "(" trimmed-line)
            (search ";" trimmed-line)) 'function-prototype)

      ;; Function call
      ((and (search "(" trimmed-line)
            (search ")" trimmed-line)
            (search ";" trimmed-line)) 'function-call)

      ;; Return statement
      ((and (search " " trimmed-line) 
            (search ";" trimmed-line) 
            (search "return" trimmed-line)) 'return-statement)
      
      (t 'unknown))))




(defun convert-comment (line)
  "Convert comments from C to Lisp."
  (format nil "; ~a" (subseq line 2))) ; This converts // comment to ; comment.


(defun convert-preprocessor (_)
  "Ignore preprocessor directives."
  "")

(defun convert-increment-decrement (line)
  "Convert C increment or decrement operations to Lisp."
  (cond
    ((search "++" line) ; Check for increment
     (let* ((var (string-trim " " (subseq line 0 (position #\+ line))))
            (converted (format nil "(~a (+ 1 ~a))" var var)))
       converted))
    ((search "--" line) ; Check for decrement
     (let* ((var (string-trim " " (subseq line 0 (position #\- line))))
            (converted (format nil "(~a (- ~a 1))" var var)))
       converted))
    (t line))) ; Return the line unchanged if no match




(defun convert-variable-definition (line)
  "Convert a C variable definition to Lisp in an immutable way."
  (let* ((trimmed-line (string-trim '(#\Space #\Tab) line))
         (parts (split-sequence " " trimmed-line))
         (variable (second parts))
         (init-value (when (search "=" trimmed-line)
                       (string-trim '(#\Space #\Tab #\;) 
                                    (second (split-sequence "=" trimmed-line))))))
    ;; Convert init-value to prefix notation if it exists
    (format nil "(let ((~a ~a))" variable (if init-value (convert-to-prefix init-value) "nil"))))




(defun convert-close-brace (line &optional (in-progn nil))
  (if in-progn
      (progn
        (setf in-progn nil)  ; Reset in-progn to false after adding extra parentheses
        (values (format nil "))") nil))  ; Return the closing parenthesis and new in-progn state
      (values (format nil ")") nil)))  ; Just return a closing parenthesis normally




(defun convert-variable-assignment (line)
  "Convert a variable assignment in C to Lisp immutably."
  (let ((parts (split-sequence "=" line)))
    (let ((variable (string-trim '(#\Space #\Tab) (first parts)))  ; Get the variable name
          (value (string-trim '(#\Space #\Tab #\;) (second parts))))  ; Get the value, trim spaces and semicolon
      (format nil "(setf ~a ~a)" variable (convert-to-prefix value)))))  ; Convert value to prefix


(defun convert-for (line)
  "Convert a for loop."
  (let* ((for-args (second (split-sequence "(" line)))
         (parts (split-sequence ";" for-args))
         (initialization (string-trim " " (first parts)))  ; Trim whitespace from initialization
         (condition (second parts))
         (iteration (third parts))
         (init-parts (split-sequence " " initialization))  ; Split initialization by spaces
         (init-var (second init-parts))                   ; Extract variable name (second part)
         (init-value (when (search "=" initialization)      ; Extract the initial value after '='
                       (string-trim " " (second (split-sequence "=" initialization)))))
         (converted-condition (convert-operation (string-trim " " condition))) ; Convert condition
         (converted-iteration (convert-operation (string-trim " " iteration)))) ; Convert iteration
         
    (let ((upper-bound (when (search "<" condition)
                         (string-trim " " (second (split-sequence "<" condition))))))

      (format nil "(loop for ~a from ~a to ~a do)" 
              init-var (string-trim " " init-value) upper-bound))))

(defun convert-if (line)
  "Convert an if statement."
  (let* ((condition (second (split-sequence "(" line)))
         (raw-line (string-trim " \n\t" line))
         (raw-condition (if condition (first (split-sequence ")" condition)) ""))
         ;; Trim leading/trailing whitespace and parentheses
         (trimmed-condition (string-trim " \n\t{}" (string-trim "()" raw-condition)))
         ;; Split conditions by logical operators
         (or-parts (split-sequence "||" trimmed-condition)))

    ;; Construct the prefix condition
    (let ((prefix-condition
           (mapconcat
            (lambda (or-part)
              (let ((and-parts (split-sequence "&&" or-part)))
                (if (= (length and-parts) 1)
                    (convert-to-prefix (first and-parts))
                    (format nil "(and ~a)" 
                            (mapconcat 'convert-to-prefix and-parts " ")))))
            or-parts " ")))

      ;; Wrap the whole condition in or if there are multiple or-parts
      (if (> (length or-parts) 1)
          (format nil "(if (or ~a)~%    (progn" prefix-condition)
          (format nil "(if ~a~%    (progn" prefix-condition)))))



(defun convert-while (line)
  "Convert a while loop statement."
  (let* ((condition (second (split-sequence "(" line)))
         ;; Trim the closing parenthesis and any leading/trailing whitespace
         (trimmed-condition (string-trim " {" (string-trim ")" condition)))
         ;; Replace the C-style operators with Lisp equivalents
         (updated-condition (convert-operation trimmed-condition))
         ;; Split the updated condition by spaces to isolate parts
         (condition-parts (split-sequence " " updated-condition))
         ;; Assuming the first part is the left operand, operator, and right operand
         (first-part (when condition-parts (first condition-parts)))
         (operator (when condition-parts (second condition-parts)))
         (second-part (when condition-parts (third condition-parts)))
         ;; Format in prefix notation
         (prefix-condition (format nil "(~a ~a ~a)" operator
                                   (convert-to-prefix first-part)
                                   (convert-to-prefix second-part))))
    ;; Remove trailing space and parentheses
    (let ((final-condition (string-trim " " (string-trim ")" prefix-condition))))
      ;; Format the output correctly with (progn) on a new line
      (format nil "(loop while ~a)~%    (progn" final-condition))))


  

(defun convert-function-definition (line)
  "Convert a C function definition to a Lisp function definition."
  (let* ((parts (split-sequence "(" line))
         (before-paren (first parts))
         (after-paren (second parts))
         (name-part (split-sequence " " (string-trim '(#\Space #\Tab) before-paren)))
         (function-name (car (last name-part))) 
         (cleaned-args (string-trim '(#\Space #\Tab #\)) after-paren)) ;; Trim closing parenthesis here
         ;; Split arguments by comma
         (arg-names (mapcar (lambda (arg)
                              (let* ((trimmed-arg (string-trim '(#\Space #\Tab #\, #\) #\{ #\}) arg)) ;; Trim spaces, commas, braces, and parentheses from each arg
                                     (split-arg (split-sequence " " trimmed-arg)) ;; Split by space
                                     (name (car (last split-arg)))) ;; Get the last element (argument name)
                                name))
                            (split-sequence "," cleaned-args))) ;; Split by commas
         ;; Join argument names into a single string
         (formatted-args (mapconcat #'identity arg-names " "))
         ;; Correct format string for the Lisp function definition
         (format "(defun ~a (~a)")) ;; Remove the extra closing parenthesis
    ;; Return the formatted Lisp function definition
    (format nil format function-name formatted-args)))


(defun string-join (strings separator)
  "Join STRINGS with SEPARATOR."
  (reduce (lambda (acc string)
            (if (string= acc "") string (format nil "~a~a~a" acc separator string)))
          strings))

(defun convert-function-prototype (line)
  (let* ((clean-line (subseq line 0 (position #\; line))) ; Remove the semicolon
         (paren-pos (position #\( clean-line))            ; Find position of '('
         (return-type (subseq clean-line 0 (position #\space clean-line)))  ; Extract return type
         (function-name (subseq clean-line (1+ (position #\space clean-line)) paren-pos)) ; Extract function name
         (args-str (subseq clean-line (1+ paren-pos) (position #\) clean-line))) ; Extract argument string
         (args-list (split-sequence ", " args-str))  ; Split arguments by comma and space
         (arg-types (mapcar (lambda (arg)
                              (first (split-sequence " " arg))) ; Extract argument types
                            args-list))
         (arg-type-symbols (mapcar (lambda (type)
                                     (cond
                                       ((string= type "int") 'integer)  ; Map "int" to 'integer
                                       ((string= type "float") 'float)  ; Map "float" to 'float
                                       ((string= type "double") 'double) ; Map "double" to 'double
                                       (t (intern (string-downcase type))))) ; Other types to symbols
                                   arg-types))
         (return-type-symbol (cond
                               ((string= return-type "int") 'integer)
                               ((string= return-type "float") 'float)
                               ((string= return-type "double") 'double)
                               (t (intern (string-downcase return-type))))))


    ;; Construct the ftype with correct parentheses format
    (let ((output `(declaim (ftype (function ,@(list arg-type-symbols return-type-symbol))
                                   ,(intern (string-downcase function-name))))))
      ;; Return the lowercase output string without altering it
      (string-downcase (princ-to-string output)))))




(defun convert-function-call (line)
  "Convert a function call to Lisp."
  (let ((function-call (subseq line 0 (search ";" line))))
    (format nil "(~a)" function-call)))

(defun convert-assignment-by-function-return (line)
  "Convert an assignment by a function return in C to Lisp."
  (let* ((parts (split-sequence "=" line))
         (variable (string-trim '(#\Space #\Tab) (first parts)))  ; Get the variable name
         (function-call (string-trim '(#\Space #\Tab #\;) (second parts))) ; Get the function call
         ;; Extract the function name and arguments
         (func-name (string-trim '(#\Space #\Tab) (subseq function-call 0 (search "(" function-call))))
         (arguments (subseq function-call (1+ (search "(" function-call)) (search ")" function-call)))
         ;; Split arguments by commas and trim each
         (arg-list (mapcar (lambda (arg) (string-trim '(#\Space #\Tab) arg))
                           (split-sequence "," arguments))))
    ;; Format the Lisp setf statement with the lowercase function name and arguments
    (format nil "(setf ~a (~a ~{~a~^ ~}))"
            variable
            (string-downcase func-name)  ; Force func-name to be lowercase
            arg-list)))


(defun convert-operation (line)
  "Convert logical or arithmetic operations from C code to Lisp."
  (replace-in-string line
    '(("+" " + ")
      ("-" " - ")
      ("*" " * ")
      ("/" " / ")
      ("%" " mod ")
      ("&&" " and ")
      ("||" " or ")
      ("!" " not ")
      ("<" " < ")
      (">" " > ")
      ("<=" " <= ")
      (">=" " >= ")
      ("==" " = "))))


(defun split-string (string delimiter)
  "Split STRING into substrings separated by DELIMITER."
  (let ((result nil)
        (start 0)
        (end 0))
    (loop
      (setq end (position delimiter string :start start))
      (if end
          (progn
            (push (subseq string start end) result)
            (setq start (1+ end))) ; Move past the delimiter
          (progn
            (push (subseq string start) result) ; Push the remaining part if any
            (return))))
    (nreverse result))) ; Return the result in original order



(defun convert-print (line)
  "Convert a C printf statement to Lisp format."
  (when (cl-ppcre:scan "^\\s*printf\\s*\\((.*)\\);" line)
    (let* ((printf-contents (second (cl-ppcre:split "[()]" line)))
           ;; Extract format string and variables separately
           (components (split-string printf-contents ","))

           ;; Assume the first component is the format string and the rest are arguments
           (initial-format-string (string-trim '(#\Space) (first components)))
           (variables (mapcar (lambda (x) (string-trim '(#\Space) x)) (rest components)))

           ;; Transform format specifiers immutably
           (formatted-string (cl-ppcre:regex-replace-all
                              "\\\\n"
                              (cl-ppcre:regex-replace-all
                               "%f"
                               (cl-ppcre:regex-replace-all "%d" initial-format-string "~a")
                               "~a")
                              "~%")))

      ;; Handle the case where there are no variables
      (if (null variables)
          (format nil "(format t ~a)~%" formatted-string)
          ;; Create the final output line using apply
          (format nil "(format t ~a ~{~a~^ ~})~%" formatted-string variables)))))



(defun convert-return (line)
  (let ((return-str (string-trim " " line))) ; Trim whitespace from both ends
    (if (and (string= (subseq return-str 0 6) "return") 
             (> (length return-str) 7)) ; Ensure there's something after "return"
        (let* ((return-value (subseq return-str 7))  ; Get the value after "return "
               (trimmed-return-value (string-trim " ;" return-value))) ; Trim spaces and semicolon
          (format nil "~a" 
                  (convert-to-prefix trimmed-return-value))) ; Convert to prefix notation
        line))) ; Return original line if not a return statement

(defun convert-to-prefix (trimmed-exp)
  "Convert a string expression TRIMMED-EXP to prefix notation."
  (if (string= trimmed-exp "")
      trimmed-exp
      (if (search "+" trimmed-exp)
          (let* ((parts (split-sequence "+" trimmed-exp))
                 (first-part (string-trim " " (first parts)))
                 (second-part (string-trim " " (second parts))))
            (format nil "(+ ~a ~a)" 
                    (convert-to-prefix first-part)
                    (convert-to-prefix second-part)))
          (if(search "-" trimmed-exp)
          (let* ((parts (split-sequence "-" trimmed-exp))
                 (first-part (string-trim " " (first parts)))
                 (second-part (string-trim " " (second parts))))
            (format nil "(- ~a ~a)" 
                    (convert-to-prefix first-part)
                    (convert-to-prefix second-part)))
          (if(search "/" trimmed-exp)
          (let* ((parts (split-sequence "/" trimmed-exp))
                 (first-part (string-trim " " (first parts)))
                 (second-part (string-trim " " (second parts))))
            (format nil "(/ ~a ~a)" 
                    (convert-to-prefix first-part)
                    (convert-to-prefix second-part)))
          (if(search "*" trimmed-exp)
          (let* ((parts (split-sequence "*" trimmed-exp))
                 (first-part (string-trim " " (first parts)))
                 (second-part (string-trim " " (second parts))))
            (format nil "(* ~a ~a)" 
                    (convert-to-prefix first-part)
                    (convert-to-prefix second-part)))
          (if(search "%" trimmed-exp)
          (let* ((parts (split-sequence "%" trimmed-exp))
                 (first-part (string-trim " " (first parts)))
                 (second-part (string-trim " " (second parts))))
            (format nil "(mod ~a ~a)" 
                    (convert-to-prefix first-part)
                    (convert-to-prefix second-part)))                
                (if (search ">" trimmed-exp)
          (let* ((parts (split-sequence ">" trimmed-exp))
                 (first-part (string-trim " " (first parts)))
                 (second-part (string-trim " " (second parts))))
            (format nil "(> ~a ~a)" 
                    (convert-to-prefix first-part)
                    (convert-to-prefix second-part)))
          (if(search "<" trimmed-exp)
          (let* ((parts (split-sequence "<" trimmed-exp))
                 (first-part (string-trim " " (first parts)))
                 (second-part (string-trim " " (second parts))))
            (format nil "(< ~a ~a)" 
                    (convert-to-prefix first-part)
                    (convert-to-prefix second-part)))
          (if(search ">=" trimmed-exp)
          (let* ((parts (split-sequence ">=" trimmed-exp))
                 (first-part (string-trim " " (first parts)))
                 (second-part (string-trim " " (second parts))))
            (format nil "(>= ~a ~a)" 
                    (convert-to-prefix first-part)
                    (convert-to-prefix second-part)))
          (if(search "<=" trimmed-exp)
          (let* ((parts (split-sequence "<=" trimmed-exp))
                 (first-part (string-trim " " (first parts)))
                 (second-part (string-trim " " (second parts))))
            (format nil "(<= ~a ~a)" 
                    (convert-to-prefix first-part)
                    (convert-to-prefix second-part)))
          (if(search "==" trimmed-exp)
          (let* ((parts (split-sequence "==" trimmed-exp))
                 (first-part (string-trim " " (first parts)))
                 (second-part (string-trim " " (second parts))))
            (format nil "(= ~a ~a)" 
                    (convert-to-prefix first-part)
                    (convert-to-prefix second-part)))
          (if(search "!=" trimmed-exp)
          (let* ((parts (split-sequence "!=" trimmed-exp))
                 (first-part (string-trim " " (first parts)))
                 (second-part (string-trim " " (second parts))))
            (format nil "(/= ~a ~a)" 
                    (convert-to-prefix first-part)
                    (convert-to-prefix second-part)))   
          (if(search "&&" trimmed-exp)
          (let* ((parts (split-sequence "&&" trimmed-exp))
                 (first-part (string-trim " " (first parts)))
                 (second-part (string-trim " " (second parts))))
            (format nil "(and ~a ~a)" 
                    (convert-to-prefix first-part)
                    (convert-to-prefix second-part)))   
          (if(search "||" trimmed-exp)
          (let* ((parts (split-sequence "||" trimmed-exp))
                 (first-part (string-trim " " (first parts)))
                 (second-part (string-trim " " (second parts))))
            (format nil "(or ~a ~a)" 
                    (convert-to-prefix first-part)
                    (convert-to-prefix second-part)))   
                                          
          trimmed-exp)))))))))))))))
          
(defun convert-definition-by-function-return (line)
  "Convert a C variable definition by function return to Lisp using let."
  (let* ((parts (split-sequence "=" line))
         ;; Extract the type and variable name from the left part
         (type-and-variable (string-trim '(#\Space #\Tab) (first parts)))
         (variable (second (split-sequence " " type-and-variable))) ; Get the variable name
         ;; Get the function call on the right side
         (function-call (string-trim '(#\Space #\Tab #\;) (second parts)))
         (func-name (string-downcase (string-trim '(#\Space #\Tab) (subseq function-call 0 (search "(" function-call)))))
         ;; Extract and clean up arguments
         (arguments (subseq function-call (1+ (search "(" function-call)) (search ")" function-call)))
         (arg-list (if (string= arguments "")
                       '()
                       (mapcar (lambda (arg) (string-trim '(#\Space #\Tab) arg))
                               (split-sequence "," arguments)))))
    
    ;; Format the Lisp let expression
    (format nil "(let ((~a (~a ~{~a~^ ~})))" variable func-name arg-list)))


(defun convert (lines in-progn)
  (if (null lines)  ; Base case: if there are no lines to process
      nil
      (let* ((line (first lines))  ; Get the first line
             (line-type-symbol (line-type line))
             (conversion-function
              (case line-type-symbol
                (comment 'convert-comment)
                (preprocessor 'convert-preprocessor)
                (variable-definition 'convert-variable-definition)
                (variable-assignment 'convert-variable-assignment)
                (function-definition 'convert-function-definition)
                (function-prototype 'convert-function-prototype)
                (function-call 'convert-function-call)
                (for-loops 'convert-for)
                (if-statements 'convert-if)
                (print-statement 'convert-print)
                (close-brace 'convert-close-brace)
                (return-statement 'convert-return)
                (while-loops 'convert-while)
                (definition-by-function-return 'convert-definition-by-function-return)
                (assignment-by-function-return 'convert-assignment-by-function-return)
                (increment-decrement 'convert-increment-decrement)
                (t (lambda (l) (format nil "; Unhandled line type: ~a" l)))))) 
        (format t "~%Processing line: ~a => Type: ~a~%" line line-type-symbol)

        ;; Convert the line
        (let ((converted (funcall conversion-function line)))  ; Convert the first line
          ;; Check for "progn" in the converted line
          (when (search "progn" (string-downcase converted))
            (setf in-progn t))  ; Set in-progn to true if "progn" is encountered in converted line 

          ;; Handle the close-brace case with updated in-progn
          (let ((final-converted (if (equal line-type-symbol 'close-brace)
                                     (multiple-value-bind (converted-string new-in-progn)
                                         (convert-close-brace line in-progn)
                                       (setf in-progn new-in-progn)  ; Update in-progn from the close-brace function
                                       converted-string)  ; Special case for close-brace
                                     converted)))  ; Convert the first line normally
            (cons final-converted (convert (rest lines) in-progn)))))))  ; Recur on the rest of the lines



(defun mapconcat (fn list separator)
  "Apply FN to each element of LIST and concatenate the results with SEPARATOR."
  (if (null list)
      ""
      (reduce (lambda (a b) (concatenate 'string a separator b))
              (mapcar fn list))))


(defun read-file (filename)
  "Read a file line by line and return a list of lines."
  (with-open-file (stream filename :direction :input)
    (loop for line = (read-line stream nil)
          while line
          collect line)))

(defun write-file (filename lines)
  "Write LINES to FILENAME."
  (with-open-file (out filename :direction :output :if-exists :supersede)
    (dolist (line lines)
      (format out "~a~%" line))))


(defun main (input-file output-file)
  "Main function to convert C code to Lisp code without mutable state."
  (let* ((c-lines (read-file input-file))  ; Read lines immutably
         (converted-results (convert c-lines nil)))  ; Convert lines immutably
    ;; Write to output immutably
    (write-file output-file converted-results)))



(main "example.c" "output.lisp") 