;;; -*- lexical-binding: t; -*-

;; GNU GCC 14.1 C++20
(defun compile-yandex-g++14.1 ()
  "Compile current .cpp file with GNU GCC 14.1 C++20."
  (interactive)
  (let* ((cpp-file (buffer-file-name))
         (build-file (replace-regexp-in-string "\\.cpp$" "" cpp-file))
         ;; g++ -O2 -lm -fno-stack-limit -std=c++20 -x c++ source.cpp -o executable
         (compile-command (format "g++ -O2 -lm -std=c++20 -x c++ %s -o %s"
                                  (shell-quote-argument cpp-file)
                                  (shell-quote-argument build-file))))

    (unless cpp-file
      (error "Buffer is not visiting a file"))

    (unless (string-match "\\.cpp$" cpp-file)
      (error "Not a C++ file"))

    ;; Compile the program
    (if (zerop (shell-command compile-command))
        ;; If compilation succeeds, run the executable
        (progn
          (message "Compilation successful. Running...")
          (async-shell-command build-file))
      ;; Show error message if compilation fails
      (error "Compilation failed"))))

;; Clang 17.0.1 C++20
(defun compile-yandex-clang++17.0.1 ()
  "Compile current .cpp file with Clang 17.0.1 C++20."
  (interactive)
  (let* ((cpp-file (buffer-file-name))
         (build-file (replace-regexp-in-string "\\.cpp$" "" cpp-file))
         ;; clang++ -std=c++20 -O2 -lm -x c++ source.cpp -o executable
         (compile-command (format "clang++ -std=c++20 -O2 -lm -x c++ %s -o %s"
                                  (shell-quote-argument cpp-file)
                                  (shell-quote-argument build-file))))

    (unless cpp-file
      (error "Buffer is not visiting a file"))

    (unless (string-match "\\.cpp$" cpp-file)
      (error "Not a C++ file"))

    ;; Compile the program
    (if (zerop (shell-command compile-command))
        ;; If compilation succeeds, run the executable
        (progn
          (message "Compilation successful. Running...")
          (async-shell-command build-file))
      ;; Show error message if compilation fails
      (error "Compilation failed"))))
