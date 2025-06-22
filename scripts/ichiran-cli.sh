#!/bin/bash

# Ichiran CLI wrapper script
# This script provides a command-line interface to ichiran functionality

# Set default options
FORMAT="text"
INPUT=""
ROMANIZE=false
KANJI_MODE=false
HELP=false

# Parse command line arguments
while [[ $# -gt 0 ]]; do
    case $1 in
        -f|--format)
            FORMAT="$2"
            shift 2
            ;;
        -i|--input)
            INPUT="$2"
            shift 2
            ;;
        -r|--romanize)
            ROMANIZE=true
            shift
            ;;
        -k|--kanji)
            KANJI_MODE=true
            shift
            ;;
        -h|--help)
            HELP=true
            shift
            ;;
        *)
            INPUT="$1"
            shift
            ;;
    esac
done

# Show help
if [ "$HELP" = true ]; then
    echo "Ichiran CLI - Japanese Text Analysis Tool"
    echo ""
    echo "Usage: ichiran-cli [OPTIONS] [TEXT]"
    echo ""
    echo "Options:"
    echo "  -f, --format FORMAT    Output format (text, json)"
    echo "  -i, --input TEXT       Input text to analyze"
    echo "  -r, --romanize        Romanize text"
    echo "  -k, --kanji           Kanji analysis mode"
    echo "  -h, --help            Show this help message"
    echo ""
    echo "Examples:"
    echo "  ichiran-cli -i \"こんにちは\""
    echo "  ichiran-cli -r -i \"こんにちは\""
    echo "  ichiran-cli -f json -i \"こんにちは\""
    echo "  ichiran-cli -k -i \"漢\""
    exit 0
fi

# Check if input is provided
if [ -z "$INPUT" ]; then
    echo "Error: No input text provided"
    echo "Use -h or --help for usage information"
    exit 1
fi

# Create temporary Lisp script
TEMP_SCRIPT="/tmp/ichiran_cli_$$.lisp"

cat > "$TEMP_SCRIPT" << EOF
(require :asdf)
(load (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname)))
(ql:quickload :ichiran :silent t)
(load "/app/settings.lisp")

(defun format-output (result format-type)
  (cond
    ((string= format-type "json")
     (format t "{\"result\": ~s}~%" result))
    (t
     (format t "~a~%" result))))

(defun main ()
  (handler-case
    (let ((input-text "$INPUT"))
      (cond
        ($ROMANIZE
         (let ((result (ichiran:romanize input-text)))
           (format-output result "$FORMAT")))
        ($KANJI_MODE
         (let ((result (ichiran:get-kanji-info input-text)))
           (format-output result "$FORMAT")))
        (t
         (let ((result (ichiran:romanize input-text :with-info t)))
           (format-output result "$FORMAT")))))
    (error (e)
      (format t "Error: ~a~%" e)
      (sb-ext:exit :code 1))))

(main)
(sb-ext:exit :code 0)
EOF

# Execute the Lisp script
sbcl --script "$TEMP_SCRIPT"
EXIT_CODE=$?

# Clean up
rm -f "$TEMP_SCRIPT"

exit $EXIT_CODE
