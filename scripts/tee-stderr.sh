#!/bin/bash

# Check if at least one argument is provided
if [ $# -eq 0 ]; then
    echo "Usage: $0 <command> [arguments...]"
    exit 1
fi

# Set the name of the error log file
ERROR_LOG="/tmp/error_log.txt"

echo "wrapping: $@" >> $ERROR_LOG

# Run the command with its arguments, redirecting stderr to tee
"$@" 2> >(tee -a "$ERROR_LOG") 

# Capture the exit status of the command
exit_status=$?

# Print a message indicating where the error log is saved
echo "Errors (if any) have been logged to $ERROR_LOG"

# Exit with the same status as the original command
exit $exit_status
