# Unary function to exit with a non-zero exit code and an error message.
error_exit() {
    >&2 echo "$1"
    exit 1
}
